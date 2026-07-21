use crate::oomir::{self, BasicBlock, CodeBlock, Constant, Function, Operand, Type};
use std::collections::{BTreeSet, HashMap, HashSet};

use super::jvm;

pub(super) const METHOD_PREFIX: &str = "$outlined$";
pub(super) const PARAMETER_PREFIX: &str = "$oomir$";
pub(super) const RESTORE_PREFIX: &str = "$outlined$restore$";

const MAX_CHUNK_COST: usize = 900;
const UNWIND_EXCEPTION_LOCAL: &str = "__rust_unwind_exception";

type VariableTypes = HashMap<String, HashSet<Type>>;
type LiveVariables = HashMap<String, HashSet<Variable>>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Variable {
    name: String,
    ty: Type,
}

#[derive(Clone)]
struct Chunk {
    blocks: BTreeSet<String>,
    entries: Vec<String>,
    name: String,
    signature: oomir::Signature,
}

#[derive(Clone)]
struct CarrierArray {
    name: String,
    element_type: Type,
    variables: HashMap<Variable, i32>,
}

#[derive(Clone)]
struct CarrierPlan {
    arrays: Vec<CarrierArray>,
}

pub(super) fn outline_large_functions(module: &mut oomir::Module) -> jvm::Result<()> {
    let module_name = module.name.clone();
    let functions = std::mem::take(&mut module.functions);
    for (_, function) in functions {
        let owner = function
            .owner_class
            .clone()
            .unwrap_or_else(|| module_name.clone());
        for outlined in outline_function(function, &owner)? {
            module.insert_function(outlined);
        }
    }
    Ok(())
}

fn outline_function(function: Function, owner: &str) -> jvm::Result<Vec<Function>> {
    let total_cost = function
        .body
        .basic_blocks
        .values()
        .map(block_cost)
        .sum::<usize>();
    if total_cost <= MAX_CHUNK_COST || function.name == "<init>" {
        return Ok(vec![function]);
    }

    let successors = successor_map(&function.body);
    let components = topological_components(&function.body, &successors)?;
    let component_costs = components
        .iter()
        .map(|component| {
            component
                .iter()
                .map(|label| block_cost(&function.body.basic_blocks[label]))
                .sum::<usize>()
        })
        .collect::<Vec<_>>();
    let mut component_chunks = Vec::<Vec<usize>>::new();
    let mut current = Vec::new();
    let mut current_cost = 0usize;
    for (component, cost) in component_costs.iter().copied().enumerate() {
        if cost > MAX_CHUNK_COST {
            if !current.is_empty() {
                component_chunks.push(std::mem::take(&mut current));
                current_cost = 0;
            }
            component_chunks.push(vec![component]);
            continue;
        }
        if !current.is_empty() && current_cost.saturating_add(cost) > MAX_CHUNK_COST {
            component_chunks.push(std::mem::take(&mut current));
            current_cost = 0;
        }
        current.push(component);
        current_cost = current_cost.saturating_add(cost);
    }
    if !current.is_empty() {
        component_chunks.push(current);
    }
    if component_chunks.len() == 1 {
        return Ok(vec![function]);
    }

    let variable_types = collect_variable_types(&function)?;
    let live_in = live_variables(&function.body, &successors, &variable_types);
    let mut block_to_chunk = HashMap::new();
    let mut chunk_blocks = Vec::new();
    for (chunk_index, component_indices) in component_chunks.iter().enumerate() {
        let mut blocks = BTreeSet::new();
        for component_index in component_indices {
            for block in &components[*component_index] {
                blocks.insert(block.clone());
                block_to_chunk.insert(block.clone(), chunk_index);
            }
        }
        chunk_blocks.push(blocks);
    }
    let mut entries = vec![BTreeSet::new(); chunk_blocks.len()];
    entries[block_to_chunk[&function.body.entry]].insert(function.body.entry.clone());
    for (source, targets) in &successors {
        let source_chunk = block_to_chunk[source];
        for target in targets {
            let target_chunk = block_to_chunk[target];
            if source_chunk != target_chunk {
                entries[target_chunk].insert(target.clone());
            }
        }
    }
    let needs_carrier = entries.iter().skip(1).any(|entries| {
        let variables = entries
            .iter()
            .flat_map(|entry| live_in[entry].iter())
            .collect::<HashSet<_>>();
        let selector_slots = usize::from(entries.len() > 1);
        selector_slots
            + variables
                .iter()
                .map(|variable| type_slots(&variable.ty))
                .sum::<usize>()
            > 250
    });
    let carrier = needs_carrier.then(|| build_carrier_plan(&entries, &live_in));

    let identity = crate::stable_hash::short_hash(
        &format!("{owner}::{}{}", function.name, function.signature),
        12,
    );
    let mut chunks = Vec::new();
    for (index, blocks) in chunk_blocks.into_iter().enumerate() {
        let chunk_entries = entries[index].iter().cloned().collect::<Vec<_>>();
        if chunk_entries.is_empty() {
            return Err(verification_error(
                &function.name,
                format!("outlined chunk {index} has no control-flow entry"),
            ));
        }
        let mut signature_parameters = Vec::new();
        if chunk_entries.len() > 1 {
            signature_parameters
                .push((format!("{PARAMETER_PREFIX}__outlined_selector"), Type::I32));
        }
        if index > 0 {
            if let Some(carrier) = &carrier {
                signature_parameters.extend(carrier.arrays.iter().map(|array| {
                    (
                        format!("{PARAMETER_PREFIX}{}", array.name),
                        carrier_array_type(array),
                    )
                }));
            } else {
                let mut parameters = HashSet::new();
                for entry in &chunk_entries {
                    parameters.extend(live_in[entry].iter().cloned());
                }
                let mut parameters = parameters.into_iter().collect::<Vec<_>>();
                parameters.sort_by(compare_variables);
                signature_parameters.extend(
                    parameters.into_iter().map(|variable| {
                        (format!("{PARAMETER_PREFIX}{}", variable.name), variable.ty)
                    }),
                );
            }
        }
        chunks.push(Chunk {
            blocks,
            entries: chunk_entries,
            name: if index == 0 {
                function.name.clone()
            } else {
                format!("{METHOD_PREFIX}{}{identity}${index}", function.name)
            },
            signature: if index == 0 {
                function.signature.clone()
            } else {
                oomir::Signature {
                    params: signature_parameters,
                    ret: function.signature.ret.clone(),
                    is_static: true,
                }
            },
        });
    }

    let function_name = function.name.clone();
    let owner_class = function.owner_class.clone();
    let mut debug_variables = Some(function.debug_variables);
    let mut source_blocks = function.body.basic_blocks;
    let mut result = Vec::with_capacity(chunks.len());
    for chunk_index in 0..chunks.len() {
        let chunk = &chunks[chunk_index];
        let mut basic_blocks = chunk
            .blocks
            .iter()
            .map(|label| {
                let block = source_blocks
                    .remove(label)
                    .expect("outlined block must exist in its source function");
                (label.clone(), block)
            })
            .collect::<HashMap<_, _>>();
        let mut external_targets = BTreeSet::new();
        for block in basic_blocks.values() {
            for target in block_successors(block) {
                if block_to_chunk[&target] != chunk_index {
                    external_targets.insert(target);
                }
            }
        }

        let mut trampolines = HashMap::new();
        for target in external_targets {
            let target_chunk = block_to_chunk[&target];
            let trampoline = format!(
                "$outlined$tail${target_chunk}${}",
                crate::stable_hash::short_hash(&target, 10)
            );
            let target_plan = &chunks[target_chunk];
            let selector = (target_plan.entries.len() > 1).then(|| {
                target_plan
                    .entries
                    .iter()
                    .position(|entry| entry == &target)
                    .expect("cross-chunk target must be an entry") as i32
            });
            let args = call_arguments(target_plan, selector, &target, &live_in, carrier.as_ref())?;
            let return_type = target_plan.signature.ret.as_ref().clone();
            let destination = return_type
                .has_jvm_value()
                .then(|| "$outlined$return".to_string());
            let mut instructions = carrier
                .as_ref()
                .map(|carrier| carrier_stores(carrier, &target, &live_in))
                .unwrap_or_default();
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: destination.clone(),
                class_name: owner.to_string(),
                method_name: target_plan.name.clone(),
                method_ty: target_plan.signature.clone(),
                args,
            });
            instructions.push(oomir::Instruction::Return {
                operand: destination.map(|name| Operand::Variable {
                    name,
                    ty: return_type,
                }),
            });
            basic_blocks.insert(
                trampoline.clone(),
                BasicBlock {
                    label: trampoline.clone(),
                    instructions,
                },
            );
            trampolines.insert(target, trampoline);
        }
        for block in basic_blocks.values_mut() {
            rewrite_targets(block, &trampolines);
        }

        let entry = if chunk_index > 0 && chunk.entries.len() > 1 {
            let dispatch = format!("$outlined$dispatch${chunk_index}");
            let invalid = format!("$outlined$invalid${chunk_index}");
            let targets = chunk
                .entries
                .iter()
                .enumerate()
                .map(|(selector, target)| (Constant::I32(selector as i32), target.clone()))
                .collect();
            basic_blocks.insert(
                dispatch.clone(),
                BasicBlock {
                    label: dispatch.clone(),
                    instructions: vec![oomir::Instruction::Switch {
                        discr: Operand::Variable {
                            name: "__outlined_selector".to_string(),
                            ty: Type::I32,
                        },
                        targets,
                        otherwise: invalid.clone(),
                    }],
                },
            );
            basic_blocks.insert(
                invalid.clone(),
                BasicBlock {
                    label: invalid,
                    instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                        exception_class: "java/lang/IllegalStateException".to_string(),
                        message: "invalid outlined Rust control-flow entry".to_string(),
                    }],
                },
            );
            dispatch
        } else {
            chunk.entries[0].clone()
        };

        if let Some(carrier) = &carrier {
            let prefix = if chunk_index == 0 {
                carrier_allocations(carrier)
            } else {
                carrier_loads(carrier, &chunk.entries, &live_in)
            };
            basic_blocks
                .get_mut(&entry)
                .expect("outlined entry block must exist")
                .instructions
                .splice(0..0, prefix);
        }

        result.push(Function {
            name: chunk.name.clone(),
            owner_class: owner_class.clone(),
            signature: chunk.signature.clone(),
            debug_variables: if chunk_index == 0 {
                debug_variables.take().unwrap_or_default()
            } else {
                Vec::new()
            },
            body: CodeBlock {
                entry,
                basic_blocks,
            },
        });
    }

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "bytecode-gen",
        format!(
            "Outlined {} (estimated OOMIR cost {total_cost}) into {} JVM methods",
            function_name,
            result.len()
        )
    );
    Ok(result)
}

fn compare_variables(left: &Variable, right: &Variable) -> std::cmp::Ordering {
    (&left.name, left.ty.to_jvm_descriptor()).cmp(&(&right.name, right.ty.to_jvm_descriptor()))
}

fn carrier_category(ty: &Type) -> (usize, Type) {
    match ty.to_jvm_descriptor().as_str() {
        "Z" => (0, Type::Boolean),
        "B" => (1, Type::I8),
        "S" => (2, Type::I16),
        "C" => (3, Type::U16),
        "I" => (4, Type::I32),
        "J" => (5, Type::I64),
        "F" => (6, Type::F32),
        "D" => (7, Type::F64),
        descriptor if descriptor == Type::Pointer(Box::new(Type::Unit)).to_jvm_descriptor() => {
            (8, Type::Pointer(Box::new(Type::Unit)))
        }
        descriptor if descriptor == Type::Slice(Box::new(Type::Unit)).to_jvm_descriptor() => {
            (9, Type::Slice(Box::new(Type::Unit)))
        }
        descriptor if descriptor == Type::Str.to_jvm_descriptor() => (10, Type::Str),
        _ => (11, Type::Class("java/lang/Object".to_string())),
    }
}

fn build_carrier_plan(entries: &[BTreeSet<String>], live_in: &LiveVariables) -> CarrierPlan {
    let mut categories = vec![HashSet::new(); 12];
    for entry in entries.iter().skip(1).flatten() {
        for variable in &live_in[entry] {
            categories[carrier_category(&variable.ty).0].insert(variable.clone());
        }
    }
    let arrays = categories
        .into_iter()
        .enumerate()
        .filter_map(|(category, variables)| {
            if variables.is_empty() {
                return None;
            }
            let mut variables = variables.into_iter().collect::<Vec<_>>();
            variables.sort_by(compare_variables);
            let element_type = carrier_category(&variables[0].ty).1;
            Some(CarrierArray {
                name: format!("$outlined$carrier${category}"),
                element_type,
                variables: variables
                    .into_iter()
                    .enumerate()
                    .map(|(index, variable)| (variable, index as i32))
                    .collect(),
            })
        })
        .collect();
    CarrierPlan { arrays }
}

fn carrier_array_type(array: &CarrierArray) -> Type {
    Type::Array(Box::new(array.element_type.clone()))
}

fn carrier_allocations(carrier: &CarrierPlan) -> Vec<oomir::Instruction> {
    carrier
        .arrays
        .iter()
        .map(|array| oomir::Instruction::NewArray {
            dest: array.name.clone(),
            element_type: array.element_type.clone(),
            size: Operand::Constant(Constant::I32(array.variables.len() as i32)),
        })
        .collect()
}

fn carrier_stores(
    carrier: &CarrierPlan,
    target_entry: &str,
    live_in: &LiveVariables,
) -> Vec<oomir::Instruction> {
    let live = &live_in[target_entry];
    let mut instructions = Vec::new();
    for array in &carrier.arrays {
        let mut variables = array
            .variables
            .iter()
            .filter(|(variable, _)| live.contains(*variable))
            .collect::<Vec<_>>();
        variables.sort_by_key(|(_, index)| **index);
        instructions.extend(variables.into_iter().map(|(variable, index)| {
            oomir::Instruction::ArrayStore {
                array: array.name.clone(),
                index: Operand::Constant(Constant::I32(*index)),
                value: Operand::Variable {
                    name: variable.name.clone(),
                    ty: variable.ty.clone(),
                },
                copy_value: false,
            }
        }));
    }
    instructions
}

fn carrier_loads(
    carrier: &CarrierPlan,
    entries: &[String],
    live_in: &LiveVariables,
) -> Vec<oomir::Instruction> {
    let mut live = entries
        .iter()
        .flat_map(|entry| live_in[entry].iter().cloned())
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    live.sort_by(compare_variables);
    let mut instructions = Vec::new();
    for variable in live {
        let (array_index, array, index) = carrier
            .arrays
            .iter()
            .enumerate()
            .find_map(|(array_index, array)| {
                array
                    .variables
                    .get(&variable)
                    .map(|index| (array_index, array, *index))
            })
            .expect("every live outlined variable must have a carrier slot");
        let temp = format!("$outlined$load${array_index}${index}");
        instructions.push(oomir::Instruction::ArrayGet {
            dest: temp.clone(),
            array: Operand::Variable {
                name: array.name.clone(),
                ty: carrier_array_type(array),
            },
            index: Operand::Constant(Constant::I32(index)),
        });
        instructions.push(oomir::Instruction::Cast {
            op: Operand::Variable {
                name: temp,
                ty: array.element_type.clone(),
            },
            ty: variable.ty,
            dest: format!("{RESTORE_PREFIX}{}", variable.name),
        });
    }
    instructions
}

fn call_arguments(
    target: &Chunk,
    selector: Option<i32>,
    target_entry: &str,
    live_in: &LiveVariables,
    carrier: Option<&CarrierPlan>,
) -> jvm::Result<Vec<Operand>> {
    let mut arguments = Vec::new();
    let mut parameters = target.signature.params.iter();
    if let Some(selector) = selector {
        let _ = parameters.next();
        arguments.push(Operand::Constant(Constant::I32(selector)));
    }
    if let Some(carrier) = carrier {
        arguments.extend(carrier.arrays.iter().map(|array| Operand::Variable {
            name: array.name.clone(),
            ty: carrier_array_type(array),
        }));
        return Ok(arguments);
    }
    for (parameter_name, ty) in parameters {
        let name = parameter_name
            .strip_prefix(PARAMETER_PREFIX)
            .expect("outlined parameter has its marker");
        let variable = Variable {
            name: name.to_string(),
            ty: ty.clone(),
        };
        if live_in[target_entry].contains(&variable) {
            arguments.push(Operand::Variable {
                name: name.to_string(),
                ty: ty.clone(),
            });
        } else {
            arguments.push(Operand::Constant(default_constant(ty)));
        }
    }
    Ok(arguments)
}

fn default_constant(ty: &Type) -> Constant {
    match ty {
        Type::I8 => Constant::I8(0),
        Type::U8 => Constant::U8(0),
        Type::I16 => Constant::I16(0),
        Type::U16 => Constant::U16(0),
        Type::F16 => Constant::F16(0),
        Type::I32 => Constant::I32(0),
        Type::U32 => Constant::U32(0),
        Type::I64 => Constant::I64(0),
        Type::U64 => Constant::U64(0),
        Type::F32 => Constant::F32(0.0),
        Type::F64 => Constant::F64(0.0),
        Type::Boolean => Constant::Boolean(false),
        Type::Char => Constant::Char('\0'),
        Type::Unit | Type::Void => Constant::Unit,
        ty => Constant::Null(ty.clone()),
    }
}

fn rewrite_targets(block: &mut BasicBlock, replacements: &HashMap<String, String>) {
    let replace = |target: &mut String| {
        if let Some(replacement) = replacements.get(target) {
            *target = replacement.clone();
        }
    };
    for instruction in &mut block.instructions {
        match instruction {
            oomir::Instruction::UnwindStart { target } | oomir::Instruction::Jump { target } => {
                replace(target)
            }
            oomir::Instruction::Branch {
                true_block,
                false_block,
                ..
            } => {
                replace(true_block);
                replace(false_block);
            }
            oomir::Instruction::Switch {
                targets, otherwise, ..
            } => {
                for (_, target) in targets {
                    replace(target);
                }
                replace(otherwise);
            }
            _ => {}
        }
    }
}

fn live_variables(
    body: &CodeBlock,
    successors: &HashMap<String, Vec<String>>,
    variable_types: &VariableTypes,
) -> LiveVariables {
    let mut uses = HashMap::new();
    let mut definitions = HashMap::new();
    for (label, block) in &body.basic_blocks {
        let mut block_uses = HashSet::new();
        let mut block_definitions = HashSet::new();
        for instruction in &block.instructions {
            for variable in instruction_uses(instruction, variable_types) {
                if !block_definitions.contains(&variable) {
                    block_uses.insert(variable);
                }
            }
            block_definitions.extend(instruction_definitions(instruction));
        }
        uses.insert(label.clone(), block_uses);
        definitions.insert(label.clone(), block_definitions);
    }

    let mut live_in = body
        .basic_blocks
        .keys()
        .map(|label| (label.clone(), HashSet::new()))
        .collect::<HashMap<_, _>>();
    loop {
        let mut changed = false;
        let mut labels = body.basic_blocks.keys().cloned().collect::<Vec<_>>();
        labels.sort();
        labels.reverse();
        for label in labels {
            let mut live_out = HashSet::new();
            for successor in &successors[&label] {
                live_out.extend(live_in[successor].iter().cloned());
            }
            for definition in &definitions[&label] {
                live_out.remove(definition);
            }
            live_out.extend(uses[&label].iter().cloned());
            if live_out != live_in[&label] {
                live_in.insert(label, live_out);
                changed = true;
            }
        }
        if !changed {
            return live_in;
        }
    }
}

fn collect_variable_types(function: &Function) -> jvm::Result<VariableTypes> {
    let mut types = HashMap::new();
    for (index, (name, ty)) in function.signature.params.iter().enumerate() {
        let oomir_name = if name == oomir::CALLER_LOCATION_PARAM_NAME {
            name.clone()
        } else {
            format!("_{}", index + 1)
        };
        insert_type(&mut types, oomir_name, ty.clone(), &function.name)?;
    }
    insert_type(
        &mut types,
        UNWIND_EXCEPTION_LOCAL.to_string(),
        Type::Class("java/lang/Throwable".to_string()),
        &function.name,
    )?;
    for block in function.body.basic_blocks.values() {
        for instruction in &block.instructions {
            for operand in instruction_operands(instruction) {
                if let Operand::Variable { name, ty } = operand {
                    insert_type(&mut types, name.clone(), ty.clone(), &function.name)?;
                }
            }
            if let Some((name, ty)) = instruction_definition_type(instruction) {
                insert_type(&mut types, name, ty, &function.name)?;
            }
        }
    }
    Ok(types)
}

fn insert_type(
    types: &mut VariableTypes,
    name: String,
    ty: Type,
    _function_name: &str,
) -> jvm::Result<()> {
    if !ty.has_jvm_value() {
        return Ok(());
    }
    types.entry(name).or_default().insert(ty);
    Ok(())
}

fn instruction_operands(instruction: &oomir::Instruction) -> Vec<&Operand> {
    use oomir::Instruction as I;
    match instruction {
        I::Add { op1, op2, .. }
        | I::Sub { op1, op2, .. }
        | I::Mul { op1, op2, .. }
        | I::Div { op1, op2, .. }
        | I::Rem { op1, op2, .. }
        | I::Eq { op1, op2, .. }
        | I::Ne { op1, op2, .. }
        | I::Lt { op1, op2, .. }
        | I::Le { op1, op2, .. }
        | I::Gt { op1, op2, .. }
        | I::Ge { op1, op2, .. }
        | I::BitAnd { op1, op2, .. }
        | I::BitOr { op1, op2, .. }
        | I::BitXor { op1, op2, .. }
        | I::Shl { op1, op2, .. }
        | I::Shr { op1, op2, .. } => vec![op1, op2],
        I::Not { src, .. } | I::Neg { src, .. } | I::Move { src, .. } => vec![src],
        I::Branch { condition, .. } => vec![condition],
        I::Return { operand } => operand.iter().collect(),
        I::CallIndirect {
            function_ptr, args, ..
        } => std::iter::once(function_ptr.as_ref())
            .chain(args.iter())
            .collect(),
        I::InvokeInterface { args, operand, .. } | I::InvokeVirtual { args, operand, .. } => {
            std::iter::once(operand).chain(args.iter()).collect()
        }
        I::Switch { discr, .. } | I::NewArray { size: discr, .. } => vec![discr],
        I::ArrayStore { index, value, .. } => vec![index, value],
        I::ArrayFill { value, .. } => vec![value],
        I::ArrayGet { array, index, .. } => vec![array, index],
        I::Length { array, .. } => vec![array],
        I::ConstructObject { args, .. } => args.iter().map(|(operand, _)| operand).collect(),
        I::SetField { value, .. } => vec![value],
        I::GetField { object, .. } | I::Cast { op: object, .. } => vec![object],
        I::InvokeStatic { args, .. } => args.iter().collect(),
        _ => Vec::new(),
    }
}

fn instruction_definition_type(instruction: &oomir::Instruction) -> Option<(String, Type)> {
    use oomir::Instruction as I;
    match instruction {
        I::Add { dest, op1, .. }
        | I::Sub { dest, op1, .. }
        | I::Mul { dest, op1, .. }
        | I::Div { dest, op1, .. }
        | I::Rem { dest, op1, .. }
        | I::BitAnd { dest, op1, .. }
        | I::BitOr { dest, op1, .. }
        | I::BitXor { dest, op1, .. }
        | I::Shl { dest, op1, .. }
        | I::Shr { dest, op1, .. } => Some((dest.clone(), op1.get_type()?)),
        I::Eq { dest, .. }
        | I::Ne { dest, .. }
        | I::Lt { dest, .. }
        | I::Le { dest, .. }
        | I::Gt { dest, .. }
        | I::Ge { dest, .. } => Some((dest.clone(), Type::Boolean)),
        I::Not { dest, src } | I::Neg { dest, src } | I::Move { dest, src } => {
            Some((dest.clone(), src.get_type()?))
        }
        I::CallIndirect {
            dest, signature, ..
        }
        | I::InvokeInterface {
            dest,
            method_ty: signature,
            ..
        }
        | I::InvokeVirtual {
            dest,
            method_ty: signature,
            ..
        }
        | I::InvokeStatic {
            dest,
            method_ty: signature,
            ..
        } => dest
            .as_ref()
            .map(|dest| (dest.clone(), signature.ret.as_ref().clone())),
        I::CreateFunctionPointer {
            dest,
            interface_name,
            ..
        } => Some((dest.clone(), Type::Interface(interface_name.clone()))),
        I::NewArray {
            dest, element_type, ..
        } => Some((dest.clone(), Type::Array(Box::new(element_type.clone())))),
        I::ArrayGet { dest, array, .. } => match array.get_type()? {
            Type::Array(element)
            | Type::Slice(element)
            | Type::MutableReference(element)
            | Type::Pointer(element) => Some((dest.clone(), *element)),
            _ => None,
        },
        I::Length { dest, .. } => Some((dest.clone(), Type::I32)),
        I::ConstructObject {
            dest, class_name, ..
        } => Some((dest.clone(), Type::Class(class_name.clone()))),
        I::GetField { dest, field_ty, .. } => Some((dest.clone(), field_ty.clone())),
        I::Cast { dest, ty, .. } => Some((dest.clone(), ty.clone())),
        _ => None,
    }
}

fn instruction_uses(
    instruction: &oomir::Instruction,
    variable_types: &VariableTypes,
) -> Vec<Variable> {
    let mut uses = instruction_operands(instruction)
        .into_iter()
        .filter_map(|operand| match operand {
            Operand::Variable { name, ty } if ty.has_jvm_value() => Some(Variable {
                name: name.clone(),
                ty: ty.clone(),
            }),
            _ => None,
        })
        .collect::<Vec<_>>();
    match instruction {
        oomir::Instruction::ArrayStore { array, .. }
        | oomir::Instruction::ArrayFill { array, .. } => {
            if let Some(types) = variable_types.get(array) {
                uses.extend(types.iter().cloned().map(|ty| Variable {
                    name: array.clone(),
                    ty,
                }));
            }
        }
        oomir::Instruction::SetField {
            object,
            owner_class,
            ..
        } => uses.push(Variable {
            name: object.clone(),
            ty: Type::Class(owner_class.clone()),
        }),
        oomir::Instruction::Rethrow => uses.push(Variable {
            name: UNWIND_EXCEPTION_LOCAL.to_string(),
            ty: Type::Class("java/lang/Throwable".to_string()),
        }),
        _ => {}
    }
    uses
}

fn instruction_definitions(instruction: &oomir::Instruction) -> Vec<Variable> {
    instruction_definition_type(instruction)
        .filter(|(_, ty)| ty.has_jvm_value())
        .map(|(name, ty)| vec![Variable { name, ty }])
        .unwrap_or_default()
}

fn successor_map(body: &CodeBlock) -> HashMap<String, Vec<String>> {
    body.basic_blocks
        .iter()
        .map(|(label, block)| (label.clone(), block_successors(block)))
        .collect()
}

fn block_successors(block: &BasicBlock) -> Vec<String> {
    let mut successors = Vec::new();
    for instruction in &block.instructions {
        match instruction {
            oomir::Instruction::UnwindStart { target } | oomir::Instruction::Jump { target } => {
                push_unique(&mut successors, target)
            }
            oomir::Instruction::Branch {
                true_block,
                false_block,
                ..
            } => {
                push_unique(&mut successors, true_block);
                push_unique(&mut successors, false_block);
            }
            oomir::Instruction::Switch {
                targets, otherwise, ..
            } => {
                for (_, target) in targets {
                    push_unique(&mut successors, target);
                }
                push_unique(&mut successors, otherwise);
            }
            _ => {}
        }
    }
    successors
}

fn push_unique(values: &mut Vec<String>, value: &str) {
    if !values.iter().any(|existing| existing == value) {
        values.push(value.to_string());
    }
}

fn topological_components(
    body: &CodeBlock,
    successors: &HashMap<String, Vec<String>>,
) -> jvm::Result<Vec<Vec<String>>> {
    fn visit(
        node: &str,
        successors: &HashMap<String, Vec<String>>,
        visited: &mut HashSet<String>,
        order: &mut Vec<String>,
    ) {
        if !visited.insert(node.to_string()) {
            return;
        }
        let mut targets = successors[node].clone();
        targets.sort();
        for target in targets {
            visit(&target, successors, visited, order);
        }
        order.push(node.to_string());
    }

    let mut order = Vec::new();
    let mut visited = HashSet::new();
    visit(&body.entry, successors, &mut visited, &mut order);
    if visited.len() != body.basic_blocks.len() {
        return Err(verification_error(
            "control flow",
            "large OOMIR function contains unreachable blocks after optimisation".to_string(),
        ));
    }
    let mut reverse = body
        .basic_blocks
        .keys()
        .map(|label| (label.clone(), Vec::new()))
        .collect::<HashMap<_, _>>();
    for (source, targets) in successors {
        for target in targets {
            reverse
                .get_mut(target)
                .expect("successor exists")
                .push(source.clone());
        }
    }
    let mut assigned = HashSet::new();
    let mut components = Vec::new();
    while let Some(node) = order.pop() {
        if assigned.contains(&node) {
            continue;
        }
        let mut component = Vec::new();
        let mut stack = vec![node];
        while let Some(current) = stack.pop() {
            if !assigned.insert(current.clone()) {
                continue;
            }
            component.push(current.clone());
            let mut predecessors = reverse[&current].clone();
            predecessors.sort();
            stack.extend(predecessors);
        }
        component.sort();
        components.push(component);
    }

    let component_for = components
        .iter()
        .enumerate()
        .flat_map(|(index, component)| component.iter().cloned().map(move |label| (label, index)))
        .collect::<HashMap<_, _>>();
    let mut dag = vec![BTreeSet::new(); components.len()];
    for (source, targets) in successors {
        let source_component = component_for[source];
        for target in targets {
            let target_component = component_for[target];
            if source_component != target_component {
                dag[source_component].insert(target_component);
            }
        }
    }
    fn topo_visit(
        component: usize,
        dag: &[BTreeSet<usize>],
        visited: &mut HashSet<usize>,
        order: &mut Vec<usize>,
    ) {
        if !visited.insert(component) {
            return;
        }
        for successor in &dag[component] {
            topo_visit(*successor, dag, visited, order);
        }
        order.push(component);
    }
    let mut component_order = Vec::new();
    let mut visited_components = HashSet::new();
    topo_visit(
        component_for[&body.entry],
        &dag,
        &mut visited_components,
        &mut component_order,
    );
    component_order.reverse();
    Ok(component_order
        .into_iter()
        .map(|index| components[index].clone())
        .collect())
}

fn block_cost(block: &BasicBlock) -> usize {
    block
        .instructions
        .iter()
        .filter(|instruction| {
            !matches!(
                instruction,
                oomir::Instruction::SourceLocation(_)
                    | oomir::Instruction::LocalVariableScope(_)
                    | oomir::Instruction::UnwindStart { .. }
                    | oomir::Instruction::UnwindEnd
            )
        })
        .count()
}

fn type_slots(ty: &Type) -> usize {
    match ty {
        Type::Unit | Type::Void => 0,
        Type::I64 | Type::U64 | Type::F64 => 2,
        _ => 1,
    }
}

fn verification_error(context: &str, message: String) -> jvm::Error {
    jvm::Error::VerificationError {
        context: format!("Function {context}"),
        message,
    }
}
