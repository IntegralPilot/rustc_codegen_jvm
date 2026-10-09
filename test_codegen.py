"""Check that selected methods need no runtime calls on paths that return normally."""
from __future__ import annotations

import re


def scalar_path_errors(bytecode: str, names: set[str]) -> list[str]:
    errors = []
    methods = re.findall(
        r"^  (?:public|private|protected).*? ([\w$]+)\([^\n]*\);\n(.*?)(?=^  (?:public|private|protected)|\Z)",
        bytecode, re.M | re.S,
    )
    found = set()
    for name, body in methods:
        if name not in names:
            continue
        found.add(name)
        instructions = list(re.finditer(r"^\s+(\d+):\s+([a-z][a-z0-9_]*)[ \t]*([^\n]*)", body, re.M))
        edges = {}
        returns = []
        for index, match in enumerate(instructions):
            pc, op, args = int(match[1]), match[2], match[3]
            following = [int(instructions[index + 1][1])] if index + 1 < len(instructions) else []
            if op.endswith("return") or op == "return":
                returns.append(pc)
                targets = []
            elif op == "athrow":
                targets = []
            elif op in ("goto", "goto_w"):
                targets = [int(args.split()[0])]
            elif op.startswith("if"):
                targets = following + [int(args.split()[0])]
            elif op in ("tableswitch", "lookupswitch"):
                table = body[match.end():body.index("}", match.end())]
                targets = [int(x) for x in re.findall(r":\s*(\d+)", table)]
            else:
                targets = following
            edges[pc] = targets
        for start, end, handler in re.findall(r"^\s+(\d+)\s+(\d+)\s+(\d+)\s+(?:Class |any)", body, re.M):
            for pc, targets in edges.items():
                if int(start) <= pc < int(end):
                    targets.append(int(handler))
        reverse = {pc: [] for pc in edges}
        for source, targets in edges.items():
            for target in targets:
                reverse[target].append(source)

        def reachable(graph, start):
            pending, seen = list(start), set()
            while pending:
                node = pending.pop()
                if node not in seen:
                    seen.add(node)
                    pending.extend(graph[node])
            return seen

        if not instructions or not returns:
            errors.append(f"{name}: missing method body or normal return")
            continue
        normal = reachable(edges, [int(instructions[0][1])]) & reachable(reverse, returns)
        for match in instructions:
            pc, op, args = int(match[1]), match[2], match[3]
            if pc not in normal:
                continue
            primitive_call = (
                re.search(r"Method java/lang/(?:Integer|Long|Float|Double|Math)\.", args)
                and re.search(r":\([ZBCSIJFD]*\)[ZBCSIJFDV](?:$|\s)", args)
            )
            if op in ("new", "anewarray", "multianewarray") or (op.startswith("invoke") and not primitive_call):
                errors.append(f"{name}: {pc}: {op} {args}")
    errors.extend(f"{name}: missing method" for name in sorted(names - found))
    return errors
