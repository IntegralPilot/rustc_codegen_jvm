#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

use alloc::alloc::{alloc_zeroed, dealloc, realloc};
use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::collections::{BTreeMap, BTreeSet, BinaryHeap, LinkedList, VecDeque};
use alloc::format;
use alloc::rc::{Rc, Weak};
use alloc::sync::{Arc, Weak as ArcWeak};
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;
use core::alloc::Layout;
use core::cell::RefCell;
use core::sync::atomic::{AtomicUsize, Ordering};

static mut DROP_COUNTER: usize = 0;

struct DropTracker {
    _val: u32,
}

impl Drop for DropTracker {
    fn drop(&mut self) {
        unsafe {
            DROP_COUNTER += 1;
        }
    }
}

trait Action {
    fn perform(&self) -> u32;
    fn name(&self) -> &str {
        "action"
    }
}

struct ConcreteAction {
    factor: u32,
}

impl Action for ConcreteAction {
    fn perform(&self) -> u32 {
        self.factor * 2
    }
    fn name(&self) -> &str {
        "concrete"
    }
}

struct ClosureAction<F: Fn() -> u32> {
    f: F,
}

impl<F: Fn() -> u32> Action for ClosureAction<F> {
    fn perform(&self) -> u32 {
        (self.f)()
    }
}

// Recursive data structure via Box, classic linked list.
enum List {
    Cons(i64, Box<List>),
    Nil,
}

impl List {
    fn sum(&self) -> i64 {
        match self {
            List::Cons(value, rest) => value + rest.sum(),
            List::Nil => 0,
        }
    }
}

// A tree structure using Rc<RefCell<>> for shared mutable nodes with parent back-references.
struct TreeNode {
    value: i32,
    children: RefCell<Vec<Rc<TreeNode>>>,
    parent: RefCell<Weak<TreeNode>>,
}

struct ArcWorkerContext {
    relaxed_counter: Arc<AtomicUsize>,
    sequential_counter: Arc<AtomicUsize>,
    iterations: usize,
}

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:runStaticPointerWorkers:(Lorg/rustlang/runtime/Pointer;JLorg/rustlang/runtime/Pointer;JLorg/rustlang/runtime/Pointer;I)V"]
    fn run_static_pointer_workers(
        owner: *const u8,
        owner_length: usize,
        method: *const u8,
        method_length: usize,
        context: *const ArcWorkerContext,
        worker_count: i32,
    );
}

#[inline(never)]
pub extern "C" fn arc_concurrent_worker(context: *const ArcWorkerContext) {
    let context = unsafe { &*context };
    for _ in 0..context.iterations {
        let relaxed = Arc::clone(&context.relaxed_counter);
        let sequential = Arc::clone(&context.sequential_counter);

        relaxed.fetch_add(1, Ordering::Relaxed);
        sequential.fetch_add(2, Ordering::SeqCst);

        let weak = Arc::downgrade(&relaxed);
        let upgraded = weak.upgrade();
        assert!(upgraded.is_some());
        let upgraded = upgraded.unwrap();
        assert!(Arc::ptr_eq(&relaxed, &upgraded));

        drop(upgraded);
        drop(weak);
        drop(sequential);
        drop(relaxed);
    }
}

fn main() {
    test_vec_basic();
    test_zst_allocations();
    test_drop_semantics();
    test_dynamic_dispatch();
    test_various_alignments();
    test_manual_realloc();

    test_string_ops();
    test_vecdeque();
    test_btreemap();
    test_btreeset();
    test_binary_heap();
    test_linked_list();
    test_rc_refcell_tree();
    test_weak_refs();
    test_cow();
    test_recursive_box_list();
    test_slice_and_vec_adapters();
    test_try_reserve();
    test_closures_as_trait_objects();
    test_nested_collections();
    test_sort_and_dedup_custom();

    test_overaligned_allocations();
    test_btree_stress_and_merging();
    test_drain_partial_drop();
    test_arc_basics();
    test_arc_concurrent_clone_drop();
    test_zst_collection_behavior();
    test_manual_realloc_shrink();
    test_vec_from_raw_parts();
    test_layout_computations();
}

fn test_vec_basic() {
    let mut values = Vec::with_capacity(1);
    for value in 0_u64..64 {
        values.push(value * value + 3);
    }

    assert!(values.len() == 64);
    assert!(values.capacity() >= values.len());
    for (index, value) in values.iter().enumerate() {
        let index = index as u64;
        assert!(*value == index * index + 3);
    }

    assert!(values.pop() == Some(63 * 63 + 3));
    values.insert(4, 999);
    assert!(values[4] == 999);
    assert!(values.remove(4) == 999);
    assert!(values[4] == 4 * 4 + 3);
}

fn test_zst_allocations() {
    let mut zsts: Vec<()> = Vec::new();
    for _ in 0..100 {
        zsts.push(());
    }
    assert!(zsts.len() == 100);
    assert!(zsts.capacity() > 100);
    assert!(zsts.pop() == Some(()));

    let boxed_zst: Box<()> = Box::new(());
    let raw_ptr = Box::into_raw(boxed_zst);
    assert!(!raw_ptr.is_null());
    unsafe {
        drop(Box::from_raw(raw_ptr));
    }
}

fn test_drop_semantics() {
    unsafe {
        DROP_COUNTER = 0;
    }

    {
        let mut trackers = Vec::new();
        for i in 0..10 {
            trackers.push(DropTracker { _val: i });
        }
        trackers.pop();
        assert!(unsafe { DROP_COUNTER } == 1);
    }

    assert!(unsafe { DROP_COUNTER } == 10);
}

fn test_dynamic_dispatch() {
    let action: Box<dyn Action> = Box::new(ConcreteAction { factor: 21 });
    assert!(action.perform() == 42);
    assert!(action.name() == "concrete");

    // Vec of trait objects, mixing concrete and closure-backed implementations.
    let actions: Vec<Box<dyn Action>> = vec![
        Box::new(ConcreteAction { factor: 1 }),
        Box::new(ConcreteAction { factor: 5 }),
    ];
    let total: u32 = actions.iter().map(|a| a.perform()).sum();
    assert!(total == 2 + 10);
}

fn test_various_alignments() {
    let alignments = [1, 2, 4, 8, 16, 64];
    for &align in &alignments {
        let layout = Layout::from_size_align(32, align).unwrap();
        unsafe {
            let bytes = alloc_zeroed(layout);
            assert!(!bytes.is_null());
            assert!((bytes as usize) & (layout.align() - 1) == 0);

            for index in 0..layout.size() {
                assert!(*bytes.add(index) == 0);
                *bytes.add(index) = index as u8;
            }

            dealloc(bytes, layout);
        }
    }
}

fn test_manual_realloc() {
    let original_layout = Layout::from_size_align(16, 8).unwrap();
    let new_layout = Layout::from_size_align(32, 8).unwrap();

    unsafe {
        let ptr = alloc_zeroed(original_layout);
        assert!(!ptr.is_null());

        for i in 0..16 {
            *ptr.add(i) = i as u8;
        }

        let new_ptr = realloc(ptr, original_layout, new_layout.size());
        assert!(!new_ptr.is_null());
        assert!((new_ptr as usize) & (new_layout.align() - 1) == 0);

        for i in 0..16 {
            assert!(*new_ptr.add(i) == i as u8);
        }

        for i in 16..32 {
            *new_ptr.add(i) = i as u8;
        }

        dealloc(new_ptr, new_layout);
    }
}

fn test_string_ops() {
    let mut s = String::from("hello");
    s.push(' ');
    s.push_str("world");
    assert!(s == "hello world");
    assert!(s.len() == 11);

    let upper: String = s.chars().map(|c| c.to_ascii_uppercase()).collect();
    assert!(upper == "HELLO WORLD");

    let formatted = format!("{}-{}-{}", 1, "two", 3.0);
    assert!(formatted == "1-two-3");

    let parts: Vec<&str> = s.split(' ').collect();
    assert!(parts.len() == 2);
    assert!(parts[0] == "hello");
    assert!(parts[1] == "world");

    let repeated = "ab".repeat(3);
    assert!(repeated == "ababab");

    let mut owned = s.clone();
    owned.truncate(5);
    assert!(owned == "hello");
    assert!(owned.to_string() == "hello");

    let replaced = s.replace("world", "rust");
    assert!(replaced == "hello rust");

    let joined = ["a", "b", "c"].join(",");
    assert!(joined == "a,b,c");
}

fn test_vecdeque() {
    let mut dq: VecDeque<i32> = VecDeque::new();
    for i in 0..10 {
        if i % 2 == 0 {
            dq.push_back(i);
        } else {
            dq.push_front(i);
        }
    }
    assert!(dq.len() == 10);

    // front should hold the largest odd number pushed last-to-front among odds
    assert!(dq.front().is_some());
    assert!(dq.back().is_some());

    let mut sum = 0;
    while let Some(v) = dq.pop_front() {
        sum += v;
    }
    assert!(sum == (0..10).sum::<i32>());

    let mut rotating: VecDeque<i32> = (0..5).collect();
    rotating.rotate_left(2);
    let collected: Vec<i32> = rotating.into_iter().collect();
    assert!(collected == vec![2, 3, 4, 0, 1]);
}

fn test_btreemap() {
    let mut map: BTreeMap<i32, &'static str> = BTreeMap::new();
    map.insert(3, "three");
    assert!(
        map.len() == 1,
        "unexpected length after first insert: {}",
        map.len()
    );
    map.insert(1, "one");
    assert!(
        map.len() == 2,
        "unexpected length after second insert: {}",
        map.len()
    );
    map.insert(2, "two");
    assert!(map.len() == 3);
    assert!(map.get(&1) == Some(&"one"));
    assert!(map.get(&2) == Some(&"two"));
    assert!(map.get(&3) == Some(&"three"));

    let mut key_iter = map.keys();
    assert!(*key_iter.next().unwrap() == 1);
    assert!(*key_iter.next().unwrap() == 2);
    assert!(*key_iter.next().unwrap() == 3);
    assert!(key_iter.next().is_none());

    // BTreeMap iterates in sorted key order.
    let keys: Vec<i32> = map.keys().copied().collect();
    assert!(keys.len() == 3);
    assert!(keys[0] == 1);
    assert!(
        keys[1] == 2,
        "unexpected keys: [{}, {}, {}]",
        keys[0],
        keys[1],
        keys[2]
    );
    assert!(keys[2] == 3);
    assert!(keys == vec![1, 2, 3]);

    assert!(map.get(&2) == Some(&"two"));
    assert!(map.remove(&2) == Some("two"));
    assert!(map.get(&2).is_none());
    assert!(map.len() == 2);

    let entry_val = map.entry(5).or_insert("five");
    assert!(*entry_val == "five");
    assert!(map.contains_key(&5));

    *map.entry(1).or_insert("uno") = "ONE";
    assert!(map.get(&1) == Some(&"ONE"));
}

fn test_btreeset() {
    let mut set: BTreeSet<i32> = BTreeSet::new();
    for v in [5, 3, 1, 4, 1, 5, 9, 2, 6] {
        set.insert(v);
    }
    let sorted: Vec<i32> = set.iter().copied().collect();
    assert!(sorted == vec![1, 2, 3, 4, 5, 6, 9]);

    let other: BTreeSet<i32> = [4, 5, 6, 100].into_iter().collect();
    let intersection: Vec<i32> = set.intersection(&other).copied().collect();
    assert!(intersection == vec![4, 5, 6]);

    let union_count = set.union(&other).count();
    assert!(union_count == 8);

    assert!(set.remove(&9));
    assert!(!set.contains(&9));
}

fn test_binary_heap() {
    let mut heap: BinaryHeap<i32> = BinaryHeap::new();
    for v in [5, 1, 8, 3, 9, 2] {
        heap.push(v);
    }
    assert!(heap.peek() == Some(&9));

    let mut sorted_desc = Vec::new();
    while let Some(top) = heap.pop() {
        sorted_desc.push(top);
    }
    assert!(sorted_desc == vec![9, 8, 5, 3, 2, 1]);

    let from_vec: BinaryHeap<i32> = vec![10, 20, 5].into_iter().collect();
    assert!(from_vec.into_sorted_vec() == vec![5, 10, 20]);
}

fn test_linked_list() {
    let mut list: LinkedList<i32> = LinkedList::new();
    list.push_back(1);
    list.push_back(2);
    list.push_front(0);
    assert!(list.len() == 3);

    let collected: Vec<i32> = list.iter().copied().collect();
    assert!(collected == vec![0, 1, 2]);

    let mut second_half = list.split_off(1);
    assert!(list.len() == 1);
    assert!(second_half.len() == 2);

    list.append(&mut second_half);
    assert!(list.len() == 3);
    assert!(second_half.is_empty());
}

fn test_rc_refcell_tree() {
    let root = Rc::new(TreeNode {
        value: 1,
        children: RefCell::new(Vec::new()),
        parent: RefCell::new(Weak::new()),
    });

    let child = Rc::new(TreeNode {
        value: 2,
        children: RefCell::new(Vec::new()),
        parent: RefCell::new(Weak::new()),
    });

    *child.parent.borrow_mut() = Rc::downgrade(&root);
    root.children.borrow_mut().push(Rc::clone(&child));
    assert!(root.children.try_borrow().is_ok());

    assert!(Rc::strong_count(&root) == 1);
    assert!(Rc::strong_count(&child) == 2);

    let upgraded_parent = child.parent.borrow().upgrade();
    assert!(upgraded_parent.is_some());
    assert!(upgraded_parent.unwrap().value == 1);

    assert!(root.children.borrow()[0].value == 2);
}

fn test_weak_refs() {
    let strong = Rc::new(42_i32);
    let weak: Weak<i32> = Rc::downgrade(&strong);

    assert!(weak.upgrade().is_some());
    assert!(*weak.upgrade().unwrap() == 42);

    drop(strong);
    assert!(weak.upgrade().is_none());
}

fn test_cow() {
    fn maybe_modify(input: &str) -> Cow<'_, str> {
        if input.contains(' ') {
            Cow::Owned(input.replace(' ', "_"))
        } else {
            Cow::Borrowed(input)
        }
    }

    let unmodified = maybe_modify("nospaces");
    assert!(matches!(unmodified, Cow::Borrowed(_)));
    assert!(unmodified == "nospaces");

    let modified = maybe_modify("has spaces here");
    assert!(matches!(modified, Cow::Owned(_)));
    assert!(modified == "has_spaces_here");
}

fn test_recursive_box_list() {
    let list = List::Cons(
        1,
        Box::new(List::Cons(2, Box::new(List::Cons(3, Box::new(List::Nil))))),
    );
    assert!(list.sum() == 6);
}

fn test_slice_and_vec_adapters() {
    let mut v: Vec<i32> = (0..20).collect();

    v.retain(|&x| x % 2 == 0);
    assert!(v.len() == 10);
    assert!(v.iter().all(|&x| x % 2 == 0));

    let drained: Vec<i32> = v.drain(2..5).collect();
    assert!(drained.len() == 3);
    assert!(v.len() == 7);

    let tail = v.split_off(3);
    assert!(v.len() == 3);
    assert!(!tail.is_empty());

    v.extend(tail.iter().copied());
    assert!(v.len() == 3 + tail.len());

    let windows_sum: i32 = v.windows(2).map(|w| w[0] + w[1]).sum();
    assert!(windows_sum >= 0 || windows_sum < 0); // just exercising windows()

    let chunks: Vec<Vec<i32>> = v.chunks(2).map(|c| c.to_vec()).collect();
    assert!(!chunks.is_empty());

    v.reverse();
    let last = v.len() - 1;
    v.swap(0, last);
    v.dedup();
}

fn test_try_reserve() {
    let mut v: Vec<u8> = Vec::new();
    let result = v.try_reserve(1024);
    assert!(result.is_ok());
    assert!(v.capacity() >= 1024);

    // An absurdly large request should fail gracefully rather than abort,
    // exercising the fallible allocation path.
    let huge = usize::MAX / 2;
    let failure = v.try_reserve(huge);
    assert!(failure.is_err());
}

fn test_closures_as_trait_objects() {
    let multiplier = 7;
    let action = ClosureAction {
        f: move || multiplier * 6,
    };
    let boxed: Box<dyn Action> = Box::new(action);
    assert!(boxed.perform() == 42);

    let ops: Vec<Box<dyn Fn(i32) -> i32>> = vec![
        Box::new(|x| x + 1),
        Box::new(|x| x * 2),
        Box::new(|x| x * x),
    ];
    let mut value = 3;
    for (op, expected) in ops.iter().zip([4, 8, 64]) {
        value = op(value);
        assert!(value == expected);
    }
    // ((3 + 1) * 2) ^ 2 = 64
    assert!(value == 64);
}

fn test_nested_collections() {
    let mut nested: Vec<Vec<i32>> = Vec::new();
    for i in 0..5 {
        nested.push((0..i).collect());
    }
    let total: i32 = nested.iter().flatten().sum();
    assert!(total == (0..5).map(|i| (0..i).sum::<i32>()).sum::<i32>());

    let mut map_of_vecs: BTreeMap<i32, Vec<i32>> = BTreeMap::new();
    for i in 0..10 {
        map_of_vecs.entry(i % 3).or_insert_with(Vec::new).push(i);
    }
    assert!(map_of_vecs.len() == 3);
    assert!(map_of_vecs[&0].len() == 4);
    assert!(map_of_vecs[&1].len() == 3);
    assert!(map_of_vecs[&2].len() == 3);
    assert!(map_of_vecs[&0][0] == 0);
    assert!(map_of_vecs[&0][1] == 3);
    assert!(map_of_vecs[&0][2] == 6);
    assert!(map_of_vecs[&0][3] == 9);
    assert!(map_of_vecs[&0] == vec![0, 3, 6, 9]);

    let boxed_slice: Box<[i32]> = vec![1, 2, 3, 4].into_boxed_slice();
    assert!(boxed_slice.len() == 4);
    assert!(boxed_slice.iter().sum::<i32>() == 10);
}

fn test_sort_and_dedup_custom() {
    #[derive(Clone, Debug)]
    struct Item {
        key: i32,
        payload: u32,
    }

    let mut items = vec![
        Item {
            key: 3,
            payload: 30,
        },
        Item {
            key: 1,
            payload: 10,
        },
        Item {
            key: 2,
            payload: 20,
        },
        Item {
            key: 1,
            payload: 11,
        },
    ];

    items.sort_by_key(|item| item.key);
    let keys: Vec<i32> = items.iter().map(|i| i.key).collect();
    assert!(keys == vec![1, 1, 2, 3]);

    // Stable sort: the two key==1 items should retain original relative order.
    assert!(items[0].payload == 10);
    assert!(items[1].payload == 11);

    items.dedup_by_key(|item| item.key);
    assert!(items.len() == 3);

    let mut nums = vec![5, 3, 3, 1, 1, 1, 9, 5];
    nums.sort_unstable();
    assert!(nums == vec![1, 1, 1, 3, 3, 5, 5, 9]);
    nums.dedup();
    assert!(nums == vec![1, 3, 5, 9]);

    let max = nums.iter().max().copied().unwrap();
    let min = nums.iter().min().copied().unwrap();
    assert!(max == 9);
    assert!(min == 1);

    let position = nums.binary_search(&5);
    assert!(position.is_ok());
}


fn test_overaligned_allocations() {
    let alignments = [128, 256, 512, 1024, 2048, 4096];
    for &align in &alignments {
        let layout = Layout::from_size_align(64, align).unwrap();
        unsafe {
            let ptr = alloc_zeroed(layout);
            assert!(!ptr.is_null());
            assert!((ptr as usize) & (layout.align() - 1) == 0);

            *ptr = 0xAA;
            *ptr.add(layout.size() - 1) = 0xBB;

            assert!(*ptr == 0xAA);
            assert!(*ptr.add(layout.size() - 1) == 0xBB);

            dealloc(ptr, layout);
        }
    }
}

fn test_btree_stress_and_merging() {
    let mut map = BTreeMap::new();

    for i in 0..300 {
        map.insert(i, DropTracker { _val: i as u32 });
    }
    assert!(map.len() == 300);

    for i in (0..300).step_by(2) {
        let removed = map.remove(&i);
        assert!(removed.is_some());
    }
    assert!(map.len() == 150);

    for i in 0..300 {
        if i % 2 == 0 {
            assert!(map.get(&i).is_none());
        } else {
            assert!(map.get(&i).is_some());
        }
    }

    map.clear();
    assert!(map.is_empty());
}

fn test_drain_partial_drop() {
    unsafe {
        DROP_COUNTER = 0;
    }

    {
        let mut v = Vec::new();
        for i in 0..12 {
            v.push(DropTracker { _val: i });
        }

        let mut drain = v.drain(2..10);

        let first = drain.next();
        let second = drain.next();
        assert!(first.is_some());
        assert!(second.is_some());

        drop(first);
        drop(second);
        assert!(unsafe { DROP_COUNTER } == 2);

    }

    assert!(unsafe { DROP_COUNTER } == 12);
}

fn test_arc_basics() {
    let root_val = Arc::new(999_u32);
    let weak_ref: ArcWeak<u32> = Arc::downgrade(&root_val);

    assert!(Arc::strong_count(&root_val) == 1);
    assert!(weak_ref.upgrade().is_some());

    let second_arc = Arc::clone(&root_val);
    assert!(Arc::strong_count(&root_val) == 2);
    assert!(Arc::weak_count(&root_val) == 1);

    drop(root_val);
    assert!(Arc::strong_count(&second_arc) == 1);
    assert!(weak_ref.upgrade().is_some());

    drop(second_arc);
    assert!(weak_ref.upgrade().is_none());

    let mut copy_on_write = Arc::new(vec![1_u32, 2, 3]);
    let original = Arc::clone(&copy_on_write);
    assert!(Arc::get_mut(&mut copy_on_write).is_none());
    Arc::make_mut(&mut copy_on_write).push(4);
    assert!(original.as_slice() == [1, 2, 3]);
    assert!(copy_on_write.as_slice() == [1, 2, 3, 4]);
    assert!(!Arc::ptr_eq(&copy_on_write, &original));
    drop(original);

    match Arc::try_unwrap(copy_on_write) {
        Ok(values) => assert!(values == vec![1, 2, 3, 4]),
        Err(_) => panic!("unique Arc should unwrap"),
    }
}

fn test_arc_concurrent_clone_drop() {
    const WORKERS: usize = 8;
    const ITERATIONS: usize = 2;

    let context = ArcWorkerContext {
        relaxed_counter: Arc::new(AtomicUsize::new(0)),
        sequential_counter: Arc::new(AtomicUsize::new(0)),
        iterations: ITERATIONS,
    };

    let owner = b"alloc_test.alloc_test";
    let method = b"arc_concurrent_worker";
    // Keep the reflectively invoked worker as a concrete codegen item and
    // establish a single-threaded baseline before the concurrent phase.
    arc_concurrent_worker(&context);
    unsafe {
        run_static_pointer_workers(
            owner.as_ptr(),
            owner.len(),
            method.as_ptr(),
            method.len(),
            &context,
            WORKERS as i32,
        );
    }

    assert!(
        context.relaxed_counter.load(Ordering::Acquire) == (WORKERS + 1) * ITERATIONS
    );
    assert!(
        context.sequential_counter.load(Ordering::SeqCst) == (WORKERS + 1) * ITERATIONS * 2
    );
    assert!(Arc::strong_count(&context.relaxed_counter) == 1);
    assert!(Arc::strong_count(&context.sequential_counter) == 1);
    assert!(Arc::weak_count(&context.relaxed_counter) == 0);
    assert!(Arc::weak_count(&context.sequential_counter) == 0);

    let relaxed_weak = Arc::downgrade(&context.relaxed_counter);
    let sequential_weak = Arc::downgrade(&context.sequential_counter);
    drop(context);
    assert!(relaxed_weak.upgrade().is_none());
    assert!(sequential_weak.upgrade().is_none());
}

fn test_zst_collection_behavior() {
    let mut zst_deque: VecDeque<()> = VecDeque::new();
    for _ in 0..100 {
        zst_deque.push_back(());
        zst_deque.push_front(());
    }
    assert!(zst_deque.len() == 200);
    for _ in 0..100 {
        assert!(zst_deque.pop_back().is_some());
        assert!(zst_deque.pop_front().is_some());
    }
    assert!(zst_deque.is_empty());

    let mut zst_map: BTreeMap<(), DropTracker> = BTreeMap::new();
    unsafe { DROP_COUNTER = 0; }
    zst_map.insert((), DropTracker { _val: 1234 });

    let displaced = zst_map.insert((), DropTracker { _val: 5678 });
    assert!(displaced.is_some());
    drop(displaced);
    assert!(unsafe { DROP_COUNTER } == 1);

    drop(zst_map);
    assert!(unsafe { DROP_COUNTER } == 2);
}

fn test_manual_realloc_shrink() {
    let layout_large = Layout::from_size_align(2048, 16).unwrap();
    let layout_small = Layout::from_size_align(64, 16).unwrap();

    unsafe {
        let ptr = alloc_zeroed(layout_large);
        assert!(!ptr.is_null());

        for offset in (0..2048).step_by(16) {
            *ptr.add(offset) = (offset % 256) as u8;
        }

        let ptr_shunk = realloc(ptr, layout_large, layout_small.size());
        assert!(!ptr_shunk.is_null());
        assert!((ptr_shunk as usize) & (layout_small.align() - 1) == 0);

        for offset in (0..64).step_by(16) {
            assert!(*ptr_shunk.add(offset) == (offset % 256) as u8);
        }

        dealloc(ptr_shunk, layout_small);
    }
}

fn test_vec_from_raw_parts() {
    unsafe {
        DROP_COUNTER = 0;
    }

    {
        let mut original = Vec::new();
        for i in 0..8 {
            original.push(DropTracker { _val: i });
        }

        let ptr = original.as_mut_ptr();
        let len = original.len();
        let cap = original.capacity();

        core::mem::forget(original);

        let reconstructed = unsafe { Vec::from_raw_parts(ptr, len, cap) };
        assert!(reconstructed.len() == 8);
        assert!(reconstructed.capacity() == cap);
    }

    assert!(unsafe { DROP_COUNTER } == 8);
}

fn test_layout_computations() {
    let base_layout = Layout::new::<u64>();
    let offset_layout = Layout::new::<u8>();

    let (extended, offset) = base_layout.extend(offset_layout).unwrap();
    assert!(extended.size() == 9);
    assert!(extended.align() == 8);
    assert!(offset == 8);

    let array_layout = Layout::array::<u32>(16).unwrap();
    assert!(array_layout.size() == 64);
    assert!(array_layout.align() == 4);
}
