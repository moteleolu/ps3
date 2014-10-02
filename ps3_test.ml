open Quadtree
open Assertions
open Printf

TEST_UNIT "new_tree_test1" =
let r1 = ((0.,1.),(0.,1.)) in 
let t1 = new_tree r1 in 
let a1 = Leaf (((0., 1.), (0., 1.)), []) in 
assert_true(t1=a1)

TEST_UNIT ""

let () = Pa_ounit_lib.Runtime.summarize()