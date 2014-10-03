open Quadtree
open Assertions
open Printf
open Nats
open City_search

TEST_UNIT "new_tree_test1" =
let r1 = ((0.,0.),(1.,1.)) in 
let t1 = new_tree r1 in 
let a1 = Node (r1, Leaf (((0.5, 0.5), (1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0., 0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [])) in 
assert_true(t1=a1)

TEST_UNIT "insert_test2" =
let r1 = ((0.,0.),(1.,1.)) in 
let t1 = new_tree r1 in 
let q1 = (0.25, 0.25) in
let s = 1 in 
let ret = insert t1 q1 s in
let a1 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1)]), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret=a1)

TEST_UNIT "fold_quad_test3" =
let r1 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1); ((0.24, 0.24), 2)]), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a1 = 0 in
let ret = 10 in
let f1 = fold_quad (fun x y-> x + snd y) a1 r1 in
assert_true (f1=ret)

TEST_UNIT "fold_region_test4" =
let r1 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1); ((0.24, 0.24), 2)]), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a1 = 0 in
let ret = 3 in
let f1 = fold_region (fun x y-> x + snd y) a1 r1 ((0.,0.), (0.5, 0.5))in
assert_true (f1=ret)

(*TEST_UNIT "load_city_data_test5" =
let s1 = "ithaca.csv" in 
let t1 = load_city_data s1 in
let n1 = Node (((0., ~-.90.), (180., 0.)), 
	Leaf (((90., ~-.45.), (180., 0.)), []), 
	Leaf (((0., ~-.45.), (90., 0.)), []), n2, 
	Leaf (((90., ~-.90.), (180., ~-.45.)), [])) in 
let n2 = Node (((0., ~-.90.), (90., ~-.45.)), Leaf (((), ()), []), Leaf (((), ()), []), Leaf (((), ()), []), Leaf (((), ()), []))
let r1 = Node (((~-.180., ~-.90.), (180.,90.)), 
	Leaf (((0., 0.), (180., 90.)), []), Leaf (((~-.180., 0.), (0., 90.)), []), 
	Leaf (((~-.180., ~-.90.), (0., 0.)), []), 
	n1) in 
assert_true (t1 = r1)*)

(*TEST_UNIT "city_search_test6" =
let t1 = [use tree created in test5 as tree] in 
let r1 = [region]
let l1 = [list in order they're found...] in 
let r1 = city_search t1 r1 in 
assert_true (r1=l1)*)


TEST_UNIT "IntNat+_test7" =
let a = Nats.IntNat.3 in 
let b = Nats.IntNat.4 in
let c = Nats.IntNat.(+) in
let d = c a b in
assert_true (d=5)


let () = Pa_ounit_lib.Runtime.summarize()