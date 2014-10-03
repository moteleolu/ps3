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
let r2 = ((0.,0.),(0.0000001,0.0000001)) in 
let t2 = new_tree r2 in 
let a2 = Leaf (((0.,0.),(0.0000001,0.0000001)), []) in 
assert_true(t2=a2)


TEST_UNIT "insert_test2" =
let r1 = ((0.,0.),(1.,1.)) in 
let t1 = new_tree r1 in 
let q1 = (0.25, 0.25) in
let s1 = 1 in 
let ret1 = insert t1 q1 s1 in
let a1 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1)]), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret1=a1)
let r2 = ((0.,0.),(1.,1.)) in 
let t2 = new_tree r2 in 
let q2 = (0.5, 0.5) in
let s2 = 1 in 
let ret2 = insert t2 q2 s2 in
let a2 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.5, 0.5),1)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret2=a2)
let r3 = ((0.,0.),(1.,1.)) in 
let t3 = new_tree r3 in 
let q3 = (0.75, 0.25) in
let s3 = 1 in 
let ret3 = insert t3 q3 s3 in
let a3 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25),1)])) in
assert_true(ret3=a3)
let r4 = ((0.,0.),(1.,1.)) in 
let t4 = new_tree r4 in 
let q4 = (0.75, 0.75) in
let s4 = 1 in 
let ret4 = insert t4 q4 s4 in
let a4 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75),1)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret4=a4)
let r5 = ((0.,0.),(1.,1.)) in 
let t5 = new_tree r5 in 
let q5 = (0.25, 0.75) in
let s5 = 1 in 
let ret5 = insert t5 q5 s5 in
let a5 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), [((0.25, 0.75),1)]), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret5=a5)
let r6 = ((0.,0.),(1.,1.)) in 
let t6 = new_tree r6 in 
let q6 = (0., 0.) in
let s6 = 1 in 
let ret6 = insert t6 q6 s6 in
let a6 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0., 0.),1)]), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret6=a6)
let r7 = ((0.,0.),(1.,1.)) in 
let t7 = new_tree r7 in 
let q7 = (1., 0.) in
let s7 = 1 in 
let ret7 = insert t7 q7 s7 in
let a7 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [((1., 0.),1)])) in
assert_true(ret7=a7)
let r8 = ((0.,0.),(1.,1.)) in 
let t8 = new_tree r8 in 
let q8 = (1., 1.) in
let s8 = 1 in 
let ret8 = insert t8 q8 s8 in
let a8 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((1., 1.),1)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret8=a8)
let r9 = ((0.,0.),(1.,1.)) in 
let t9 = new_tree r9 in 
let q9 = (0., 1.) in
let s9 = 1 in 
let ret9 = insert t9 q9 s9 in
let a9 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), [((0., 1.),1)]), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret9=a9)
let r10 = ((0.,0.),(1.,1.)) in 
let t10 = new_tree r10 in 
let q10 = (0.125, 0.375) in
let s10 = 1 in 
let ret10 = insert t10 q10 s10 in
let n1 = Node (((0., 0.), (0.5, 0.5)), Leaf (((0.25, 0.25), (0.5, 0.5)), []), 
	Leaf (((0., 0.25), (0.25, 0.5)), [((0.125, 0.375), 1)]), Leaf (((0., 0.), 
    (0.25, 0.25)), []), Leaf (((0.25, 0.), (0.5, 0.25)), [])) in 
let a10 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	n1, Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret10=a10)
let r11 = ((0.,0.),(1.,1.)) in 
let t11 = new_tree r11 in 
let q11 = (0.5, 0.) in
let s11 = 1 in 
let ret11 = insert t11 q11 s11 in
let a11 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), []), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.5, 0.), 1)]), Leaf (((0.5, 0.), (1., 0.5)), [])) in
assert_true(ret11=a11)


TEST_UNIT "fold_quad_test3" =
let q1 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1); ((0.24, 0.24), 2)]), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a1 = 0 in
let ret1 = 10 in
let f1 = fold_quad (fun x y-> x + snd y) a1 q1 in
assert_true (f1=ret1)
let q2 = Leaf (((0., 0.), (1., 1.)), [((0.5, 0.5), 2)]) in 
let a2 = 1 in 
let ret2 = 3 in 
let f2 = fold_quad (fun x y -> x + snd y) a2 q2 in 
assert_true (f2=ret2)
let q3 = Leaf (((0., 0.), (1., 1.)), [((0.5, 0.5), 2); ((0.25, 0.5), 3)]) in 
let a3 = 1 in 
let ret3 = 6 in 
let f3 = fold_quad (fun x y -> x + snd y) a3 q3 in 
assert_true (f3=ret3)
let n4 = Node (((0., 0.5), (0.5, 1.)), 
	Leaf (((0.25, 0.75), (0.5, 1.)), [((0.375, 0.875), 5)]), 
	Leaf (((0., 0.75), (0.25, 1.)), []), Leaf (((0., 0.5), (0.25, 0.75)), []), 
	Leaf (((0.25, 0.), (0.5, 0.75)), [])) in 
let q4 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), n4, 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1); ((0.24, 0.24), 2)]), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a4 = 0 in
let ret4 = 15 in
let f4 = fold_quad (fun x y-> x + snd y) a4 q4 in
assert_true (f4=ret4)


TEST_UNIT "fold_region_test4" =
let r1 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1); ((0.24, 0.24), 2)]), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a1 = 0 in
let ret1 = 3 in
let f1 = fold_region (fun x y-> x + snd y) a1 r1 ((0.,0.), (0.5, 0.5))in
assert_true (f1=ret1)
let r2 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), []), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a2 = 0 in
let ret2 = 0 in
let f2 = fold_region (fun x y-> x + snd y) a2 r2 ((0.,0.), (0.5, 0.5))in
assert_true (f2=ret2)
let r3 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1); ((0.24, 0.24), 2)]), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a3 = 1 in
let ret3 = 4 in
let f3 = fold_region (fun x y-> x + snd y) a3 r3 ((0.,0.), (0.5, 0.5))in
assert_true (f3=ret3)

(*let r4 = Leaf (((0., 0.), (0.5, 0.5)), []) in 
let a4 = 0 in
let ret4 = 4 in
let f4 = fold_region (fun x y-> x + snd y) a4 r4 ((0.,0.), (0.5, 0.5))in
assert_true (f4=ret4)
let r5 = Node (((0.,0.),(1.,1.)), Leaf (((0.5, 0.5),(1., 1.)), [((0.75, 0.75), 3)]), Leaf (((0., 0.5), (0.5, 1.)), []), 
	Leaf (((0.,0.), (0.5, 0.5)), [((0.25, 0.25),1); ((0.24, 0.24), 2)]), Leaf (((0.5, 0.), (1., 0.5)), [((0.75, 0.25), 4)])) in 
let a5 = 1 in
let ret5 = 4 in
let f5 = fold_region (fun x y-> x + snd y) a5 r5 ((0.,0.), (0.5, 0.5))in
assert_true (f5=ret5)*)

TEST_UNIT "load_city_data_test5" = 
let s1 = "one_city_test1.csv" in 
let t1 = load_city_data s1 in 
let r1 = Node (((~-.180., ~-.90.), (180.,90.)), Leaf (((0., 0.), (180., 90.)), [((0., 0.), "Gulf of Guinea")]), Leaf (((~-.180., 0.), (0., 90.)), []), 
	Leaf (((~-.180., ~-.90.), (0., 0.)), []), Leaf (((0., ~-.90.), (180., 0.)), [])) in 
assert_true (t1=r1)
(*let r1 = Leaf (((~-.180., ~-.90.), (180., 90.)), [((0., 0.), "Gulf of Guinea")]) in *)

(*let s2 = "one_city_test2.csv" in 
let t2 = load_city_data s2 in 
let r2 = in 
assert_true (t2=r2)
let s3 = "one_city_test3.csv" in 
let t3 = load_city_data s3 in 
let r3 = in 
assert_true (t3=r3)
let s4 = "two_city_test1.csv" in 
let t4 = load_city_data s4 in 
let r4 = in 
assert_true (t4=r4)
let s5 = "two_city_test2" in 
let t5 = load_city_data s5 in 
let r5 = in 
assert_true (t5=r5)*)


(*TEST_UNIT "city_search_test6" =
let t1 = [use tree created in test5 as tree] in 
let r1 = [region]
let l1 = [list in order they're found...] in 
let r1 = city_search t1 r1 in 
assert_true (r1=l1)*)

module Alientest: AlienMapping = struct 
type aliensym = string 
  let int_of_aliensym sym = int_of_string sym
  let one = "1"
  let zero = "0"
  (*let aliensym_of_int i= string_of_int i*)
end 

TEST_UNIT "AlienNatFn_test_7"=
assert_true((Alientest.int_of_aliensym Alientest.zero)= 0);
let module AlienFn = AlienNatFn(Alientest) in   
let a= AlienFn.zero in
let b= AlienFn.zero in
assert_true(a=b);
assert_true (AlienFn.(===) (AlienFn.(+) AlienFn.zero AlienFn.one) (AlienFn.nat_of_int 1));
assert_true (AlienFn.(<) (AlienFn.nat_of_int 5) (AlienFn.nat_of_int 6));
assert_true (AlienFn.int_of_nat (AlienFn.nat_of_int 5) =5);
assert_true (AlienFn.(===) (AlienFn.( * ) (AlienFn.nat_of_int 5) (AlienFn.nat_of_int 5)) (AlienFn.nat_of_int 25))


TEST_UNIT "IntNat_test_8"=
let module Mod=IntNat in
assert_true (Mod.(<) Mod.zero Mod.one );
assert_true (Mod.(===) (Mod.nat_of_int 5) (Mod.(+) (Mod.nat_of_int 3) (Mod.nat_of_int 2)));
assert_true (Mod.int_of_nat (Mod.nat_of_int 5)=5);
assert_true (Mod.(===) (Mod.( * ) (Mod.nat_of_int 5) (Mod.nat_of_int 5)) (Mod.nat_of_int 25));
assert_raises (Some Mod.Unrepresentable) Mod.nat_of_int ~-5;
assert_raises (Some Mod.Unrepresentable) (Mod.(+) (Mod.nat_of_int max_int)) (Mod.nat_of_int 1);
assert_raises (Some Mod.Unrepresentable) (Mod.( * ) (Mod.nat_of_int max_int)) (Mod.nat_of_int 2)


TEST_UNIT "ListNat_test_9"=
let module Mod=ListNat in
assert_true (Mod.(<) Mod.zero Mod.one );
assert_true (Mod.(===) (Mod.nat_of_int 5) (Mod.(+) (Mod.nat_of_int 3) (Mod.nat_of_int 2)));
assert_true (Mod.int_of_nat (Mod.nat_of_int 5)=5);
assert_true (Mod.(===) (Mod.( * ) (Mod.nat_of_int 5) (Mod.nat_of_int 5)) (Mod.nat_of_int 25));
assert_raises (Some Mod.Unrepresentable) Mod.nat_of_int ~-5;


TEST_UNIT " NatConvertFn_test_10" =
let module Mod= IntNat in
let module Conv= NatConvertFn (Mod) in
assert_true (Conv.int_of_nat (Mod.nat_of_int 5) = 5);
assert_true ((Conv.nat_of_int 5) = (Mod.nat_of_int 5))

let () = Pa_ounit_lib.Runtime.summarize()
