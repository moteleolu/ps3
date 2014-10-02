type coord = float * float
type region = coord * coord
(*'a quadtree represents quadtrees by making a Node a quadtree made up of four regions each of which are subtrees. A subtree can either be 
*another Node of four regions/subtrees or an individual region, called a Leaf, if the quadtree has reached its base subdivision.
*)
type 'a quadtree =
    Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

let is_divisible (r: region) = 
  match r with ((x1,y1), (x2,y2)) -> ((x1-.x2)**2. +. (y1-.y2)**2.) > min_diagonal

let new_tree (r:region) : 'a quadtree = 
  if not (is_divisible r) then Leaf (r,[])
  else let (x1, x2, x3, y1, y2, y3) = match r with ((x1,y1), (x2,y2))-> (x1, x2, ((x1+.x2)/.2.), y1, y2, ((y1+.y2)/.2.)) in  
       Node (r, Leaf (((x3,y3), (x2,y2)),[]), Leaf (((x1,y3), (x3,y2)),[]),
            Leaf (((x1,y1), (x3,y3)),[]), Leaf (((x3,y1), (x2,y3)),[]))

(*Adds a tree t2 to the approprate quadrant in t1 
t1 must be a node or a divisible leaf *)
let rec insert_tree (t1: 'a quadtree) (t2: 'a quadtree) =
  match t1 with 
    Leaf (r,_)-> insert_tree (new_tree r) t2
  | Node (((x1,y1), (x2,y2)), q1,q2,q3,q4)-> let x3= (x1+.x2)/.2. and y3= (y1+.y2)/.2. in 

  match t2 with 
    Node (((tx1,ty1), (tx2,ty2)),_,_,_,_)-> if tx1>=x3 && ty1>=y3 
                                            then Node (((x1,y1), (x2,y2)), t2,q2,q3,q4)
                                            else if tx1<=x3 && ty1>=y3 
                                            then Node (((x1,y1), (x2,y2)), q1,t2,q3,q4)
                                            else if tx1<=x3 && ty1<=y3 
                                            then Node (((x1,y1), (x2,y2)), q1,q2,t2,q4)
                                            else Node (((x1,y1), (x2,y2)), q1,q2,q3,t2)
  | Leaf (((tx1,ty1), (tx2,ty2)),_)-> if tx1>=x3 && ty1>=y3 
                                      then Node(((x1,y1), (x2,y2)), t2,q2,q3,q4)
                                      else if tx1<=x3 && ty1>=y3 
                                      then Node (((x1,y1), (x2,y2)), q1,t2,q3,q4)
                                      else if tx1<=x3 && ty1<=y3 
                                      then Node (((x1,y1), (x2,y2)), q1,q2,t2,q4)
                                      else Node (((x1,y1), (x2,y2)), q1,q2,q3,t2)


let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  match q with 
    Leaf (a,b) -> if not (is_divisible a) then Leaf (a, (c,s)::b)
                  else insert_tree q (insert(new_tree a) c s)
  | Node (((x1,y1), (x2,y2)), q1,q2,q3,q4) -> let x3= (x1+.x2)/.2. and y3= (y1+.y2)/.2. in 

  match c with (tx1,ty1) -> if tx1>=x3 && ty1>=y3 
                            then insert_tree q (insert q1 c s)
                            else if tx1<=x3 && ty1>=y3 
                            then insert_tree q (insert q2 c s)
                            else if tx1<=x3 && ty1<=y3 
                            then insert_tree q (insert q3 c s)
                            else insert_tree q (insert q4 c s)                          

							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)(a: 'a) (t: 'b quadtree): 'a =
  match t with 
    Leaf ( _, b)-> List.fold_left f a b 
  | Node (_,q1,q2,q3,q4)-> fold_quad f (fold_quad f (fold_quad f (fold_quad f a q1) q2) q3) q4

	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) (r : region) : 'a =
  match (t, r) with 
    (Leaf (((x1,y1), (x2,y2)), b), ((tx1,ty1), (tx2,ty2)))-> if  (tx1 >= x1 && tx1 <= x2 && ty1 >= y1 && ty2<=y2)
                                                             then  List.fold_left f a b
                                                             else a 
  | (Node (_,q1,q2,q3,q4),_)-> fold_quad f (fold_quad f (fold_quad f (fold_quad f a q1) q2) q3) q4
