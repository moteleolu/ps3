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

(*Determines whether or not a region is large enough to be divided into four 
*quadtrees/made into a Node
*Precondition: Takes in a valid 2-dimensional region
*Postcondition: Returns true if the region can be divided and false otherwise
*)
let is_divisible (r: region) = 
  match r with ((x1,y1), (x2,y2)) -> ((x1-.x2)**2. +. (y1-.y2)**2.) > min_diagonal


(*Creates a new quadtree of the Node constructor
*Precondition: Takes a valide 2-dimensional region
*Postcondition: Returns a Node spanning the region specified
*)
let new_tree (r:region) : 'a quadtree = 
  if not (is_divisible r) then Leaf (r,[])
  else let (x1, x2, x3, y1, y2, y3) = match r with ((x1,y1), (x2,y2))-> (x1, x2, ((x1+.x2)/.2.), y1, y2, ((y1+.y2)/.2.)) in  
       Node (r, Leaf (((x3,y3), (x2,y2)),[]), Leaf (((x1,y3), (x3,y2)),[]),
            Leaf (((x1,y1), (x3,y3)),[]), Leaf (((x3,y1), (x2,y3)),[]))


(*Inserts a value into a quadtree and the specified coordinates
*Precondition: The coordinates are within the region of the quadtree and the 
*value to be inserted is of the same type as the values already in the quadtree
*Postcondition: Returns a quadtree containing the given value at the specified 
*coordinates
*)
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  let (x1, x2, x3, y1, y2, y3) = 
    match q with 
      Node (((x1,y1), (x2,y2)), q1,q2,q3,q4) -> (x1, x2, ((x1+.x2)/.2.), y1, y2, ((y1+.y2)/.2.))
    | Leaf (((x1,y1), (x2,y2)), _) -> (x1, x2, ((x1+.x2)/.2.), y1, y2, ((y1+.y2)/.2.)) in 

  let is_on_edge_or_center (tr : 'a quadtree) (coor : coord) : bool =
    match coor with 
    (tx1, ty1) -> if (x3 = tx1 && (y3 = ty1 || y1 = ty1 || y2 = ty1)) || (x1 = tx1 && 
                  (y3 = ty1 || y1 = ty1 || y2 = ty1)) || (x2 = tx1 && (y3 = ty1 || 
                  y1 = ty1 || y2 = ty1)) then true 
                  else false in 

  let is_in_reg1 (coor : coord) : bool =
    match coor with 
    (tx1, ty1) -> if tx1>=x3 && ty1>=y3 then true else false in

  let is_in_reg2 (coor : coord) : bool =
    match coor with 
    (tx1, ty1) -> if tx1<=x3 && ty1>=y3 then true else false in 

  let is_in_reg3 (coor : coord) : bool =
    match coor with 
    (tx1, ty1) -> if tx1<=x3 && ty1<=y3 then true else false in 

  match (q, c) with 
    (Leaf (a,b), _) -> if (not (is_divisible a)) || is_on_edge_or_center q c 
                  then Leaf (a, (c,s)::b) 
                  else insert (new_tree a) c s
  | (Node (r, q1,q2,q3,q4), _) -> if is_in_reg1 c 
                            then Node (r, insert q1 c s, q2, q3, q4)
                            else if is_in_reg2 c 
                            then Node (r, q1, insert q2 c s, q3, q4)
                            else if is_in_reg3 c  
                            then Node (r, q1, q2, insert q3 c s, q4)
                            else Node (r, q1, q2, q3, insert q4 c s)                          


(*Folds across all the values in a quadtree, applying the given function to 
*each value of the tree using the given accumulator, a
*Precondition: The quadtree is a valid quadtree and the function can be applied
*to the values of the quadtree
*Postcondition: Returns the value of the function applied to every value of the 
*quadtree and folded with the given accumulator
*)							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)(a: 'a) (t: 'b quadtree): 'a =
  match t with 
    Leaf ( _, b)-> List.fold_left f a b 
  | Node (_,q1,q2,q3,q4)-> fold_quad f (fold_quad f (fold_quad f (fold_quad f a q1) q2) q3) q4


(*Folds across all the values in a given region of a quadtree, applying the 
*given function to each value of the region using the given accumulator, a
*Precondition: r is a valid region that is included in the region spanned by 
*the quadtree. If the quadtree is a Leaf, the region must be the exact region of the Leaf. 
*The quadtree is also a valid quadtree 
Postcondition: Returns the value of the function applied to every value of the 
*region within the quadtree and folded with the given accumulator
*)	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) (r : region) : 'a =
  match (t, r) with 
    (Leaf (((x1,y1), (x2,y2)), b), ((tx1,ty1), (tx2,ty2)))-> if  (tx1 >= x1 && tx1 <= x2 && ty1 >= y1 && ty2<=y2)
                                                             then  List.fold_left f a b
                                                             else a 
  | (Node (_,q1,q2,q3,q4),_)-> fold_region f (fold_region f (fold_region f (fold_region f a q1 r) q2 r) q3 r) q4 r
