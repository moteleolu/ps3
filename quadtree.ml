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

let new_tree (r:region) : 'a quadtree = 
  Leaf (r,[])
         
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  let is_divisible lf =
    let (x1,x2,y1,y2) =
      match lf with 
        ((a,b), (c,d)), _ -> (a,b,c,d) in
    let x = x2-x1 in 
    let y = y2-y1 in 
    let h = (x^2) + (y^2) in 
    (min_diagonal)^2 > h in

  let x3 = (x1+x2)/2 in 
  let y3 = (y1+y2)/2 in 
  let a = 
    match q with   
        Leaf (r, lst) when is_divisible -> Node (r, new_tree ((x3, x2), (y3, y2)), 
      	  new_tree ((x1, x3), (y3, y2)), new_tree ((x1, x3), (y1, y3)), 
      	  new_tree ((x3, x2), (y1, y3)))
      | _ -> q in 
  
  let find_reg tr = 
    match tr with 
      (r, l1, l2, l3, l4) -> if c <

  let (c1, c2) =
    match c with 
    (a, b) -> (a, b) in 
  
  match a with 
    Leaf (a, b) -> Leaf (a, (c,s)::b)
    | _ -> if (c1=x3 & c2 = y3) then Leaf (r, ) else insert (helper a)

							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  =
  failwith "TODO"
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
=
  failwith "TODO"

