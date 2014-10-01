open Parser
open Quadtree

let load_city_data (s:string) : string quadtree = 
	let lst = parse s in
    let tr= new_tree ((~-.180., ~-.90.),(180.,90.)) in
    let helper tr a = match a with (b,c,d)-> insert tr (b,c) d in
	List.fold_left helper tr lst

let city_search (q: string quadtree) (r : region) : string list = 
	fold_region (fun x y-> snd y :: x) [] q r 
