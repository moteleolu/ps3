open Parser
open Quadtree

(*Creates a string quadtree containing the cities named in a given file
*Precondition: Each line in the file contains the data for one city in the
*format "Latitude, Longitude, City Name" and the file is a .csv file
*Postcondition: Returns a string quadtree containing all the cities listed
*in the file at the correct location (latitude and longitude)
*)
let load_city_data (s:string) : string quadtree = 
	let lst = parse s in
    let tr= new_tree ((~-.180., ~-.90.),(180.,90.)) in
    let add_city (tr : string quadtree) (a : city) = 
      match a with (b,c,d)-> insert tr (c, b) d in
	List.fold_left add_city tr lst

(*Finds all the cities and their locations of a given string quadtree
*Precondition: The quadtree is a valid string quadtree whose values are city
*names and the region is a valid region completely contained within the region
*spanned by the quadtree
*Postcondition: Returns a string list containing all the names and locations of the 
*cities found within the given region of the quadtree in the following format:
*"Latitude, Longitude, City Name"
*)
let city_search (q: string quadtree) (r : region) : string list = 
	fold_region (fun x y-> match fst y with (a,b) ->( string_of_float b^ "," ^string_of_float a^ "," ^snd y) :: x) [] q r 
