(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t

  (*val zero is the identity element for addition. Adding val zero to a number 
    n will return n.
  *)
  val zero : t

  (*val one is the identity element for multiplication. Multiplying a number n 
    by val one will return n.
  *)
  val one : t

  (*Precondition: Takes two arguments of the same type.
    Postcondition: Returns the sum of the two values as a value of the same 
    type of the arguments.
    Computes the sum of two values. 
    + is associative: (a+b)+c = a+(b+c)
    + is commutative: a+b = b+a
    val zero is the identity element for +
  *)
  val ( + ) : t -> t -> t

  (*Precondition: Takes two arguments of the same type.
    Postcondition: Returns the product of the two values as a value of the 
    same type of the arguments.
    Computes the product of two values.
    * is associative: (a*b)*c = a*(b*c)
    * is commutative: a*b = b*a
    * can be distributed over +: a*(b+c) = (a*b)+(a*c)
    val one is the identity element for *
  *)
  val ( * ) : t -> t -> t 

  (*Precondition: Takes two arguments of the same type.
    Postcondition: Returns true if the first argument is less than the second 
    argument and false otherwise.
    Determines whether the first argument is less than the second argument, 
    returning true if it is and false if it is not.
  *)
  val ( < ) : t -> t -> bool

  (*Precondition: Takes two arguments of the same type.
    Postcondition: Returns true if the two arguments are equal to each other 
    and false otherwise.
    Determines whether the first argument is equal to the second argument, 
    returning true if it is and false if it is not.
  *)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable

  (*Precondition: Takes one valid natural number as an argument.
    Postcondition: Returns a valid value of type int.
    Converts a natural number to an int if the natural number can be 
    represented as a valid int. If it cannot, an exception is thrown.
  *)
  val int_of_nat: t -> int

  (*Precondition: Takes a valid type int as an argument.
    Postcondition: Returns the int as a valid natrual number.
    Converts a value of type int to a natural number if the int can be 
    represented as a valid natrual number. If it cannot, an exception is thrown.
  *)
  val nat_of_int: int -> t
end

module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end

type sign = Positive | Negative
let sign_int (n:int) : sign = 
  if n >= 0 then Positive else Negative
let sum_overflows (i1:int) (i2:int) : bool = 
  sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
(* END: DO NOT CHANGE THIS CODE *)

(* Add your solution here for IntNat, ListNat, NatConvertFn, 
   and AlienNatFn, being careful to use the declarations and
   types specified in the problem set. *)

module IntNat: NATN = struct 
  type t = int 
  exception Unrepresentable
  let zero= 0
  let one =1


  type sign = Positive | Negative 

  let sign_int (n:int) : sign = if n >= 0 then Positive else Negative

  let sum_overflows (i1:int) (i2:int) : bool = 
    sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1

  let prod_overflows (i1:int) (i2:int) :bool=
    sign_int i1 = sign_int i2 && sign_int (i1 * i2)<> sign_int i1
    
  let ( + ) t1 t2 = if t1 + t2 < 0 || sum_overflows t1 t2 
                    then raise (Unrepresentable) else t1+t2  
  let ( * ) t1 t2 = if t1 * t2 < 0 || prod_overflows t1 t2 
                    then raise (Unrepresentable) else t1*t2
  let ( < ) t1 t2 =  t1<t2
  let ( === ) t1 t2 = t1=t2

  let int_of_nat t1 = t1
  let nat_of_int n1 = if n1 < 0 || n1= max_int then raise Unrepresentable else n1
end

module ListNat: NATN = struct
(* The list [a1; ...; an] represents the
* natural number n. That is, the list lst represents
* length(lst). The empty list represents 0. The values of * the list elements are irrelevant. *)
  type t =  int list
  exception Unrepresentable 

  let zero =[]
  let one = [1]

  type sign = Positive | Negative 

  let sign_int (n:int) : sign = if n >= 0 then Positive else Negative

  let sum_overflows (i1:int) (i2:int) : bool = 
    sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1

  let prod_overflows (i1:int) (i2:int) :bool=
    sign_int i1 = sign_int i2 && sign_int (i1 * i2)<> sign_int i1

  let rec nat_of_int n1 = if Pervasives.( < ) n1 0 || n1 = max_int then raise Unrepresentable else 
  match n1 with 
    0 -> zero
  | n -> 1:: nat_of_int (n-1)   


  let ( + ) (t1:int list) (t2:int list) = 
  if sum_overflows (List.length t1) (List.length t2)
  then raise (Unrepresentable) else nat_of_int (List.length t1 + List.length t2) 

  let ( * ) (t1:int list) (t2:int list)  = if prod_overflows (List.length t1) (List.length t2) then 
  raise (Unrepresentable) else nat_of_int ((List.length t1) * (List.length t2))

  let ( < ) t1 t2 = List.length t1 < List.length t2

  let ( === ) t1 t2 = List.length t1 = List.length t2

  let int_of_nat t1 = List.length t1
end


module NatConvertFn (N: NATN) = struct
  let int_of_nat (n : N.t) : int =
    (*match n with 
      zero -> 0
    | one -> 1
    | (one + nat) -> int_of_nat nat + 1*)
    (*let count = N.zero in 
    let ans = 0 in 
    while ((not (count N.=== n)) & count <= max_int) do 
      let ans = ans + 1 in 
      let count = count + N.one in count
    done*)
    N.int_of_nat (n)

    (*if max_int < n.int_of_nat then raise (Unrepresentable) else n.int_of_nat*)
  let rec nat_of_int (n: int) : N.t = 
    (*match n with 
      0 -> N.zero
    | 1 -> N.one
    | _ -> (+) N.one nat_of_int(n-1)*)
    N.nat_of_int (n)
end


module AlienNatFn (M: AlienMapping): NATN = struct 
type t = M.aliensym list
exception Unrepresentable 

let zero =[M.zero]
let one = [M.one]

let ( + ) t1 t2= List.fold_left (fun x y-> y::x) t1 t2 

let ( * ) t1 t2= 
let rec helper (x:M.aliensym list) (n:int) = (match n with 1-> x
| y -> ( + ) x (helper x (n-1)))  
in 
let i= List.fold_left (fun x y-> Pervasives.(+) (M.int_of_aliensym y) x) 0 t1  in
helper t2 i

let ( < ) t1 t2 = (List.fold_left (fun x y-> Pervasives.(+) (M.int_of_aliensym y) x) 0 t1) < 
(List.fold_left (fun x y-> Pervasives.(+) (M.int_of_aliensym y) x) 0 t2 ) 

let ( === ) t1 t2 = (List.fold_left (fun x y-> Pervasives.(+) (M.int_of_aliensym y) x) 0 t1) =
(List.fold_left (fun x y-> Pervasives.(+) (M.int_of_aliensym y) x) 0 t2 )

let int_of_nat t1 = List.fold_left (fun x y-> Pervasives.(+) (M.int_of_aliensym y) x) 0 t1

let rec nat_of_int i1 = match i1 with 0 -> zero |1 -> one
|n -> ( + ) one (nat_of_int (n-1))  


end 

