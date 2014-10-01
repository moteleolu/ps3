(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t

  (*val zero is the identity element for addition. Adding val zero to a number 
    n will return n.
  *)
  val zero : t

  (*val one is the identity element for multiplication. Multiplying a number n 
    by val one will retrun n.
  *)
  val one : t

  (*Precondition: Takes two valid natural numbers as arguments.
    Postcondition: Returns the sum of the two values as a natural number.
    Computes the sum of two values. 
    + is associative: (a+b)+c = a+(b+c)
    + is commutative: a+b = b+a
    val zero is the identity element for +
  *)
  val ( + ) : t -> t -> t

  (*Precondition: Takes two valid natural numbers as arguments.
    Postcondition: Returns the product of the two values as a natural number.
    Computes the product of two values.
    * is associative: (a*b)*c = a*(b*c)
    * is commutative: a*b = b*a
    * can be distributed over +: a*(b+c) = (a*b)+(a*c)
    val one is the identity element for *
  *)
  val ( * ) : t -> t -> t 

  (*Precondition: Takes two valid natural numbers as arguments.
    Postcondition: Returns true if the first argument is less than the second 
    argument and false otherwise.
    Determines whether the first argument is less than the second argument, 
    returning true if it is and false if it is not.
  *)
  val ( < ) : t -> t -> bool

  (*Precondition: Takes two valid natural numbers as arguments.
    Postcondition: Returns true if the two arguments are equal to each other 
    and false otherwise.
    Determines whether the first argument is equal to the second argument, 
    returning true if it is and false if it is not.
  *)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable

  (*Precondition: Takes a valid natural number as an argument.
    Postcondition: Returns a valid value of type int.
    Converts a natural number to an int if the natural number can be 
    represented as a valid int. If it cannot, an exception is thrown.
  *)
  val int_of_nat: t -> int

  (*Precondition: Takes a valid type int as an argument.
    Postcondition: Returns a valide natrual number.
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

let ( + ) t1 t2 = if t1 + t2 < 0 || sum_overflows t1 t2 then raise (Unrepresentable) else t1+t2  
let ( * ) t1 t2 = if t1 * t2 < 0 || prod_overflows t1 t2 then raise (Unrepresentable) else t1*t2
let ( < ) t1 t2 =  t1<t2
let ( === ) t1 t2 = t1=t2

let int_of_nat t1 = t1
let nat_of_int n1 = if n1 < 0 the raise Unrepresentable else n1
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

let rec nat_of_int n1 = if Pervasives.( < ) n1 0 then raise Unrepresentable else 
match n1 with 0 -> zero
|n -> 1:: nat_of_int (n-1)   


let ( + ) (t1:int list) (t2:int list) = 
if sum_overflows (List.length t1) (List.length t2)
then raise (Unrepresentable) else nat_of_int (List.length t1 + List.length t2) 

let ( * ) (t1:int list) (t2:int list)  = if prod_overflows (List.length t1) (List.length t2) then 
raise (Unrepresentable) else nat_of_int ((List.length t1) * (List.length t2))

let ( < ) t1 t2 = List.length t1 < List.length t2

let ( === ) t1 t2 = List.length t1 = List.length t2

let int_of_nat t1 = List.length t1


end
