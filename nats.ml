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


