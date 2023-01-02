(** stores the line and position of the token *)
type loc = Lexing.position

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module Var_name : ID
module Object_name : ID
module Field_name : ID
module Method_name : ID
module Function_name : ID

(** determines if a value is mutable *)
type modifier = Mutable

(** determines if a reference is being borrowed *)
type borrowed_ref = Borrowed

type generic_type = Generic

(** Defines types of expressions in a program*)
type type_expr =
  | TEInt
  | TEObject   of Object_name.t * type_expr option
      (** specify type parameters for generic classes(optional) *)
  | TEVoid
  | TEFacts
  | TEYarn
  | TEGeneric


type field_defn = TField of modifier option * type_expr * Field_name.t

(** Parameter of a function can be "borrowed" *)
type param = TParam of type_expr * Var_name.t * borrowed_ref option

val get_params_types : param list -> type_expr list

(** Binary operators for expressions *)
type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpIntDiv
  | BinOpMod
  | BinOpLessThan
  | BinOpLessThanEq
  | BinOpGreaterThan
  | BinOpGreaterThanEq
  | BinOpAnd
  | BinOpOr
  | BinOpEq
  | BinOpNotEq

(** unary operators *)
type un_op = UnOpNot | UnOpNeg

val string_of_loc : loc -> string
(** helper functions to convert types to equivalent string representations*)

val string_of_modifier : modifier -> string
val string_of_type : type_expr -> string
val string_of_bin_op : bin_op -> string
val string_of_un_op : un_op -> string
val string_of_maybe_borrowed : borrowed_ref option -> string
val string_of_maybe_generic : generic_type option -> string

(** Exceptions *)

(** Thrown if a later compiler stage encounters generic type but expects it to be
    desugared *)
exception NotDesugaredGenericType of string
