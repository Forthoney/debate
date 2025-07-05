signature ARG =
sig
  datatype 'a arity =
    Zero of unit -> 'a option
  | Optional of string option -> 'a option
  | AtLeastOne of string list -> 'a option
  | Any of string list -> 'a option
  | Exactly of int * (string list -> 'a option)

  (* pseudoconstructor *)
  val OptionalWithDefault: (string -> 'a option * 'string) -> 'a arity
  val set: 'config -> ('config -> 'a ref) -> 'a -> unit
end

structure Arg =
struct
  datatype 'a arity =
    Zero of unit -> 'a option
  | Optional of string option -> 'a option
  | AtLeastOne of string list -> 'a option
  | Any of string list -> 'a option
  | Exactly of int * (string list -> 'a option)

  (* pseudoconstructor *)
  fun OptionalWithDefault (f, default) =
    Optional (fn v => f (Option.getOpt (v, default)))

  fun set config prop newVal =
    SOME (prop config := newVal)
end
