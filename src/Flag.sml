signature FLAG =
sig
  datatype args =
    Zero of unit -> unit
  | Optional of string option -> unit
  | AtLeastOne of string list -> unit
  | Any of string list -> unit
  | Exactly of int * (string list -> unit)

  type t = {name: string, alias: string list, help: string, args: args}

  val toCombinator: t -> (unit -> unit) Combinator.parser
  val toHelp: t -> string
end

structure Flag: FLAG =
struct
  datatype args =
    Zero of unit -> unit
  | Optional of string option -> unit
  | AtLeastOne of string list -> unit
  | Any of string list -> unit
  | Exactly of int * (string list -> unit)

  type t = {name: string, alias: string list, help: string, args: args}

  fun toCombinator {name, alias, args, help} =
    let
      open Combinator
      val expanded = {name = name, args = args} :: map (fn s => {name = s, args = args}) alias
      fun convert {name, args} =
        let
          infix andThen
          infix or

          fun lazy f v () = f v

          val flag = exact name
          fun multiArg action range =
            fmap (lazy action) (flag andThen consumeRange range)
        in
          case args of
            Zero action => fmap (lazy action o ignore) flag
          | Optional action =>
              fmap (lazy action)
                (flag andThen try (satisfy (not o String.isPrefix "-")))
          | AtLeastOne action => multiArg action {low = 1, hi = NONE}
          | Any action => multiArg action {low = 0, hi = NONE}
          | Exactly (n, action) => multiArg action {low = n, hi = SOME n}
        end
    in
      or' (map convert expanded)
    end
    
  fun toHelp {name, alias, help, args} =
    "  " ^ String.concatWith ", " (name :: alias) ^ "\t" ^ help
end
