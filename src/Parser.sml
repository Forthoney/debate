structure Parser =
struct
  datatype args =
    Zero of unit -> unit
  | Optional of string option -> unit
  | AtLeastOne of string list -> unit
  | Any of string list -> unit
  | Exactly of int * (string list -> unit)

  type flag = {name: string, args: args}

  fun toCombinator {name, args} =
    let
      open Combinator
      infix andThen
      infix or

      val flag = exact name
      fun multiArg action range =
        fmap action (flag andThen consumeRange range)
    in
      case args of
        Zero action => fmap (action o ignore) flag
      | Optional action => fmap action (flag andThen try (satisfy (not o String.isPrefix "-")))
      | AtLeastOne action => multiArg action {low = 1, hi = NONE}
      | Any action => multiArg action {low = 0, hi = NONE}
      | Exactly (n, action) => multiArg action {low = n, hi = SOME n}
    end
    
end
