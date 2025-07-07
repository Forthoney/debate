structure BaseArgument:
sig
  datatype 'a arity =
    Zero of unit -> 'a option
  | Optional of string option -> 'a option
  | AtLeastOne of string list -> 'a option
  | Any of string list -> 'a option
  | Exactly of int * (string list -> 'a option)

  val toCombinator: 'a arity -> (unit -> 'a option) Combinator.parser
end =
struct
  datatype 'a arity =
    Zero of unit -> 'a option
  | Optional of string option -> 'a option
  | AtLeastOne of string list -> 'a option
  | Any of string list -> 'a option
  | Exactly of int * (string list -> 'a option)

  fun toCombinator arg =
    let
      fun lazy f v () = f v
      open Combinator
    in
      case arg of
        Zero action => fmap (lazy action) noOp
      | Optional action =>
          fmap (lazy action) (try (satisfy (not o String.isPrefix "-")))
      | AtLeastOne action =>
          fmap (lazy action) (consumeRange {low = 1, hi = NONE})
      | Any action => fmap (lazy action) (consumeRange {low = 0, hi = NONE})
      | Exactly (n, action) =>
          fmap (lazy action) (consumeRange {low = n, hi = SOME n})
    end
end
