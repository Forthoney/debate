structure Argument =
struct
  structure Int =
    TypedArgumentFn (struct type t = int val fromString = Int.fromString end)
  structure Real =
    TypedArgumentFn (struct type t = real val fromString = Real.fromString end)
  structure Bool =
    TypedArgumentFn (struct type t = bool val fromString = Bool.fromString end)

  structure String =
    TypedArgumentFn (struct type t = string val fromString = SOME end)

  fun none f name =
    Combinator.fmap (fn () => Action.unwrap (name, NONE) o f)
      Combinator.noOp
end
