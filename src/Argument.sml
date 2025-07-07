structure Argument =
struct
  structure Int =
    TypedArgumentFn (struct type a = int val fromString = Int.fromString end)
  structure Real =
    TypedArgumentFn (struct type a = real val fromString = Real.fromString end)
  structure Bool =
    TypedArgumentFn (struct type a = bool val fromString = Bool.fromString end)

  val zero = BaseArgument.Zero

  fun set target value =
    SOME (target := value)
end
