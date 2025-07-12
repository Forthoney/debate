structure Argument =
struct
  fun none f =
    ("", fn name => Combinator.fmap (fn () => Action.unwrap (name, NONE) o f)
      Combinator.noOp)

  fun flag {name, alias, desc, args = (metavar, args)} =
    { name = name
    , alias = alias
    , help = "  " ^ String.concatWith ", " (name :: alias) ^ metavar ^ "\t" ^ desc
    , args = args
    }

  fun anonymous (metavar, args) =
    { metavar = metavar
    , args = args
    }

  structure Int =
    TypedArgumentFn (struct type t = int val fromString = Int.fromString end)
  structure Real =
    TypedArgumentFn (struct type t = real val fromString = Real.fromString end)
  structure Bool =
    TypedArgumentFn (struct type t = bool val fromString = Bool.fromString end)
  structure String =
    TypedArgumentFn (struct type t = string val fromString = SOME end)
end
