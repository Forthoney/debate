structure Argument =
struct
  type 'a parser = 'a ArgumentMeta.parser
  type 'a t = 'a ArgumentMeta.arg

  fun none f =
    { metavar = ""
    , parser = fn name =>
        Combinator.fmap (fn () => Action.unwrap (name, NONE) o f)
          Combinator.noOp
    }

  fun flag {name, alias, desc, args = {metavar, parser}} =
    { name = name
    , alias = alias
    , help =
        "  " ^ String.concatWith ", " (name :: alias) ^ metavar ^ "\t" ^ desc
    , args = parser
    }

  fun anonymous (metavar, args) = {metavar = metavar, args = args}

  structure Int =
    TypedArgumentFn (struct type t = int val fromString = Int.fromString end)
  structure Real =
    TypedArgumentFn (struct type t = real val fromString = Real.fromString end)
  structure Bool =
    TypedArgumentFn (struct type t = bool val fromString = Bool.fromString end)
  structure String =
    TypedArgumentFn (struct type t = string val fromString = SOME end)
end
