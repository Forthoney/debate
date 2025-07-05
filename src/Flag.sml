structure Flag:
sig
  type 'a t =
    {name: string, alias: string list, help: string, args: 'a BaseArgument.arity}

  val toCombinator: 'a t -> (unit -> 'a option) Combinator.parser
  val toHelp: 'a t -> string
end =
struct
  type 'a t =
    {name: string, alias: string list, help: string, args: 'a BaseArgument.arity}

  fun toCombinator {name, alias, args, help} =
    let
      open Combinator
      val expanded =
        {name = name, args = args}
        :: map (fn s => {name = s, args = args}) alias
      fun convert {name, args} =
        let
          infix andThen
          infix or

          (* Do not execute/evaluate the actions yet since a failure may take place further down the line *)
          fun lazy f v () = f v

          val flag = exact name
          fun multiArg action range =
            fmap (lazy action) (flag andThen consumeRange range)
        in
          case args of
            BaseArgument.Zero action => fmap (lazy action o ignore) flag
          | BaseArgument.Optional action =>
              fmap (lazy action)
                (flag andThen try (satisfy (not o String.isPrefix "-")))
          | BaseArgument.AtLeastOne action => multiArg action {low = 1, hi = NONE}
          | BaseArgument.Any action => multiArg action {low = 0, hi = NONE}
          | BaseArgument.Exactly (n, action) => multiArg action {low = n, hi = SOME n}
        end
    in
      or' (map convert expanded)
    end

  fun toHelp {name, alias, help, args} =
    "  " ^ String.concatWith ", " (name :: alias) ^ "\t" ^ help
end
