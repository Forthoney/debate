structure Anonymous:
sig
  type 'a t = {metavar: string, args: 'a BaseArgument.arity}

  val toHelp: 'a t -> string

  val toCombinator: 'a t -> (unit -> 'a option) Combinator.parser
end =
struct
  type 'a t = {metavar: string, args: 'a BaseArgument.arity}

  open BaseArgument

  fun toHelp {metavar, args} =
    case args of
      Zero _ => ""
    | Optional _ => "[" ^ metavar ^ "]"
    | AtLeastOne _ => metavar ^ "..."
    | Any _ => "[" ^ metavar ^ "]" ^ "..."
    | Exactly (n, _) =>
        let
          fun repeat i =
            if i <= n then metavar ^ Int.toString i ^ " " ^ repeat (i + 1)
            else ""
        in
          repeat 0
        end

  fun toCombinator {args, metavar} = BaseArgument.toCombinator args
end
