structure Anonymous:
sig
  type 'a t = {metavar: string, args: 'a BaseArgument.arity}

  val toCombinator: 'a t -> (unit -> 'a option) Combinator.parser
end =
struct
  type 'a t = {metavar: string, args: 'a BaseArgument.arity}

  fun toCombinator {args, metavar} =
    BaseArgument.toCombinator args
end
