structure Anonymous:
sig
  type 'a t = {metavar: string, args: string -> 'a Action.t Combinator.parser}

  val toHelp: 'a t -> string

  val toCombinator: 'a t -> 'a Action.t Combinator.parser
end =
struct
  type 'a t = {metavar: string, args: string -> 'a Action.t Combinator.parser}

  fun toHelp {metavar, args} = metavar

  fun toCombinator {args, metavar} = args metavar
end
