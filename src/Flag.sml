structure Flag:
sig
  type 'a t =
    { name: string
    , alias: string list
    , help: string
    , args: 'a BaseArgument.arity
    }

  val toCombinator: 'a t -> (unit -> 'a option) Combinator.parser
  val toHelp: 'a t -> string
end =
struct
  type 'a t =
    { name: string
    , alias: string list
    , help: string
    , args: 'a BaseArgument.arity
    }

  fun toCombinator {name, alias, args, help} =
    let
      open Combinator
      infix andThen

      val args = BaseArgument.toCombinator args
      val allParsers = map (fn name => exact name andThen args) (name :: alias)
    in
      or' allParsers
    end

  fun toHelp {name, alias, help, args} =
    "  " ^ String.concatWith ", " (name :: alias) ^ "\t" ^ help
end
