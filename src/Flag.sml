structure Flag:
sig
  type 'a t =
    { name: string
    , alias: string list
    , help: string
    , args: string -> 'a Action.t Combinator.parser
    }

  val toCombinator: 'a t -> 'a Action.t Combinator.parser
  val toHelp: 'a t -> string
end =
struct
  type 'a t =
    { name: string
    , alias: string list
    , help: string
    , args: string -> 'a Action.t Combinator.parser
    }

  fun toCombinator {name, alias, args, help} =
    let
      open Combinator
      infix andThen

      val allParsers =
        map (fn name => exact name andThen args name) (name :: alias)
    in
      or' allParsers
    end

  fun toHelp {name, alias, help, args} =
    "  " ^ String.concatWith ", " (name :: alias) ^ "\t" ^ help
end
