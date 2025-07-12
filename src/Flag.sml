structure Flag:
sig
  type 'a t =
    {name: string, alias: string list, help: string, args: 'a Argument.parser}

  val toCombinator: 'a t -> 'a Action.t Combinator.parser
end =
struct
  type 'a t =
    {name: string, alias: string list, help: string, args: 'a Argument.parser}

  fun toCombinator {name, alias, args, help} =
    let
      open Combinator
      infix andThen

      val allParsers =
        map (fn name => exact name andThen args name) (name :: alias)
    in
      or' allParsers
    end
end
