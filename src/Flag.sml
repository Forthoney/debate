signature FLAG =
sig
  type t = {name: string, alt: string option, help: string, arity: Arity.t}
  type flag_info = {name: string, alt: string option, help: string}

  val binaryFlag: flag_info -> t
  val parse: t -> string list Combinator.parser
end

local open Result
in
  structure Flag: FLAG =
  struct
    type t = {name: string, alt: string option, help: string, arity: Arity.t}
    type flag_info = {name: string, alt: string option, help: string}

    fun binaryFlag {name, alt, help} =
      {name = name, alt = alt, help = help, arity = Arity.zero}

    fun parse {name, alt, arity, help} xs =
      let
        open Combinator
        val it =
          case alt of
            SOME alt => or (exact name, exact alt) xs
          | NONE => exact name xs
      in
        case it of
          Err e => Err e
        | Ok (name, xs) => consumeRange arity xs
      end
  end
end

val f = Flag.binaryFlag {name = "--help", alt = NONE, help = "show help"}
val _ =
  case Flag.parse f ["--help"] of
    Result.Ok v => ()
  | Result.Err e => raise Fail e

val _ =
  case Flag.parse f ["help"] of
    Result.Ok v => raise Fail "unexpected"
  | Result.Err e => ()
