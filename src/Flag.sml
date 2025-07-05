signature FLAG =
sig
  datatype args =
    Zero of unit -> unit
  | Optional of string option -> unit
  | AtLeastOne of string list -> unit
  | Any of string list -> unit
  | Exactly of int * (string list -> unit)

  type t = {name: string, short: char option, help: string, args: args}

  val toCombinator: t -> unit Combinator.parser
  val toHelp: t -> string
end

local open Result
in
  structure Flag: FLAG =
  struct
    datatype args =
      Zero of unit -> unit
    | Optional of string option -> unit
    | AtLeastOne of string list -> unit
    | Any of string list -> unit
    | Exactly of int * (string list -> unit)

    type t = {name: string, short: char option, help: string, args: args}

    fun shortToString c = "-" ^ Char.toString c

    fun toCombinator {name, short, args, help} =
      let
        open Combinator
        infix andThen
        infix or
        val flag =
          case short of
            SOME short => exact name or exact (shortToString short)
          | NONE => exact name
        fun argList action range =
          fmap action (flag andThen consumeRange range)
      in
        case args of
          Zero action => fmap (action o ignore) flag
        | Optional action => raise Fail "impl"
        | AtLeastOne action => argList action {low = 1, hi = NONE}
        | Any action => argList action {low = 0, hi = NONE}
        | Exactly (n, action) => argList action {low = n, hi = SOME n}
      end

    fun toHelp {name, short, args, help} =
      let
        val short = Option.getOpt
          (Option.map (fn s => shortToString s ^ ", ") short, "    ")
      in
        "  " ^ short ^ name ^ "\t" ^ help
      end
  end
end
