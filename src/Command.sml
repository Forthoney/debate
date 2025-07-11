signature COMMAND =
sig
  type ret
  val description: string
  val flags: ret Flag.t list
  val trailing: ret Anonymous.t
end

functor CommandFn(Cmd: COMMAND):
sig
  val parse: string list -> Cmd.ret list
end =
struct
  exception Help of string

  val helpMsg = ref ""

  val helpFlag =
    { name = "--help"
    , alias = []
    , help = "show help"
    , args = Argument.none (fn _ => raise Help (!helpMsg))
    }

  val allFlags = Cmd.flags @ [helpFlag]

  (* This weird mutation hack is needed because help needs its own help message at instantiation time *)
  val _ =
    let
      val usage =
        String.concatWith " "
          ["Usage:", CommandLine.name (), Anonymous.toHelp Cmd.trailing]
    in
      helpMsg
      :=
      String.concatWith "\n"
        (Cmd.description :: usage :: "\nOptions" :: map Flag.toHelp allFlags)
    end

  fun parse args =
    let
      open Combinator

      val flagActions = (repeat o or' o map Flag.toCombinator) allFlags
      val argAction = Anonymous.toCombinator Cmd.trailing
      val allActions = bind (flagActions, fn actions =>
        fmap (fn a => a :: actions) argAction)
    in
      case allActions args of
        Result.Ok (actions, []) => map (fn act => act ()) actions
      | Result.Ok (_, x::xs) => raise Fail ("unexpected argument " ^ x)
      | Result.Err e => raise Fail e
    (* fmap (Option.compose (ignore, eval)) allActions args *)
    (* handle (Help s) => (print (s ^ "\n"); OS.Process.exit OS.Process.success) *)
    end

end

val level = ref 0
val other = ref 10
val file = ref false

val verbose =
  { name = "--verbose"
  , alias = []
  , args = Argument.Int.optionalWithDefault (Action.set level, 20)
  , help = "verbosity"
  }

val file =
  { name = "--file"
  , alias = ["-f"]
  , args = Argument.Bool.optionalWithDefault (Action.set file, true)
  , help = "file thingy"
  }

val anon =
  { metavar = "FILE"
  , args = Argument.Int.optionalWithDefault (Action.set other, 0)
  }

structure Demo =
struct
  type ret = unit
  val description = "A demo command"
  val flags = [verbose, file]
  val trailing = anon
end

structure DemoFn = CommandFn(Demo)

val _ = DemoFn.parse (CommandLine.arguments ())

val _ = (print o Int.toString o !) level
val _ = (print o Int.toString o !) other
