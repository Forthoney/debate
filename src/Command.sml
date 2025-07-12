signature COMMAND =
sig
  type ret
  val description: string
  val flags: ret Flag.t list
  val anonymous: ret Anonymous.t
end

functor CommandFn(Cmd: COMMAND):
sig
  exception Help of string
  val parse: string list -> Cmd.ret list
end =
struct
  exception Help of string

  val helpMsg = ref ""

  val help = Argument.flag
    { name = "--help"
    , alias = []
    , desc = "show help"
    , args = Argument.none (fn _ => raise Help (!helpMsg))
    }

  val allFlags = Cmd.flags @ [help]

  (* This weird mutation hack is needed because help needs its own help message at instantiation time *)
  val _ =
    let
      val usage =
        String.concatWith " "
          ["Usage:", CommandLine.name (), Anonymous.toHelp Cmd.anonymous]
    in
      helpMsg
      :=
      String.concatWith "\n"
        (Cmd.description :: usage :: "\nOptions" :: map #help allFlags)
    end

  fun parse args =
    let
      open Combinator

      val flagActions = (repeat o or' o map Flag.toCombinator) allFlags
      val argAction = Anonymous.toCombinator Cmd.anonymous
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
val other = ref ""

val verbose = Argument.flag
  { name = "--verbose"
  , alias = []
  , args = Argument.Int.optionalWithDefault ("VERBOSITY", Action.set level, 20)
  , desc = "veerbosity control"
  }

structure Demo =
struct
  type ret = unit
  val description = "A demo command"
  val flags = [verbose]
  val anonymous = Argument.anonymous (Argument.String.optionalWithDefault ("FILE", Action.set other, "cat"))
end

structure DemoFn = CommandFn(Demo)

val _ = DemoFn.parse (CommandLine.arguments ()) handle DemoFn.Help s => (print s; [])

val _ = (print o Int.toString o !) level
val _ = (print o !) other
