signature COMMAND =
sig
  val description: string
  val flags: unit Flag.t list
  val trailing: unit Anonymous.t
end

functor CommandFn(Cmd: COMMAND) =
struct
  exception Help of string

  val helpMsg = String.concatWith "\n"
    (Cmd.description
     ::
     String.concatWith " "
       ["Usage:", CommandLine.name (), Anonymous.toHelp Cmd.trailing]
     :: "\nOptions:" :: map Flag.toHelp Cmd.flags @ ["  --help\tshow help"])

  val helpFlag =
    { name = "--help"
    , alias = []
    , help = "show help"
    , args = Argument.zero (fn () => raise Help helpMsg)
    }

  val allFlags = Cmd.flags @ [helpFlag]

  fun parse args =
    let
      open Combinator
      infix andThen

      val flagActions = (repeat o or' o map Flag.toCombinator) allFlags
      val argAction = Anonymous.toCombinator Cmd.trailing
      val allActions = bind (flagActions, fn actions =>
        fmap (fn a => a :: actions) argAction)
      val eval =
        let
          fun loop acc [] =
                SOME (rev acc)
            | loop acc (action :: rest) =
                case action () of
                  SOME v => loop (v :: acc) rest
                | NONE => NONE
        in
          loop []
        end
      infix or
    in
      fmap (Option.compose (ignore, eval)) allActions args
      handle (Help s) => (print (s ^ "\n"); OS.Process.exit OS.Process.success)
    end

end

val level = ref 0
val other = ref 10
val file = ref false

val verbose =
  { name = "--verbose"
  , alias = []
  , args = Argument.Int.optionalWithDefault (Argument.set level, 20)
  , help = "verbosity"
  }

val file =
  { name = "--file"
  , alias = ["-f"]
  , args = Argument.Bool.optionalWithDefault (Argument.set file, true)
  , help = "file thingy"
  }

val anon =
  { metavar = "FILE"
  , args = Argument.Int.optionalWithDefault (Argument.set other, 0)
  }

structure Demo =
struct
  val description = "A demo command"
  val flags = [verbose, file]
  val trailing = anon
end

structure DemoFn = CommandFn(Demo)

val _ =
  case DemoFn.parse (CommandLine.arguments ()) of
    Result.Ok (_, _) => ()
  | Result.Err e => raise Fail e

val _ = (print o Int.toString o !) level
val _ = (print o Int.toString o !) other
