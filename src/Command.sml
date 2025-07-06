structure Command =
struct
  exception Help of string

  fun parse flags args =
    let
      val helpMsg = String.concatWith "\n" (map Flag.toHelp flags) ^ "\nshow help\n"
      val help =
        { name = "--help"
        , alias = []
        , help = ""
        , args = Argument.zero (fn () => raise Help helpMsg)
        }
      open Combinator

      val flagActions = (repeat o or' o map Flag.toCombinator) (help::flags)
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
      fmap (Option.compose (ignore, eval)) flagActions args
      handle (Help s) => (print s; OS.Process.exit OS.Process.success)
    end
end

val level = ref 0

val verbose =
  { name = "--verbose"
  , alias = []
  , args = Argument.Int.optionalWithDefault (Argument.set level, 20)
  , help = "verbosity"
  }

val _ = Command.parse [verbose] (CommandLine.arguments ())
val _ = (print o Int.toString o !) level
