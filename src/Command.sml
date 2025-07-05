structure Command =
struct
  fun new flags =
    let
      val helpMsg = String.concatWith "\n" (map Flag.toHelp flags)
      val help =
        { name = "--help"
        , alias = []
        , help = "show help"
        , args = Arg.Zero (fn () => SOME (print (helpMsg ^ "\n")))
        }
      open Combinator

      val flagActions = (repeat o or' o map Flag.toCombinator) flags
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
      fmap (Option.compose (ignore, eval)) flagActions
      or (fmap (fn f => f ()) o Flag.toCombinator) help
    end
end

val config = {verbose = ref ""}

val verbose =
  { name = "--verbose"
  , alias = []
  , args = Arg.OptionalWithDefault (Arg.set config #verbose, "default")
  , help = "verbosity"
  }

val _ = Command.new [verbose] (CommandLine.arguments ())
val _ = (print o ! o #verbose) config
