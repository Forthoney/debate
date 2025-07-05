structure Command =
struct
  fun new flags =
    let
      val helpMsg = String.concatWith "\n" (map Flag.toHelp flags)
      val help =
        { name = "--help"
        , alias = []
        , help = "show help"
        , args = Flag.Zero (fn () => print (helpMsg ^ "\n"))
        }
      open Combinator
    in
      or
        ( (fmap (List.app (fn f => f ())) o repeat o or' o map Flag.toCombinator)
            flags
        , (fmap (fn f => f ()) o Flag.toCombinator) help
        )
    end
end

val verbose =
  { name = "--verbose"
  , alias = []
  , args = Flag.Zero (fn () => print ("verbose" ^ "\n"))
  , help = "verbosity"
  }

val _ = Command.new [verbose] ["--foo", "--help"]
