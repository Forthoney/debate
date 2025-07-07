functor TypedArgumentFn
  (T:
   sig
     type a
     val fromString: string -> a option
   end) =
struct
  type a = T.a

  open BaseArgument

  fun listConvert f args =
    let val converted = List.mapPartial T.fromString args
    in if length converted = length args then f converted else NONE
    end

  fun optional f =
    Optional (f o Option.mapPartial T.fromString)
  fun optionalWithDefault (f, default) =
    optional (fn v => f (Option.getOpt (v, default)))
  val atLeastOne = AtLeastOne o listConvert
  val any = Any o listConvert
  fun exactly (n, f) =
    Exactly (n, listConvert f)
end
