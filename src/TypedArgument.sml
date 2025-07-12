signature TYPED_ARGUMENT =
sig
  type t
  type 'a arg_info = string -> 'a Action.t Combinator.parser
  val optional: (string * (t option -> 'a option)) -> string * 'a arg_info
  val optionalWithDefault: (string * (t -> 'a option) * t) -> string * 'a arg_info
  val one: (string * (t -> 'a option)) -> string * 'a arg_info
end

functor TypedArgumentFn
  (Cvt:
   sig
     type t
     val fromString: string -> t option
   end): TYPED_ARGUMENT =
struct
  type t = Cvt.t
  type 'a arg_info = string -> 'a Action.t Combinator.parser

  open Combinator

  fun optional (metavar, f) =
    ("[" ^ metavar ^ "]",
    fn name => fmap
      (fn v =>
         fn () =>
           (Action.unwrap (name, v) o f
            o Option.mapPartial Cvt.fromString) v)
      (try (satisfy (not o String.isPrefix "-"))))

  fun optionalWithDefault (metavar, f, default) =
    optional (metavar, fn v => f (Option.getOpt (v, default)))

  fun one (metavar, f) =
    (metavar, 
    fn name => fmap
      (fn v =>
         fn () =>
           (Action.unwrap (name, SOME v)
            o Option.composePartial (f, Cvt.fromString)) v)
      (satisfy (not o String.isPrefix "-")))
end
