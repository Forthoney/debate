structure Action:
sig
  type 'a t = unit -> 'a
  exception Action of {name: string, cause: string option}

  val set: 'a ref -> 'a -> unit option
  val unwrap: string * string option -> 'a option -> 'a
end =
struct
  type 'a t = unit -> 'a
  exception Action of {name: string, cause: string option}

  fun set target value =
    SOME (target := value)

  fun unwrap _ (SOME v) = v
    | unwrap (name, cause) NONE =
        raise Action {name = name, cause = cause}
end
