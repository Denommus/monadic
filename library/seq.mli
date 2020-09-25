module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a Stdlib.Seq.t Wrapped.t

  val create : 'a Stdlib.Seq.t Wrapped.t -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end
with type 'a t = 'a Stdlib.Seq.t Wrapped.t

module Make : sig
  include Monad.MAKE_T with type 'a wrapped := 'a

  val run : 'a t -> 'a Stdlib.Seq.t

  val create : 'a Stdlib.Seq.t -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end
with type 'a t = 'a Stdlib.Seq.t
