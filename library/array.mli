module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a array Wrapped.t

  val create : 'a array Wrapped.t -> 'a t
end
with type 'a t = 'a array Wrapped.t

module Make : sig
  include Monad.MAKE

  val run : 'a t -> 'a array

  val create : 'a array -> 'a t
end
with type 'a t = 'a array
