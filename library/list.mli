module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a list Wrapped.t

  val lift : 'a list Wrapped.t -> 'a t
end
with type 'a t = 'a list Wrapped.t

module Make : sig
  include Monad.MAKE

  val run : 'a t -> 'a list

  val lift : 'a list -> 'a t
end
with type 'a t = 'a list
