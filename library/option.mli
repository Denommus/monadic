module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a option Wrapped.t

  val lift : 'a option Wrapped.t -> 'a t
end
with type 'a t = 'a option Wrapped.t

module Make : sig
  include Monad.MAKE

  val run : 'a t -> 'a option

  val lift : 'a option -> 'a t

  val none : unit -> 'a t
end
with type 'a t = 'a option
