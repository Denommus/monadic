module MakeT : functor
  (Wrapped : Monad.MONAD)
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> ('a, e) result Wrapped.t

  val lift : ('a, e) result Wrapped.t -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result Wrapped.t

module Make : functor
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE

  val run : 'a t -> ('a, e) result

  val lift : ('a, e) result -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result
