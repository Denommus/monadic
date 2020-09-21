module MakeT : functor
  (Wrapped : Monad.MONAD)
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> ('a, e) result Wrapped.t

  val create : ('a, e) result Wrapped.t -> 'a t

  val error : e -> 'a t

  val ok : 'a -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result Wrapped.t

module Make : functor
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE_T with type 'a wrapped := 'a

  val run : 'a t -> ('a, e) result

  val create : ('a, e) result -> 'a t

  val error : e -> 'a t

  val ok : 'a -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result

module MakePlusT : functor (Wrapped : Monad.MONAD) (E : Monad.MONOID) -> sig
  type e

  include Monad.MAKE_PLUS_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> ('a, e) result Wrapped.t

  val create : ('a, e) result Wrapped.t -> 'a t

  val error : e -> 'a t

  val ok : 'a -> 'a t
end

module MakePlus : functor (E : Monad.MONOID) -> sig
  type e

  include Monad.MAKE_PLUS_T with type 'a wrapped := 'a

  val run : 'a t -> ('a, e) result

  val create : ('a, e) result -> 'a t

  val error : e -> 'a t

  val ok : 'a -> 'a t
end
