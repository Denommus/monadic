module MakeFT : functor
  (Wrapped : Monad.FUNCTOR)
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE_F_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> ('a, e) result Wrapped.t

  val lift : ('a, e) result Wrapped.t -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result Wrapped.t

module MakeF : functor
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE_F

  val run : 'a t -> ('a, e) result

  val lift : ('a, e) result -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result

module MakeAT : functor
  (Wrapped : Monad.APPLICATIVE)
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE_A_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> ('a, e) result Wrapped.t

  val lift : ('a, e) result Wrapped.t -> 'a t

  val error : e -> 'a t

  val ok : 'a -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result Wrapped.t

module MakeA : functor
  (E : sig
     type t
   end)
  -> sig
  type e

  include Monad.MAKE_A

  val run : 'a t -> ('a, e) result

  val lift : ('a, e) result -> 'a t

  val error : e -> 'a t

  val ok : 'a -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result

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

  include Monad.MAKE

  val run : 'a t -> ('a, e) result

  val lift : ('a, e) result -> 'a t

  val error : e -> 'a t

  val ok : 'a -> 'a t
end
with type e = E.t
with type 'a t = ('a, E.t) result
