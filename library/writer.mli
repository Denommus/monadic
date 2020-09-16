module type MONOID = sig
  type t

  val empty : t

  val append : t -> t -> t
end

module MakeFT : functor
  (Wrapped : Monad.FUNCTOR)
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE_F_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> ('a * w) Wrapped.t

  val lift : ('a * w) Wrapped.t -> 'a t
end
with type w = W.t

module MakeF : functor
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE_F

  val run : 'a t -> 'a * w

  val lift : 'a * w -> 'a t
end
with type w = W.t

module MakeAT : functor
  (Wrapped : Monad.APPLICATIVE)
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE_A_T with type 'a wrapped := 'a Wrapped.t

  val tell : w -> unit t

  val run : 'a t -> ('a * w) Wrapped.t

  val lift : ('a * w) Wrapped.t -> 'a t
end
with type w = W.t

module MakeA : functor
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE_A

  val tell : w -> unit t

  val run : 'a t -> 'a * w

  val lift : 'a * w -> 'a t
end
with type w = W.t

module MakeT : functor
  (Wrapped : Monad.MONAD)
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val tell : w -> unit t

  val run : 'a t -> ('a * w) Wrapped.t

  val lift : ('a * w) Wrapped.t -> 'a t
end
with type w = W.t

module Make : functor
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE

  val tell : w -> unit t

  val run : 'a t -> 'a * w

  val lift : 'a * w -> 'a t
end
with type w = W.t
