module type MONOID = sig
  type t

  val empty : t

  val append : t -> t -> t
end

module MakeT : functor
  (Wrapped : Monad.MONAD)
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val tell : w -> unit t

  val run : 'a t -> ('a * w) Wrapped.t

  val create : ('a * w) Wrapped.t -> 'a t
end
with type w = W.t

module Make : functor
  (W : MONOID)
  -> sig
  type w

  include Monad.MAKE

  val tell : w -> unit t

  val run : 'a t -> 'a * w

  val create : 'a * w -> 'a t
end
with type w = W.t
