module type MONOID = sig
  type t
  val empty: t
  val append: t -> t -> t
end

module MakeT:
functor (Wrapped: Monad.MONAD)(W: MONOID) -> sig
  type w
  include Monad.MONAD
  val pure: 'a -> 'a t
  val tell: w -> unit t
  val runWriter: 'a t -> ('a * w) Wrapped.t
  val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  module Syntax: sig
    val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  end
end with type w = W.t


module Make:
functor (W: MONOID) -> sig
  type w
  include Monad.MONAD
  val pure: 'a -> 'a t
  val tell: w -> unit t
  val runWriter: 'a t -> ('a * w)
  val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  module Syntax: sig
    val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  end
end with type w = W.t
