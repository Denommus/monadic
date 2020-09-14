module MakeT:
functor (Wrapped: Monad.MONAD) (R: sig type t end) -> sig
  type r
  include Monad.MONAD
  val pure: 'a -> 'a t
  val peek: r t
  val run: 'a t -> init:r -> 'a Wrapped.t
  val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  module Syntax: sig
    val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  end

  val elevate: 'a Wrapped.t -> 'a t
  val lift: (r -> 'a Wrapped.t) -> 'a t
end with type r = R.t

module Make:
functor (R: sig type t end) -> sig
  type r
  include Monad.MONAD
  val pure: 'a -> 'a t
  val peek: r t
  val run: 'a t -> init:r -> 'a
  val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  module Syntax: sig
    val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  end
  val lift: (r -> 'a) -> 'a t
end with type r = R.t
