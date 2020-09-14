module MakeT:
functor (Wrapped: Monad.MONAD) (S: sig type t end) -> sig
  type s
  include Monad.MONAD
  val pure: 'a -> 'a t
  val get: s t
  val put: s -> unit t
  val run: 'a t -> init:s -> ('a * s) Wrapped.t
  val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  module Syntax: sig
    val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  end

  val elevate: 'a Wrapped.t -> 'a t
  val lift: (s -> ('a * s) Wrapped.t) -> 'a t
end with type s = S.t

module Make:
functor (S: sig type t end) -> sig

  type s
  include Monad.MONAD
  val pure: 'a -> 'a t
  val get: s t
  val put: s -> unit t
  val run: 'a t -> init:s -> ('a * s)
  val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  module Syntax: sig
    val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  end
  val lift: (s -> ('a * s)) -> 'a t
end with type s = S.t
