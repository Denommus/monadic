module MakeT:
functor (Wrapped: Monad.MONAD) (S: sig type t end) -> sig
  type s
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t
  val get: s t
  val put: s -> unit t
  val run: 'a t -> init:(s ref) -> 'a Wrapped.t
  val lift: (s ref -> 'a Wrapped.t) -> 'a t
end with type s = S.t

module Make:
functor (S: sig type t end) -> sig

  type s
  include Monad.MAKE
  val get: s t
  val put: s -> unit t
  val run: 'a t -> init:(s ref) -> 'a
  val lift: (s ref -> 'a) -> 'a t
end with type s = S.t
