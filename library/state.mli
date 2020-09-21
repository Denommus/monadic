module MakeT : functor
  (Wrapped : Monad.MONAD)
  (S : sig
     type t
   end)
  -> sig
  type s

  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val get : s t

  val put : s -> unit t

  val run : 'a t -> init:s -> ('a * s) Wrapped.t

  val create : (s -> ('a * s) Wrapped.t) -> 'a t
end
with type s = S.t

module Make : functor
  (S : sig
     type t
   end)
  -> sig
  type s

  include Monad.MAKE_T with type 'a wrapped := 'a

  val get : s t

  val put : s -> unit t

  val run : 'a t -> init:s -> 'a * s

  val create : (s -> 'a * s) -> 'a t
end
with type s = S.t
