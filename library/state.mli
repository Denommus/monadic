module MakeFT : functor
  (Wrapped : Monad.FUNCTOR)
  (S : sig
     type t
   end)
  -> sig
  type s

  include Monad.MAKE_F_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> init:s -> ('a * s) Wrapped.t

  val lift : (s -> ('a * s) Wrapped.t) -> 'a t
end
with type s = S.t

module MakeF : functor
  (S : sig
     type t
   end)
  -> sig
  type s

  include Monad.MAKE_F

  val run : 'a t -> init:s -> 'a * s

  val lift : (s -> 'a * s) -> 'a t
end
with type s = S.t

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

  val lift : (s -> ('a * s) Wrapped.t) -> 'a t
end
with type s = S.t

module Make : functor
  (S : sig
     type t
   end)
  -> sig
  type s

  include Monad.MAKE

  val get : s t

  val put : s -> unit t

  val run : 'a t -> init:s -> 'a * s

  val lift : (s -> 'a * s) -> 'a t
end
with type s = S.t
