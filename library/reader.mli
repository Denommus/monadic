module MakeFT : functor
  (Wrapped : Monad.FUNCTOR)
  (R : sig
     type t
   end)
  -> sig
  type r

  include Monad.MAKE_F_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> init:r -> 'a Wrapped.t

  val lift : (r -> 'a Wrapped.t) -> 'a t
end
with type r = R.t

module MakeF : functor
  (R : sig
     type t
   end)
  -> sig
  type r

  include Monad.MAKE_F

  val run : 'a t -> init:r -> 'a

  val lift : (r -> 'a) -> 'a t
end
with type r = R.t

module MakeAT : functor
  (Wrapped : Monad.APPLICATIVE)
  (R : sig
     type t
   end)
  -> sig
  type r

  include Monad.MAKE_A_T with type 'a wrapped := 'a Wrapped.t

  val peek : r t

  val run : 'a t -> init:r -> 'a Wrapped.t

  val lift : (r -> 'a Wrapped.t) -> 'a t
end
with type r = R.t

module MakeA : functor
  (R : sig
     type t
   end)
  -> sig
  type r

  include Monad.MAKE_A

  val peek : r t

  val run : 'a t -> init:r -> 'a

  val lift : (r -> 'a) -> 'a t
end
with type r = R.t

module MakeT : functor
  (Wrapped : Monad.MONAD)
  (R : sig
     type t
   end)
  -> sig
  type r

  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val peek : r t

  val run : 'a t -> init:r -> 'a Wrapped.t

  val lift : (r -> 'a Wrapped.t) -> 'a t
end
with type r = R.t

module Make : functor
  (R : sig
     type t
   end)
  -> sig
  type r

  include Monad.MAKE

  val peek : r t

  val run : 'a t -> init:r -> 'a

  val lift : (r -> 'a) -> 'a t
end
with type r = R.t
