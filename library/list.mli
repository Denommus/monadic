module MakeFT : functor
  (Wrapped : Monad.FUNCTOR)
  -> sig
  include Monad.MAKE_F_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a list Wrapped.t

  val lift : 'a list Wrapped.t -> 'a t
end
with type 'a t = 'a list Wrapped.t

module MakeF : sig
  include Monad.MAKE_F

  val run : 'a t -> 'a list

  val lift : 'a list -> 'a t
end
with type 'a t = 'a list

module MakeAT : functor
  (Wrapped : Monad.APPLICATIVE)
  -> sig
  include Monad.MAKE_A_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a list Wrapped.t

  val lift : 'a list Wrapped.t -> 'a t
end
with type 'a t = 'a list Wrapped.t

module MakeA : sig
  include Monad.MAKE_A

  val run : 'a t -> 'a list

  val lift : 'a list -> 'a t
end
with type 'a t = 'a list

module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a list Wrapped.t

  val lift : 'a list Wrapped.t -> 'a t
end
with type 'a t = 'a list Wrapped.t

module Make : sig
  include Monad.MAKE

  val run : 'a t -> 'a list

  val lift : 'a list -> 'a t
end
with type 'a t = 'a list
