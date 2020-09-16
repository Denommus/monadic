module MakeFT : functor
  (Wrapped : Monad.FUNCTOR)
  -> sig
  include Monad.MAKE_F_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a option Wrapped.t

  val lift : 'a option Wrapped.t -> 'a t
end
with type 'a t = 'a option Wrapped.t

module MakeF : sig
  include Monad.MAKE_F

  val run : 'a t -> 'a option

  val lift : 'a option -> 'a t
end
with type 'a t = 'a option

module MakeAT : functor
  (Wrapped : Monad.APPLICATIVE)
  -> sig
  include Monad.MAKE_A_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a option Wrapped.t

  val lift : 'a option Wrapped.t -> 'a t

  val none : unit -> 'a t

  val some : 'a -> 'a t
end
with type 'a t = 'a option Wrapped.t

module MakeA : sig
  include Monad.MAKE_A

  val run : 'a t -> 'a option

  val lift : 'a option -> 'a t

  val none : unit -> 'a t

  val some : 'a -> 'a t
end
with type 'a t = 'a option

module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a option Wrapped.t

  val lift : 'a option Wrapped.t -> 'a t

  val none : unit -> 'a t

  val some : 'a -> 'a t
end
with type 'a t = 'a option Wrapped.t

module Make : sig
  include Monad.MAKE

  val run : 'a t -> 'a option

  val lift : 'a option -> 'a t

  val none : unit -> 'a t

  val some : 'a -> 'a t
end
with type 'a t = 'a option
