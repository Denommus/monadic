module MakeFT
    (Wrapped : Monad.FUNCTOR) (S : sig
      type t
    end) =
struct
  module WrappedInfix = Monad.FunctorInfix (Wrapped)
  open WrappedInfix.Syntax

  type s = S.t

  module StateFunctor : Monad.FUNCTOR with type 'a t = s -> ('a * s) Wrapped.t =
  struct
    type 'a t = s -> ('a * s) Wrapped.t

    let map f x s =
      let+ result, s' = x s in
      (f result, s')
  end

  include StateFunctor
  include Monad.FunctorInfix (StateFunctor)

  module Utils = struct
    let elevate w s =
      let+ x = w in
      (x, s)

    let run m ~init = m init [@@inline]

    let lift x = x [@@inline]
  end

  include Utils
end

module MakeF = MakeFT (Identity)

(* State requires Monad even for its Applicative interface,
   so implementing MakeAT for it would be a waste of time *)

module MakeT
    (Wrapped : Monad.MONAD) (S : sig
      type t
    end) =
struct
  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax
  module Functor = MakeFT (Wrapped) (S)

  type s = Functor.s

  module StateMonad : Monad.MONAD with type 'a t = s -> ('a * s) Wrapped.t =
  struct
    include Functor.StateFunctor

    type 'a t = s -> ('a * s) Wrapped.t

    let pure v s = Wrapped.pure (v, s)

    let apply fa sa s =
      let* f, result1 = fa s in
      let+ s', result2 = sa result1 in
      (f s', result2)

    let bind m k s =
      let* result, s' = m s in
      k result s'

    let join m = bind m (fun x -> x)
  end

  (* Adding all the monadic functions to the outer scope *)
  include StateMonad

  (* Adding the infix functions *)
  include Monad.MonadInfix (StateMonad)
  include Functor.Utils

  (* The State functions themselves *)
  let get s = Wrapped.pure (s, s)

  let put s _ = Wrapped.pure ((), s)
end

module Make = MakeT (Identity)
