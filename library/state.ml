module MakeT(Wrapped: Monad.MONAD)(S: sig type t end) = struct

  type s = S.t

  module WrappedSyntax = Monad.MonadSyntax(Wrapped)

  open WrappedSyntax

  module StateMonad: Monad.MONAD with type 'a t = s -> ('a * s) Wrapped.t = struct

    type 'a t = s -> ('a * s) Wrapped.t

    let pure v = fun s -> Wrapped.pure (v, s)

    let map f x = fun s -> let+ result, s' = x s in (f result, s')

    let apply fa sa = fun s ->
      let* f, result1 = fa s in
      let+ s', result2 = sa result1 in
      f s', result2

    let bind m k = fun s ->
      let* result, s' = m s in k result s'

    let join m = bind m (fun x -> x)

  end

  (* Adding all the monadic functions to the outer scope *)
  include StateMonad

  let elevate w = fun s -> let+ x = w in x, s

  (* Adding the infix functions *)
  include Monad.MonadInfix(StateMonad)

  (* Adding the syntax extensions as an internal module *)
  module Syntax = Monad.MonadSyntax(StateMonad)

  (* The State functions themselves *)
  let get = fun s -> Wrapped.pure (s, s)

  let put s =  fun _ -> Wrapped.pure ((), s)

  let run m ~init = m init

  let lift x = x [@@inline]

end

module Make = MakeT(Identity)
