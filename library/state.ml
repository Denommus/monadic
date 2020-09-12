module Make(Wrapped: Monad.MONAD)(S: sig type t end) = struct

  type s = S.t

  module InternalStateMonad = struct

    module WrappedSyntax = Monad.MonadSyntax(Wrapped)

    open WrappedSyntax

    type 'a t = s -> ('a * s) Wrapped.t

    let pure v = fun s -> Wrapped.pure (v, s)

    let map f x = fun s -> let+ result, s' = x s in (f result, s')

    let apply fa sa = fun s -> let open WrappedSyntax in
      let* f, result1 = fa s in
      let+ s', result2 = sa result1 in
      f s', result2

    let bind m k = fun s ->
      let* result, s' = m s in k result s'

  end

  (* Implementing join from bind and pure *)
  module StateMonad = Monad.DefaultJoin(InternalStateMonad)

  (* Adding all the monadic functions to the outer scope *)
  include StateMonad

  (* Adding the infix functions *)
  include Monad.MonadInfix(StateMonad)

  (* Adding the syntax extensions as an internal module *)
  module Syntax = Monad.MonadSyntax(StateMonad)

  (* The State functions themselves *)
  let get = fun s -> Wrapped.pure (s, s)

  let put s =  fun _ -> Wrapped.pure ((), s)

  let runState m ~init = m init

end

module MakeIdentity (S: sig type t end) = struct
  include Make(Identity)(S)
  let runState m ~init = m init |> Identity.unlift
end
