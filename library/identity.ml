module InternalIdentityMonad = struct
  type 'a t = 'a

  let pure v = v [@@inline]

  let map f x = f x [@@inline]

  let apply fa xa = fa xa [@@inline]

  let bind m f = f m [@@inline]
end

module IdentityMonad = Monad.DefaultJoin(InternalIdentityMonad)

include IdentityMonad

include Monad.MonadInfix(IdentityMonad)

module Syntax = Monad.MonadSyntax(IdentityMonad)
