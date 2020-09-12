module InternalIdentityMonad = struct
  type 'a t = 'a

  let pure v = v

  let map f x = f x

  let apply fa xa = fa xa

  let bind m f = f m
end

module IdentityMonad = Monad.DefaultJoin(InternalIdentityMonad)

include IdentityMonad

include Monad.MonadInfix(IdentityMonad)

module Syntax = Monad.MonadSyntax(IdentityMonad)
