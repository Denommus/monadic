module MakeT (Wrapped : Monad.MONAD) = struct
  module WrappedSyntax = Monad.MonadSyntax (Wrapped)
  open WrappedSyntax

  module ListMonad : Monad.MONAD with type 'a t = 'a list Wrapped.t = struct
    type 'a t = 'a list Wrapped.t

    let pure v = [ v ] |> Wrapped.pure

    let map f x =
      let+ v = x in
      Stdlib.List.map f v

    let apply fa xa =
      let+ fs = fa and+ xs = xa in
      let accum curr acc =
        let l1 = Stdlib.List.map curr xs in
        l1 @ acc
      in
      Stdlib.List.fold_right accum fs []

    let sequence ms =
      let open Monad.ApplicativeFunctions (Wrapped) in
      sequence ms

    let join v =
      let* (x : 'a t list) = v in
      let+ xs = sequence x in
      Stdlib.List.concat xs

    let bind m f = join (map f m)
  end

  include ListMonad

  let elevate v =
    let+ x = v in
    [ x ]

  include Monad.MonadInfix (ListMonad)
  module Syntax = Monad.MonadSyntax (ListMonad)

  let run m = m [@@inline]

  let lift x = x [@@inline]
end

module Make = MakeT (Identity)
