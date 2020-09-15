module MakeT (Wrapped : Monad.MONAD) = struct
  module WrappedSyntax = Monad.MonadSyntax (Wrapped)
  open WrappedSyntax
  module ListMonad = List.MakeT (Wrapped)

  module ZipApplicative : Monad.APPLICATIVE with type 'a t = 'a list Wrapped.t =
  struct
    type 'a t = 'a list Wrapped.t

    let pure = ListMonad.pure

    let map = ListMonad.map

    let rec zip xs ys =
      match (xs, ys) with
      | l1 :: ls1, l2 :: ls2 -> (l1, l2) :: zip ls1 ls2
      | _ -> []

    let apply: ('a -> 'b) t -> 'a t -> 'b t =
      fun fa xa ->
      let+ f = fa
      and+ x = xa in
      let c (x, y) = x y in
      zip f x |> Stdlib.List.map c
  end

  include ZipApplicative

  let elevate = ListMonad.elevate

  include Monad.ApplicativeInfix (ZipApplicative)
  module Syntax = Monad.ApplicativeSyntax (ZipApplicative)
end

module Make = MakeT (Identity)
