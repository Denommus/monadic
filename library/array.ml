module MakeT (Wrapped : Monad.MONAD) = struct
  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax

  module ArrayMonad : Monad.MONAD with type 'a t = 'a array Wrapped.t = struct
    type 'a t = 'a array Wrapped.t

    let pure v = [| v |] |> Wrapped.pure

    let map f x =
      let+ v = x in
      Stdlib.Array.map f v

    let apply fa xa =
      let+ fs = fa and+ xs = xa in
      let accum curr acc =
        let l1 = Stdlib.Array.map curr xs in
        Stdlib.Array.append l1 acc
      in
      Stdlib.Array.fold_right accum fs [||]

    let sequence ms =
      let open Monad.ApplicativeFunctions (Wrapped) in
      sequence_array ms

    let join v =
      let* (x : 'a t array) = v in
      let+ xs = sequence x in
      Stdlib.Array.to_list xs |> Stdlib.Array.concat

    let bind m f = join (map f m)
  end

  include ArrayMonad

  let elevate v =
    let+ x = v in
    [| x |]

  include Monad.MonadInfix (ArrayMonad)

  let run m = m [@@inline]

  let create x = x [@@inline]
end

module Make = MakeT (Identity)
