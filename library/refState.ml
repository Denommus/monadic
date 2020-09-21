module MakeT
    (Wrapped : Monad.MONAD) (S : sig
      type t
    end) =
struct
  type s = S.t

  module ReaderMonad =
    Reader.MakeT
      (Wrapped)
      (struct
        type t = S.t ref
      end)

  include ReaderMonad
  open Syntax

  let put v =
    let+ x = peek in
    x := v

  let get =
    let+ x = peek in
    !x

  let run m ~init =
    let r = ref init in
    let result = run m ~init:r in
    (result, !r)

  module WrappedInfix = Monad.MonadInfix (Wrapped)

  let create f =
    create @@ fun r ->
    let open WrappedInfix.Syntax in
    let+ result, new_state = f !r in
    r := new_state;
    result
end

module Make = MakeT (Identity)

module MakePlusT
    (Wrapped : Monad.MONAD_PLUS) (S : sig
      type t
    end) =
struct
  module RefStateMonad = MakeT (Wrapped) (S)

  type s = S.t

  module AppendAndEmpty = struct
    type 'a t = 'a RefStateMonad.t

    let append xa ya =
      let open RefStateMonad in
      create @@ fun r ->
      let x, r' = run xa ~init:r in
      let y, r'' = run ya ~init:r' in
      Wrapped.map (fun a -> (a, r'')) @@ Wrapped.append x y

    let empty () = RefStateMonad.create (fun _ -> Wrapped.empty ())
  end

  module RefStateMonadPlus =
    Monad.CreateMonadPlus (RefStateMonad) (AppendAndEmpty)
  include RefStateMonadPlus
  include Monad.MonadPlusInfix (RefStateMonadPlus)

  let create = RefStateMonad.create

  let run = RefStateMonad.run

  let get = RefStateMonad.get

  let put = RefStateMonad.put

  let elevate = RefStateMonad.elevate
end
