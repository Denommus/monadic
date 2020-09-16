module MakeFT
    (Wrapped : Monad.FUNCTOR) (S : sig
      type t
    end) =
struct
  type s = S.t

  module WrappedInfix = Monad.FunctorInfix (Wrapped)

  module ReaderFunctor =
    Reader.MakeFT
      (Wrapped)
      (struct
        type t = S.t ref
      end)

  include ReaderFunctor

  let run m ~init =
    let r = ref init in
    let result = run m ~init:r in
    (result, !r)

  let lift f =
    lift @@ fun r ->
    let open WrappedInfix.Syntax in
    let+ result, new_state = f !r in
    r := new_state;
    result
end

module MakeF = MakeFT (Identity)

module MakeAT
    (Wrapped : Monad.APPLICATIVE) (S : sig
      type t
    end) =
struct
  type s = S.t

  module WrappedInfix = Monad.FunctorInfix (Wrapped)

  module ReaderApplicative =
    Reader.MakeAT
      (Wrapped)
      (struct
        type t = S.t ref
      end)

  include ReaderApplicative
  open ReaderApplicative.Syntax

  (* TODO figure out how to remove this duplicate *)
  let run m ~init =
    let r = ref init in
    let result = run m ~init:r in
    (result, !r)

  let lift f =
    lift @@ fun r ->
    let open WrappedInfix.Syntax in
    let+ result, new_state = f !r in
    r := new_state;
    result

  let put v =
    let+ x = peek in
    x := v

  let get =
    let+ x = peek in
    !x
end

module MakeA = MakeAT (Identity)

module MakeT
    (Wrapped : Monad.MONAD) (S : sig
      type t
    end) =
struct
  type s = S.t

  module Applicative = MakeAT (Wrapped) (S)

  module ReaderMonad =
    Reader.MakeT
      (Wrapped)
      (struct
        type t = S.t ref
      end)

  module WrappedInfix = Monad.FunctorInfix (Wrapped)
  include ReaderMonad
  open ReaderMonad.Syntax

  (* TODO figure out how to remove this duplicate *)
  let run m ~init =
    let r = ref init in
    let result = run m ~init:r in
    (result, !r)

  let lift f =
    lift @@ fun r ->
    let open WrappedInfix.Syntax in
    let+ result, new_state = f !r in
    r := new_state;
    result

  let put v =
    let+ x = peek in
    x := v

  let get =
    let+ x = peek in
    !x
end

module Make = MakeT (Identity)
