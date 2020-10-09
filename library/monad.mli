module type MONOID = sig
  type t

  val empty : t

  val append : t -> t -> t
end

module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t
end

module type ALTERNATIVE = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val empty : unit -> 'a t

  val append : 'a t -> 'a t -> 'a t
end

module type MONAD = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t
end

module type MONAD_PLUS = sig
  type 'a t

  val empty : unit -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t
end

module FunctorInfix : functor (F : FUNCTOR) -> sig
  val ( <$> ) : ('a -> 'b) -> 'a F.t -> 'b F.t

  module Syntax : sig
    val ( let+ ) : 'a F.t -> ('a -> 'b) -> 'b F.t
  end
end

module ApplicativeInfix : functor (A : APPLICATIVE) -> sig
  val ( <$> ) : ('a -> 'b) -> 'a A.t -> 'b A.t

  val ( <*> ) : ('a -> 'b) A.t -> 'a A.t -> 'b A.t

  module Syntax : sig
    val ( let+ ) : 'a A.t -> ('a -> 'b) -> 'b A.t

    val ( and+ ) : 'a A.t -> 'b A.t -> ('a * 'b) A.t
  end
end

module AlternativeInfix : functor (A : ALTERNATIVE) -> sig
  val ( <$> ) : ('a -> 'b) -> 'a A.t -> 'b A.t

  val ( <*> ) : ('a -> 'b) A.t -> 'a A.t -> 'b A.t

  module Syntax : sig
    val ( let+ ) : 'a A.t -> ('a -> 'b) -> 'b A.t

    val ( and+ ) : 'a A.t -> 'b A.t -> ('a * 'b) A.t
  end

  val ( <|> ) : 'a A.t -> 'a A.t -> 'a A.t
end

module MonadInfix : functor (M : MONAD) -> sig
  val ( <$> ) : ('a -> 'b) -> 'a M.t -> 'b M.t

  val ( <*> ) : ('a -> 'b) M.t -> 'a M.t -> 'b M.t

  val ( >>= ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t

  module Syntax : sig
    val ( let+ ) : 'a M.t -> ('a -> 'b) -> 'b M.t

    val ( and+ ) : 'a M.t -> 'b M.t -> ('a * 'b) M.t

    val ( let* ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
  end
end

module MonadPlusInfix : functor (M : MONAD_PLUS) -> sig
  val ( <$> ) : ('a -> 'b) -> 'a M.t -> 'b M.t

  val ( <*> ) : ('a -> 'b) M.t -> 'a M.t -> 'b M.t

  val ( >>= ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t

  module Syntax : sig
    val ( let+ ) : 'a M.t -> ('a -> 'b) -> 'b M.t

    val ( and+ ) : 'a M.t -> 'b M.t -> ('a * 'b) M.t

    val ( let* ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
  end

  val ( <|> ) : 'a M.t -> 'a M.t -> 'a M.t
end

module type MAKE_T = sig
  type 'a wrapped

  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t

  val elevate : 'a wrapped -> 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type MAKE_PLUS_T = sig
  type 'a wrapped

  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t

  val elevate : 'a wrapped -> 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val ( <|> ) : 'a t -> 'a t -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t
end

module CreateMonadPlus : functor
  (M : MONAD)
  (C : sig
     type 'a t = 'a M.t

     val append : 'a t -> 'a t -> 'a t

     val empty : unit -> 'a t
   end)
  -> sig
  type 'a t = 'a M.t

  val empty : unit -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( <|> ) : 'a t -> 'a t -> 'a t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type COLLECTION = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val init : int -> (int -> 'a) -> 'a t

  val cons : 'a -> 'a t -> 'a t

  val empty : 'a t
end

module SeqCollection : COLLECTION with type 'a t = 'a Stdlib.Seq.t

module ListCollection : COLLECTION with type 'a t = 'a list

module ArrayCollection : COLLECTION with type 'a t = 'a array

module ApplicativeFunctionsGeneric : functor
  (C : COLLECTION)
  (A : APPLICATIVE)
  -> sig
  val ( *> ) : 'a A.t -> 'b A.t -> 'b A.t

  val ( <* ) : 'a A.t -> 'b A.t -> 'a A.t

  val sequence : 'a A.t C.t -> 'a C.t A.t

  val sequence_ : 'a A.t C.t -> 'b C.t A.t

  val a_map : ('a -> 'b A.t) -> 'a C.t -> 'b C.t A.t

  val a_map_ : ('a -> 'b A.t) -> 'a C.t -> 'c C.t A.t

  val a_filter : ('a -> bool A.t) -> 'a C.t -> 'a C.t A.t

  val traverse : ('a -> 'b A.t) -> 'a C.t -> 'b C.t A.t

  val a_for : 'a C.t -> ('a -> 'b A.t) -> 'b C.t A.t

  val a_for_ : 'a C.t -> ('a -> 'b A.t) -> unit A.t

  val lift2 : ('a -> 'b -> 'c) A.t -> 'a A.t -> 'b A.t -> 'c A.t

  val lift3 : ('a -> 'b -> 'c -> 'd) A.t -> 'a A.t -> 'b A.t -> 'c A.t -> 'd A.t

  val lift4 :
    ('a -> 'b -> 'c -> 'd -> 'e) A.t ->
    'a A.t ->
    'b A.t ->
    'c A.t ->
    'd A.t ->
    'e A.t

  val m_when : bool -> unit A.t -> unit A.t

  val m_unless : bool -> unit A.t -> unit A.t

  val m_replicate : int -> 'a A.t -> 'a C.t A.t

  val m_replicate_ : int -> 'a A.t -> unit A.t
end

module ApplicativeFunctions : functor (A : APPLICATIVE) -> sig
  val ( *> ) : 'a A.t -> 'b A.t -> 'b A.t

  val ( <* ) : 'a A.t -> 'b A.t -> 'a A.t

  val sequence : 'a A.t SeqCollection.t -> 'a SeqCollection.t A.t

  val sequence_ : 'a A.t SeqCollection.t -> 'b SeqCollection.t A.t

  val a_map : ('a -> 'b A.t) -> 'a SeqCollection.t -> 'b SeqCollection.t A.t

  val a_map_ : ('a -> 'b A.t) -> 'a SeqCollection.t -> 'c SeqCollection.t A.t

  val a_filter :
    ('a -> bool A.t) -> 'a SeqCollection.t -> 'a SeqCollection.t A.t

  val traverse : ('a -> 'b A.t) -> 'a SeqCollection.t -> 'b SeqCollection.t A.t

  val a_for : 'a SeqCollection.t -> ('a -> 'b A.t) -> 'b SeqCollection.t A.t

  val a_for_ : 'a SeqCollection.t -> ('a -> 'b A.t) -> unit A.t

  val lift2 : ('a -> 'b -> 'c) A.t -> 'a A.t -> 'b A.t -> 'c A.t

  val lift3 : ('a -> 'b -> 'c -> 'd) A.t -> 'a A.t -> 'b A.t -> 'c A.t -> 'd A.t

  val lift4 :
    ('a -> 'b -> 'c -> 'd -> 'e) A.t ->
    'a A.t ->
    'b A.t ->
    'c A.t ->
    'd A.t ->
    'e A.t

  val m_when : bool -> unit A.t -> unit A.t

  val m_unless : bool -> unit A.t -> unit A.t

  val m_replicate : int -> 'a A.t -> 'a SeqCollection.t A.t

  val m_replicate_ : int -> 'a A.t -> unit A.t
end

module ApplicativeFunctionsList : functor (A : APPLICATIVE) -> sig
  val ( *> ) : 'a A.t -> 'b A.t -> 'b A.t

  val ( <* ) : 'a A.t -> 'b A.t -> 'a A.t

  val sequence : 'a A.t ListCollection.t -> 'a ListCollection.t A.t

  val sequence_ : 'a A.t ListCollection.t -> 'b ListCollection.t A.t

  val a_map : ('a -> 'b A.t) -> 'a ListCollection.t -> 'b ListCollection.t A.t

  val a_map_ : ('a -> 'b A.t) -> 'a ListCollection.t -> 'c ListCollection.t A.t

  val a_filter :
    ('a -> bool A.t) -> 'a ListCollection.t -> 'a ListCollection.t A.t

  val traverse :
    ('a -> 'b A.t) -> 'a ListCollection.t -> 'b ListCollection.t A.t

  val a_for : 'a ListCollection.t -> ('a -> 'b A.t) -> 'b ListCollection.t A.t

  val a_for_ : 'a ListCollection.t -> ('a -> 'b A.t) -> unit A.t

  val lift2 : ('a -> 'b -> 'c) A.t -> 'a A.t -> 'b A.t -> 'c A.t

  val lift3 : ('a -> 'b -> 'c -> 'd) A.t -> 'a A.t -> 'b A.t -> 'c A.t -> 'd A.t

  val lift4 :
    ('a -> 'b -> 'c -> 'd -> 'e) A.t ->
    'a A.t ->
    'b A.t ->
    'c A.t ->
    'd A.t ->
    'e A.t

  val m_when : bool -> unit A.t -> unit A.t

  val m_unless : bool -> unit A.t -> unit A.t

  val m_replicate : int -> 'a A.t -> 'a ListCollection.t A.t

  val m_replicate_ : int -> 'a A.t -> unit A.t
end

module ApplicativeFunctionsArray : functor (A : APPLICATIVE) -> sig
  val ( *> ) : 'a A.t -> 'b A.t -> 'b A.t

  val ( <* ) : 'a A.t -> 'b A.t -> 'a A.t

  val sequence : 'a A.t ArrayCollection.t -> 'a ArrayCollection.t A.t

  val sequence_ : 'a A.t ArrayCollection.t -> 'b ArrayCollection.t A.t

  val a_map : ('a -> 'b A.t) -> 'a ArrayCollection.t -> 'b ArrayCollection.t A.t

  val a_map_ :
    ('a -> 'b A.t) -> 'a ArrayCollection.t -> 'c ArrayCollection.t A.t

  val a_filter :
    ('a -> bool A.t) -> 'a ArrayCollection.t -> 'a ArrayCollection.t A.t

  val traverse :
    ('a -> 'b A.t) -> 'a ArrayCollection.t -> 'b ArrayCollection.t A.t

  val a_for : 'a ArrayCollection.t -> ('a -> 'b A.t) -> 'b ArrayCollection.t A.t

  val a_for_ : 'a ArrayCollection.t -> ('a -> 'b A.t) -> unit A.t

  val lift2 : ('a -> 'b -> 'c) A.t -> 'a A.t -> 'b A.t -> 'c A.t

  val lift3 : ('a -> 'b -> 'c -> 'd) A.t -> 'a A.t -> 'b A.t -> 'c A.t -> 'd A.t

  val lift4 :
    ('a -> 'b -> 'c -> 'd -> 'e) A.t ->
    'a A.t ->
    'b A.t ->
    'c A.t ->
    'd A.t ->
    'e A.t

  val m_when : bool -> unit A.t -> unit A.t

  val m_unless : bool -> unit A.t -> unit A.t

  val m_replicate : int -> 'a A.t -> 'a ArrayCollection.t A.t

  val m_replicate_ : int -> 'a A.t -> unit A.t
end

module MonadFunctionsGeneric : functor (C : COLLECTION) (M : MONAD) -> sig
  val m_fold : ('a -> 'b -> 'a M.t) -> 'a -> 'b C.t -> 'a M.t

  val m_fold_ : ('a -> 'b -> 'a M.t) -> 'a -> 'b C.t -> unit M.t

  val ( >=> ) : ('a -> 'b M.t) -> ('b -> 'c M.t) -> 'a -> 'c M.t
end

module MonadFunctions : functor (M : MONAD) -> sig
  val m_fold : ('a -> 'b -> 'a M.t) -> 'a -> 'b SeqCollection.t -> 'a M.t

  val m_fold_ : ('a -> 'b -> 'a M.t) -> 'a -> 'b SeqCollection.t -> unit M.t

  val ( >=> ) : ('a -> 'b M.t) -> ('b -> 'c M.t) -> 'a -> 'c M.t
end

module MonadFunctionsList : functor (M : MONAD) -> sig
  val m_fold : ('a -> 'b -> 'a M.t) -> 'a -> 'b ListCollection.t -> 'a M.t

  val m_fold_ : ('a -> 'b -> 'a M.t) -> 'a -> 'b ListCollection.t -> unit M.t

  val ( >=> ) : ('a -> 'b M.t) -> ('b -> 'c M.t) -> 'a -> 'c M.t
end

module MonadFunctionsArray : functor (M : MONAD) -> sig
  val m_fold : ('a -> 'b -> 'a M.t) -> 'a -> 'b ArrayCollection.t -> 'a M.t

  val m_fold_ : ('a -> 'b -> 'a M.t) -> 'a -> 'b ArrayCollection.t -> unit M.t

  val ( >=> ) : ('a -> 'b M.t) -> ('b -> 'c M.t) -> 'a -> 'c M.t
end

module AlternativeFunctions : functor (A : ALTERNATIVE) -> sig
  val guard : bool -> unit A.t

  val some : 'a A.t -> 'a Stdlib.Seq.t A.t

  val many : 'a A.t -> 'a Stdlib.Seq.t A.t

  val optional : 'a A.t -> 'a option A.t
end
