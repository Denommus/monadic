open OUnit2

let empty_list = []

module ReaderWriterTest = struct
  module WriterString = Monadic.Writer.Make (struct
    type t = string

    let empty = ""

    let append = ( ^ )
  end)

  module ReaderWriter = Monadic.Reader.MakeT (WriterString) (Int)

  let reader_writer =
    let open ReaderWriter.Syntax in
    let open WriterString in
    let open ReaderWriter in
    let* foo = peek in
    let* _ = string_of_int foo |> tell |> elevate in
    let* _ = tell "bar" |> elevate in
    ReaderWriter.pure 20

  let test _ =
    let reader_result = ReaderWriter.run ~init:10 reader_writer in
    let writer_result = WriterString.run reader_result in
    assert_equal (20, "10bar") writer_result
end

module RefStateTest = struct
  module RefState = Monadic.RefState.Make (String)

  let test _ =
    let state =
      let open RefState.Syntax in
      let open RefState in
      let+ foo = get
      and+ _ = put "Bar"
      and+ _ = create (fun s -> ("", s ^ "Blah")) in
      foo
    in
    let result, state = RefState.run state ~init:"Foo" in
    assert_equal result "Foo";
    assert_equal state "BarBlah"
end

module ListTest = struct
  open Monadic.List.Make.Syntax

  let test _ =
    let list =
      let+ x = [ 3; 4; 5 ] and+ y = [ 2; 1; 10 ] in
      x * y
    in
    assert_equal [ 6; 3; 30; 8; 4; 40; 10; 5; 50 ] list
end

module ZipTest = struct
  open Monadic.Zip.Syntax

  let test _ =
    let list =
      let+ x = [ 3; 4; 5 ] and+ y = [ 2; 1; 10 ] in
      x * y
    in
    assert_equal [ 6; 4; 50 ] list
end

module ComposingTest = struct
  module Reader = Monadic.Reader.Make (String)
  module Option = Monadic.Option.Make
  module ReaderOption = Monadic.Composition.ComposeApplicative (Reader) (Option)

  let reader_option =
    let open ReaderOption in
    let open ReaderOption.Syntax in
    let+ foo = elevate Reader.peek and+ bar = pure "10" in
    foo ^ bar

  let reader_option2 =
    let open ReaderOption.Syntax in
    let+ foo = reader_option and+ bar = Option.none () |> Reader.pure in
    foo ^ bar

  let test _ =
    assert_equal (Some "Blah10") @@ Reader.run reader_option ~init:"Blah";
    assert_equal None @@ Reader.run reader_option2 ~init:"Bleh"
end

let suite =
  "Example"
  >::: [
         "test_transform" >:: ReaderWriterTest.test;
         "test_ref_state" >:: RefStateTest.test;
         "test_list" >:: ListTest.test;
         "test_zip" >:: ZipTest.test;
         "test_composing" >:: ComposingTest.test;
       ]

let () = run_test_tt_main suite
