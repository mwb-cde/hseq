(*--*-ocaml-*--
  Copyright (c) 2017-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(*
/*
  Include into a test file with
    #include "TestSupport.inc"

  Compile the test file with:

  Compile the test file using the C preprocessor
    ocamlc -pp cpp
*/
*)

(** Test support. *)
module TestSupport =
  struct

    (** File locations. *)
    type loc = { file: string; line: int }
    let loc f l = { file = f; line = l }

    let string_of_loc (l: loc) =
      l.file ^ ":" ^(string_of_int l.line)

    type result = Pass | Fail | Skip | Exn of exn

    let string_of_result result =
      match result with
      | Pass -> "PASS"
      | Fail -> "FAIL"
      | Skip -> "SKIPPED"
      | Exn(e) -> "EXN(" ^ (Printexc.to_string e) ^")"

    let print_result oc result =
      Printf.fprintf oc "%s" (string_of_result result)

    type testsuite =
      {
        mutable name: string;
        mutable passes: int;
        mutable fails: int;
        mutable skipped: int;
      }

    let make_testsuite n =
      { name = n; passes = 0; fails = 0; skipped = 0 }

    let add_testsuite x y =
      x.passes <- x.passes + y.passes;
      x.fails <- x.fails + y.fails;
      x.skipped <- x.skipped + y.skipped;
      x

    let print_testsuite oc suite =
      Printf.fprintf oc "%s: passes = %i, fails = %i, skipped = %i"
                     suite.name suite.passes suite.fails suite.skipped

    let print_results oc testsuites =
      List.iter (print_testsuite oc) testsuites

    let mark_result suite b =
      match b with
      | Pass -> suite.passes <- suite.passes + 1
      | Skip -> suite.skipped <- suite.skipped + 1
      | _ -> suite.fails <- suite.fails + 1

    let mark_pass suite =
      mark_result suite Pass

    let mark_fail suite =
      mark_result suite Fail

    let mark_skipped suite =
      mark_result suite Skip

    module Vars =
      struct
        (* Output channels. *)
        let outs = ref stdout
        let errs = ref stderr

        (* List of testsuites. *)
        let (testsuites_var: ((testsuite) list) ref) = ref []

      end

    let outs () = !Vars.outs
    let set_outs oc = Vars.outs := oc

    let errs () = !Vars.errs
    let set_errs oc = Vars.errs := oc

    let warning loc msg =
      Printf.fprintf (errs()) "%s: warning %s"
                     (string_of_loc loc) msg

    let error loc msg =
      Printf.fprintf (errs()) "%s: error: %s"
                     (string_of_loc loc) msg;
      exit (-1)

    let report = Printf.fprintf (outs())

    let set_testsuites l = Vars.testsuites_var := l
    let testsuites () = !Vars.testsuites_var

    let has_testsuite () =
      match testsuites () with
      | [] -> false
      | _ -> true

    let push_testsuite t = set_testsuites (t::(testsuites()))
    let pop_testsuite () =
      match testsuites() with
      | x::xs -> (set_testsuites xs; x)
      | _ -> raise (Invalid_argument "pop_testsuite")
    let top_testsuite () =
      match testsuites() with
      | x::_ -> x
      | _ -> raise (Invalid_argument "top_testsuite")

    let mark_testsuite b =
      mark_result (top_testsuite()) b

    (** Process the outcome of an expect function. *)
    let process_test_outcome oc (test: string) (l: loc) (r: result) =
      (if r <> Fail
      then
        Printf.fprintf oc "%s: %s\n%!" test (string_of_result r)
      else
        Printf.fprintf oc "%s: %s (%s)\n%!"
                       test (string_of_result r) (string_of_loc l));
      if has_testsuite()
      then mark_testsuite r
      else ()

    (** Process the summary of a testsuite. *)
    let process_testsuite_summary oc suite =
      let summary = if suite.fails > 0 then Fail else Pass in
      Printf.fprintf oc "";
      print_testsuite oc suite;
      Printf.fprintf oc " (%s)\n%!" (string_of_result summary);
      summary

    let testsuite_begin n =
      push_testsuite (make_testsuite n);
      Printf.fprintf (outs()) "%s:\n%!%!" n

    let testsuite_end loc =
      if not (has_testsuite())
      then failwith ((string_of_loc loc) ^": no testsuite to end.")
      else
        let suite = pop_testsuite() in
        let summary = process_testsuite_summary (outs()) suite
        in
        (summary, suite)

    (** Test functions for expect. *)

    let make_pred_test pred a b =
      if pred a b
      then Pass
      else Fail

    (** Expect equal. *)
    let test_eql a b =
      if a = b
      then Pass
      else Fail

    (** Expect not equal. *)
    let test_neq a b =
      if a = b
      then Fail
      else Pass

    (** Expect an exception *)
    let test_exn (a: 'a -> 'b) (b: 'a) (e: exn) =
      begin
        try (ignore (a b)); Fail
        with x -> if x = e then Pass else Fail
      end

    let expect_exn (test_name: string) (l: loc)
                   (a: 'a -> 'b) (b:'a) (e: exn) =
      let result = test_exn a b e
      in
      process_test_outcome (outs()) test_name l result

    (** Make a test. *)
    let expect_binop_test
          (test: ('a -> 'b -> result))
          (test_name: string)
          (l: loc) (a: 'a) (b: 'b) =
      let result = try test a b with e -> Exn e
      in
      process_test_outcome (outs()) test_name l result

    (* Run a list of tests. Return [false] if there were any failures and [true]
       otherwise. *)
    let run_tests tests =
      let try_test test =
        try Some (test())
        with x ->
          report
            "%s\n%!" ("Failed with exception: " ^ Printexc.to_string x);
          None
      in
      let results = List.map try_test tests
      in
      let failed_testsuites =
        let is_failure x =
          match x with
          | None -> true
          | Some(Fail, _) -> true
          | _ -> false
        in
        List.filter is_failure results
      in
      (* Return true if no testsuite failed. *)
      if failed_testsuites = []
      then true
      else false

  end

#define PRINT(s) (TestSupport.report "%s\n%!" (s))
#define FILELOC() (TestSupport.loc __FILE__ __LINE__)

#define TESTSUITE_BEGIN(NAME) TestSupport.testsuite_begin (NAME)
#define TESTSUITE_END() (TestSupport.testsuite_end FILELOC())

#define MAKE_EXPECT_BINOP(TEST,OP,A,B) \
(TestSupport.expect_binop_test TEST (#A ^(OP)^ #B) (FILELOC()) (A) (B))

#define EXPECT_EXN(A,B,E) \
(TestSupport.expect_exn (#A ^ " " ^ #B ^ " (expect: "^ #E ^")" ) (FILELOC()) \
(A) (B) (E))

#define EXPECT_EQL(A,B) \
MAKE_EXPECT_BINOP(TestSupport.test_eql, " = ", A, B)

#define EXPECT_NEQ(A,B) \
MAKE_EXPECT_BINOP(TestSupport.test_neq, " <> ", A, B)

#define EXPECT_TRUE(A) \
MAKE_EXPECT_BINOP(TestSupport.test_eql, " = ", A, true)

#define EXPECT_FALSE(A) \
MAKE_EXPECT_BINOP(TestSupport.test_eql, " = ", A, false)
