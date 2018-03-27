(*----
  Name: terms.ml
  Copyright Matthew Wahab 2017
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(* Compile with:

  Compile with
   ocamlc -pp cpp
      -I BUILD_ROOT/hseq nums.cma unix.cma hseq.cma

  where build_ROOT is the root of the hseq build directory.
  and   cpp is the C preproccessor
 *)

#include "TestSupport.inc"

(* Open HSeq modules. *)

open HSeq

(** Test the testsuite support. *)
let test_testsupport() =
  begin
  TESTSUITE_BEGIN("Test support");

  EXPECT_EQL (1, 1);
  EXPECT_EQL (1, 0);

  EXPECT_NEQ (1, 0);
  EXPECT_NEQ (1, 1);

  EXPECT_TRUE(true);
  EXPECT_TRUE(false);

  EXPECT_FALSE(false);
  EXPECT_FALSE(true);

  TESTSUITE_END()
  end

(** Tags. *)
let test_tags() =
  TESTSUITE_BEGIN("Tags");

  let a = Tag.make "a"
  and a1 = Tag.make "a"
  and b = Tag.make "b"
  and b1 = Tag.make "b"
  in
  EXPECT_TRUE(Tag.equal a a);
  EXPECT_FALSE(Tag.equal a b);
  EXPECT_FALSE(Tag.equal a a1);
  EXPECT_TRUE(Tag.equal a1 a1);
  EXPECT_FALSE(Tag.equal b b1);

  EXPECT_EQL(Tag.contents a, "a");
  EXPECT_EQL(Tag.contents a1, "a");

  EXPECT_EQL(Tag.contents b, "b");
  EXPECT_EQL(Tag.contents b1, "b");

  TESTSUITE_END()

let test_basics() =
  TESTSUITE_BEGIN("Basics");

  let a = Gtypes.mk_gtype_id "a"
  and a_1 = Gtypes.mk_gtype_id "a"
  and b = Gtypes.mk_gtype_id "b"
  and b_1 = Gtypes.mk_gtype_id "b"
  and aa = Gtypes.mk_gtype_id "aa"
  and ab = Gtypes.mk_gtype_id "ab"
  and bb = Gtypes.mk_gtype_id "bb"
  and ba = Gtypes.mk_gtype_id "ba"
  in
  EXPECT_TRUE(Gtypes.gtype_id_equal a a);
  EXPECT_TRUE(Gtypes.gtype_id_equal a_1 a_1);
  EXPECT_FALSE(Gtypes.gtype_id_equal a a_1);
  EXPECT_FALSE(Gtypes.gtype_id_equal a_1 a);

  EXPECT_TRUE(Gtypes.gtype_id_equal b b);
  EXPECT_TRUE(Gtypes.gtype_id_equal b_1 b_1);
  EXPECT_FALSE(Gtypes.gtype_id_equal b a);
  EXPECT_FALSE(Gtypes.gtype_id_equal b b_1);

  EXPECT_EQL(Gtypes.gtype_id_compare a a, Order.Equal);
  EXPECT_EQL(Gtypes.gtype_id_compare a_1 a_1, Order.Equal);
  EXPECT_NEQ(Gtypes.gtype_id_compare a a_1, Order.Equal);
  EXPECT_NEQ(Gtypes.gtype_id_compare a_1 a, Order.Equal);

  EXPECT_EQL(Gtypes.gtype_id_compare a aa, Order.LessThan);
  EXPECT_EQL(Gtypes.gtype_id_compare aa ab, Order.LessThan);
  EXPECT_EQL(Gtypes.gtype_id_compare aa ba, Order.LessThan);
  EXPECT_EQL(Gtypes.gtype_id_compare ba bb, Order.LessThan);

  EXPECT_EQL(Gtypes.gtype_id_compare aa a, Order.GreaterThan);
  EXPECT_EQL(Gtypes.gtype_id_compare b aa, Order.GreaterThan);
  EXPECT_EQL(Gtypes.gtype_id_compare ba b, Order.GreaterThan);
  EXPECT_EQL(Gtypes.gtype_id_compare bb ba, Order.GreaterThan);

  let (a_ty: Gtypes.t) = Gtypes.mk_vartype a
  and a1_ty = Gtypes.mk_vartype a_1
  and b_ty = Gtypes.mk_vartype b
  and b1_ty = Gtypes.mk_vartype b_1
  in
  let a_bnd = Basic.mk_binding Basic.All "a" a_ty
  and a1_bnd = Basic.mk_binding Basic.All "a" a1_ty
  and b_bnd = Basic.mk_binding Basic.All "b" b_ty
  and b1_bnd = Basic.mk_binding Basic.All "b" b_ty
  and b2_bnd = Basic.mk_binding Basic.All "b" b1_ty
  and b3_bnd = Basic.mk_binding Basic.Ex "b" b1_ty
  in
  EXPECT_TRUE(Basic.binder_equality a_bnd a_bnd);
  EXPECT_FALSE(Basic.binder_equality a_bnd a1_bnd);
  EXPECT_FALSE(Basic.binder_equality a_bnd b_bnd);

  EXPECT_TRUE(Basic.binder_equality b_bnd b_bnd);
  EXPECT_FALSE(Basic.binder_equality b_bnd b1_bnd);

  EXPECT_FALSE(Basic.binder_equality b1_bnd b2_bnd);
  EXPECT_FALSE(Basic.binder_equality b2_bnd b3_bnd);

  TESTSUITE_END()

let test_terms() =
  TESTSUITE_BEGIN("Terms");

#define EXPECT_TERM_EQL(A,B) \
MAKE_EXPECT_BINOP((TestSupport.make_pred_test Term.equals), " = ", A, B)

#define EXPECT_TERM_NEQ(A,B) \
MAKE_EXPECT_BINOP\
((TestSupport.make_pred_test (fun x y -> not (Term.equals x y))), " <> ", A, B)

  let (a_ty: Gtypes.t) = Gtypes.mk_var "a"
  and b_ty = Gtypes.mk_var "b"
  and c_ty = Gtypes.mk_var "c"
  in
  let a_all = Basic.mk_binding Basic.All "a" a_ty
  and a1_all = Basic.mk_binding Basic.All "a" a_ty
  and b_all = Basic.mk_binding Basic.All "b" b_ty
  and c_lam = Basic.mk_binding Basic.All "c" c_ty
  in
  let a_free = Term.mk_free "a" a_ty
  and a_bnd = Term.mk_bound a_all
  and a_ident = Term.mk_short_ident "a"
  and a_meta = Term.mk_meta "a" a_ty

  and b_free = Term.mk_free "b" b_ty
  and b_bnd = Term.mk_bound b_all
  and b_ident = Term.mk_short_ident "b"
  and b_meta = Term.mk_meta "b" b_ty

  and c_free = Term.mk_free "" c_ty
  and c_bnd = Term.mk_bound c_lam
  and c_ident = Term.mk_short_ident "c"
  and c_meta = Term.mk_meta "c" c_ty

  and a1_free = Term.mk_free "a" a_ty
  and a1_bnd = Term.mk_bound a1_all
  and a1_ident = Term.mk_short_ident "a"
  and a1_meta = Term.mk_meta "a" a_ty

  in

  EXPECT_TERM_EQL(a_free, a_free);
  EXPECT_TERM_NEQ(a_free, a_bnd);

  EXPECT_TERM_NEQ(a_free, a_bnd);
  EXPECT_TERM_NEQ(a_free, a_ident);
  EXPECT_TERM_NEQ(a_free, a_meta);

  EXPECT_TERM_EQL(a_bnd, a_bnd);
  EXPECT_TERM_NEQ(a_bnd, a_free);
  EXPECT_TERM_NEQ(a_bnd, a_ident);
  EXPECT_TERM_NEQ(a_bnd, a_meta);

  EXPECT_TERM_EQL(a_ident, a_ident);
  EXPECT_TERM_NEQ(a_ident, a_free);
  EXPECT_TERM_NEQ(a_ident, a_bnd);
  EXPECT_TERM_NEQ(a_ident, a_meta);

  EXPECT_TERM_EQL(a_meta, a_meta);
  EXPECT_TERM_NEQ(a_meta, a_free);
  EXPECT_TERM_NEQ(a_meta, a_ident);
  EXPECT_TERM_NEQ(a_meta, a_bnd);

  EXPECT_TERM_NEQ(a_free, b_free);
  EXPECT_TERM_NEQ(a_free, b_bnd);
  EXPECT_TERM_NEQ(a_free, b_ident);
  EXPECT_TERM_NEQ(a_free, b_meta);

  EXPECT_TERM_EQL(a_free, a1_free);
  EXPECT_TERM_NEQ(a_free, b_bnd);
  EXPECT_TERM_NEQ(a_free, a1_ident);
  EXPECT_TERM_NEQ(a_free, a1_meta);

  EXPECT_TERM_NEQ(a_ident, b_free);
  EXPECT_TERM_NEQ(a_ident, b_bnd);
  EXPECT_TERM_NEQ(a_ident, b_ident);
  EXPECT_TERM_NEQ(a_ident, b_meta);

  EXPECT_TERM_NEQ(a_meta, b_free);
  EXPECT_TERM_NEQ(a_meta, b_bnd);
  EXPECT_TERM_NEQ(a_meta, b_ident);
  EXPECT_TERM_NEQ(a_meta, b_meta);

  EXPECT_TERM_NEQ(a_bnd, b_free);
  EXPECT_TERM_NEQ(a_bnd, b_bnd);
  EXPECT_TERM_NEQ(a_bnd, b_ident);
  EXPECT_TERM_NEQ(a_meta, b_meta);

  EXPECT_TERM_NEQ(a_bnd, a1_bnd);
  EXPECT_TERM_NEQ(a1_bnd, a_bnd);

  let a_vars = [ a_bnd; a_free; a_ident; a_meta ]
  and b_vars = [ b_bnd; b_free; b_ident; b_meta ]
  and c_vars = [ c_bnd; c_free; c_ident; c_meta ]
  in
  let sb1 =
    List.fold_left2
      (fun sb x y -> Term.bind x y sb) (Term.empty_subst())
      a_vars b_vars
  in
  EXPECT_TERM_EQL(Term.subst sb1 a_free, Term.subst sb1 a1_free);
  EXPECT_TERM_EQL(Term.subst sb1 a1_free, Term.subst sb1 a_free);

  List.iter2 (fun x y -> EXPECT_TERM_EQL(Term.find x sb1, y)) a_vars b_vars;
  List.iter
    (fun x -> EXPECT_EXN(Term.find x, sb1, Not_found))
    [ a1_bnd; a1_ident; a1_meta; b_bnd; b_free; b_ident; b_meta ];
  List.iter
    (fun x -> EXPECT_TRUE(Term.member x sb1)) a_vars;
  List.iter
    (fun x -> EXPECT_FALSE(Term.member x sb1))
    [ a1_bnd; a1_ident; a1_meta; b_bnd; b_free; b_ident; b_meta ];

  List.iter2
    (fun x y -> EXPECT_TERM_EQL(Term.subst sb1 x, y))
    a_vars b_vars;

  List.iter2
    (fun x y -> EXPECT_TERM_EQL(Term.subst sb1 x, y))
    [ a1_bnd; a1_ident; a1_meta; b_bnd; b_free; b_ident; b_meta ]
    [ a1_bnd; a1_ident; a1_meta; b_bnd; b_free; b_ident; b_meta ];

  let sb2 =
    List.fold_left2 (fun sb x y -> Term.bind x y sb) sb1 b_vars c_vars
  in

  List.iter2 (fun x y -> EXPECT_TERM_EQL(Term.subst sb2 x, y)) a_vars c_vars;

  TESTSUITE_END()

(* The list of tests and the function to run them. *)
let test_list =
  [
    (* test_testsupport; *)
    test_tags;
    test_basics;
    test_terms;
  ]

(* Run the tests. *)
let _ =
  if (TestSupport.run_tests test_list)
  then exit 0
  else exit (-1)
