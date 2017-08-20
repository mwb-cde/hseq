(*----
  Name: types.ml
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

#define EXPECT_TYPE_EQL(A,B) \
MAKE_EXPECT_BINOP((TestSupport.make_pred_test Gtypes.equals), " = ", A, B)

#define EXPECT_TYPE_NEQ(A,B) \
MAKE_EXPECT_BINOP\
((TestSupport.make_pred_test (fun x y -> not (Gtypes.equals x y))), \
" <> ", A, B)

let test_types() =
  TESTSUITE_BEGIN("Types");

  let (a_ty: Basic.gtype) = Gtypes.mk_var "a"
  and a_wty = Gtypes.mk_weak "a"
  and a1_ty = Gtypes.mk_var "a"
  and a1_wty = Gtypes.mk_weak "a"
  and b_ty = Gtypes.mk_var "b"
  and b_wty = Gtypes.mk_weak "b"
  and c_ty = Gtypes.mk_var "c"
  and c_wty = Gtypes.mk_weak "c"
  in
  EXPECT_TYPE_EQL(a_ty, a_ty);
  EXPECT_TYPE_NEQ(a_ty, a_wty);
  EXPECT_TYPE_NEQ(a_ty, a1_ty);
  EXPECT_TYPE_NEQ(a_ty, a1_wty);
  EXPECT_TYPE_NEQ(a_ty, b_ty);
  EXPECT_TYPE_NEQ(a_ty, b_wty);

  EXPECT_TYPE_EQL(a_wty, a_wty);
  EXPECT_TYPE_NEQ(a_wty, a_ty);
  EXPECT_TYPE_NEQ(a_wty, a1_ty);
  EXPECT_TYPE_NEQ(a_wty, a1_wty);
  EXPECT_TYPE_NEQ(a_wty, b_ty);
  EXPECT_TYPE_NEQ(a_wty, b_wty);

  let f1 = Ident.mk_name "f1"
  and f2 = Ident.mk_name "f2"
  and g1 = Ident.mk_name "g1"
  and g2 = Ident.mk_name "g2"
  in
  let f1_cty = Gtypes.mk_constr f1 [a_ty; b_ty; c_ty]
  and f1a_cty = Gtypes.mk_constr f1 [a1_ty; b_ty; c_ty]
  and f2_cty = Gtypes.mk_constr f2 [a1_ty; b_ty; c_ty]
  in
  EXPECT_TYPE_EQL(f1_cty, f1_cty);
  EXPECT_TYPE_NEQ(f1_cty, f1a_cty);
  EXPECT_TYPE_NEQ(f1_cty, f2_cty);

  EXPECT_TYPE_NEQ(f2_cty, f1_cty);
  EXPECT_TYPE_NEQ(f2_cty, f1a_cty);

  let g1_cty = Gtypes.mk_constr g1 [a_wty; b_ty]
  and g1a_cty = Gtypes.mk_constr g1 [a_wty]
  and g2_cty = Gtypes.mk_constr g2 [a1_ty; b_ty; c_ty]
  in
  EXPECT_TYPE_EQL(g1_cty, g1_cty);
  EXPECT_TYPE_NEQ(g1_cty, g1a_cty);
  EXPECT_TYPE_NEQ(g1_cty, g2_cty);

  EXPECT_TYPE_NEQ(g1a_cty, g1_cty);
  EXPECT_TYPE_NEQ(g1a_cty, g2_cty);

  EXPECT_TYPE_NEQ(g2_cty, g1a_cty);

  let types_1 = [a_ty; b_ty; f1_cty; f1a_cty]
  and types_2 = [b_wty; f2_cty; g1_cty; a1_ty]
  in
  let sb_1 =
    List.fold_left2 (fun sb x y -> Gtypes.bind x y sb)
                    (Gtypes.empty_subst())
                    types_1 types_2
  in

  List.iter2
    (fun x y -> EXPECT_TYPE_EQL(Gtypes.lookup x sb_1, y))
    [a_ty; b_ty; f1_cty; f1a_cty]
    [b_wty; f2_cty; g1_cty; a1_ty];

  List.iter2
    (fun x y -> EXPECT_TYPE_EQL(Gtypes.subst x sb_1, y))
    [a_ty; a_wty; b_ty; f1_cty; f1a_cty]
    [b_wty; a_wty; f2_cty; g1_cty; a1_ty];

  (* Weak variables don't bind to variables. *)
  EXPECT_EXN(Gtypes.lookup a_wty, sb_1, Not_found);
  EXPECT_TYPE_NEQ(Gtypes.subst a_wty sb_1, c_ty);
  EXPECT_TYPE_EQL(Gtypes.subst a_wty sb_1, a_wty);

  EXPECT_TRUE(Gtypes.bind b_wty f1a_cty (Gtypes.empty_subst()); true);

  EXPECT_TRUE(try Gtypes.bind b_wty a_ty (Gtypes.empty_subst()); false
              with Failure(_) -> true);

  EXPECT_TRUE(Gtypes.bind b_wty c_wty (Gtypes.empty_subst()); true);


  TESTSUITE_END()

(* The list of tests and the function to run them. *)
let test_list =
  [
    (* test_testsupport; *)
    test_types;
  ]

(* Run the tests. *)
let _ =
  if (TestSupport.run_tests test_list)
  then exit 0
  else exit (-1)
