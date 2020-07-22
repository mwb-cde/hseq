(*----
  Copyright (c) 2017-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(* Compile with:

  Compile with
   ocamlc -pp cpp
      -I BUILD_ROOT/hseq unix.cma hseq.cma

  where build_ROOT is the root of the hseq build directory.
  and   cpp is the C preproccessor
 *)

#include "TestSupport.inc"

(* Open HSeq modules. *)

open HSeq

#define EXPECT_TYPE_EQL(A,B) \
MAKE_EXPECT_BINOP((TestSupport.make_pred_test Gtype.equals), " = ", A, B)

#define EXPECT_TYPE_NEQ(A,B) \
MAKE_EXPECT_BINOP\
((TestSupport.make_pred_test (fun x y -> not (Gtype.equals x y))), \
" <> ", A, B)

(* Form the list of pairs P such that forall a, b in L, (a, b) in P *)
let list_prod lst =
  let rec inner x l rslt =
    match l with
    | [] -> rslt
    | (y::ys) -> inner x ys ((x, y)::rslt)
  and prod l rslt =
    match l with
    | [] -> rslt
    | (x::xs) -> prod xs (inner x lst rslt)
  in
  prod lst []

let test_types() =
  TESTSUITE_BEGIN("Types");

  let (a_ty: Gtype.t) = Gtype.mk_var "a"
  and a_wty = Gtype.mk_weak "a"
  and a1_ty = Gtype.mk_var "a"
  and a1_wty = Gtype.mk_weak "a"
  and b_ty = Gtype.mk_var "b"
  and b_wty = Gtype.mk_weak "b"
  and c_ty = Gtype.mk_var "c"
  and c_wty = Gtype.mk_weak "c"
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
  let f1_cty = Gtype.mk_constr f1 [a_ty; b_ty; c_ty]
  and f1a_cty = Gtype.mk_constr f1 [a1_ty; b_ty; c_ty]
  and f2_cty = Gtype.mk_constr f2 [a1_ty; b_ty; c_ty]
  in
  EXPECT_TYPE_EQL(f1_cty, f1_cty);
  EXPECT_TYPE_NEQ(f1_cty, f1a_cty);
  EXPECT_TYPE_NEQ(f1_cty, f2_cty);

  EXPECT_TYPE_NEQ(f2_cty, f1_cty);
  EXPECT_TYPE_NEQ(f2_cty, f1a_cty);

  let g1_cty = Gtype.mk_constr g1 [a_wty; b_ty]
  and g1a_cty = Gtype.mk_constr g1 [a_wty]
  and g2_cty = Gtype.mk_constr g2 [a1_ty; b_ty; c_ty]
  in
  EXPECT_TYPE_EQL(g1_cty, g1_cty);
  EXPECT_TYPE_NEQ(g1_cty, g1a_cty);
  EXPECT_TYPE_NEQ(g1_cty, g2_cty);

  EXPECT_TYPE_NEQ(g1a_cty, g1_cty);
  EXPECT_TYPE_NEQ(g1a_cty, g2_cty);

  EXPECT_TYPE_NEQ(g2_cty, g1a_cty);

  let h1_app = Gtype.mk_app a_ty a_wty
  and h2_app = Gtype.mk_app g1_cty b_ty
  and h3_app = Gtype.mk_app c_ty g2_cty
  in
  List.iter
    (fun x -> EXPECT_TYPE_EQL(x, x))
    [h1_app; h2_app; h3_app];

  List.iter2
    (fun x y -> EXPECT_TYPE_NEQ(x, y))
    [h1_app; h2_app; h3_app] [h2_app; h3_app; h1_app];

  let full_list =
    [a_ty; a_wty; a1_ty; a1_wty; b_ty; b_wty; c_ty; c_wty;
     f1_cty; f1a_cty; f2_cty; g1_cty; g1a_cty; g2_cty;
     h1_app; h2_app; h3_app]
  in
  let full_prod_list = list_prod full_list
  in
  List.iter
    (fun (x, y) -> EXPECT_EQL((Gtype.equals x y), (Gtype.equals y x)))
    full_prod_list;

  let types_1 = [a_ty; b_ty; f1_cty; f1a_cty]
  and types_2 = [b_wty; f2_cty; g1_cty; a1_ty]
  in
  let sb_1 =
    List.fold_left2 (fun sb x y -> Gtype.Subst.bind x y sb)
                    (Gtype.Subst.empty())
                    types_1 types_2
  in
  List.iter2
    (fun x y -> EXPECT_TYPE_EQL(Gtype.Subst.lookup x sb_1, y))
    [a_ty; b_ty; f1_cty; f1a_cty]
    [b_wty; f2_cty; g1_cty; a1_ty];

  List.iter2
    (fun x y -> EXPECT_TYPE_EQL(Gtype.subst x sb_1, y))
    [a_ty; a_wty; b_ty; f1_cty; f1a_cty]
    [b_wty; a_wty; f2_cty; g1_cty; a1_ty];

  (* Weak variables don't bind to variables. *)
  EXPECT_EXN(Gtype.Subst.lookup a_wty, sb_1, Not_found);
  EXPECT_TYPE_NEQ(Gtype.subst a_wty sb_1, c_ty);
  EXPECT_TYPE_EQL(Gtype.subst a_wty sb_1, a_wty);

  EXPECT_TRUE(ignore(Gtype.Subst.bind b_wty f1a_cty
                       (Gtype.Subst.empty())); true);

  EXPECT_TRUE(try ignore(Gtype.Subst.bind b_wty a_ty (Gtype.Subst.empty()));
                  false
              with Failure(_) -> true);

  EXPECT_TRUE(ignore(Gtype.Subst.bind b_wty c_wty (Gtype.Subst.empty())); true);


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
