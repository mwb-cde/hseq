(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

begin_theory "nums" ["Bool"];;

declare (read_unchecked ((Term.name Nums.plusid)^": num -> num -> num"))
  ~pp:(9, infixl, Some "+");;

declare (read_unchecked
           ((Term.name Nums.minusid)^": num -> num -> num"))
  ~pp:(10, infixl, Some "-");;

declare (read_unchecked
           ((Term.name Nums.multid)^": num -> num -> num"))
  ~pp:(11, infixl, Some "*");;

declare (read_unchecked
           ((Term.name Nums.negid)^":  num -> num"))
  ~pp:(12, prefix, Some "~");;

declare (read_unchecked
           ((Term.name Nums.maxid)^": num -> num -> num"));;

declare (read_unchecked
           ((Term.name Nums.minid)^": num -> num -> num"));;

declare (read_unchecked
           ((Term.name Nums.gtid)^": num -> num -> bool"))
  ~pp:(15, infixl, Some ">");;

declare (read_unchecked
           ((Term.name Nums.geqid)^": num -> num -> bool"))
  ~pp:(15, infixl, Some ">=");;

declare (read_unchecked
           ((Term.name Nums.ltid)^": num -> num -> bool"))
  ~pp:(15, infixl, Some "<");;

declare (read_unchecked
           ((Term.name Nums.leqid)^": num -> num -> bool"))
  ~pp:(15, infixl, Some "=<");;

axiom "nat_induction"
<< !P: ((P 0) and
  (!a: ((a>=0) and (P a)) => (P (a+1))))
  =>
(!x: (x>=0) => (P x))
>>;;

axiom "strong_nat_induction"
<< !P:
  (!a : (a>=0)
     => (!b: (b>=0) => (b<a) => (P b))
     => (P a))
  =>
(!x: x>=0 => (P x)) >>;;

close_theory();;

let _ = Display.print_theory (theory "");;
 
