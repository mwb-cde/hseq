(*----
  Name: numsScript.ml
  Copyright Matthew Wahab 2005-2016
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

begin_theory "nums" ["Bool"];;

declare (Commands.read_unchecked
           ((Basic.name Nums.plusid)^": num -> num -> num"))
  ~pp:(9, infixl, Some "+");;

declare (Commands.read_unchecked
           ((Basic.name Nums.minusid)^": num -> num -> num"))
  ~pp:(10, infixl, Some "-");;

declare (Commands.read_unchecked
           ((Basic.name Nums.multid)^": num -> num -> num"))
  ~pp:(11, infixl, Some "*");;

declare (Commands.read_unchecked
           ((Basic.name Nums.negid)^":  num -> num"))
  ~pp:(12, prefix, Some "~");;

declare (Commands.read_unchecked
           ((Basic.name Nums.maxid)^": num -> num -> num"));;

declare (Commands.read_unchecked
           ((Basic.name Nums.minid)^": num -> num -> num"));;

declare (Commands.read_unchecked
           ((Basic.name Nums.gtid)^": num -> num -> bool"))
  ~pp:(15, infixl, Some ">");;

declare (Commands.read_unchecked
           ((Basic.name Nums.geqid)^": num -> num -> bool"))
  ~pp:(15, infixl, Some ">=");;

declare (Commands.read_unchecked
           ((Basic.name Nums.ltid)^": num -> num -> bool"))
  ~pp:(15, infixl, Some "<");;

declare (Commands.read_unchecked
           ((Basic.name Nums.leqid)^": num -> num -> bool"))
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
