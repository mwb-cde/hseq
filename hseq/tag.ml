(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

type ('a)t = ('a)ref

let make a = ref a
let named (x: string) = make x
let create () = named ""

let contents x = !x
let name = contents

let equal x y = x == y
