HSeq Theorem Prover
===================

HSeq is a tactical theorem prover for typed higher-order logic. It
is experimental, alpha-quality code which may have soundness bugs.

**Requirements:**

* OCaml 4.03 or later
* GNU Make.
* Unix/Linux build
* Texinfo, to build the documentaion (optional).

For installation instructions, see file INSTALL.

The system builds as a theorem prover library and an interface to the
OCaml toplevel. 

Programs
--------

* hseq - The interactive theorem prover.
* hseqb <file> - Run the theorem prover on script <file>
* hseqc <file> - Compile script <file> against the HSeq libraries.

Use option --help with hseqb and hseqc for the list of supported options.

Documentation
-------------

Basic documentation is installed in directories doc/html and doc/info. By
default, the documentation directories are installed into <prefix>/share/hseq.

License
-------

HSeq is distributed under the terms of the Lesser GNU General Public
License, see files COPYING and COPYING.LESSER for details.

Copyright Matthew Wahab, 2005-2006.
