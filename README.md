This directory contains a source-to-source translator that recognizes a subset of Flux and translates
it to Lisp code.

Due to it being "just" a source-to-source translator, this program operates in a somewhat simplified
manner compared to the "real" Flux compiler.  It has no intermediate representation and instead
stores what semantic information it needs directly on the AST.

## Restrictions

   * Modules and imports are completely ignored.  Everything is pretended to be defined at the global
     level.
   * Pretty much no checking is performed.  Errors will usually show only when running the generated
     Lisp code.
   * The compiler operates very differently from "the real thing" in that it goes straight from ASTs
     to generated code without employing an intermediate representation (as appropriate for a source-
	 to-source translator but not very appropriate for a real compiler).
