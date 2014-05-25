This directory contains a source-to-source translator that recognizes a subset of Flux and translates
it to Lisp code.

## Restrictions

   * Modules and imports are completely ignored.  Everything is pretended to be defined at the global
     level.
   * Pretty much no checking is performed.  Errors will usually show only when running the generated
     Lisp code.
