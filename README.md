This directory contains a source-to-source translator that recognizes a subset of Flux and translates
it to Lisp code.

Albeit being a source-to-source translator, this program goes through a full AST->IR->output transformation
process rather than trying to translate directly from AST to output.  I tried the latter approach first
and found that I still know to little about all the things it takes to translate Flux to take shortcuts.
This also shows in that some phases in the compiler could be combined, but without knowing how things work
exactly, trying to attempt this up-front turned out to be premature optimization that caused many problems.

This shows, for example, in the parsing step that only produces an AST and does not attempt any symbol table
operations.  This requires re-scanning the AST to a certain degree later in order to discover definitions
that could have already been discoverd during the parsing step.
