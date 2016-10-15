# Slidecoding - a mix of slides and code

Mix markdown slides and source code.

Do you usually show code in your slides? How do you demo or emphasize part of it?
You may be used to switch between your talk and your IDE or REPL, but that's
breaking the flow and most of the time the surrounding environment of the IDE or
the REPL can be disturbing for the audience.

Slidecoding propose you to mix livecoding and slides by

 - easily embed code from a classic project
 - let you open a REPL right from the slides, in a specific context with all helpers
   and dataset you may need to demo the cool code

With this setup, the code you show is actually running in a standard project, no
risk to be wrong on stage!

For now, only Haskell is supported. Specification is in my mind to let others
add support to other languages.

# Under the hood

2 main parts:

 - index source code to be able to link functions/methods from the markdown
 - REPL support

Markdown parsing is done with the amazing [pandoc](http://pandoc.org).

Frédéric Menou
