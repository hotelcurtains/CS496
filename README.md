# Programming Language Fundamentals

[This site](https://cs3110.github.io/textbook/chapters/preface/install.html) 
has the best advice for setting up OCaml than I could find anywhere else.
I have it on the Ubuntu WSL2 but it's effectively the same in an Ubuntu VM.

Run the interpreter for a language with `dune utop` in `src/<language>/lib`.

Running the interpreter creates a bunch of extra files to make its next startup quicker.
If you don't want these files, run `dune clean` in `src/<language>/lib`.

`main.pdf` is Professor Bonelli's textbook. The Markdown files contain my notes. The textbook is from at latest September 2025, and my notes are not infallible. Use them at your own risk.