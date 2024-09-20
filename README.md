# Grid Reversal

Nat's idea for a proof outline:
[https://www.overleaf.com/read/gvdryjvjkwmy#07062e](https://www.overleaf.com/read/gvdryjvjkwmy#07062e)

Use:

1. cabal run
2. Type in whatever you want reversed using the alphabet (e.g. BBAAbbaa)
3. Program prints out the direct reversal of that, computes the grid, computes the word from
that and prints it out (they should be the same or there's a bug). Also creates
a .dot file, and if you have graphviz, a .pdf file, for you to view the grid.

There were a few problems and TODOs for Nat; they are all done now!

1. DONE: There's a nasty bug where the grid gets disconnected as vertices get updated. It
shouldn't affect the output word, but it does make the grid invalid. The fix is either a
lot more code or a refactor; Nat chose to refactor.
2. DONE: Nat thinks that it's probably best if the cells stay quadrilateral. He plans to
use the epsilon-sigma-epsilon-sigma cells to enforce that
3. DONE: There's a lack of tests. This will be easy to fuzz test. Now Nat has added random tests;
100 tests on random words of length 10 with 6 strands. They compare between a direct
word reversal and a reversal via grid (everything passes)
4. DONE: There's now a way to visualize these grids. They are output to graphviz now
