# Grid Reversal

There are a few problems and TODOs for Nat; 1 and 2 are done now!

1. DONE: There's a nasty bug where the grid gets disconnected as vertices get updated. It
shouldn't affect the output word, but it does make the grid invalid. The fix is either a
lot more code or a refactor; Nat chose to refactor.
2. DONE: Nat thinks that it's probably best if the cells stay quadrilateral. He plans to
use the epsilon-sigma-epsilon-sigma cells to enforce that
3. There's a lack of tests. This will be easy to fuzz test
4. There's no way to visualize these grids. Nat plans to output them to graphviz as
something quick and easy

He plans to tackle them in order.
