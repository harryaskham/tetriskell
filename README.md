# tetriskell

Tetris implemented directly in the terminal (i.e. plain stdout, no curses) in order to better learn Haskell.

Includes both human play and an AI agent demoed below (see `Agent.hs`).

![demo](demo2.gif)

To run, simply: `stack run`.

- `j`, `k`, `l` are left, down, right
- `u` rotates CW, `i` rotates CCW
- `spacebar` drops
- `h` holds the current piece
