# tic-tac-toe

## Notes

1) Use ADTs instead of String for player type.
2) Separate functionality (use pure functions) from IO, which makes it easier to test.
3) Add a simulation function: runs a sequence of moves (again, for testing).
4) The board could just be a `Data.Sequence`, which allows simple index based gets AND sets.
   This makes the `makeMove` function simpler.
5) Instead of requiring the user to enter 2d coordinates, they could just enter a position.
   For example:

```
Pos     View    Model
1 2 3   X O X
4 5 6 = O X O = [X,O,X,O,X,O,X,-,-]
7 8 9   X - -
```

Here, input positions would be [1, 2, 3, 6, 5, 4, 7] for simulation.
Model index is a simple calculation of `position - 1`.

## TODO

- Print board as grid
- Add tests
