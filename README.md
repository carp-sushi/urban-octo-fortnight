# tic-tac-toe

## Notes

The provided code is a solid solution, and given the scope of the project would probably be fine as
is. However, here are some improvements and additions I think could be made.

- Use ADT instead of String for player.
- Use `Data.Array` or `Data.Sequence` instead of [] for the board. This allows simpler access and
  modification.
- Use `<>` instead of `++` for combination.
- Add tests using something like tasty, hspec.
- Add a simulation function: runs a game given a sequence of coordinates (again, for testing).

## Alternative Design

As an alternative to the current design, the board could be modeled as a sequence.
Also, instead of requiring the user to enter coordinates, a simple positional layout could be used.
Most people count from one, and this prevents mistakes where users would enter row/col coordinates
in the wrong order.

### Design Visual

```
Positions    View     Model
---------    -----    -------------------
1 2 3        X O X
4 5 6        O X O    [X,O,X,O,X,O,X,-,-]
7 8 9        X - -
---------    -----    -------------------
```

Model index is a simple calculation of `position - 1`.

In the example above, the input positions would be [1, 2, 3, 6, 5, 4, 7] for game simulation.

For reference, see the prototype code on GitHub at
https://github.com/carp-sushi/urban-octo-fortnight

### Simulation Output

```
$ stack run
X O X
O X O
X - -
Winner = Player1
```
