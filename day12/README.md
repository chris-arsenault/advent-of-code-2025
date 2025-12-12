## Approach

- **Shape handling:** Read each 3×3 shape, enumerate all unique orientations (four rotations and a horizontal flip), and normalise coordinates so top-left is `(0,0)`.

- **Region parsing:** For every `WxH` region line, collect the board size and the required multiplicity of each shape.

- **Quick pruning:** Reject immediately if the total requested area exceeds `W*H`.

- **Exact cover with a battle-tested library:** For manageable boards (`<=400` cells and `<=60` pieces), formulate tiling as exact cover and hand it to the `dlx` Algorithm X implementation:
  - Columns: one primary column per individual piece copy (must be covered) and one secondary column per board cell (enforces non-overlap but need not be covered).
  - Rows: each legal placement of a shape copy lists its piece column plus the cell columns it occupies.
  - The first solution returned proves feasibility.

- **Large regions:** For bigger instances, rely on the cheap checks (area) to avoid explosive search while still giving a fast answer for the provided inputs.
