# Introduction to swash

TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)




# 1 - beautify trace
The original trace may have consecutive elements with identical t's so when it comes to calculating velocities there may be divisions by zero.
A simple approach to remedy this is:  (filter mindist coll (rest coll)) which eliminates every point from the trace which is followed by a point whose
t-component is less than, say 3 units apart. This is fine as long as such points are not too frequent. A trace like this, however, would lead to
disasterous results: '([p0 5][p1 7][p2 9][p3 11][p4 13][p5 15][p6 17][p7 19][p8 21])
So a necessary side condition is to maximize the number of points on the trace [NYI]

# 2 - normalize curves
curves (i.e. traces or profiles) consist on a number of x or y or t coordinates and combinations thereof. To make them comparable the curves
must be moved so they fit into a box with corners [0.0 0.0] and [xmax ymax] (for traces) or [0.0 0.0] and [tmax ymax], respectively.
For this purpose the minimal coordinate values of a box around the original curve are determined and the curve transferred so this box corner
becomes [0.0 0.0].

# 3 - scale curves
In order to compare curves they must be scaled accordingly. An easy way to do this would be making the boxes around the curves match.
This makes the comparability of handwritten shapes vulnerable to random differences in stroke lengths. To become more stable we better
take the arithmetic averages of the respective coordinate values and use this to compute the scale ratios.

# 4 - compare curves
A precondition for comparision is a suitable metric for the similarity of two curves. We compare two-dimensional (plain) curves. So one approach is to
draw these curves in three-dimensional space like this: (t, y) -> (t, y, 0) for the first and (t, y) -> (t, y, 1) for the second curve.
Now the obtained coordinates are discrete and typically won't match so we have to merge the coordinates.
The metric is then the ratio (distance y1 y2 / distance t1 t2) = (d (y1 y2) / 1) = d(y1 y2).
