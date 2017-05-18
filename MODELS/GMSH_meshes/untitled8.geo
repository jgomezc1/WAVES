cl=0.05;
l=20.00;
h=10.00;

Point(1) = {0, 0, 0, cl};
Point(2) = {l, 0, 0, cl};
Point(3) = {l, h, 0, cl};
Point(4) = {0, h, 0, cl};

Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};

Line Loop(1) = {1, 2, 3, 4};
Plane Surface(1) = {1};


Transfinite Surface {1} Alternated;
Recombine Surface {1};

Physical Surface(1000) = {1};

Physical Line(1) = {3};
Physical Line(2) = {2,4};