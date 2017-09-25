L  = 5.00;
h1 = 0.30;
h2 = 2.50;
cl = 0.20;

Point(1) = {-L, 0, 0, cl};
Point(2) = {L, 0, 0, cl};
Point(3) = {L, h1, 0, cl};
Point(4) = {-L, h1, 0, cl};
Point(5) = {L, h1+h2, 0, cl};
Point(6) = {-L, h1+h2, 0, cl};

Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line(5) = {3, 5};
Line(6) = {5, 6};
Line(7) = {6, 4};

Line Loop(8) = {1, 2, 3, 4};
Plane Surface(9) = {8};
Transfinite Surface {9} Alternated;
Recombine Surface {9};

Line Loop(10) = {5, 6, 7, -3};
Plane Surface(11) = {10};
Transfinite Surface {11} Alternated;
Recombine Surface {11};

Physical Line(100) = {5, 6, 7};
Physical Surface(10000) = {9};
Physical Surface(20000) = {11};
Physical Line(200) = {1};

//Mesh.SecondOrderIncomplete = 1;
