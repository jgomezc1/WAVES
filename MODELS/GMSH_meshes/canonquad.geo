cl=0.01;
l =0.80;
h =0.80;
l1=0.30;
l2=0.20;
h1=0.20;

Point(1) = {0, 0, 0, cl};
Point(2) = {l1, 0, 0, cl};
Point(3) = {l1, h1, 0, cl};
Point(4) = {l1+l2, h1, 0, cl};
Point(5) = {l1+l2, 0, 0, cl};
Point(6) = {l, 0, 0, cl};
Point(7) = {l, h1, 0, cl};
Point(8) = {l, h, 0, cl};
Point(9) = {l1+l2, h, 0, cl};
Point(10) = {l1, h, 0, cl};
Point(11) = {0, h, 0, cl};
Point(12) = {0, h1, 0, cl};


Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 5};
Line(5) = {5, 6};
Line(6) = {6, 7};
Line(7) = {7, 8};
Line(8) = {8, 9};
Line(9) = {9, 10};
Line(10) = {10, 11};
Line(11) = {11, 12};
Line(12) = {12, 1};
Line(13) = {12, 3};
Line(14) = {3, 10};
Line(15) = {4, 7};
Line(16) = {9, 4};



Line Loop(1) = {1, 2, -13, 12};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(2) = {4, 5, 6, -15};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {11, 13, 14, 10};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};

Line Loop(4) = {3, -16, 9, -14};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {15, 7, 8, 16};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
Recombine Surface {5};

Physical Surface(1000) = {1, 2, 3, 4, 5};
//Physical Surface(1000) = {4, 5};

Physical Line(1) = {8, 9, 10};
Physical Line(2) = {11, 12, 6, 7};

Physical Line(3) = {1, 2, 3, 4, 5};

//Mesh.SecondOrderIncomplete = 1;



