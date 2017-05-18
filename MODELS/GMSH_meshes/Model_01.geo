//Cuidado que todo lo tengo amplificado por 10.0
cl=0.2;
cl1=0.1;
l =30.0;
h =20.00;

l1=0.50;
l2=1.50;

h1=l1;
h2=l2;
h4=10.0;
h3=h-h1-h2-h4;
 

Point(1)  = {0       , 0    , 0, cl1};
Point(2)  = {l1      , 0    , 0, cl1};
Point(3)  = {l1+l2   , 0    , 0, cl };
Point(4)  = {l1+l2   , h4   , 0, cl };
Point(5)  = {l-l1-l2 , h4   , 0, cl };
Point(6)  = {l-l1-l2 , 0    , 0, cl };
Point(7)  = {l-l1    , 0    , 0, cl1};
Point(8)  = {l       , 0    , 0, cl1};
Point(9)  = {l       , h-h1 , 0, cl1};
Point(10) = {l       , h    , 0, cl1};
Point(11) = {l-l1    , h    , 0, cl1};
Point(12) = {l1      , h    , 0, cl1};
Point(13) = {0       , h    , 0, cl1};
Point(14) = {0       , h-h1 , 0, cl1};
Point(15) = {l-l1-l2 , h3+h4, 0, cl };
Point(16) = {l1+l2   , h3+h4, 0, cl };
Point(17) = {l-l1    , h-h1 , 0, cl1};
Point(18) = {l1      , h-h1 , 0, cl1};


Line(1)  = {1 , 2 };
Line(2)  = {2 , 3 };
Line(3)  = {3 , 4 };
Line(4)  = {4 , 5 };
Line(5)  = {5 , 6 };
Line(6)  = {6 , 7 };
Line(7)  = {7 , 8 };
Line(8)  = {8 , 9 };
Line(9)  = {9 , 10};
Line(10) = {10, 11};
Line(11) = {11, 12};
Line(12) = {12, 13};

Line(13) = {13, 14};
Line(14) = {14, 1 };
Line(15) = {7 , 17};
Line(16) = {17, 18};
Line(17) = {18, 2 };
Line(18) = {5 , 15};
Line(19) = {15, 16};
Line(20) = {16, 4 };
Line(21) = {17, 9 };
Line(22) = {11, 17};
Line(23) = {14, 18};
Line(24) = {18, 12};

Line(25) = {3 , 6 };


Line Loop(1) = {23, 24, 12, 13};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(2) = {-16, -22, 11, -24};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {21, 9, 10, 22};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};


Line Loop(4) = {1, -17, -23, 14};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {2, 3, -20, -19, -18, 5, 6, 15, 16, 17};
Plane Surface(5) = {5};
//Transfinite Surface {5} Alternated;
Recombine Surface {5};

Line Loop(6) = {4, 18, 19, 20};
Plane Surface(6) = {6};
Transfinite Surface {6} Alternated;
Recombine Surface {6};

Line Loop(7) = {7, 8, -21, -15};
Plane Surface(7) = {7};
Transfinite Surface {7} Alternated;
Recombine Surface {7};

Line Loop(8) = {25, -5, -4, -3};
Plane Surface(8) = {8};
Transfinite Surface {8} Alternated;
Recombine Surface {8};


Physical Surface(1000) = {1, 2, 3, 4, 5, 6, 7, 8};
//Physical Surface(1000) = {4, 5};

//For horizontal lines
Physical Line(1) = {10, 11, 12};

//For vertical lines
Physical Line(2) = {8, 9, 13, 14};

Physical Line(3) = {1, 2, 25, 6, 7};

//Mesh.SecondOrderIncomplete = 1;



