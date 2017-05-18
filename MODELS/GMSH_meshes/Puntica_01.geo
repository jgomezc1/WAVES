//Cuidado que todo lo tengo amplificado por 10.0
cl1 =  0.10;
cl2 =  0.15;
cl3 =  0.25;
cl4=   0.10;
l   = 50.00;
h   = 25.00;
hest=  2.00;
hbox=  1.00;
lbox=  2.00;

////////////////////

l1=  0.20;
l2=  0.50;
l3=  l/2-l1-l2-lbox/2;

h1=l1;
h2=h-h1-hest;
h3=h-hest-h1-0.4;

////////////////////

Point(1)  = {0          , 0          , 0, cl1};
Point(2)  = {l1         , 0          , 0, cl1};
Point(3)  = {l1+l2      , 0          , 0, cl2};
Point(4)  = {l1+l2      , hest       , 0, cl2};
Point(5)  = {l1+l2      , hest+h3    , 0, cl3};
Point(6)  = {l-l1-l2    , hest+h3    , 0, cl3};
Point(7)  = {l-l1-l2    , hest       , 0, cl2};
Point(8)  = {l-l1-l2    , 0          , 0, cl2};
Point(9)  = {l-l1       , 0          , 0, cl1};
Point(10) = {l          , 0          , 0, cl1};
Point(11) = {l          , hest       , 0, cl1};
Point(12) = {l          , h-h1       , 0, cl1};
Point(13) = {l          , h          , 0, cl1};
Point(14) = {l-l1       , h          , 0, cl1};
Point(15) = {l1         , h          , 0, cl1};
Point(16) = {0          , h          , 0, cl1};
Point(17) = {0          , h-h1       , 0, cl1};
Point(18) = {0          , hest       , 0, cl1};
Point(19) = {l-l1       , hest       , 0, cl1};
Point(20) = {l-l1       , h-h1       , 0, cl1};
Point(21) = {l1         , h-h1       , 0, cl1};
Point(22) = {l1         , hest       , 0, cl1};


Point(23) = {(l-lbox)/2 , 0          , 0, cl4};
Point(24) = {(l+lbox)/2 , 0          , 0, cl4};
Point(25) = {(l+lbox)/2 , hbox       , 0, cl4};
Point(26) = {(l-lbox)/2 , hbox       , 0, cl4};


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
Line(14) = {14, 15};
Line(15) = {15, 16};
Line(16) = {16, 17};
Line(17) = {17, 18};
Line(18) = {18, 1 };
Line(19) = {7 , 19};
Line(20) = {19, 20};
Line(21) = {20, 21};
Line(22) = {21, 22};
Line(23) = {22, 4 };
Line(24) = {2 , 22};
Line(25) = {22, 18};
Line(26) = {11, 19};
Line(27) = {19, 9 };
Line(28) = {14, 20};
Line(29) = {20, 12};
Line(30) = {17, 21};
Line(31) = {21, 15};

Line(32) = {3 , 23};
Line(33) = {23, 26};
Line(34) = {26, 25};
Line(35) = {25, 24};
Line(36) = {24, 8 };
Line(37) = {7 , 4 };
Line(38) = {23, 24};


Line Loop(1) = {15, 16, 30, 31};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(2) = {-21, -28, 14, -31};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {12, 13, 28, 29};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};

Line Loop(4) = {-22, -30, 17, -25};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {4, 5, 6, 19, 20, 21, 22, 23};
Plane Surface(5) = {5};
//Transfinite Surface {5} Alternated;
Recombine Surface {5};

Line Loop(6) = {11, -29, -20, - 26};
Plane Surface(6) = {6};
Transfinite Surface {6} Alternated;
Recombine Surface {6};

Line Loop(7) = {24, 25, 18, 1};
Plane Surface(7) = {7};
Transfinite Surface {7} Alternated;
Recombine Surface {7};

Line Loop(8) = {2, 3, -23, -24};
Plane Surface(8) = {8};
//Transfinite Surface {8} Alternated;
Recombine Surface {8};

Line Loop(9) = {7, 8, -27, -19};
Plane Surface(9) = {9};
//Transfinite Surface {9} Alternated;
Recombine Surface {9};

Line Loop(10) = {9, 10, 26, 27};
Plane Surface(10) = {10};
Transfinite Surface {10} Alternated;
Recombine Surface {10};

Line Loop(11) = {-37, -6, -5, -4};
Plane Surface(11) = {11};
//Transfinite Surface {11} Alternated;
Recombine Surface {11};

Line Loop(12) = {32, 33, 34, 35, 36, -7, 37, -3};
Plane Surface(12) = {12};
//Transfinite Surface {12} Alternated;
Recombine Surface {12};

Line Loop(13) = {38, -35, -34, -33};
Plane Surface(13) = {13};
Transfinite Surface {13} Alternated;
Recombine Surface {13};

//////////////////////////

Physical Surface(1000) = {1, 2, 3, 4, 6};           //Incoming Half
Physical Surface(2000) = {7, 10};                   //Incoming Estrato
Physical Surface(3000) = {5, 11};                   //Half-space
Physical Surface(4000) = {8, 9, 12};                //Estrato
Physical Surface(5000) = {13};                      //Valle


////////////////////////////

//Fronteras absorbentes

//For horizontal lines
Physical Line(1) = {14};

//For vertical lines
Physical Line(2) = {11, 17};

//For vertical lines in strata
Physical Line(3) = {18, 10};

////////////////////////////

//Results in the free surface
Physical Line(4) = {2, 32, 38, 36, 8};

//Incoming elements in the half space
//Physical Line(5) = {19, 20, 21, 22, 23, 24, 25};

//Incoming elements in strata
//Physical Line(6) = {32, 35};

//Mesh.SecondOrderIncomplete = 1;























