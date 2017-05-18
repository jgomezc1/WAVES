//Cuidado que todo lo tengo amplificado por 10.0
cl1 =  0.10;
cl2 =  0.15;
cl3 =  0.25;
cl4=   0.10;
l   = 60.00;
h   = 30.00;
hest=  2.00;
hbox=  1.00;

////////////////////

l1=  5.00;
l2=  4.80;
l3=  0.20;
l4= 19.00;
l5=  1.00;

////////////////////

h1=l1;
h2=l2;
h3=l3;
h4=h-hest-h1-h2-h3;

Point(1)  = {0          , 0          , 0, cl2};
Point(2)  = {l1         , 0          , 0, cl2};
Point(3)  = {l1+l2      , 0          , 0, cl1};
Point(4)  = {l1+l2+l3   , 0          , 0, cl1};
Point(5)  = {l1+l2+l3   , hest       , 0, cl1};
Point(6)  = {l1+l2+l3   , hest+h4    , 0, cl1};
Point(7)  = {l-l1-l2-l3 , hest+h4    , 0, cl1};
Point(8)  = {l-l1-l2-l3 , hest       , 0, cl1};
Point(9)  = {l-l1-l2-l3 , 0          , 0, cl1};
Point(10) = {l-l1-l2    , 0          , 0, cl1};
Point(11) = {l-l1       , 0          , 0, cl2};
Point(12) = {l          , 0          , 0, cl2};
Point(13) = {l          , hest       , 0, cl2};
Point(14) = {l          , h          , 0, cl3};
Point(15) = {0          , h          , 0, cl3};
Point(16) = {0          , hest       , 0, cl2};
Point(17) = {l1         , hest       , 0, cl2};
Point(18) = {l1+l2      , hest       , 0, cl1};
Point(19) = {l1+l2      , hest+h4    , 0, cl1};
Point(20) = {l1+l2      , h-h1-h2    , 0, cl1};
Point(21) = {l1+l2+l3   , h-h1-h2    , 0, cl1};
Point(22) = {l-l1-l2-l3 , h-h1-h2    , 0, cl1};
Point(23) = {l-l1-l2    , h-h1-h2    , 0, cl1};
Point(24) = {l-l1-l2    , h-h1-h2-h3 , 0, cl1};
Point(25) = {l-l1-l2    , hest       , 0, cl1};
Point(26) = {l-l1       , hest       , 0, cl2};
Point(27) = {l1         , h-h1       , 0, cl3};
Point(28) = {l-l1       , h-h1       , 0, cl3};
Point(29) = {l/2-l5     , 0          , 0, cl4};
Point(30) = {l/2-l5     , hbox       , 0, cl4};

Point(33) = {l/2+l5     , hbox       , 0, cl4};
Point(34) = {l/2+l5     , 0          , 0, cl4};


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
Line(16) = {16, 1 };
Line(17) = {16, 17};
Line(18) = {17, 18};
Line(19) = {18, 19};
Line(20) = {19, 20};
Line(21) = {20, 21};
Line(22) = {21, 22};
Line(23) = {22, 23};
Line(24) = {23, 24};
Line(25) = {24, 25};
Line(26) = {25, 26};
Line(27) = {26, 13};
Line(28) = {17, 27};
Line(29) = {27, 28};
Line(30) = {28, 26};

Line(31) = {2 , 17};
Line(32) = {3 , 18};
Line(33) = {18, 5 };
Line(34) = {26, 11};
Line(35) = {25, 10};
Line(36) = {25, 8 };
Line(37) = {19, 6 };
Line(38) = {6 , 21};
Line(39) = {22, 7 };
Line(40) = {7 , 24};
Line(41) = {4 , 29};
Line(42) = {34, 9 };

Line(44) = {33, 30};

Line(46) = {34, 33};

Line(50) = {30, 29};
Line(51) = {29, 34};

Line(52) = {8 ,  5};






Line Loop(1) = {28, 29, 30, 27, 13, 14, 15, 17};
Plane Surface(1) = {1};
//Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(2) = {18, 19, 20, 21, 22, 23, 24, 25, 26, -30, -29, -28};
Plane Surface(2) = {2};
//Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {37, 38, -21, -20};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};

Line Loop(4) = {6, -39, -22, -38};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {39, 40, -24, -23};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
Recombine Surface {5};

Line Loop(6) = {33, 5, -37, -19};
Plane Surface(6) = {6};
Transfinite Surface {6} Alternated;
Recombine Surface {6};

Line Loop(7) = {-52, -7, -6, -5};
Plane Surface(7) = {7};
Transfinite Surface {7} Alternated;
Recombine Surface {7};

Line Loop(8) = {-36, -25, -40, 7};
Plane Surface(8) = {8};
Transfinite Surface {8} Alternated;
Recombine Surface {8};

Line Loop(10) = {1, 31, -17, 16};
Plane Surface(10) = {10};
Transfinite Surface {10} Alternated;
Recombine Surface {10};

Line Loop(11) = {2, 32, -18, -31};
Plane Surface(11) = {11};
//Transfinite Surface {11} Alternated;
Recombine Surface {11};

Line Loop(12) = {3, 4, -33, -32};
Plane Surface(12) = {12};
Transfinite Surface {12} Alternated;
Recombine Surface {12};

Line Loop(13) = {8, 9, -35, 36};
Plane Surface(13) = {13};
Transfinite Surface {13} Alternated;
Recombine Surface {13};

Line Loop(14) = {10, -34, -26, 35};
Plane Surface(14) = {14};
//Transfinite Surface {14} Alternated;
Recombine Surface {14};

Line Loop(15) = {11, 12, -27, 34};
Plane Surface(15) = {15};
Transfinite Surface {15} Alternated;
Recombine Surface {15};

Line Loop(17) = {51, 46, 44, 50};
Plane Surface(17) = {17};
Transfinite Surface {17} Alternated;
Recombine Surface {17};


Line Loop(19) = {41, -50, -44, -46, 42, -8, 52, -4};
Plane Surface(19) = {19};
//Transfinite Surface {19} Alternated;
Recombine Surface {19};

//////////////////////////

Physical Surface(1000) = {3, 4, 5, 6, 8};           //Incoming Half
Physical Surface(2000) = {12, 13};                  //Incoming Estrato
Physical Surface(3000) = {1, 2, 7};                 //Half-space
Physical Surface(4000) = {10, 11, 19, 14, 15};      //Estrato
Physical Surface(5000) = {17};                      //Valle

//For horizontal lines
Physical Line(1) = {14};

//For vertical lines
Physical Line(2) = {13, 15};

//For vertical lines in strata
Physical Line(3) = {12, 16};

//Results in the free surface
Physical Line(4) = {41, 51, 42};

//Incoming elements in the half space
Physical Line(5) = {19, 20, 21, 22, 23, 24, 25};

//Incoming elements in strata
Physical Line(6) = {32, 35};

//Mesh.SecondOrderIncomplete = 1;























