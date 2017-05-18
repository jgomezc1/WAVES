//Cuidado que todo lo tengo amplificado por 10.0
cl1=0.10;
cl2=0.15;
cl3=0.25;
l = 70.0;
h = 35.00;

////////////////////

l1= 5.00;
l2= 0.20;
l3= 4.80;
l4= 5.00;
l5=15.00;
l6= 0.20;
l7= 2.30;
l8= 2.50;

////////////////////

l9=15.00;

////////////////////

h2=l3;
h3=l2;
h4= 5.00;
h5=15.00;
h6= 5.00;
h7=15.00;
h8= 4.80;
h9= 0.20;
h1=h-h2-h3-h4;

Point(1)  = {l9           , 0    , 0, cl3};
Point(2)  = {l9+l1        , 0    , 0, cl1};
Point(3)  = {l9+l1+l2     , 0    , 0, cl1};
Point(4)  = {l9+l1+l2+l3  , 0    , 0, cl3};
Point(5)  = {l9+l1+l2+l3  , h1   , 0, cl3};
Point(6)  = {l9+l-l1-l2-l3, h1   , 0, cl3};
Point(7)  = {l9+l-l1-l2-l3, 0    , 0, cl3};
Point(8)  = {l9+l-l1-l2   , 0    , 0, cl1};
Point(9)  = {l9+l-l1      , 0    , 0, cl1};
Point(10) = {l9+l         , 0    , 0, cl3};
Point(11) = {l9+l         , h    , 0, cl3};
Point(12) = {l9+0         , h    , 0, cl3};
Point(13) = {l9+l-l1-l2   , h1+h2, 0, cl1};
Point(14) = {l9+l-l1      , h1+h2, 0, cl1};
Point(15) = {l9+l-l1      , h-h4 , 0, cl1};
Point(16) = {l9+l-l1-l2   , h-h4 , 0, cl1};
Point(17) = {l9+l1        , h1+h2, 0, cl1};
Point(18) = {l9+l1+l2     , h1+h2, 0, cl1};
Point(19) = {l9+l1+l2     , h-h4 , 0, cl1};
Point(20) = {l9+l1        , h-h4 , 0, cl1};

Point(21) = {l9+l1+l2+l3+l4  , 0    , 0, cl2};
Point(22) = {l9+l-l1-l2-l3-l4, 0    , 0, cl2};
Point(23) = {l9+l-l1-l2-l3-l4, h5   , 0, cl2};
Point(24) = {l9+l-l1-l2-l3-l4, h5+h6, 0, cl2};
Point(25) = {l9+l1+l2+l3+l4  , h5+h6, 0, cl2};
Point(26) = {l9+l1+l2+l3+l4  , h5   , 0, cl2};

Point(27) = {l9+l1+l2+l3+l4+l5   ,       0, 0, cl1};
Point(28) = {l9+l1+l2+l3+l4+l5+l6,       0, 0, cl1};
Point(29) = {l9+l/2+l8+l7        ,       0, 0, cl1};
Point(30) = {l9+l/2+l8+l7+l6     ,       0, 0, cl1};
Point(31) = {l9+l/2+l8+l7+l6     ,      h8, 0, cl1};
Point(32) = {l9+l/2+l8+l7+l6     ,   h8+h9, 0, cl1};
Point(33) = {l9+l/2+l8+l7        ,   h8+h9, 0, cl1};
Point(34) = {l9+l/2-l8-l7        ,   h8+h9, 0, cl1};
Point(35) = {l9+l/2-l8-l7-l6     ,   h8+h9, 0, cl1};
Point(36) = {l9+l/2-l8-l7-l6     ,      h8, 0, cl1};
Point(37) = {l9+l/2-l8-l7        ,      h8, 0, cl1};
Point(38) = {l9+l/2+l8+l7        ,      h8, 0, cl1};
Point(39) = {l9+l/2-l8           ,       0, 0, cl1};
Point(40) = {l9+l/2              ,       0, 0, cl1};
Point(41) = {l9+l/2+l8           ,       0, 0, cl1};




////////////////////

Point(42) = {0,0,0, cl3};
Point(43) = {2*l9+l,0,0, cl3};

Point(44) = {2*l9+l,h+15,0, cl3};
Point(45) = {0,h+15,0, cl3};

////////////////////



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
Line(12) = {12, 1 };

Line(13) = {8 , 13};
Line(14) = {13, 18};
Line(15) = {18, 3 };
Line(16) = {2 , 17};
Line(17) = {17, 20};
Line(18) = {20, 19};
Line(19) = {19, 16};
Line(20) = {16, 15};
Line(21) = {15, 14};
Line(22) = {14, 9 };
Line(23) = {17, 18};
Line(24) = {18, 19};
Line(25) = {16, 13};
Line(26) = {13, 14};

Line(27) = { 4, 21};
Line(28) = {21, 26};
Line(29) = {26, 23};
Line(30) = {23, 22};
Line(31) = {22,  7};
Line(32) = {23, 24};
Line(33) = {24, 25};
Line(34) = {25, 26};

Line(35) = {21, 27};
Line(36) = {27, 28};
Line(37) = {28, 39};
Line(38) = {39, 40};
Line(39) = {40, 41};
Line(40) = {41, 29};
Line(41) = {29, 30};
Line(42) = {30, 22};
Line(43) = {30, 31};
Line(44) = {31, 32};
Line(45) = {32, 33};
Line(46) = {33, 34};
Line(47) = {34, 35};
Line(48) = {35, 36};
Line(49) = {36, 27};
Line(50) = {36, 37};
Line(51) = {37, 34};
Line(52) = {33, 38};
Line(53) = {38, 31};
Line(54) = {29, 38};
Line(55) = {38, 37};
Line(56) = {37, 28};



Circle(57) = {41, 40, 39};

//////////////////////////

Line(58) = {10, 43};
Line(59) = {43, 44};
Line(60) = {44, 45};
Line(61) = {45, 42};
Line(62) = {42, 1 };

//////////////////////////

Line Loop(17) = {1, 16, 17, 18, 19, 20, 21, 22, 9, 10, 11, 12};
Plane Surface(17) = {17};
//Transfinite Surface {17} Alternated;
Recombine Surface {17};

Line Loop(2) = {23, 24, -18, -17};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {-14, -25, -19, -24};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};

Line Loop(4) = {25, 26, -21, -20};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {2, -15, -23, -16};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
Recombine Surface {5};

Line Loop(1) = {3, 4, 5, 6, 7, 13, 14, 15};
Plane Surface(1) = {1};
//Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(7) = {8, -22, -26, -13};
Plane Surface(7) = {7};
Transfinite Surface {7} Alternated;
Recombine Surface {7};

Line Loop(8) = {27, 28, -34, -33, -32, 30, 31, -6, -5, -4};
Plane Surface(8) = {8};
//Transfinite Surface {8} Alternated;
Recombine Surface {8};

Line Loop(9) = {32, 33, 34, 29};
Plane Surface(9) = {9};
//Transfinite Surface {9} Alternated;
Recombine Surface {9};

Line Loop(10) = {42, -30, -29, -28, 35, -49, -48, -47, -46, -45, -44, -43};
Plane Surface(10) = {10};
//Transfinite Surface {10} Alternated;
Recombine Surface {10};

Line Loop(11) = {47, 48, 50, 51};
Plane Surface(11) = {11};
Transfinite Surface {11} Alternated;
Recombine Surface {11};

Line Loop(12) = {-55, -52, 46, -51};
Plane Surface(12) = {12};
Transfinite Surface {12} Alternated;
Recombine Surface {12};

Line Loop(13) = {44, 45, 52, 53};
Plane Surface(13) = {13};
Transfinite Surface {13} Alternated;
Recombine Surface {13};

Line Loop(14) = {36, -56, -50, 49};
Plane Surface(14) = {14};
Transfinite Surface {14} Alternated;
Recombine Surface {14};

Line Loop(15) = {37, -57, 40, 54, 55, 56};
Plane Surface(15) = {15};
//Transfinite Surface {15} Alternated;
Recombine Surface {15};

Line Loop(16) = {41, 43, -53, -54};
Plane Surface(16) = {16};
Transfinite Surface {16} Alternated;
Recombine Surface {16};

Line Loop(6) = {38, 39, 57};
Plane Surface(6) = {6};
//Transfinite Surface {6} Alternated;
Recombine Surface {6};


//////////////////////////

Line Loop(18) = {58, 59, 60, 61, 62, -12, -11, -10};
Plane Surface(18) = {18};
//Transfinite Surface {18} Alternated;
Recombine Surface {18};

//////////////////////////

Physical Surface(1000) = {1, 17, 8, 18};        //Half-space
Physical Surface(2000) = {2, 3, 4, 5, 7};       //Incoming
Physical Surface(3000) = {9};                   //Sub-estrato
Physical Surface(4000) = {10, 15};              //Material conteniendo el valle
Physical Surface(5000) = {6};                   //Valle semi-circular
Physical Surface(6000) = {11, 12, 13, 14, 16};  //Incoming-modified

//For horizontal lines
Physical Line(1) = {60};

//For vertical lines
Physical Line(2) = {59, 61};

//Results in the free surface
Physical Line(3) = {62, 1, 2, 3, 27, 35, 36, 37, 38, 39, 40, 41, 42, 31, 7, 8, 9, 58};

//Results for de Modified DRM
Physical Line(4) = {43, 44, 45, 46, 47, 48, 49};

//For incoming elements
Physical Line(5) = {16, 17, 18, 19, 20, 21, 22};

//Mesh.SecondOrderIncomplete = 1;



