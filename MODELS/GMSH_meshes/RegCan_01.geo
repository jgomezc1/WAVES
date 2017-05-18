//Cuidado que todo lo tengo amplificado por 10.0

//Tamaño máximo de los elementos de la malla
cl1 =  0.10;
cl2 =  0.15;
cl3 =  0.25;
cl4=   0.10;

//Dimensiones características dominio

l   = 100.40;
h   =  50.00;
hest=  2.00;
hbox=  hest+0.50;
lbox=  2.00;

//Dimensión a restar

lresta= 0.0;


//Ubicaciones puntos

l1 =   0.20;
l2 =   3.00;
l3 =  97.40; 
l4 = 100.20;
l5 =  38.20;
l6 =  38.40;
l7 =  43.20;
l8 =  45.20;
l9 =  50.00;
l10=  50.20;
l11=  2.0/Tan(Pi/3)+l10;
l12=  60.20;
l14=  70.20;
l13=  l14-2.0/Tan(Pi/3); 


h1=  49.80;
h2=  50.00;
h3=  47.00;
h4=   7.30;
h5=   7.50;
h6=  10.0*Tan(Pi/3);

////////////////////
//Puntos

Point(1)  = {0          , 0          , 0, cl1};
Point(2)  = {l1         , 0          , 0, cl1};
Point(3)  = {l2         , 0          , 0, cl2};
Point(4)  = {l2         , hest       , 0, cl2};
Point(5)  = {l2         , h3         , 0, cl3};
Point(6)  = {l3         , h3         , 0, cl3};
Point(7)  = {l3         , hest       , 0, cl2};
Point(8)  = {l3         , 0          , 0, cl2};
Point(9)  = {l4         , 0          , 0, cl1};
Point(10) = {l          , 0          , 0, cl1};
Point(11) = {l          , hest       , 0, cl1};
Point(12) = {l          , h1         , 0, cl1};
Point(13) = {l          , h2         , 0, cl1};
Point(14) = {l4         , h2         , 0, cl1};
Point(15) = {l1         , h2         , 0, cl1};
Point(16) = {0          , h2         , 0, cl1};
Point(17) = {0          , h1         , 0, cl1};
Point(18) = {0          , hest       , 0, cl1};
Point(19) = {l4         , hest       , 0, cl1};
Point(20) = {l4         , h1         , 0, cl1};
Point(21) = {l1         , h1         , 0, cl1};
Point(22) = {l1         , hest       , 0, cl1};


Point(23) = {l5         , 0          , 0, cl1};
Point(24) = {l6         , 0          , 0, cl1};
Point(25) = {l7         , 0          , 0, cl4};
Point(26) = {l7+lbox/2  , 0          , 0, cl4};
Point(27) = {l8         , 0          , 0, cl4};
Point(28) = {l9         , 0          , 0, cl1};
Point(29) = {l10        , 0          , 0, cl1};
Point(30) = {l11        , hest       , 0, cl2};
Point(31) = {l12        , h6         , 0, cl3};
Point(32) = {l13        , hest       , 0, cl2};
Point(33) = {l14        , 0          , 0, cl2};
Point(34) = {l10        , hest       , 0, cl1};
Point(35) = {l10        , h4         , 0, cl1};
Point(36) = {l10        , h5         , 0, cl1};
Point(37) = {l9         , h5         , 0, cl1};
Point(38) = {l6         , h5         , 0, cl1};
Point(39) = {l5         , h5         , 0, cl1};
Point(40) = {l5         , h4         , 0, cl1};
Point(41) = {l5         , hest       , 0, cl1};
Point(42) = {l9         , hest       , 0, cl1};
Point(43) = {l9         , h4         , 0, cl1};
Point(44) = {l6         , h4         , 0, cl1};
Point(45) = {l6         , hest       , 0, cl1};
Point(46) = {l8         , hest       , 0, cl4};
Point(47) = {l8         , hbox       , 0, cl4};
Point(48) = {l7         , hbox       , 0, cl4};
Point(49) = {l7         , hest       , 0, cl4};


////////////////////
//Líneas

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

Line(32) = { 3, 23};
Line(33) = {23, 24};
Line(34) = {24, 25};
Line(35) = {25, 26};
Line(36) = {26, 27};
Line(37) = {27, 28};
Line(38) = {28, 29};
Line(39) = {29, 30};
Line(40) = {30, 31};
Line(41) = {31, 32};
Line(42) = {32, 33};
Line(43) = {33, 8 };
Line(44) = { 7, 32};
Line(45) = {30, 34};
Line(46) = {42, 46};
Line(47) = {49, 45};
Line(48) = {41, 4 };
Line(49) = {27, 46};
Line(50) = {46, 47};
Line(51) = {47, 48};
Line(52) = {48, 49};
Line(53) = {49, 25};
Line(54) = {29, 34};
Line(55) = {34, 35};
Line(56) = {35, 36};
Line(57) = {36, 37};
Line(58) = {37, 38};
Line(59) = {38, 39};
Line(60) = {39, 40};
Line(61) = {40, 41};
Line(62) = {41, 23};
Line(63) = {28, 42};
Line(64) = {42, 43};
Line(65) = {43, 44};
Line(66) = {44, 45};
Line(67) = {45, 24};
Line(68) = {42, 34};
Line(69) = {37, 43};
Line(70) = {43, 35};
Line(71) = {41, 45};
Line(72) = {40, 44};
Line(73) = {44, 38};


////////////////////
//Superficies

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

Line Loop(11) = {-48, -61, -60, -59, -58, -57, -56, -55, -45, 40, 41, -44, -6, -5, -4};
Plane Surface(11) = {11};
//Transfinite Surface {11} Alternated;
Recombine Surface {11};

Line Loop(12) = {32, -62, 48, -3};
Plane Surface(12) = {12};
//Transfinite Surface {12} Alternated;
Recombine Surface {12};

Line Loop(13) = {39, 45, -54};
Plane Surface(13) = {13};
//Transfinite Surface {13} Alternated;
Recombine Surface {13};

Line Loop(14) = {72, 73, 59, 60};
Plane Surface(14) = {14};
Transfinite Surface {14} Alternated;
Recombine Surface {14};

Line Loop(15) = {-65, -69, 58, -73};
Plane Surface(15) = {15};
Transfinite Surface {15} Alternated;
Recombine Surface {15};

Line Loop(16) = {56, 57, 69, 70};
Plane Surface(16) = {16};
Transfinite Surface {16} Alternated;
Recombine Surface {16};

Line Loop(17) = {71, -66, -72, 61};
Plane Surface(17) = {17};
Transfinite Surface {17} Alternated;
Recombine Surface {17};

Line Loop(18) = {-47, -52, -51, -50, -46, 64, 65, 66};
Plane Surface(18) = {18};
//Transfinite Surface {18} Alternated;
Recombine Surface {18};

Line Loop(19) = {55, -70, -64, 68};
Plane Surface(19) = {19};
Transfinite Surface {19} Alternated;
Recombine Surface {19};

Line Loop(20) = {33, -67, -71, 62};
Plane Surface(20) = {20};
Transfinite Surface {20} Alternated;
Recombine Surface {20};

Line Loop(21) = {34, -53, 47, 67};
Plane Surface(21) = {21};
Transfinite Surface {21} Alternated;
Recombine Surface {21};

Line Loop(22) = {35, 36, 49, 50, 51, 52, 53};
Plane Surface(22) = {22};
//Transfinite Surface {22} Alternated;
Recombine Surface {22};

Line Loop(23) = {37, 63, 46, -49};
Plane Surface(23) = {23};
Transfinite Surface {23} Alternated;
Recombine Surface {23};

Line Loop(24) = {38, 54, -68, -63};
Plane Surface(24) = {24};
Transfinite Surface {24} Alternated;
Recombine Surface {24};

Line Loop(25) = {42, 43, -7, 44};
Plane Surface(25) = {25};
//Transfinite Surface {25} Alternated;
Recombine Surface {25};


////////////////////
//Superficies físicas

Physical Surface(1000) = {1, 2, 3, 4, 6};               //Incoming Half
Physical Surface(2000) = {7, 10};                       //Incoming Estrato
Physical Surface(3000) = {5, 11, 18};                   //Half-space
Physical Surface(4000) = {8, 9, 12, 21, 23, 13, 25};    //Estrato
Physical Surface(5000) = {22};                          //Valle
Physical Surface(6000) = {14, 15, 16, 17, 19};          //Incoming Half Modelo Pequeño
Physical Surface(7000) = {20, 24};                      //Incoming Estrato Modelo Pequeño

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
Physical Line(4) = {34, 35, 36, 37};

//Incoming elements in the half space
//Physical Line(5) = {19, 20, 21, 22, 23, 24, 25};

//Incoming elements in strata
//Physical Line(6) = {32, 35};

//Mesh.SecondOrderIncomplete = 1;













































