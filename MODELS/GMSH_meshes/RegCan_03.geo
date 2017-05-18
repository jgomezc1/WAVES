//Cuidado que todo lo tengo amplificado por 10.0

//Tamaño máximo de los elementos de la malla
cl1 =  0.10;
cl2 =  0.25;

//Dimensiones características dominio

l   = 100.40;
h   =  50.00;
lc  =  10.00;

//Ubicaciones puntos

l1 =   0.20;
l2 =   3.00;
l3 =  97.40; 
l4 = 100.20;
l5 =  30.20;
l6 =  30.40;
l7 =  35.20;
l8 =  40.20;
l9 =  45.20;
l10=  50.00;
l11=  50.20;
l12=  60.20;
l13=  70.20;
l14=  70.20;

h1=  49.80;
h2=  50.00;
h3=  47.00;
h4=   9.80;
h5=  10.00;
h6=  10.0*Tan(Pi/3);

////////////////////
//Puntos

Point(17) = {l5         , 0          , 0, cl1};
Point(18) = {l6         , 0          , 0, cl1};
Point(19) = {l7         , 0          , 0, cl1};
Point(20) = {l8         , 0          , 0, cl1};
Point(21) = {l9         , 0          , 0, cl1};
Point(22) = {l10        , 0          , 0, cl1};
Point(23) = {l11        , 0          , 0, cl1};
Point(26) = {l5         , h4         , 0, cl1};
Point(27) = {l6         , h4         , 0, cl1};
Point(28) = {l10        , h4         , 0, cl1};
Point(29) = {l11        , h4         , 0, cl1};
Point(30) = {l11        , h5         , 0, cl1};
Point(31) = {l10        , h5         , 0, cl1};
Point(32) = {l6         , h5         , 0, cl1};
Point(33) = {l5         , h5         , 0, cl1};

////////////////////
//Líneas

Line(23) = {17, 18};
Line(24) = {18, 19};
Line(25) = {21, 22};
Line(26) = {22, 23};

Line(30) = {23, 29};
Line(31) = {29, 30};
Line(32) = {30, 31};
Line(33) = {31, 32};
Line(34) = {32, 33};
Line(35) = {33, 26};
Line(36) = {26, 17};
Line(37) = {22, 28};
Line(38) = {28, 27};
Line(39) = {27, 18};

Circle(40) = {21, 20, 19};

Line(41) = {31, 28};
Line(42) = {28, 29};
Line(43) = {26, 27};
Line(44) = {27, 32};

//Line(45) = {30, 34};
//Line(46) = {42, 46};


////////////////////
//Superficies

Line Loop(7) = {34, 35, 43, 44};
Plane Surface(7) = {7};
Transfinite Surface {7} Alternated;
Recombine Surface {7};

Line Loop(8) = {33, -44, -38, -41};
Plane Surface(8) = {8};
Transfinite Surface {8} Alternated;
Recombine Surface {8};

Line Loop(9) = {31, 32, 41, 42};
Plane Surface(9) = {9};
Transfinite Surface {9} Alternated;
Recombine Surface {9};

Line Loop(10) = {23, -39, -43, 36};
Plane Surface(10) = {10};
Transfinite Surface {10} Alternated;
Recombine Surface {10};

Line Loop(11) = {24, -40, 25, 37, 38, 39};
Plane Surface(11) = {11};
//Transfinite Surface {11} Alternated;
Recombine Surface {11};

Line Loop(12) = {26, 30, -42, -37};
Plane Surface(12) = {12};
Transfinite Surface {12} Alternated;
Recombine Surface {12};

//Line Loop(14) = {45, 46, 40};
//Plane Surface(14) = {14};
//Transfinite Surface {14} Alternated;
//Recombine Surface {14};

////////////////////
//Superficies físicas

Physical Surface(1000) = {7, 8, 9, 10, 12};             //Incoming Half Modelo Pequeño
Physical Surface(2000) = {11};                          //Half-space
//Physical Surface(3000) = {14};                         //Valle

////////////////////////////
//Results in the free surface
Physical Line(4) = {24, 40, 25};

//Mesh.SecondOrderIncomplete = 1;

////////////////////////////
//Fronteras absorbentes

//For horizontal lines
Physical Line(1) = {32, 33, 34};

//For vertical lines
Physical Line(2) = {30, 31, 35, 36};













































