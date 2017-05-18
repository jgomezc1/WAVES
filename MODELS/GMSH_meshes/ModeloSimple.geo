//Cuidado que todo lo tengo amplificado por 10.0
cl1 =  0.10;
cl2 =  0.10;
l   = 1.00;
h1  = 0.250;
h2  = 0.50;
h3  = 1.00;

l1=  l/2;

////////////////////

Point(1)  = {0  , 0   , 0, cl1};
Point(2)  = {l1 , 0   , 0, cl1};
Point(3)  = {l  , 0   , 0, cl1};
Point(4)  = {l  , h1  , 0, cl1};
Point(5)  = {l  , h2  , 0, cl1};
Point(6)  = {l  , h3  , 0, cl2};
Point(7)  = {l1 , h3  , 0, cl2};
Point(8)  = {0  , h3  , 0, cl2};
Point(9)  = {0  , h2  , 0, cl1};
Point(10) = {0  , h1  , 0, cl1};
Point(11) = {l1 , h1  , 0, cl1};
Point(12) = {l1 , h2  , 0, cl1};

Line(1)  = {1 , 2 };
Line(2)  = {2 , 3 };
Line(3)  = {3 , 4 };
Line(4)  = {4 , 5 };
Line(5)  = {5 , 6 };
Line(6)  = {6 , 7 };
Line(7)  = {7 , 8 };
Line(8)  = {8 , 9 };
Line(9)  = {9 , 10};
Line(10) = {10,  1};
Line(11) = { 4, 11};
Line(12) = {11,  2};
Line(13) = {11, 10};
Line(14) = { 5, 12};
Line(15) = {12, 11};
Line(16) = {12,  9};
Line(17) = {12,  7};


Line Loop(1) = {1, -12, 13, 10};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(2) = {2, 3, 11, 12};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {-11, 4, 14, 15};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};

Line Loop(4) = {5, 6, -17, -14};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {17, 7, 8, -16};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
Recombine Surface {5};

Line Loop(6) = {-13, -15, 16, 9};
Plane Surface(6) = {6};
Transfinite Surface {6} Alternated;
Recombine Surface {6};

//////////////////////////

Physical Surface(1000) = {1, 2, 3, 6};             //Estrato
Physical Surface(2000) = {4, 5};                   //Roca


////////////////////////////

//Fronteras absorbentes

//Estrato
Physical Line(1) = {3, 4, 9, 10};

//Roca
Physical Line(2) = {5, 6, 7, 8};

////////////////////////////

//Resulten superficie libre
//Physical Line(7000) = {1, 2};


//Mesh.SecondOrderIncomplete = 1;























