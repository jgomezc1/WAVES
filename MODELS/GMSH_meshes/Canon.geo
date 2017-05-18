//Cuidado que todo lo tengo amplificado por 10.0
cl1=0.100;		//Dimension elementos en el incoming, NO MODIFICAR
cl2=0.200;		//Dimension de otros elementos

l = 20.0;		//Longitud total del dominio
h = 10.0;		//Altura total del dominio

l1=0.20;		//Ancho de la franja en la cual defino el strip, NO MODIFICAR
l2=0.70;		//Longitud donde pongo la otra linea para disminuir cantidad de elementos

//Points
Point(1 ) = {0   , 0   , 0, cl1};
Point(2 ) = {l1  , 0   , 0, cl1};

Point(3 ) = {l2  , 0   , 0, cl2};
Point(4 ) = {l2  , h-l2, 0, cl2};
Point(5 ) = {l-l2, h-l2, 0, cl2};
Point(6 ) = {l-l2, 0   , 0, cl2};

Point(7 ) = {l-l1, 0   , 0, cl1};
Point(8 ) = {l   , 0   , 0, cl1};
Point(9 ) = {l   , h-l1, 0, cl1};
Point(10) = {l   , h   , 0, cl1};
Point(11) = {l-l1, h   , 0, cl1};
Point(12) = {l1  , h   , 0, cl1};
Point(13) = {0   , h   , 0, cl1};
Point(14) = {0   , h-l1, 0, cl1};
Point(15) = {l-l1, h-l1, 0, cl1};
Point(16) = {l1  , h-l1, 0, cl1};

//Lines
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
Line(15) = {7 , 15};
Line(16) = {15, 16};
Line(17) = {16, 2 };
Line(18) = {11, 15};
Line(19) = {15,  9};
Line(20) = {14, 16};
Line(21) = {16, 12};

//Surfaces
Line Loop(1) = {7, 8, -19, -15};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(2) = {9, 10, 18, 19};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {-16, -18, 11, -21};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};


Line Loop(4) = {12, 13, 20, 21};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {1, -17, -20, 14};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
Recombine Surface {5};

Line Loop(6) = {2, 3, 4, 5, 6, 15, 16, 17};
Plane Surface(6) = {6};
//Transfinite Surface {6} Alternated;
Recombine Surface {6};


//Physical surfaces
//A los elementos del strip se les asigna la superficie física 10000
//De ahí en adelante, se va creciendo cada mil.
//Por ejemplo, otra superficie, se llamará 11000, la otra 12000
//En el archivo de entrada, se colocan tantos materiales como
//superficies físicas se tengan en este archivo

Physical Surface(10000) = {1, 2, 3, 4, 5}; //Elementos incoming, strip

Physical Surface(11000) = {6};

//Absorbing boundaries
//Estos elementos tipo lineas, van por la parte externa del dominio
//y ojo que van de derecha a izquierda.
//OJO, que estos elementos van amarrados a la superficie física 10000, para poder sacar
//solamente los elementos que son incoming

Physical Line(1) = {8, 9, 10, 11, 12, 13, 14};


/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////

//Acá voy a adicionar la información del cañón, o lo que sea que necesite

bc= 2.0;		//Ancho del cañón
hc= 1.0;		//Altura del cañón


//Points
Point(17) = {l/2-bc/2, 0  , 0, cl2};
Point(18) = {l/2     , hc , 0, cl2};
Point(19) = {l/2+bc/2, 0  , 0, cl2};

//Lines
Line(22)  = {3 , 17};
Line(23)  = {17, 18};
Line(24)  = {18, 19};
Line(25)  = {19, 6 };

//Surfaces
Line Loop(7) = {22, 23, 24, 25, -5, -4, -3 };
Plane Surface(7) = {7};
Recombine Surface {7};

//Physical surfaces
Physical Surface(12000) = {7};

//Physical lines
//Results to be printed out
//Elementos línea en los cuales se va a escribir la respuesta, el programa de mallado
//se encarga de tomar los nodos que están en estas lineas y eliminar los nodos
//que se encuentran repetidos.

Physical Line(2) = {22, 23, 24, 25, 2, 6};



















