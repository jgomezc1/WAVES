//Cuidado que todo lo tengo amplificado por 10.0
cl1=0.20;		//Dimension elementos en el incoming, NO MODIFICAR
cl2=0.20;		//Dimension de otros elementos

l = 30.0;		//Longitud total del dominio
h = 15.0;		//Altura total del dominio

l1=0.40;		//Ancho de la franja en la cual defino el strip, NO MODIFICAR

//Points
Point(1 ) = {-l/2   , 0   , 0, cl1};
Point(2 ) = {-l/2+l1  , 0   , 0, cl1};
Point(3 ) = {l/2-l1  , 0   , 0, cl1};
Point(4 ) = {l/2  , 0, 0, cl1};
Point(5 ) = {l/2, h-l1, 0, cl1};
Point(6 ) = {l/2, h   , 0, cl1};
Point(7 ) = {l/2-l1, h   , 0, cl1};


Point(8 ) = {-l/2+l1   , h, 0, cl1};
Point(9 ) = {-l/2   , h, 0, cl1};
Point(10) = {-l/2   , h-l1   , 0, cl1};

Point(11) = {l/2-l1, h-l1   , 0, cl1};


Point(12) = {-l/2+l1, h-l1   , 0, cl1};

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
Line(10) = {10, 1};
Line(11) = {3, 11};
Line(12) = {11, 12};
Line(13) = {12, 2};
Line(14) = {7, 11 };
Line(15) = {11, 5};
Line(16) = {10, 12};
Line(17) = {12, 8};

//Surfaces
Line Loop(1) = {2, 11, 12, 13};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

Line Loop(2) = {8, 9, 16, 17};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

Line Loop(3) = {-12, -14, 7, -17};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};


Line Loop(4) = {5, 6, 14, 15};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

Line Loop(5) = {1, -13, -16, 10};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
Recombine Surface {5};

Line Loop(6) = {3, 4, -15, -11};
Plane Surface(6) = {6};
Transfinite Surface {6} Alternated;
Recombine Surface {6};







//Physical surfaces
//A los elementos del strip se les asigna la superficie física 10000
//De ahí en adelante, se va creciendo cada mil.
//Por ejemplo, otra superficie, se llamará 11000, la otra 12000
//En el archivo de entrada, se colocan tantos materiales como
//superficies físicas se tengan en este archivo

Physical Surface(10000) = {2, 3, 4, 5, 6}; //Elementos incoming, strip

Physical Surface(11000) = {1};

//Absorbing boundaries
//Estos elementos tipo lineas, van por la parte externa del dominio
//y ojo que van de derecha a izquierda.
//OJO, que estos elementos van amarrados a la superficie física 10000, para poder sacar
//solamente los elementos que son incoming
Physical Line(1) = {4, 5, 6, 7, 8, 9, 10};

//Results to be printed out
//Elementos línea en los cuales se va a escribir la respuesta, el programa de mallado
//se encarga de tomar los nodos que están en estas lineas y eliminar los nodos
//que se encuentran repetidos.
Physical Line(2) = {2};

//Mesh.SecondOrderIncomplete = 1;



