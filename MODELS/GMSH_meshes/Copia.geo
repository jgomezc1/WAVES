//Cuidado que todo lo tengo amplificado por 10.0
cl1=1.00;		//Dimension elementos en el incoming, NO MODIFICAR

b = 10.0;		//Longitud total del dominio en X
l = 20.0;		//Longitud total del dominio en Y
h = 20.0;		//Altura total del dominio en Z


//Points
Point(1) = { l, 0, 0, cl1};
Point(2) = { l, b, 0, cl1};
Point(3) = { 0, b, 0, cl1};
Point(4) = { 0, 0, 0, cl1};

Point(5) = { l, 0, h, cl1};
Point(6) = { l, b, h, cl1};
Point(7) = { 0, b, h, cl1};
Point(8) = { 0, 0, h, cl1};

//Lines
Line(1)  = {1 , 2 };
Line(2)  = {2 , 3 };
Line(3)  = {3 , 4 };
Line(4)  = {4 , 1 };

Line(5)  = {5 , 6 };
Line(6)  = {6 , 7 };
Line(7)  = {7 , 8 };
Line(8)  = {8 , 5 };

Line(9)  = {1 , 5 };
Line(10) = {2 , 6 };
Line(11) = {3 , 7 };
Line(12) = {4 , 8 };

//Surfaces
Line Loop(1) = {-4, -3, -2, -1};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
//Recombine Surface {1};

Line Loop(2) = {5, 6, 7, 8};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
//Recombine Surface {2};

Line Loop(3) = {1, 10, -5, -9};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
//Recombine Surface {3};


Line Loop(4) = {2, 11, -6, -10};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
//Recombine Surface {4};

Line Loop(5) = {3, 12, -7, -11};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
//Recombine Surface {5};

Line Loop(6) = {4, 9, -8, -12};
Plane Surface(6) = {6};
Transfinite Surface {6} Alternated;

Transfinite Line{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}=21 ;

Recombine Surface {1, 2, 3, 4, 5, 6};

//Transfinite Volume{1} = {1, 2, 3, 4, 5, 6, 7, 8};

















