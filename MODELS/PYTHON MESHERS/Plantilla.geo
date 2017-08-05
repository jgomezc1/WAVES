//Note: Everything has been scaled by a factor of 10.0
cl1=0.100;		//Characteristic dimension of the incoming elements. DO NOT MODIFY.
cl2=0.100;		//Characteristic dimension for the elements along other regions of the domain.

l = 30.0;		//Total domain length.
h = 15.0;		//Total domain height.

l1=0.20;		//Width of the strip. DO NOT MODIFY
l2=3.50;		//Length of the additional auxiliary line used to decrease the number of elements.

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

//Plane Surfaces (Right strip)

Line Loop(1) = {7, 8, -19, -15};
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

// Right strip-lower corner

Line Loop(2) = {9, 10, 18, 19};
Plane Surface(2) = {2};
Transfinite Surface {2} Alternated;
Recombine Surface {2};

//Bottom strip

Line Loop(3) = {-16, -18, 11, -21};
Plane Surface(3) = {3};
Transfinite Surface {3} Alternated;
Recombine Surface {3};

//Left strip-lower corner

Line Loop(4) = {12, 13, 20, 21};
Plane Surface(4) = {4};
Transfinite Surface {4} Alternated;
Recombine Surface {4};

//Left strip

Line Loop(5) = {1, -17, -20, 14};
Plane Surface(5) = {5};
Transfinite Surface {5} Alternated;
Recombine Surface {5};

//Domain enclosed by the strip

Line Loop(6) = {2, 3, 4, 5, 6, 15, 16, 17};
Plane Surface(6) = {6};
//Transfinite Surface {6} Alternated;
Recombine Surface {6};


/*Physical surfaces
The strip elements are assigned physical surface 20000
There are as many material profiles as physical surfaces.
*/

// Incoming elements forming the strip
Physical Surface(20000) = {1, 3 , 5}; 

// Elements sorrounding the strip
Physical Surface(10000) = {2 , 4 };

// Elements inside the problem domain
Physical Surface(30000) = {6};

/*Absorbing boundaries are located along the external boundary. These are line
 elements defined from right to left. These elements are tied to the
physical surface 10000.
*/
Physical Line(100) = {8, 9, 10, 11, 12, 13, 14};

/* The next physical line defines the location of points where
displacement histories are desired.
*/
Physical Line(200) = {15, 16 , 17};


Transfinite Line {1  , 20} = 1 Using Progression 1;
Transfinite Line {7  , 19} = 1 Using Progression 1;
Transfinite Line {21 , 18} = 1 Using Progression 1;

//Mesh.SecondOrderIncomplete = 1;



