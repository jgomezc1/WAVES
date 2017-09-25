//Note: Everything has been scaled by a factor of 10.0
cl=0.2000;		//Characteristic dimension of the elements.

l = 30.0;		//Total domain length.
h = 15.0;		//Total domain height.

//Points
Point(1 ) = {0  , 0   , 0, cl};
Point(2 ) = {l  , 0   , 0, cl};
Point(3 ) = {l  , h   , 0, cl};
Point(4 ) = {0  , h   , 0, cl};


//Lines
Line(1)  = {1 , 2 };
Line(2)  = {2 , 3 };
Line(3)  = {3 , 4 };
Line(4)  = {4 , 1 };



//Plane Surfaces (Right strip)

Line Loop(1) = {1 , 2 , 3 , 4 };
Plane Surface(1) = {1};
Transfinite Surface {1} Alternated;
Recombine Surface {1};

/*Physical surfaces
The strip elements are assigned physical surface 20000
There are as many material profiles as physical surfaces.
*/

Physical Surface(30000) = {1};

/*Absorbing boundaries are located along the external boundary. These are line
 elements defined from right to left. These elements are tied to the
physical surface 10000.
*/
Physical Line(100) = {2 , 3 , 4};

//Mesh.SecondOrderIncomplete = 1;



