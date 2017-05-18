// Inputs
	squareSide = 200; //m
	meshThickness = 100; //m	squareSide / 10; 
	gridsize = 1.0;	//m 	squareSide / 10;

    // Geometry
//	Point(1) = {-squareSide/2, -squareSide/2, -meshThickness/2, gridsize};
//	Point(2) = {squareSide/2, -squareSide/2, -meshThickness/2, gridsize};
//	Point(3) = {squareSide/2, squareSide/2, -meshThickness/2, gridsize};
//	Point(4) = {-squareSide/2, squareSide/2, -meshThickness/2, gridsize};
	
	Point(1) = {-squareSide/2, -squareSide/2, 0.0, gridsize};
	Point(2) = {squareSide/2, -squareSide/2,  0.0, gridsize};
	Point(3) = {squareSide/2, squareSide/2,   0.0, gridsize};
	Point(4) = {-squareSide/2, squareSide/2,  0.0, gridsize};

//	Point(1) = {0, 0, 0, gridsize};
//	Point(2) = {squareSide, 0, 0, gridsize};
//	Point(3) = {squareSide, squareSide, 0, gridsize};
//	Point(4) = {0, squareSide, 0, gridsize};

	Line(1) = {1, 2};				// bottom line
	Line(2) = {2, 3};				// right line
	Line(3) = {3, 4};				// top line
	Line(4) = {4, 1};				// left line
	Line Loop(5) = {1, 2, 3, 4}; 	
	Plane Surface(6) = {5};
 
    //Transfinite surface:
	Transfinite Surface {6};
	Recombine Surface {6};
 
	surfaceVector[] = Extrude {0, 0, meshThickness} {
	 Surface{6};
	 Layers{50};
	 Recombine;
	};

// Layers{#}: # define el número de divisiones en dirección Z

/*	 surfaceVector contains in the following order:
	[0]	- front surface (opposed to source surface)
	[1] - extruded volume
	[2] - bottom surface (belonging to 1st line in "Line Loop (6)")
	[3] - right surface (belonging to 2nd line in "Line Loop (6)")
	[4] - top surface (belonging to 3rd line in "Line Loop (6)")
	[5] - left surface (belonging to 4th line in "Line Loop (6)") */

	Physical Surface("front") = surfaceVector[0];
	Physical Volume("internal") = surfaceVector[1];
	Physical Surface("bottom") = surfaceVector[2];
	Physical Surface("right") = surfaceVector[3];
	Physical Surface("top") = surfaceVector[4];
	Physical Surface("left") = surfaceVector[5];
	Physical Surface("back") = {6};
	



























































