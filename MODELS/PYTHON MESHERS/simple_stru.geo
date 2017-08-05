l1 = DefineNumber[ 1.0, Name "Parameters/l1" ];
l2 = DefineNumber[ 0.5, Name "Parameters/l2" ];
h1 = DefineNumber[ 1.0, Name "Parameters/h1" ];
h2 = DefineNumber[ 0.5, Name "Parameters/h2" ];
Point(1) = {-l1, 0, 0, 1.0};
Point(2) = {l1, 0, 0, 1.0};
Point(3) = {l1, h1, 0, 1.0};
Point(4) = {-l1, h1, 0, 1.0};
Point(5) = {-l1-l2, 0, 0, 1.0};
Point(6) = {l1+l2, 0, 0, 1.0};
Point(7) = {l1+l2, h1, 0, 1.0};
Point(8) = {l1+l2, h1+h2, 0, 1.0};
Point(9) = {l1, h1+h2, 0, 1.0};
Point(10) = {-l1, h1+h2, 0, 1.0};
Point(11) = {-l1-l2, h1+h2, 0, 1.0};
Point(12) = {-l1-l2, h1, 0, 1.0};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line(5) = {2, 6};
Line(6) = {6, 7};
Line(7) = {7, 3};
Line(8) = {7, 8};
Line(9) = {8, 9};
Line(10) = {9, 3};
Line(11) = {9, 10};
Line(12) = {10, 4};
Line(13) = {4, 12};
Line(14) = {10, 11};
Line(15) = {11, 12};
Line(16) = {12, 5};
Line(17) = {5, 1};

Line Loop(18) = {1, 2, 3, 4};
Plane Surface(19) = {18};

Line Loop(20) = {5, 6, 7, -2};
Plane Surface(21) = {20};


Line Loop(22) = {17, -4, 13, 16};
Plane Surface(23) = {22};

Line Loop(24) = {-3, -10, 11, 12};
Plane Surface(25) = {24};

Line Loop(26) = {-7, 8 , 9 , 10};
Plane Surface(27) = {26};

Line Loop(28) = {-13 , -12, 14 , 15};
Plane Surface(29) = {28};

Recombine Surface {19};
Recombine Surface {21};
Recombine Surface {23};
Recombine Surface {25};
Recombine Surface {27};
Recombine Surface {29};

Physical Line(100) = {6 , 8 , 9 , 11 , 14 , 15 , 16};
Physical Line(200) = {2 , 3 , 4 };

Physical Surface(10000) = {19};
Physical Surface(20000) = {21 , 23 , 25};
Physical Surface(30000) = {27 , 29};

npoints_horizontal = 5;
npoints_vertical = 3;

Transfinite Line {16, 4, 2, 6} = npoints_vertical Using Progression 1;
Transfinite Line {11, 3, 1} = npoints_horizontal Using Progression 1;
Transfinite Line {17, 13, 14, 15, 12, 10, 9, 7, 8, 5} = 2 Using Progression 1;

Transfinite Surface {23};
Transfinite Surface {29};
Transfinite Surface {25};
Transfinite Surface {27};
Transfinite Surface {21};
Transfinite Surface {19};
