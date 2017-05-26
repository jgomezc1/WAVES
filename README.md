# WAVES: 2D and 3D Explicit Finite Element Analysis for Wave Propagation Problems in Semi-infinite Media.

This REPO contains a fortran code to conduct wave propagation analysis in semi-inifinite elastic media using
an explict finite element code. The program can be used to conduct either plane wave propagation analysis using the
domain-reduction-method from Bilak et al (2000) or to determine the response under generalized dynamic loads.
For problems involving semi-inifinite domains the code uses traditional Lysmer-Khulemeyer absorbing boundaries.
The finite element models are defined in terms of text files which can be created using the third party open
mesher GMSH. The gmsh-created-files can be easily processed with simple python scripts using meshio.py.

## Authors
- [Juan David Gómez Cataño](http://www.eafit.edu.co/docentes-investigadores/Paginas/juan-gomez.aspx),
    Professor at Universidad EAFIT.
- [Juan Carlos Vergara Gallego](https://github.com/jvergar2), PhD Student at
    Universidad EAFIT.
    
 ## Instructions
The code is written in fortran; moststly in fortran 77 syntax.
To use it clone the repo with

    git clone https://github.com/jgomezc1/WAVES.git
   
uncompress the zip folder and compile the files using the provided make file.

To conduct an analysis the input file and the executable file must reside on the same folder.

## License
This project is licensed under the
[MIT license](http://en.wikipedia.org/wiki/MIT_License). The documents are
licensed under
[Creative Commons Attribution License](http://creativecommons.org/licenses/by/4.0/)


