"""
Use meshio to read gmsh created meshes and generate the input file
*.inp to conduct dynamic analysis with the explicit FEM code WAVES.for
@authors: Juan Gomez
          Juan Vergara
"""
from __future__ import division
import meshio
import numpy as np
import fileinput
import glob

points, cells, point_data, cell_data , field_data = \
    meshio.read("ModeloSimple.msh")
#
# Nodal Data
#
npoints = points.shape[0]
nodes_array = np.zeros((npoints , 6))     # Dimensiona el arreglo de npoints x 6
nodes_array[: , 0] = range(1, npoints+1)  # Asigna identificadores de nodos comenzando en 1
nodes_array[:, 4:6] = points[:, :2]       # Coloca los 2 datos a partir de la columna 2 en .msh en las pocisiones 4 y 5 de nodes
nodes_array[nodes_array[:, 1]==0, 1] = 2  # Coloca el numero de g.d.l en la columna 1 de nodes
#
# Element data
# Cuadrilateros de 9 nodos
#
elements1 = cells["quad9"] + 1            # Coloca las conectividades pero adiciona 1 para Fortranizar
nquads = elements1.shape[0]
els1_array = np.zeros([nquads, 14], dtype=int)
els1_array[:, 0] = range(1, nquads + 1)
els1_array[: , 1] = 3                     # Coloca el tipo de elemento segun DAMIAN (3 para quad9)
els1_array[: , 2] = 18                    # Coloca el numero de grados de libertad para el elemento
mater = cell_data['quad9']['physical']
mater_id = {1000: 1, 2000: 2}                                       # Diccionario con perfiles de materiales
els1_array[:, 3] = [mater_id[mat] for mat in mater]
els1_array[:, 4]   = 9                                              # Escribe el numero de nudos del elemento
els1_array[:, 5::] = elements1                                      # Escribe las conectividades del elemento

# Elementos linea de 3 nodos (Fronteras abosrbentes)
#
elements2 = cells["line3"] + 1
nline3 = elements2.shape[0]
els2_array = np.zeros([nline3, 8], dtype=int)
els2_array[:, 0] = range(nquads + 1, nquads + nline3 + 1)
els2_array[: , 1] = 7                                               # Coloca el tipo de elemento segun DAMIAN (7 para frontera de Lysmr)
els2_array[: , 2] = 6                                               # Coloca el numero de grados de libertad para el elemento
mater = cell_data['line3']['physical']
mater_id = {1: 1, 2: 2}                                             # Diccionario con perfiles de materiales
els2_array[:, 3] = [mater_id[mat] for mat in mater]
els2_array[: , 4] = 3
els2_array[:, 5::] = elements2
#
np.savetxt("1nodes.txt", nodes_array,
           fmt=("%d", "%d", "%d" , "%d" , "%.4f", "%.4f"))
np.savetxt("4eles1.txt", els1_array, fmt="%d")
np.savetxt("5eles2.txt", els2_array, fmt="%d")


file_list = glob.glob("*.txt")

with open('malla_DAMIAN.inp', 'w') as file:
    input_lines = fileinput.input(file_list)
    file.writelines(input_lines)