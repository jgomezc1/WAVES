"""
Using meshio and mesh_waves create the input file
for the model Plantilla.msh
@autor Juan Gomez
"""
from __future__ import division
import meshio
import mesh_waves as msw
import numpy as np
import fileinput
import glob
#
points, cells, point_data, cell_data  , field_data = \
    meshio.read("plantilla.msh")
#
nfin , els1_array  = msw.face_recognition(cells , cell_data , 20000 , 200 , 0)
nini = nfin
nfin , els2_array  = msw.ele_writer(cells , cell_data , 'quad9' , 10000 , 3 , 1 , 18 , 9 , nini )
nini = nfin
nfin , els3_array  = msw.ele_writer(cells , cell_data , 'quad9' , 30000 , 3 , 1 , 18 , 9 , nini )
nini = nfin
nfin , els4_array  = msw.boundary_writer(cells , cell_data , 100 , 3 , nini )
nodes_array = msw.node_writer(points , point_data)

np.savetxt("1nodes.txt", nodes_array,
           fmt=("%d", "%d", "%d" , "%d" , "%.4f", "%.4f"))
np.savetxt("4eles.txt", els1_array   , fmt="%d")
np.savetxt("5eles.txt", els2_array   , fmt="%d")
np.savetxt("6eles.txt", els3_array   , fmt="%d")
np.savetxt("7eles.txt", els4_array   , fmt="%d")


file_list = glob.glob("*.txt")

with open('Plantilla.inp', 'w') as file:
    input_lines = fileinput.input(file_list)
    file.writelines(input_lines)