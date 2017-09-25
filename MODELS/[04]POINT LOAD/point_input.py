"""
Creates th WAVES.for mesh for a
square domain under a point load.
@autor Juan Gomez
"""
from __future__ import division
import meshio
import mesh_waves as msw
import numpy as np
import fileinput
import glob
#%%
points, cells, point_data, cell_data , field_data = \
    meshio.read("point.msh")
#
nodes_array = msw.node_writer(points , point_data)
print len(nodes_array)
#%%
# Domain elements.
#
nfin , els1_array  = msw.ele_writer(cells , cell_data , 'quad9' , 30000 , 3 , 1 , 18 , 9 , 0 )
nini = nfin
#
#%%
# Absorbing boundaries
#
nfin , els2_array  = msw.boundary_writer(cells , cell_data , 100 , 2 , nini )
#
#%%
#
np.savetxt("2nodes.txt", nodes_array,
           fmt=("%d", "%d", "%d" , "%d" , "%.4f", "%.4f"))
np.savetxt("5eles.txt", els1_array   , fmt="%d")
np.savetxt("6eles.txt", els2_array   , fmt="%d")
file_list = glob.glob("*.txt")

with open('point.inp', 'w') as file:
    input_lines = fileinput.input(file_list)
    file.writelines(input_lines)