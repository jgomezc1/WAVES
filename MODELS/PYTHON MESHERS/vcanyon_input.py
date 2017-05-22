"""
Use meshio to read gmsh created meshes and generate input files
for dynamic analysis with the explicit FEM code WAVES.
INPUT SCRIPT FOR V-SHAPED CANYON
@autor Juan Gomez
"""
from __future__ import division
import meshio
import numpy as np
import fileinput
import glob

points, cells, point_data, cell_data , field_data = \
    meshio.read("vcanyon.msh")
#
# Nodal Data
#
npoints = points.shape[0]
nodes_array = np.zeros((npoints , 6))     # Dimensions the nodal points array of size npoints x 6
nodes_array[: , 0] = range(1, npoints+1)  # Assigns nodal IDs starting with 1
nodes_array[:, 4:6] = points[:, :2]       # Writes down two fields (starting in column 2 in th .msh file) in columns 4 and 5.
nodes_array[nodes_array[:, 1]==0, 1] = 2  # Rites down the number of degrees of freedom per node.
#
# Half-space and strip elements
# Quad9
#
elements1 = cells["quad9"] + 1            # Writes down element connevctivities (adds 1 to fortranize)
nquads = elements1.shape[0]
els1_array = np.zeros([nquads, 14], dtype=int)
els1_array[:, 0] = range(1, nquads + 1)
els1_array[: , 1] = 3                     # Assigns the element type according to WAVES (3 for quad9)
els1_array[: , 2] = 18                    # Assigns the number of d.o.f for the element
mater1 = cell_data['quad9']['physical']   # Captures the identifirs for the physical surfaces associatted to the elements
                                          # to be used in the specification of material profiles (10000 for strip; 11000 for half-space)
mater_id1 = {10000: 1, 11000: 2}          # Dictionary storing material profiles (1 for strip; 2 for half-space)
els1_array[:, 3] = [mater_id1[mat] for mat in mater1]
els1_array[:, 4]   = 9                                              # Number of nodes for the element
els1_array[:, 5::] = elements1                                      # Writes down the element connectivities.
#
# Absorbing boundaries
# Line3 
#
elements2 = cells["line3"] + 1
line_id = cell_data["line3"]["physical"]
mater_id2 = {100: 2 , 200: 1}          # Dictionary storing material profiles (1 for strip; 2 for half-space)
abs_id = [cont for cont, _ in enumerate(line_id[:]) if line_id[cont] == 100]
els2_array = np.zeros([len(abs_id), 8], dtype=int)
els2_array[: , 0] = range(nquads + 1, nquads + len(abs_id) + 1)
els2_array[: , 1] = 7                                               # Assigns the element type according to WAVES (7 for Lysmer boundary)
els2_array[: , 2] = 6                                               # Assigns the number of d.o.f for the element            
els2_array[: , 3] = [mater_id2[mat] for mat in line_id if mat == 100]
els2_array[: , 4] = 3                                               # Number of nodes for the element
els2_array[:, 5::] = elements2[abs_id, :]                           # Writes down the element connectivities.
#
results_id = [cont for cont, _ in enumerate(line_id[:]) if line_id[cont] == 200]
results_nodes = elements2[results_id, :]
results_nodes = results_nodes.flatten()
results_nodes = list(set(results_nodes))
#
neles = nquads + len(abs_id)
#
print "PARAMETERS FOR HEADING BLOCK"
print npoints , neles , 2 , 1.0 , 2001 , 2 , 9 , 18 , 5 , 0 , 0 , 0
print 20 , 1  
np.savetxt("1nodes.txt", nodes_array,
           fmt=("%d", "%d", "%d" , "%d" , "%.4f", "%.4f"))
np.savetxt("4eles1.txt", els1_array   , fmt="%d")
np.savetxt("5eles2.txt", els2_array   , fmt="%d")
np.savetxt("6nres.txt" , results_nodes, fmt="%d")
#
#
file_list = glob.glob("*.txt")

with open('vcanyon.inp', 'w') as file:
    input_lines = fileinput.input(file_list)
    file.writelines(input_lines)