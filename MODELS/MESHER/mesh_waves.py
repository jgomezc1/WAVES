"""
Using meshio:
    Mesh writing subroutines to create
    input files for the finite element
    code WAVES.
@autor Juan Gomez
"""
from __future__ import division
import numpy as np

def face_recognition(cells , cell_data , phy_sur , phy_lin , nini):
    """
    For the incoming elements required to create a plane wave
    this function extracts the set according to the physical 
    surface and identifies the element face in contact with
    th scatterer.
    INPUT PARAMTERS:
    ---------------
        cell and cell_data: Are the dictionaries creatd by meshio
        phy_sur: Integer defining the physical surface for the strip.
        phy_lin: Integer defining the physical line for the strip-scatterer interface
        nini   : Integer defining the element id for the first element in the set.
    OUTPUT PARAMTERS:
    ----------------
        nf        : Integer defining the element id for the last element in the set
        els1_array: Integer array with the elemental data according to WAVES.
    """
    
    elements1 = cells["quad9"] + 1 
    phy_surface = cell_data["quad9"]['physical']
    strip_id = [cont for cont, _ in enumerate(phy_surface[:]) if phy_surface[cont] == phy_sur]
    els1_array = np.zeros([len(strip_id) , 14], dtype=int)
    elcon      = np.zeros([len(strip_id) , 9 ], dtype=int)
    els1_array[: , 0] = range(1 + nini , len(strip_id) + 1 + nini)
#
    els1_array[: , 3] = 5
    els1_array[: , 1] = 3
#
    els1_array[: , 2] = 18
    els1_array[: , 4] = 9
    els1_array[: , 5::] = elements1[strip_id, :]
    elcon[:] = elements1[strip_id , :]
    #
    # Identify the lines along the scatterer
    #
    line_inco = cells["line3"] + 1
    phy_lines = cell_data['line3']['physical']
    sca_id = [cont for cont, _ in enumerate(phy_lines[:]) if phy_lines[cont] == phy_lin]
    els2_array = np.zeros([len(sca_id), 3], dtype=int)
    els2_array =  line_inco[sca_id, :]
    #
    # Extracts the element faces lying along the scatterer
    #
    nlines = len(sca_id)
    for i in range(nlines):
        id_n = els2_array[i , 2]
        ir , ic = np.where(elcon == id_n)
        els1_array[ir , 3] = ic-3
        els1_array[ir , 1] = 5
    
    nf = nini + len(strip_id)
    return nf , els1_array 

def ele_writer(cells , cell_data , ele_tag , phy_sur , ele_type , mat_tag , ndof , nnode , nini):
    """
    Extracts a subset of elements from a complete mesh according to the physical surface
    phy_sur and writes down the proper fields into an elements array.
    INPUT PARAMTERS:
    ---------------
        cell and cell_data: Are the dictionaries creatd by meshio.
        ele_tag : String defining the element type according to meshio (e.g., quad9 , line3, etc).
        phy_sur : Integer defining the physical surface for the subset.
        ele_type: Integer defining the element type according to WAVES.
        mat_tag : Integer defining the material profile for the subset.
        ndof    : Integer defining the number of degrees of freedom for the elements.
        nnode   : Integer defining the number of nodes for the element.
        nini   : Integer defining the element id for the first element in the set.
    OUTPUT PARAMTERS:
    ----------------
        nf        : Integer defining the element id for the last element in the set
        els_array : Integer array with the elemental data according to WAVES.
    """
    
    elements = cells[ele_tag]+1           # Element connectivities (adds 1 to fortranize)
    phy_surface = cell_data[ele_tag]['physical']
    ele_id = [cont for cont, _ in enumerate(phy_surface[:]) if phy_surface[cont] == phy_sur]    
    els_array = np.zeros([len(ele_id) , 5 + nnode], dtype=int)
    els_array[: , 0] = range(1 + nini , len(ele_id) + 1 + nini )
    els_array[: , 1] = ele_type
    els_array[: , 2] = ndof
    els_array[: , 3] = mat_tag
    els_array[: , 4] = nnode
    els_array[: , 5::] = elements[ele_id, :]
    nf = nini + len(ele_id)
    
    return nf , els_array

def boundary_writer(cells , cell_data , phy_lin , mat_tag , nini ):
    """
    Extracts a subset of line elements from a complete mesh and corresponding
    to Lysmer absorbing  boundaries. The elements are identified according to
    the physical line phy_lin.
    INPUT PARAMTERS:
    ---------------
        cell and cell_data: Are the dictionaries created by meshio.
        ele_tag : String defining the element type according to meshio (e.g., quad9 , line3, etc).
        phy_lin: Integer defining thw physical line for the strp-scatterer interface
        mat_tag : Integer defining the material profile for the subset.
        nini   : Integer defining the element id for the first element in the set.
    OUTPUT PARAMTERS:
    ----------------
        nf        : Integer defining the element id for the last element in the set
        els_array : Integer array with the elemental data according to WAVES.
    """
    
    elements = cells["line3"] + 1
    line_id   = cell_data["line3"]["physical"]
    abs_id = [cont for cont, _ in enumerate(line_id[:]) if line_id[cont] == phy_lin]
    els_array = np.zeros([len(abs_id), 8], dtype=int)
    els_array[: , 0] = range(1 + nini , len(abs_id) + 1 + nini)
    els_array[: , 1] = 7                                               
    els_array[: , 2] = 6                                                           
    els_array[: , 3] = mat_tag
    els_array[: , 4] = 3
    els_array[: , 5] = elements[abs_id, 1]
    els_array[: , 6] = elements[abs_id, 2]
    els_array[: , 7] = elements[abs_id, 0]
    nf = nini + len(abs_id)                           
    
    return nf , els_array


def node_writer(points , point_data):
    """
    Writes down the nodal data as required by WAVES.
        INPUT PARAMTERS:
    ---------------
        points and point_data: Are the dictionaries creatd by meshio.
    OUTPUT PARAMTERS:
    ----------------
        nodes_array : Integer array with the nodal data according to WAVES.
    """
    
    npoints = points.shape[0]
    nodes_array = np.zeros((npoints , 6))     
    nodes_array[: , 0] = range(1, npoints+1)  
    nodes_array[:, 4:6] = points[:, :2]/10.0      
    nodes_array[nodes_array[:, 1]==0, 1] = 2
    
    return nodes_array


