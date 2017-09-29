"""
Creates th WAVES.for mesh for a
square domain under a point load.
@autor Juan Gomez
"""
from __future__ import division
beta_min  = 300.0
alpha_max = 600.0
f_max = 4.0
h_c = beta_min/10.0/f_max
c_l = 2.0* h_c
dt = c_l/2.0/20.0/alpha_max
print h_c , c_l , dt
#
C_L = 10.0
DT = C_L/2.0/20.0/alpha_max
print DT