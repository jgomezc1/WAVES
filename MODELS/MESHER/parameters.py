"""
Creates th WAVES.for mesh for a
square domain under a point load.
@autor Juan Gomez
"""
from __future__ import division

TM = 2.0
beta_min  =1000.0
alpha_max =2000.0
f_max = 4.0
h_c = beta_min/10.0/f_max
c_l = 2.0* h_c
dt = c_l/2.0/20.0/alpha_max
nincs = TM/dt
#
print 'DATOS CALCULADOS'
print '------------------'
print
print 'Tamanio caracteristico' , h_c
print 'Parametro de la malla'  , c_l
print 'Delta de tiempo' , dt
print 'Numero de incrementos' , nincs
#
print
print 'DATOS RECALCULADOS'
print '------------------'
print
C_L = 25.0
H_C = C_L/2.0
DT = C_L/2.0/20.0/alpha_max
print 'Delta de tiempo recalculado' , DT , 'para nuevo cl'
print 'Tamanio caracteristico h_c recalculado para nuevo cl' , H_C
NINCS = TM/DT
print 'Numero de incrementos' , NINCS , 'para tiempo maximo' , TM