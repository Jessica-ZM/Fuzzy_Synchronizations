# -*- coding: utf-8 -*-
# Programa para graficar desde archivo
# Jessica Zaqueros M

import numpy  as np
import matplotlib.pyplot as plt

# Se leen los datos del archivo data1.txt y se guardan en el vector data
# data = np.loadtxt('chaosLE4LMI_SNNN.txt')
data = np.loadtxt('chaosLE4LMI_CNO.txt')
# Se guarda las variables de cada columna del archivo
t = data[:, 0]
x1 = data[:, 1]
x2 = data[:, 2]
x3 = data[:, 3]
y1 = data[:, 4]
y2 = data[:, 5]
y3 = data[:, 6]
e1 = data[:, 7]
e2 = data[:, 8]
e3 = data[:, 9]
u1 = data[:, 10]
u2 = data[:, 11]
u3 = data[:, 12]
itera = data[:, 13]

# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# codigo para generar las graficas para el articulo
parameters = {'axes.labelsize': 28,
          'xtick.labelsize': 20,
          'ytick.labelsize': 20,
          'legend.fontsize': 20}
plt.rcParams.update(parameters)

#Espacio de fase
plot=plt.figure(figsize=[9, 7])
ax = plt.axes(projection='3d')
ax.plot3D(x1,x2,x3,'g-', label=r'master')
ax.plot3D(y1,y2,y3,'m--', label=r'slave' )
plt.legend(loc='best')
#plt.title('Phase space')
ax.set_xlabel('$x_1$', labelpad = 20)
ax.set_ylabel('$x_2$', labelpad = 20)
ax.set_zlabel('$x_3$', labelpad = 20)
ax.view_init(75,290)
# Save figure with nice margin
plt.savefig('PhaseSpaceLE4LMI.png', dpi = 300, bbox_inches = 'tight', pad_inches = .1)
plt.show()

parameters = {'axes.labelsize': 24,
          'xtick.labelsize': 20,
          'ytick.labelsize': 20,
          'legend.fontsize': 24}
plt.rcParams.update(parameters)

# # #Grafica de las variables de estado respecto al tiempo t
# # plot=plt.figure()
# # plt.plot(t,x1,'y-',label=r'$x_1(t)$')
# # plt.plot(t,x2,'c-',label=r'$x_2(t)$')
# # plt.plot(t,x3,'m-',label=r'$x_3(t)$')
# # plt.title('Time series')
# # plt.ylabel('State variables')
# # plt.xlabel('Time')
# # plt.legend(loc='best')
# # plt.xlim(0, 150)
# # ymin = min(min(x1),min(x2),min(x3))
# # ymax = max(max(x1),max(x2),max(x3))
# # plt.ylim(ymin,ymax)
# # plt.grid(True)
# # plt.show()

#Grafica de las variables de estado respecto a las iteraciones
plot=plt.figure()
plt.plot(itera,x1,'g-',label=r'$x_1$')
plt.plot(itera,y1,'m--',label=r'$y_1$')
plt.title('Iterations series')
plt.ylabel('State variables')
plt.xlabel('Iterations')
plt.legend(loc='best')
plt.xlim(0, 19200)
ymin = min(min(x1),min(y1))
ymax = max(max(x1),max(y1))
plt.ylim(ymin,ymax)
plt.grid(True)
plt.show()

plot=plt.figure()
plt.plot(itera,x2,'g-',label=r'$x_2$')
plt.plot(itera,y2,'m--',label=r'$y_2$')
plt.title('Iterations series')
plt.ylabel('State variables')
plt.xlabel('Iterations')
plt.legend(loc='best')
plt.xlim(0, 19200)
ymin = min(min(x2),min(y2))
ymax = max(max(x2),max(y2))
plt.ylim(ymin,ymax)
plt.grid(True)
plt.show()

plot=plt.figure()
plt.plot(itera,x3,'g-',label=r'$x_3$')
plt.plot(itera,y3,'m--',label=r'$y_3$')
plt.title('Iterations series')
plt.ylabel('State variables')
plt.xlabel('Iterations')
plt.legend(loc='best')
plt.xlim(0, 19200)
ymin = min(min(x3),min(y3))
ymax = max(max(x3),max(y3))
plt.ylim(ymin,ymax)
plt.grid(True)
plt.show()


# #Error
# plot=plt.figure()
# plt.plot(itera,e1,'-',label=r'$e_1$')
# plt.plot(itera,e2,':',label=r'$e_2$')
# plt.plot(itera,e3,'y--',label=r'$e_3$')
# # plt.ylabel('Error')
# plt.xlabel('Iterations')
# plt.legend(loc='best')
# plt.xlim(0, 19200)
# ymin = min(min(e1),min(e2),min(e3))
# ymax = max(max(e1),max(e2),max(e3))
# plt.ylim(ymin,ymax)
# plt.grid(True)
# plt.show()

# #Error log
# plot=plt.figure()
# plt.xscale('log')
# plt.plot(itera,e1,'-',label=r'$e_1$')
# plt.plot(itera,e2,':',label=r'$e_2$')
# plt.plot(itera,e3,'y--',label=r'$e_3$')
# # plt.ylabel('Error')
# plt.xlabel('Iterations')
# plt.legend(loc='best')
# plt.xlim(1, 19200)
# ymin = min(min(e1),min(e2),min(e3))
# ymax = max(max(e1),max(e2),max(e3))
# plt.ylim(ymin,ymax)
# plt.grid(True)
# # plt.savefig('ErrorLE4LMI.png')
# plt.show()

#Control
plot=plt.figure(figsize=[8, 6])
plt.plot(itera,u1,'c-',label=r'$u_1$')
plt.plot(itera,u2,'r:',label=r'$u_2$')
plt.plot(itera,u3,'k--',label=r'$u_3$')
plt.ylabel('Control')
plt.xlabel('Iterations')
plt.legend(loc='best')
plt.xlim(0, 19200)
ymin = min(min(u1),min(u2),min(u3))
ymax = max(max(u1),max(u2),max(u3))
plt.ylim(ymin,ymax)
plt.grid(True)
# Save figure with nice margin
plt.savefig('ControlLE4LMI.png', dpi = 300, bbox_inches = 'tight', pad_inches = .1)
plt.show()


# #Control log
# plot=plt.figure()
# plt.xscale('log')
# plt.plot(itera,u1,'c-',label=r'$u_1$')
# plt.plot(itera,u2,'r:',label=r'$u_2$')
# plt.plot(itera,u3,'k--',label=r'$u_3$')
# # plt.ylabel('Control')
# plt.xlabel('Iterations')
# plt.legend(loc='best')
# plt.xlim(1, 19200)
# ymin = min(min(u1),min(u2),min(u3))
# ymax = max(max(u1),max(u2),max(u3))
# plt.ylim(ymin,ymax)
# plt.grid(True)
# plt.savefig('ControlLE4LMI.png')
# plt.show()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# codigo para calcular el error relativo

er1 = abs(e1)/x1
er2 = abs(e2)/x2
# er3 = abs(e3)/x3

# una forma de eevitar la division entre cero
er3 = np.zeros_like(e1)

for i in range(0,19200):
    if x3[i] == 0:
        er3[i] = 0
    else :
        er3[i] = abs(e3[i])/x3[i]
        
# # si el valor de x1, x2 o x3 es cero la division no se puede realizar
# #  esta es otra forma de cal cular el error relativo que evita ese problema
# rx1 = max(x1)-min(x1)
# rx2 = max(x2)-min(x2)
# rx3 = max(x3)-min(x3)
# er1 = abs(e1)/rx1
# er2 = abs(e2)/rx2
# er3 = abs(e3)/rx3

# li = 1
# #Error rel log
# plot=plt.figure()
# plt.xscale('log')
# plt.plot(itera[li:],er1[li:],'c-',label=r'$er_1$')
# plt.plot(itera[li:],er2[li:],'r:',label=r'$er_2$')
# plt.plot(itera[li:],er3[li:],'k--',label=r'$er_3$')
# plt.ylabel('relative error')
# # plt.xlabel('Iterations')
# # plt.legend(loc='best')
# plt.xlim(li, 19200)
# #ymin = min(min(er1),min(er2),min(er3))
# #ymax = max(max(er1),max(er2),max(er3))
# #plt.ylim(ymin,ymax)
# plt.grid(True)
# # plt.savefig('RelErrorLE4LMI.png')
# plt.show()

viter = np.zeros(8000)
cont = 0
# Ciclo para verificar las condiciones
for i in range(0,19200):
    if er1[i] > 0.02 or er2[i]> 0.02 or er3[i] > 0.02:
        cont = cont + 1
        viter[cont]= i
        # print(i)
        
miter = max(viter) 


print(miter)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  # Error zoom

# fig1 = plt.figure()
# ax1 = fig1.add_subplot(111)
# # ax1.plot(itera,e1, '-',itera,e2, ':', itera, e3, '--')
# ax1.plot(itera,e1,'-',label=r'$e_1$')
# ax1.plot(itera,e2,':',label=r'$e_2$')
# ax1.plot(itera,e3,'y--',label=r'$e_3$')
# plt.xlim(0, 19200)
# ymin = min(min(e1),min(e2),min(e3))
# ymax = max(max(e1),max(e2),max(e3))
# plt.ylim(ymin,ymax)
# plt.legend(loc='best')
# plt.xlabel('Iterations')
# plt.ylabel('Error')
# plt.grid(True)

# l1 = 2000
# l2 = 3500
# # ax2 = plt.axes([.5, .5, .3, .3])#, axisbg='y')
# # ax2.plot(itera[l1:l2],e1[l1:l2], '-', itera[l1:l2],e2[l1:l2] , ':', itera[l1:l2],e3[l1:l2], '--')
# # plt.setp(ax2)
# # ax_new = fig1.add_axes([0.3, 0.60, 0.3, 0.25]) # the position of zoom-out plot compare to the ratio of zoom-in plot 
# ax_new = fig1.add_axes([0.2, 0.2, 0.3, 0.25]) 
# ax_new.plot(itera[l1:l2],e1[l1:l2], '-', itera[l1:l2],e2[l1:l2] , ':', itera[l1:l2],e3[l1:l2], '--')
# plt.xlim(l1, l2)
# plt.xticks(visible=False)
# plt.yticks(visible=False)
# plt.grid(True)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# otro zoom del error

parameters = {'axes.labelsize': 24,
          'xtick.labelsize': 20,
          'ytick.labelsize': 20,
          'legend.fontsize': 24}
plt.rcParams.update(parameters)

win = 1500
l2 = miter + 117
l1 = l2 - win

# punto medio
pm = (l2+l1)/2.0

# convertimos a enteros
l1 = int(l1)
l2 = int(l2)
pm = int(pm)
fig, ax = plt.subplots(figsize=[8, 6])

# ax.plot(itera, e1)
ax.plot(itera,e1,'-',label=r'$e_1$')
ax.plot(itera,e2,':',label=r'$e_2$')
ax.plot(itera,e3,'y--',label=r'$e_3$')
plt.xlim(0, 19200)
ymin = min(min(e1),min(e2),min(e3))
ymax = max(max(e1),max(e2),max(e3))
plt.ylim(ymin,ymax)
plt.legend(loc='best')
plt.xlabel('Iterations')
# ax.set_xticks([4000, 8000, 12000, 16000])
# ax.set_xticklabels([4000, 8000, 12000, 16000])
ax.set_xticks([3000, 6000, 9000, 12000, 15000, 18000])
ax.set_xticklabels([3000, 6000, 9000, 12000, 15000, 18000])
plt.ylabel('Error')
plt.grid(True)


parameters = {'axes.labelsize': 16,
          'xtick.labelsize': 16,
          'ytick.labelsize': 16,
          'legend.fontsize': 16}
plt.rcParams.update(parameters)

# inset axes....
# axins = ax.inset_axes([0.2, 0.1, 0.45, 0.35])
axins = ax.inset_axes([0.25, 0.15, 0.45, 0.35])
axins.plot(itera[l1:l2],e1[l1:l2], '-', itera[l1:l2],e2[l1:l2] , ':', itera[l1:l2],e3[l1:l2], '--')
# sub region of the original image
y1min = min(min(e1[l1:l2]),min(e2[l1:l2]),min(e3[l1:l2]))
y2max = max(max(e1[l1:l2]),max(e2[l1:l2]),max(e3[l1:l2]))
x1, x2, y1, y2 = l1, l2, y1min-0.0002, y2max+0.0002
axins.set_xlim(x1, x2)
axins.set_ylim(y1, y2)
axins.set_xticks([l1, pm, l2])
axins.set_xticklabels([l1, pm, l2])
# axins.set_xticklabels([])
# axins.set_yticklabels([])
axins.grid(False)

ax.indicate_inset_zoom(axins, edgecolor="black")

# Save figure with nice margin
plt.savefig('EzoomLE4LMI.png', dpi = 300, bbox_inches = 'tight', pad_inches = .1)
plt.show()

