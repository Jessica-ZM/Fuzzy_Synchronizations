# -*- coding: utf-8 -*-
# Programa para graficar desde archivo
# Jessica Zaqueros M

import numpy  as np
import matplotlib.pyplot as plt
from matplotlib import colors as mcolors

# Se leen los datos del archivo data1.txt y se guardan en el vector data
data1 = np.loadtxt('chaosChua88.txt')
data2 = np.loadtxt('chaosChua89.txt')
data3 = np.loadtxt('chaosChua99.txt')
data4 = np.loadtxt('chaosChua100.txt')

# Se guarda las variables de cada columna del archivo
x11 = data1[:, 1]
x12 = data1[:, 2]
x13 = data1[:, 3]
e11 = data1[:, 7]
e12 = data1[:, 8]
e13 = data1[:, 9]
itera = data1[:, 13]

x31 = data3[:, 1]
x32 = data3[:, 2]
x33 = data3[:, 3]

e21 = data2[:, 7]
e22 = data2[:, 8]
e23 = data2[:, 9]

e31 = data3[:, 7]
e32 = data3[:, 8]
e33 = data3[:, 9]

e41 = data4[:, 7]
e42 = data4[:, 8]
e43 = data4[:, 9]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
# codigo para generar las graficas para el articulo

# parameters = {'axes.labelsize': 20,
#           'xtick.labelsize': 16,
#           'ytick.labelsize': 16,
#           'legend.fontsize': 16}
# plt.rcParams.update(parameters)

# #Tres errores del una gamma
# plot=plt.figure()
# plt.xscale('log')
# plt.plot(itera,e31,'b-',label=r'$e_1(t)$')
# plt.plot(itera,e32,'c--',label=r'$e_2(t)$')
# plt.plot(itera,e33,'violet',label=r'$e_3(t)$')
# plt.ylabel('Error')
# plt.xlabel('Iterations')
# plt.legend(loc='best')
# plt.title('$\gamma = 0.99$')
# plt.xlim(1, 19200)
# ymin = min(min(e31),min(e32),min(e33))
# ymax = max(max(e31),max(e32),max(e33))
# plt.ylim(ymin,ymax)
# plt.grid(True)
# plt.show()


# #Error 1 log
# plot=plt.figure()
# plt.xscale('log')
# plt.plot(itera,e11,'m',label=r'$\gamma = 0.88$')
# plt.plot(itera,e21,'pink',label=r'$\gamma = 0.89$')
# plt.plot(itera,e31,'b--',label=r'$\gamma = 0.99$')
# plt.plot(itera,e41,'c--',label=r'$\gamma = 1.00$')
# # plt.ylabel('$e_1$')
# # plt.xlabel('Iterations')
# plt.legend(loc='best')
# plt.xlim(1, 19200)
# ymin = min(min(e11), min(e21), min(e31), min(e41))
# ymax = max(max(e11), max(e21), max(e31), max(e41))
# plt.ylim(ymin,ymax)
# plt.grid(True)
# plt.savefig('Error1ChuaPDC.png')
# plt.show()


# #Error 2 log
# plot=plt.figure()
# plt.xscale('log')
# plt.plot(itera,e12,'m',label=r'$\gamma = 0.88$')
# plt.plot(itera,e22,'pink',label=r'$\gamma = 0.89$')
# plt.plot(itera,e32,'b--',label=r'$\gamma = 0.99$')
# plt.plot(itera,e42,'c--',label=r'$\gamma = 1.00$')
# # plt.ylabel('$e_2$')
# # plt.xlabel('Iterations')
# # plt.legend(loc='best')
# plt.xlim(1, 19200)
# ymin = min(min(e12), min(e22), min(e32), min(e42))
# ymax = max(max(e12), max(e22), max(e32), max(e42))
# plt.ylim(ymin,ymax)
# plt.grid(True)
# plt.savefig('Error2ChuaPDC.png')
# plt.show()

# #Error 2 log
# plot=plt.figure()
# plt.xscale('log')
# plt.plot(itera,e13,'m-',label=r'$\gamma = 0.88$')
# plt.plot(itera,e23,'pink',label=r'$\gamma = 0.89$')
# plt.plot(itera,e33,'b--',label=r'$\gamma = 0.99$')
# plt.plot(itera,e43,'c--',label=r'$\gamma = 1.00$')
# # plt.ylabel('$e_3$')
# # plt.xlabel('Iterations')
# # plt.legend(loc='best')
# plt.xlim(1, 19200)
# ymin = min(min(e13), min(e23), min(e33), min(e43))
# ymax = max(max(e13), max(e23), max(e33), max(e43))
# plt.ylim(ymin,ymax)
# plt.grid(True)
# plt.savefig('Error3ChuaPDC.png')
# plt.show()


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# codigo para calcular el error relativo
# No hay problemas con la division entre cero
# er1 = abs(e11)/x11
# er2 = abs(e12)/x12
# er3 = abs(e13)/x13

er1 = abs(e31)/x31
er2 = abs(e32)/x32
er3 = abs(e33)/x33


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
# # plt.savefig('RelErrorChuaLMI.png')
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
# zoom error 1

parameters = {'axes.labelsize': 24,
          'xtick.labelsize': 20,
          'ytick.labelsize': 20,
          'legend.fontsize': 22}
plt.rcParams.update(parameters)

# version 1a
# win = 800
# l2 = miter - 1141
# l1 = l2 - win

# version 2a
# start1 = int(miter/2)
# win = 400
# l2 = start1 + win
# l1 = start1 - win

# punto medio
# pm = (l2+l1)/2.0

# convertimos a enteros
# l1 = int(l1)
# l2 = int(l2)
# pm = int(pm)

# para articulo
l1 = 280
pm = 680
l2 = 1080

fig, ax = plt.subplots(figsize=[8, 6])

# ax.plot(itera, e1)
ax.plot(itera,e11,'m',label=r'$\gamma = 0.88$')
ax.plot(itera,e21,'pink',label=r'$\gamma = 0.89$')
ax.plot(itera,e31,'b--',label=r'$\gamma = 0.99$')
ax.plot(itera,e41,'c--',label=r'$\gamma = 1.00$')
plt.xlim(0, 19200)
ymin = min(min(e11), min(e21), min(e31), min(e41))
ymax = max(max(e11), max(e21), max(e31), max(e41))
plt.ylim(ymin,ymax)
plt.legend(loc='best')
plt.xlabel('Iterations')
# ax.set_xticks([4000, 8000, 12000, 16000])
# ax.set_xticklabels([4000, 8000, 12000, 16000])
ax.set_xticks([3000, 6000, 9000, 12000, 15000, 18000])
ax.set_xticklabels(['3000', '6000', '9000', '12,000', '15,000', '18,000'], rotation=45)
plt.ylabel('$e_1$')
plt.grid(True)


parameters = {'axes.labelsize': 16,
          'xtick.labelsize': 16,
          'ytick.labelsize': 16,
          'legend.fontsize': 16}
plt.rcParams.update(parameters)

# inset axes....
# axins = ax.inset_axes([0.2, 0.1, 0.45, 0.35])
axins = ax.inset_axes([0.21, 0.2, 0.38, 0.35])
axins.plot(itera[l1:l2],e11[l1:l2], 'm', itera[l1:l2],e21[l1:l2] , 'pink', itera[l1:l2],e31[l1:l2], 'b--', itera[l1:l2],e41[l1:l2], 'c--')
# sub region of the original image
y1min = min(min(e11[l1:l2]), min(e21[l1:l2]), min(e31[l1:l2]), min(e41[l1:l2]))
y2max = max(max(e11[l1:l2]), max(e21[l1:l2]), max(e31[l1:l2]), max(e41[l1:l2]))
x1, x2, y1, y2 = l1, l2, y1min-0.0001, y2max+0.005
axins.set_xlim(x1, x2)
axins.set_ylim(y1, y2)
axins.set_xticks([l1, pm, l2])
axins.set_xticklabels([l1, pm, l2])
# axins.set_xticklabels([])
# axins.set_yticklabels([])
axins.grid(False)

ax.indicate_inset_zoom(axins, edgecolor="black")

# # Save figure with nice margin
plt.savefig('E1zoomChuaPDC.png', dpi = 300, bbox_inches = 'tight', pad_inches = .1)
# plt.show()


# zoom error 2

parameters = {'axes.labelsize': 24,
          'xtick.labelsize': 20,
          'ytick.labelsize': 20,
          'legend.fontsize': 22}
plt.rcParams.update(parameters)


fig, ax = plt.subplots(figsize=[8, 6])

# 
ax.plot(itera,e12,'m',label=r'$\gamma = 0.88$')
ax.plot(itera,e22,'pink',label=r'$\gamma = 0.89$')
ax.plot(itera,e32,'b--',label=r'$\gamma = 0.99$')
ax.plot(itera,e42,'c--',label=r'$\gamma = 1.00$')
plt.xlim(0, 19200)
ymin = min(min(e12), min(e22), min(e32), min(e42))
ymax = max(max(e12), max(e22), max(e32), max(e42))
plt.ylim(ymin,ymax)
# plt.legend(loc='best')
plt.xlabel('Iterations')
# ax.set_xticks([4000, 8000, 12000, 16000])
# ax.set_xticklabels([4000, 8000, 12000, 16000])
ax.set_xticks([3000, 6000, 9000, 12000, 15000, 18000])
ax.set_xticklabels(['3000', '6000', '9000', '12,000', '15,000', '18,000'], rotation=45)
plt.ylabel('$e_2$')
plt.grid(True)


parameters = {'axes.labelsize': 16,
          'xtick.labelsize': 16,
          'ytick.labelsize': 16,
          'legend.fontsize': 16}
plt.rcParams.update(parameters)

# inset axes....
# axins = ax.inset_axes([0.2, 0.1, 0.45, 0.35])
axins = ax.inset_axes([0.25, 0.4, 0.38, 0.35])
axins.plot(itera[l1:l2],e12[l1:l2], 'm', itera[l1:l2],e22[l1:l2] , 'pink', itera[l1:l2],e32[l1:l2], 'b--', itera[l1:l2],e42[l1:l2], 'c--')
# sub region of the original image
y1min = min(min(e12[l1:l2]), min(e22[l1:l2]), min(e32[l1:l2]), min(e42[l1:l2]))
y2max = max(max(e12[l1:l2]), max(e22[l1:l2]), max(e32[l1:l2]), max(e42[l1:l2]))
x1, x2, y1, y2 = l1, l2, y1min-0.0001, y2max+0.005
axins.set_xlim(x1, x2)
axins.set_ylim(y1, y2)
axins.set_xticks([l1, pm, l2])
axins.set_xticklabels([l1, pm, l2])
# axins.set_xticklabels([])
# axins.set_yticklabels([])
axins.grid(False)

ax.indicate_inset_zoom(axins, edgecolor="black")

# # Save figure with nice margin
plt.savefig('E2zoomChuaPDC.png', dpi = 300, bbox_inches = 'tight', pad_inches = .1)
# plt.show()

# zoom error 3

parameters = {'axes.labelsize': 24,
          'xtick.labelsize': 20,
          'ytick.labelsize': 20,
          'legend.fontsize': 22}
plt.rcParams.update(parameters)



fig, ax = plt.subplots(figsize=[8, 6])


ax.plot(itera,e13,'m',label=r'$\gamma = 0.88$')
ax.plot(itera,e23,'pink',label=r'$\gamma = 0.89$')
ax.plot(itera,e33,'b--',label=r'$\gamma = 0.99$')
ax.plot(itera,e43,'c--',label=r'$\gamma = 1.00$')
plt.xlim(0, 19200)
ymin = min(min(e13), min(e23), min(e33), min(e43))
ymax = max(max(e13), max(e23), max(e33), max(e43))
plt.ylim(ymin,ymax+0.03)
# plt.legend(loc='best')
plt.xlabel('Iterations')
# ax.set_xticks([4000, 8000, 12000, 16000])
# ax.set_xticklabels([4000, 8000, 12000, 16000])
ax.set_xticks([3000, 6000, 9000, 12000, 15000, 18000])
ax.set_xticklabels(['3000', '6000', '9000', '12,000', '15,000', '18,000'], rotation=45)
plt.ylabel('$e_3$')
plt.grid(True)


parameters = {'axes.labelsize': 16,
          'xtick.labelsize': 16,
          'ytick.labelsize': 16,
          'legend.fontsize': 16}
plt.rcParams.update(parameters)

# inset axes....
# axins = ax.inset_axes([0.2, 0.1, 0.45, 0.35])
axins = ax.inset_axes([0.25, 0.4, 0.38, 0.35])
axins.plot(itera[l1:l2],e13[l1:l2], 'm', itera[l1:l2],e23[l1:l2] , 'pink', itera[l1:l2],e33[l1:l2], 'b--', itera[l1:l2],e43[l1:l2], 'c--')
# sub region of the original image
y1min = min(min(e13[l1:l2]), min(e23[l1:l2]), min(e33[l1:l2]), min(e43[l1:l2]))
y2max = max(max(e13[l1:l2]), max(e23[l1:l2]), max(e33[l1:l2]), max(e43[l1:l2]))
x1, x2, y1, y2 = l1, l2, y1min-0.0001, y2max+0.02
axins.set_xlim(x1, x2)
axins.set_ylim(y1, y2)
axins.set_xticks([l1, pm, l2])
axins.set_xticklabels([l1, pm, l2])
# axins.set_xticklabels([])
# axins.set_yticklabels([])
axins.grid(False)

ax.indicate_inset_zoom(axins, edgecolor="black")

# # Save figure with nice margin
plt.savefig('E3zoomChuaPDC.png', dpi = 300, bbox_inches = 'tight', pad_inches = .1)
# plt.show()

