# -*- coding: utf-8 -*-
"""
Script para crear las graficas de cajas con eje cortado
Created on Thu Oct 24, 2022

@author: jessi
"""
# Programa para graficar desde archivo
# Jessica Zaqueros M

import numpy  as np
import matplotlib.pyplot as plt

a = np.zeros( (30, 6) )

# Se leen los datos del archivo data1.txt y se guardan en el vector data
data1 = np.loadtxt('EvaluationsCompleteChua30IC.txt')
data2 = np.loadtxt('EvaluationsProjectiveChua30.txt')

# Se guarda las variables de cada columna del archivo
comp = data1[:, 1]
alfa = data2[:, 1]


# variables para los limites de los datos con valores grandes
z1 = min(comp)
z2 = max(comp)

alfa05 = alfa[0:30]
# print(alfa05)

alfa1 = alfa[30:60]    
# print(alfa1)

alfa2 = alfa[60:90]
# print(alfa2)

alfa3 = alfa[90:120]    
# print(alfa3)

alfa4 = alfa[120:150]
# print(alfa4)

alfa8 = alfa[150:180]    
# print(alfa8)


# variables para los limites de los datos con valores pequenos
a1 = min(min(alfa05), min(alfa1), min(alfa2), min(alfa3), min(alfa4), min(alfa8))

a2 = max(max(alfa05), max(alfa1), max(alfa2), max(alfa3), max(alfa4), max(alfa8))

iter0 = [comp, alfa05, alfa1, alfa2, alfa3, alfa4, alfa8]

# para modificar el tamano de letra en las graficas
parameters = {'axes.labelsize': 22,
          'xtick.labelsize': 18,
          'ytick.labelsize': 18,
          'legend.fontsize': 16}
plt.rcParams.update(parameters)


# codigo para obtener los ejes rotos (separados)
fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True, figsize=(16, 10))
fig.subplots_adjust(hspace=0.05)  # adjust space between axes


# plot the same data on both axes
ax1.boxplot(iter0,notch ='True')
ax2.boxplot(iter0,notch ='True')


# zoom-in / limit the view to different portions of the data
ax1.set_ylim(z1-200, z2+200)  # outliers only
ax1.set_yticks([12000, 14000, 16000, 18000])
ax1.set_yticklabels(['12,000', '14,000', '16,000', '18,000'])
ax2.set_ylim(a1-50, a2+50)  # most of the data

# hide the spines between ax and ax2
ax1.spines.bottom.set_visible(False)
ax2.spines.top.set_visible(False)
ax1.xaxis.tick_top()
ax1.tick_params(labeltop=False)  # don't put tick labels at the top
ax2.xaxis.tick_bottom()



# Now, let's turn towards the cut-out slanted lines.
# We create line objects in axes coordinates, in which (0,0), (0,1),
# (1,0), and (1,1) are the four corners of the axes.
# The slanted lines themselves are markers at those locations, such that the
# lines keep their angle and position, independent of the axes size or scale
# Finally, we need to disable clipping.

d = .5  # proportion of vertical to horizontal extent of the slanted line
kwargs = dict(marker=[(-1, -d), (1, d)], markersize=12,
              linestyle="none", color='k', mec='k', mew=1, clip_on=False)
ax1.plot([0, 1], [0, 0], transform=ax1.transAxes, **kwargs)
ax2.plot([0, 1], [1, 1], transform=ax2.transAxes, **kwargs)


ax1.grid(True)
ax2.grid(True)
#ax1 = plt.gca()
ax1.set_xticklabels(['', '', '', '', '', '', '', 'Complete', 'alpha = 0.5',
                    'alpha = 1.0', 'alpha = 2.0',
                    'alpha = 3.0', 'alpha = 4.0',
                    'alpha = 8.0'])

ax1.set_ylabel('Iterations', loc = 'bottom')

# Save figure with nice margin
plt.savefig('IterationsBoxPlot.png', dpi = 300, bbox_inches = 'tight', pad_inches = .1)
plt.show()

