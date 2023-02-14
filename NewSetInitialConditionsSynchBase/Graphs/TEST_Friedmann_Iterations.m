%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Archivo para realizar el analisis estadistico de las 30 condiciones
% iniciales nuevas
% Se realizaron las sincronizaciones completa y proyectiva
% con las alfas: 0.5, 1, 2, 3, 4, 8
% Elaborado: JZM
% Fecha 24-10-2022
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc; clear; clearvars; % limpiamos memoria y pantalla
close all

%% 
% Cargamos el archivo con los datos de las evaluaciones
Complete = load('EvaluationsCompleteChua30IC.txt');
Projective = load('EvaluationsProjectiveChua30.txt');
%%
% creamos las variables
comp = Complete(:,2);
proy05 = Projective(1:30,2);
proy10 = Projective(31:60,2);
proy20 = Projective(61:90,2);
proy30 = Projective(91:120,2);
proy40 = Projective(121:150,2);
proy80 = Projective(151:180,2);

%%
% se realiza el test de t
% para la sincronizacion completa y la proyectiva alpha=1

[h1,p1,ci,stats1] = ttest(comp,proy10,'Tail', 'right', 'Alpha', 0.05)
%% 
% se calculan el promedio y la desviacion estandar de la sincronizacion 
% completa y la proyectiva alpha=1
m1 = mean(comp);
s1 = std(comp);
%
m2 = mean(proy10);
s2 = std(proy10);

%%
% analisis del factor de escalamiento (alfa) de la sincronizaciones 
% proyectivas 0.5, 1, 2, 3, 4, 8

% guardamos las iteraciones en una sola matriz
proy = [proy05, proy10, proy20, proy30, proy40, proy80];


t = array2table([[1:30]' proy],'VariableNames',{'InitCond' 'a05' 'a10' 'a20' 'a30' 'a40' 'a80'});
rm = fitrm(t,'a05-a80~InitCond');
r = ranova(rm);
r1 = multcompare(rm, 'Time', 'ComparisonType', 'dunn-sidak')

% % test de Friedmann
%   [p, t, stats] = friedman(proy,30)
% %%
% % multiple comparison test
%   [c , m] = multcompare(stats, 'Alpha',0.05, 'CType', 'dunn-sidak')

%%
% GCI = [ones(8,1); 2*ones(8,1); 3*ones(8,1); 4*ones(6,1)];
% t = array2table([GCI [1:30]' proy],'VariableNames',{'Groups' 'InitCond' 'a05' 'a10' 'a20' 'a30' 'a40' 'a80'});
% rm = fitrm(t,'a05-a80~InitCond+Groups');
% r = ranova(rm);
% multcompare(rm, 'Time', 'ComparisonType', 'dunn-sidak')