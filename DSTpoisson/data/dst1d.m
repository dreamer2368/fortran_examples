clear all
close all
clc

L=2; N=64;

fileID = fopen('phi.bin');
phi = fread(fileID,N,'double');

%%
close all

dx = L/(N-1); xg = dx*(0:N-1);

plot(xg,phi,'-k',xg,sin(pi*xg/2/L),'.--r');
% plot(xg,phi,'-k',xg,xg/2.*(xg-2*L),'.--r');

%%
close all
Nk = [64, 128, 256];
ek = [   4.0543090768746138E-005, 1.0154166833933819E-005, 2.5408376722724204E-006];
loglog(Nk,ek,'-k',Nk,1./(Nk).^2,'-r');