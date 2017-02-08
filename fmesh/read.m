clear all
close all
clc

fileID = fopen('output.bin');
N = fread(fileID,2,'int32'); Ng=N(1); N=N(2);
fileID = fopen('x.bin');
x = fread(fileID,N,'double');
fileID = fopen('y.bin');
y = fread(fileID,N,'double');
fileID = fopen('pg.bin');
pg = fread(fileID,(2*Ng+1)*(2*Ng+1),'double');
pg = reshape(pg,[2*Ng+1,2*Ng+1]);
fileID = fopen('pg_dx.bin');
pg_dx = fread(fileID,(2*Ng+1)*(2*Ng+1),'double');
pg_dx = reshape(pg_dx,[2*Ng+1,2*Ng+1]);
fileID = fopen('pg1.bin');
pg1 = fread(fileID,(2*Ng+1)*(2*Ng+1),'double');
pg1 = reshape(pg1,[2*Ng+1,2*Ng+1]);

xg = linspace(-0.5,0.5,2*Ng+1);
yg = linspace(-sqrt(3),sqrt(3),2*Ng+1);

figure(1)
mesh(xg,yg,pg');

figure(2)
plot(x,y,'.');
% axis([-2 2 -sqrt(3) sqrt(3)]);
axis([-0.5 0.5 -0.5 0.5]);

figure(3)
mesh(xg,yg,pg1');

figure(4)
mesh(xg,yg,pg_dx');