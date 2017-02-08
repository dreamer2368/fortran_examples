clear
close all
clc

% Read array from binary file with prescribed dimensions
A = readBinary('A.dat',[6 4],'double')

% Read array from binary file with unknown dimensions (as a vector)
A = readBinary('A.dat','double')