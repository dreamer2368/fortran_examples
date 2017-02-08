close all
clear all
clc

spec = importdata('readme.out');
L = spec(1); N = spec(2);

dx = L/N;
xg = linspace(0,L,N+1)'; xg = xg(1:N);
[X,Y,Z] = meshgrid(xg,xg,xg);
R = sqrt( (X-xg(N/2)).^2 + (Y-xg(N/2)).^2 + (Z-xg(N/2)).^2 );
R = reshape(R, [N*N*N, 1]);

fileID = fopen('x.bin');
x = fread(fileID,N*N*N,'double');
x = reshape(x,[N,N,N]);

fileID = fopen('solx.bin');
solx = fread(fileID,N*N*N,'double');
solx = reshape(solx,[N,N,N]);

fileID = fopen('rhs.bin');
rhs = fread(fileID,N*N*N,'double');
rhs = reshape(rhs,[N,N,N]);

fileID = fopen('y.bin');
y = fread(fileID,N*N*N*3,'double');
y = reshape(y,[N*N*N, 3]);

fileID = fopen('soly.bin');
soly = fread(fileID,N*N*N*3,'double');
soly = reshape(soly,[N*N*N, 3]);
% y = sqrt( y(:,1).^2 + y(:,2).^2 + y(:,3).^2 );

%%
close all

% figure(1)
% loglog(R,abs(x),'.k',R,.1./R,'.r');
% 
% figure(2)
% loglog(R,y,'.k',R,.1*R.^(-2),'.r');

t = 32;
% figure(1)
% mesh(squeeze(X(:,:,t)),squeeze(Y(:,:,t)),squeeze(x(:,:,t)));

yt = reshape(y(:,1), [N,N,N]);
figure(2)
mesh(X(:,:,t),Y(:,:,t),yt(:,:,t));
% mesh(Z(:,t,:),X(:,t,:),yt(:,t,:));
% mesh(Y(t,:,:),Z(t,:,:),yt(t,:,:));
ylabel('x'); xlabel('y'); zlabel('Ex');
title('$E_x$ at $z=L/2$ with Multyplying $k_x$','Interpreter','Latex');
set(gca,'fontsize',25);

% figure(3)
% mesh(X(:,:,t),Y(:,:,t),rhs(:,:,t));

%%
% solx = (X.^3-3/2*L*X.^2+L^2/2*X).*(Y.^3-3/2*L*Y.^2+L^2/2*Y).*(Z.^3-3/2*L*Z.^2+L^2/2*Z);

% figure (4)
% mesh(squeeze(X(:,:,t)),squeeze(Y(:,:,t)),squeeze(solx(:,:,t)));
% title('Potential at $z=L/2$','Interpreter','Latex');
% ylabel('x'); xlabel('y'); zlabel('Ex');
% set(gca,'fontsize',25);

solyt = reshape(soly(:,1), [N,N,N]);
figure(5)
mesh(X(:,:,t),Y(:,:,t),solyt(:,:,t));
ylabel('x'); xlabel('y'); zlabel('Ex');
title('$E_x$ with Centered difference','Interpreter','Latex');
set(gca,'fontsize',25);

%%

fileID = fopen('yFFTk.bin');
KXFFT = fread(fileID,N*N*N,'double');
KXFFT = reshape(KXFFT,[N,N,N]);

fileID = fopen('yFFT.bin');
CDFFT = fread(fileID,N*N*N,'double');
CDFFT = reshape(CDFFT,[N,N,N]);

kx = 1:N;
out = (kx>=N/2); kx(out) = kx(out) - N;
kx = kx*2i*pi/L;
[KX, KY, KZ] = meshgrid(kx,kx,kx);
KR = sqrt( abs( KX.^2 + KY.^2 + KZ.^2 ) );

figure(6)
plot( reshape(KR,[N*N*N,1]), reshape(KXFFT,[N*N*N,1]), '.k');
figure(7)
plot( reshape(KR,[N*N*N,1]), reshape(CDFFT,[N*N*N,1]), '.k');