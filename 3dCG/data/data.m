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

fileID = fopen('rhs.bin');
rhs = fread(fileID,N*N*N,'double');
rhs = reshape(rhs,[N,N,N]);

fileID = fopen('y.bin');
y = fread(fileID,N*N*N*3,'double');
y = reshape(y,[N*N*N, 3]);
% y = sqrt( y(:,1).^2 + y(:,2).^2 + y(:,3).^2 );

%%
close all

% figure(1)
% loglog(R,abs(x),'.k',R,.1./R,'.r');
% 
% figure(2)
% loglog(R,y,'.k',R,.1*R.^(-2),'.r');

t = 16;
figure(1)
mesh(X(:,:,t),Y(:,:,t),x(:,:,t));

yt = reshape(y(:,1), [N,N,N]);
figure(2)
mesh(X(:,:,t),Y(:,:,t),yt(:,:,t));
% mesh(Z(:,t,:),X(:,t,:),yt(:,t,:));
% mesh(Y(t,:,:),Z(t,:,:),yt(t,:,:));
xlabel('x'); ylabel('y');

figure(3)
mesh(X(:,:,t),Y(:,:,t),rhs(:,:,t));

%%

ex = zeros(3,1); ey = zeros(3,1);
ex(1) = 2.0082180970648622E-004; ex(2) = 8.0357767937666580E-004; ex(3) = 3.2189644400801853E-003;
ey(1) = 1.2616992980589714E-003; ey(2) = 5.0474053731663915E-003; ey(3) = 2.0199360769927566E-002;
Nk = [128, 64, 32];

figure(4)
loglog(Nk,ex,'o-',Nk,ey,'o-',Nk,1./Nk./Nk,'-r','LineWidth',3);
xlabel('Ng'); ylabel('error');
h=legend('$\nabla^2x=b$','$\nabla x$','$\mathcal{O}(N^{-2})$');
set(h,'Interpreter','Latex');
set(gca,'fontsize',35);