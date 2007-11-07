#! /usr/bin/octave -qf
% measuring Octave computing times

t0=time();
load mnist.txt
disp("load");
disp(time()-t0)


x = mnist(:,1:784);
d = mnist(:,785);


t0=time();
xc = x - repmat(mean(x),rows(x),1);
disp("x - repmat(mean(x),rows(x),1)");
disp(time()-t0)

t0=time();
mc = (xc'*xc)/rows(x);
disp("(xc'*xc)/rows(x)");
disp(time()-t0)

t0=time();
[v,l]=eig(mc);
disp("eig");
disp(time()-t0)

disp(flipud(diag(l))(1:10));