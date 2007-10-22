#! /usr/bin/octave -qf
1; % measuring Octave computing times

function r = rot(a)
    c = cos(a);
    s = sin(a);
    r = [ c , 0, s;
          0,  1, 0;
         -s,  0, c];
end

t0=time();
x = linspace(0,1,1E5);
ac = eye(3);
for a=x
    ac = rot(a)*ac;
end

disp(ac);
disp(time()-t0)
