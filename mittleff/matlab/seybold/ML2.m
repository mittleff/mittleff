function E=ML2(z,alpha,beta,tol,r_0)
% ML berechnet die Mittagleffler-Funktion an einer scalaren Stelle
% im Falle alpha<=1 und q<abs(z) mit der Integraldarstellung
% hier wird die Gebietseinteilung der Darstellung bestimmt

% r_0=0.5;  Radius des Integrationsrings von P(\varphi)

Arg_M=5*pi*alpha/6; % Mitte zwischen den beiden Conturlinien 
                    % phi=2/3\pi\alpha und pi\alpha


%% Fallunterscheidung nach abs(arg(z))  

ARG=angle(z);

if (abs(ARG)<=Arg_M);
% Integraldarstellung im Gebiet G+ bezuglich der Gebietseinteilung mit
% der Kontur bei \pi\alpha
%    disp('z in G1+');
     E=G2(z,alpha,beta,tol,r_0);
else 
% Integraldarstellung im Gebiet G- bezuglich der Gebietseinteilung mit
% der Kontur bei 2/3*\pi\alpha
%    disp('z in G2-');
     E=G3(z,alpha,beta,tol,r_0);
end;


 


