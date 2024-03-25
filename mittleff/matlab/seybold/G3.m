function E=G3(z,alpha,beta,tol,r_0)
% Fallunterscheidung nach verschiedenem \beta im Gebiet G-
% falls z auf der Kontur liegt
% durch verformen der Kontur mit 
% \delta=pi*alpha -> \delta=2/3*pi*alpha liegt z nun in G-, 
% d.h. (abs(ARG)>(pi*alpha))

% Aufruf der Integrationsformeln
% r_0=0.5 global vorgegeben
if  (beta<=1)
%    hier muss der Fall \beta<=1 hin und der rest muss wie unten 
%    integriert werden
    E=1./(pi*alpha)*IntegrateK_contour(@K_contour,z,0,tol,alpha,beta);
 else	        
    E=1./(pi*alpha)*IntegrateK_contour(@K_contour,z,r_0,2*tol/3,alpha,beta)+...
       r_0.^((1+(1-beta)/alpha))./(2*pi*alpha).*...
      IntegrateP_contour(@P,z,tol/3,alpha,beta,r_0); 
 end;

 
 
 