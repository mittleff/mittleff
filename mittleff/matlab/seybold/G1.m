function E=G1(z,alpha,beta,tol,r_0)
% Fallunterscheidung nach verschiedenem \beta im Gebiet G-

% wenn: (abs(ARG)>(pi*alpha))
% Aufruf der Integrationsformeln
% r_0=0.5

 if (beta==(1+alpha))
    E=-sin(pi*alpha)/(pi*alpha).*IntegrateK(@K1,z,0,tol,alpha,beta)-1./z;
 elseif (beta<=1)
 %    hier muss der Fall \beta<=1 hin und der rest muss wie unten 
 %    integriert werden
    E=1./(pi*alpha)*IntegrateK(@K,z,0,tol,alpha,beta);
 else	        
    E=1./(pi*alpha)*IntegrateK(@K,z,r_0,2*tol/3,alpha,beta)+...
       r_0.^((1+(1-beta)/alpha))./(2*pi*alpha).*...
       IntegrateP(@P,z,tol/3,alpha,beta,r_0); 
  end;

 
 
 