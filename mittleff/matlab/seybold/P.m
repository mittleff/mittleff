function y=P(phi,varargin)
% Integrand auf dem Kreissektor der Contour
z=varargin{1};
alpha=varargin{2};
beta=varargin{3};
epsilon=varargin{4};

  omega=epsilon.^(1/alpha)*sin(phi./alpha)+phi.*(1+(1-beta)/alpha);   
  Zaehler=exp(epsilon.^(1/alpha)*cos(phi./alpha)).*(cos(omega)+i.*sin(omega));
  Nenner=epsilon.*exp(i.*phi)-z;

     y=Zaehler./Nenner;
    
