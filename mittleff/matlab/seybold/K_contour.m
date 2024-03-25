function y=K_contour(r,varargin)
% Integral auf dem Strahl falls z auf der Contour liegt
    z=varargin{1};
     alpha=varargin{2};
     beta=varargin{3};

     c=r.^((1-beta)/alpha).*exp(-1/2*r.^(1/alpha));
     psi=sqrt(3)/2*r.^(1/alpha)+2*pi/3*(1-beta);
     eta=sqrt(3)/2*r.^(1/alpha)+2*pi/3*(alpha+(1-beta));
     Zaehler=(r.*sin(psi)-z.*sin(eta));
     Nenner=(r.^2-2.*r.*z.*cos(2*pi/3*alpha)+z.^2);

     
  y=c.*Zaehler./Nenner;

