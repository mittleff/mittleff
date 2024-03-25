function y=K(r,varargin)
% Integrand auf dem Strahl
     z=varargin{1};
     alpha=varargin{2};
     beta=varargin{3};

       c=r.^((1-beta)/alpha).*exp(-r.^(1/alpha));

       Zaehler=(r.*sin(pi.*(1-beta))-z.*sin(pi*(1-beta+alpha)));

       Nenner=(r.^2-2.*r.*z.*cos(pi*alpha)+z.^2);
  

   y=c.*Zaehler./Nenner;
