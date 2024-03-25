function y=K1(r,varargin)
 %funktion K fuer den fall beta=1+alpha       
 alpha=varargin{2};
     z=varargin{1};
       
       Zaehler=exp(-r.^(1/alpha));
       Nenner=(r.^2-2.*r.*z.*cos(pi.*alpha)+z.^2);
  
   y=Zaehler./Nenner;
