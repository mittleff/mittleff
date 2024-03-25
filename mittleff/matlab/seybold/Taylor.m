function E=Taylor(z,alpha,beta,tol);
% Berechnung der MittagLefflerfunktion mit der Taylorentwicklung im Gebiet
% abs(z)<q=0.9
% z muss scalar sein da die Abbruchbedingung der Reihe vom Betrag von z 
% abhaengt
% beachte abs(z)<1!!!!
r1=ceil(abs((1-beta)/alpha))+1;
if z==0
   r2=1;
else
   r2=ceil(abs(log(tol*(1-abs(z)))/log(abs(z))))+1;
end;


k_0=max(r1,r2);
E=0;   
for k=0:k_0
  % exp-ln trick
  if(alpha*k+beta>25)  
    E=E+(z.^k*exp(-log(Gamma(alpha*k+beta))));
  else
     E=E+(z.^k/Gamma(alpha*k+beta));
  end;
end;
