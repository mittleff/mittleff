function E=MittagLeffler(z,alpha,beta,tol)
% Berechnung der MittagLefflerfunktion fuer alle Eingabefaelle Z
% Z=Scalar Z=Vektor Z=Matrix 
% beta\in [-50..50], \alpha>0;

q=0.9;      % Kreisradius in dem  die Taylorentwickung
r_0=0.5;    % Kreisradius der Kontur
% q_Max=??? % Kreisradius der Assymptotischen Entwicklung 
% Bestimmt ob z ein Vektor oder eine Matrix oder ein scalar ist
  [M,N]=size(z);
  E=zeros(M,N);
% und legt eine Ergebnisvariable an.

% Schleife zur Berechnung der Werte fuer alle Eintraege von E
% aus einem Vekor oder einer Matrix wird ein Scalar gemacht
 for s=1:M
        for t=1:N

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          if (abs(z(s,t))<=q)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Taylorentwicklung falls abs(z)<q=0.9	 
% dies ist unabhaengig von \alpha und \beta
	    E(s,t)=Taylor(z(s,t),alpha,beta,tol);
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	  elseif (abs(z(s,t))>q_MAX)                    %
%     Assymptotische Reihen falls implementiert         %
%     weiter Fallunterscheidung fuer verschiedene arg(z)%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	  
	  elseif (alpha>1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% falls \alpha>1 muss E_\alpha_\beta iterativ aus E_\alpha<1_\beta
% berechnet werden
               m=ceil((alpha-1)/2)+1; 
	       % Abbruchbedingung fuer die Iteration
	            for L=-m:1:m 
		        E(s,t)=E(s,t)+ML2(z(s,t).^(1/(2*m+1))...
			*exp(2*pi*i*L/(2*m+1)),alpha/(2*m+1),beta,tol,r_0);
		    end;
	    E(s,t)=E(s,t)/(2*m+1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	    
	  else 
% jetzt muss \alpha<1 und abs(z)>q sein	    
	   E(s,t)=ML2(z(s,t),alpha,beta,tol,r_0);
	  end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	end;	
 end;
 