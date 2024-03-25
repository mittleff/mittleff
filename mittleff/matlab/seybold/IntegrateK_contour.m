function y=IntegrateK_contour(f,z,R_0,tol,varargin)
     alpha=varargin{1};
     beta=varargin{2};
% Integratinsroutine wenn z auf der Contour liegt und dann die
% Contourverbogen wird. Dadurch aendert sich der Integrand was auch eine
% andere Abschaetzung fuer die Obergrenze des Integrationsintervalls zur
% Folge hat
     
rho1=tol/2; %Fehler durch die Abschaetzung
rho2=tol/2; %Fehler durch die Integration
% Damit wird der Gesamt Fehler wieder rho1+rho2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
     if beta>=0
               R1=(-2*log(pi*2^beta*rho1/12)).^alpha;
               R2=2*abs(z);
               R3=2^alpha;
          R_Max=max([R1,R2,R3]);
     else
                  Nenner=12*(abs(beta)+2)*(4*abs(beta))^abs(beta);
               R1=(-4*log((pi*2^beta*rho1)/Nenner))^alpha;
	       R2=(2*(abs(beta)+1))^alpha;
	       R3=2*abs(z);
	  R_Max=max([R1,R2,R3]) ;
     end;
     
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    R_1=2.5*(R1+R3); % dieses R_1 sollte aus der Funtion K(r) noch berechnet
            % werden!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	    
% Integrationsroutine Waehlen	
% die Integration ueber das Intervall [R_0..R_Max] wird in 2 Teile
% aufgespalten: [R_0..R_1] und [R_1..R_Max]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gauss Lobatto Integration
if R_Max>R_1
  y=quadl(f,R_0,R_1,rho2/2,[],z,alpha,beta)...
    +quadl(f,R_1,R_Max,rho2/2,[],z,alpha,beta);
else
  y=quadl(f,R_0,R_Max,rho2,[],z,alpha,beta);
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alternative Gauss-Kronrod, 7(15)  Quadratur Formel
% oder ueber Optionen eine Gauss-Legendre Quadratur
% options.tol=rho2/2;
% [options.nodes options.weights] = quadg('gausslegendre',1); 
% y=quadg(f,R_0,R_1,options,z,alpha,beta)...
%  +quadg(f,R_1,R_Max,options,z,alpha,beta)+
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alternativ Romberg Verfahren
%
% y=Rombint(@f,R_0,R_1,rho2/2,z,alpha,beta)...
%  +Rombint(@f,R_1,R_Max,rho2/2,z,alpha,beta);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
