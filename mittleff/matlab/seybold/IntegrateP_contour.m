function y=IntegrateP_contour(f,z,tol,varargin)
% Integration ueber das Kreisbogensegment im Fall dass z auf der Contour
% liegt. Dann wird die Contour so verbogen, dass z wieder in G- landet:
% pi*alpha-> \delta=2/3*pi*alpha
% Die Integration ueber P(\varphi) laeuft dann von -\delta...\delta 
     alpha=varargin{1};
     beta=varargin{2};
     epsilon=varargin{3};
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	    
% Integrationsroutine Waehlen	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gauss Lobatto Integration
     y=quadl(f,-2*pi*alpha/3,2*pi*alpha/3,tol,[],z,alpha,beta,epsilon);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alternative Gauss-Kronrod, 7(15)  Quadratur Formel
% oder ueber Optionen eine Gauss-Legendre Quadratur
% options.tol=tol;
% [options.nodes options.weights] = quadg('gausslegendre',1); 
% y=quadg(f,-2*pi*alpha/3,2*pi*alpha/3,options,z,alpha,beta,epsilon);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alternativ Romberg Verfahren
%
% y=Rombint(f,-2*pi*alpha/3,2*pi*alpha/3,tol,z,alpha,beta,epsilon);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%