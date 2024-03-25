function y=IntegrateP(f,z,tol,varargin)
% Integration ueber den Kreisbogen von -pi*alpha...pi*alpha
     alpha=varargin{1};
     beta=varargin{2};
     epsilon=varargin{3};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	    
% Integrationsroutine Waehlen	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gauss Lobatto Integration
     y=quadl(f,-pi*alpha,pi*alpha,tol,[],z,alpha,beta,epsilon);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alternative Gauss-Kronrod, 7(15)  Quadratur Formel
% oder ueber Optionen eine Gauss-Legendre Quadratur
% options.tol=tol;
% [options.nodes options.weights] = quadg('gausslegendre',1); 
% y=quadg(f,-pi*alpha,pi*alpha,options,z,alpha,beta,epsilon);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alternativ Romberg Verfahren
%
% y=Rombint(f,-pi*alpha,pi*alpha,tol,z,alpha,beta,epsilon);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%