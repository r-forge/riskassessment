% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{solve8quadratic} 
% 
\alias{solve8quadratic} 
% 
\title{solves a degree two polynomial} 
% 
\usage{ 
  solve8quadratic(y,ky,kx2,kx,kk,dx=NULL,x0=NULL,monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  solve8quadratic(1:10, 1,1,0,-20); 
  solve8quadratic(   3,-1,1,1,  1); 
  solve8quadratic(   3,-1,1,1,  1,c(0.5,1.5)); 
} 
% 
\details{ 
   
  When \code{dx} is defined only one root is returned, 
  belonging to the interval; if it is not possible (root(s) 
  exist(s) and do(es) not comply) a fatal error 
  is issued.\cr 
  When every real number complies with the equation, according 
  to available arguments, the returning value is \code{x0}, 
  \code{mean(dx)} or \code{0}. 
  When \code{is.null(dx)} either one or two roots is 
  returned with \code{NA} when the solution is complex. 
   
} 
% 
\description{ 
   
  This function returns the root (or two roots) of 
  the equation \code{ky*y + kx2*x^2 + kx*x + kk = 0}. 
  When \code{dx} is not null, it is supposed to give 
  the interval where the root lies, in that case only 
  one root is returned.\cr 
  The first parameter can be a vector of any 
  length and all computations are vectorized.\cr 
  Only real roots are considered. 
   
} 
% 
\value{ 
   
  A matrix having one or two columns according to the values of 
  \code{ky,kx2,kx,kk,dx}. 
   
} 
% 
 
\arguments{ 
    \item{y}{Vector of values for which the equation must be satisfied.} 
    \item{ky}{Coefficient for \code{y}.} 
    \item{kx2}{Coefficient for \code{x^2}.} 
    \item{kx}{Coefficient for \code{x}.} 
    \item{kk}{Constant coefficient.} 
    \item{dx}{\code{NULL} or the interval (\code{numeric(2)}) for the roots.} 
    \item{x0}{\code{NULL} or a proposal in case of indetermination.} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \code{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{quadratic} 
\keyword{solve} 
