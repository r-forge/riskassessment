% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{create8star} 
% 
\alias{create8star} 
% 
\title{returns a star /rbmn/} 
% 
\usage{ 
  create8star(rho=rep(0.5,2),
              lambda=rep(0.4,4),
              sigi=rep(1,length(rho)),
              sigc=sqrt(1-sum(rho^2)),
              sigo=sqrt(1-lambda^2),
              quoi="gema",check=rbmn0$check$v) 
} 
% 
\examples{ 
  create8star(check=TRUE); 
  create8star(rho=rep(0.2,10),check=TRUE); 
  create8star(rho=rep(0,10),quoi="vari",check=TRUE); 
  create8star(rho=rep(1,4),sigc=0,quoi="vari",check=TRUE); 
} 
% 
\description{ 
   
  A star /rbmn/ is a /rbmn/ comprising a centre node to which enter 
  \samp{ni=length(rho)} independent input nodes and from which 
  come out \samp{no=length(lambda)} output nodes. This 
  gives roughly the configuration of a star, the centre node being the center 
  of the star and the other nodes some kind of radiuses around it. 
   
} 
% 
\value{ 
   
  The resulting structure with standard names. 
   
} 
% 
 
\arguments{ 
    \item{rho}{ Gives the regression coefficients of each input nodes, which are the parents of the center node.} 
    \item{lambda}{ Gives the regression coefficients of the centre node for each output node.} 
    \item{sigi}{ Vector of the standard deviation of the input nodes.} 
    \item{sigc}{ Scalar giving the conditional standard deviation of the center node. The default value ensures a marginal variance of 1.} 
    \item{sigo}{ Vector giving the conditional standard deviation of the output nodes. The default value ensures a marginal variance of 1.} 
    \item{quoi}{ Indicates the desired type of output. The different possibilities are \samp{gema} for a generating matrix, \samp{vari} for the variance matrix.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{creation} 
\keyword{star} 
