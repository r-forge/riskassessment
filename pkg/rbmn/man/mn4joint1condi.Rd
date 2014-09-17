% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{mn4joint1condi} 
% 
\alias{mn4joint1condi} 
% 
\title{computes a joint distribution from a marginal and a conditional one for multinormal distributions} 
% 
\usage{ 
  mn4joint1condi(lmar,lcon,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  lcon <- list(mu=c(D=2,E=4), 
  rho=matrix(1:6,2,dimnames=list(LETTERS[4:5], 
  LETTERS[1:3])), 
  gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5]))); 
   
  print8mn(mn4joint1condi(rbmn0$mn1$v,lcon,check=FALSE),check=FALSE); 
} 
% 
\description{ 
   
  returns the expectation and 
  variance of the multinormal normal distribution 
  defined through a marginal subcomponent (\samp{lmar}) and 
  a conditional distribution (\samp{lcon}). 
   
} 
% 
\value{ 
   
  A list with two components: \samp{$mu} of the expectation vector and 
  \samp{$gamma} of the joint variance matrix; that is a /mn/ object. 
   
} 
% 
 
\arguments{ 
    \item{lmar}{ /mn/ object defining the distribution of the marginal part with \samp{$mu}, its expectation, and \samp{$gamma}, its variance matrix (in fact a /mn/ object).} 
    \item{lcon}{ /mn/ object defining the distribution of the conditional part.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{distribution} 
\keyword{distribution} 
\keyword{multinormal} 
\section{Future}{ 
(after the paper publication: change the components names of 
\samp{lcon} to be consistent with the output of function 
\samp{condi4joint} when regression coefficients are outputted. 
} 
