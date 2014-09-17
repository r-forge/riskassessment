% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn2gema} 
% 
\alias{nbn2gema} 
% 
\title{computes a /gema/ from  a /nbn/} 
% 
\usage{ 
  nbn2gema(nbn,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  identical(nbn2gema(rbmn0$nbn2$v),rbmn0$gema2$v); 
} 
% 
\description{ 
   
  from a /nbn/ object defining a normal 
  Bayesian network, computes the vector \samp{mu} and 
  the matrix \samp{li} such that if the vector \samp{E} 
  is a vector of i.i.d. centred and standardized normal, then 
  \samp{mu + li \%*\% E} has the same distribution as the 
  input /nbn/. 
   
} 
% 
\value{ 
   
  a list with the two following components: 
  \samp{mu} and \samp{li}. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ \samp{nbn} object for which the generating matrices must be obtained.} 
    \item{check}{ Must the argument be checked?} 
} 
 
% 
\keyword{generating--matrix} 
\keyword{normal--Bayesian--network} 
\section{Future}{ 
Doesn't work for rbmn0$nbn4$v !!! 
} 
