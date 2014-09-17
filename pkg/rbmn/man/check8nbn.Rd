% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{check8nbn} 
% 
\alias{check8nbn} 
% 
\title{checks a /nbn/ object} 
% 
\usage{ 
  check8nbn(nbn,topo=TRUE) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  check8nbn(rbmn0$nbn1$v); 
  check8nbn(rbmn0$adja1$v); 
} 
% 
\details{ 
   
  Looking a the code of this function provides a way to know which 
  are the requirements of a /nbn/ object. 
   
} 
% 
\description{ 
   
  checks the consistency of \samp{nbn} as a /nbn/ object, 
  returns some clues if inconsistent. 
   
} 
% 
\value{ 
   
  \samp{character(0)} or a \samp{character} containing some clue 
  about the discovered inconsistency. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The \samp{nbn} object to check.} 
    \item{topo}{ Must the existence of a topological order be checked?} 
} 
 
% 
\keyword{check} 
\keyword{normal--Bayesian--network} 
