% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn4nbn} 
% 
\alias{nbn4nbn} 
% 
\title{From a /nbn/ computes the 'normalized' /nbn/} 
% 
\usage{ 
  nbn4nbn(nbn,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(nbn4nbn(rbmn0$nbn4$v)); 
} 
% 
\details{ 
   
  This normalization makes easy the study of the 
  /nbn/ structure. 
   
} 
% 
\description{ 
   
  returns a /nbn/ object with the 
  same structure as \samp{nbn} but all \samp{$mu} 
  are put to zero, all \samp{$sigma} to one as well 
  as \samp{$regcof}. 
   
} 
% 
\value{ 
   
  The resulting \samp{nbn}. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The \samp{nbn} object to transform.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{normal--Bayesian--network} 
\section{Future}{ 
To be integrated into 'normalize8nbn' since it is a kind of normalization 
} 
