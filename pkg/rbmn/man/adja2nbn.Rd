% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{adja2nbn} 
% 
\alias{adja2nbn} 
% 
\title{standardized /nbn/ from an adjacency matrix} 
% 
\usage{ 
  adja2nbn(adja) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(adja2nbn(adja4nbn(rbmn0$nbn3$v,check=FALSE)),check=TRUE); 
} 
% 
\description{ 
   
  returns a \samp{nbn} object 
  with O/1 regression coefficients having \samp{adja} as 
  adjacency matrix. 
   
} 
% 
\value{ 
   
  The corresponding standardized \samp{nbn} object. 
   
} 
% 
 
\arguments{ 
    \item{adja}{ The initial adjacency matrix.} 
} 
 
% 
\keyword{adjacency--matrix} 
\keyword{normal--Bayesian--network} 
\seealso{ 
\code{\link{adja4nbn}} 
} 
