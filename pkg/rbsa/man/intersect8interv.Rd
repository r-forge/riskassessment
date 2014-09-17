% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{intersect8interv} 
% 
\alias{intersect8interv} 
% 
\title{computes the interval intersection of two intervals} 
% 
\usage{ 
  intersect8interv(int1,int2,monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  intersect8interv(numeric(0),1:2); 
  intersect8interv(c(1,10),c(-3,5)); 
  intersect8interv(c(1,10),c(10,12)); 
  intersect8interv(c(1,10),c(11,12)); 
  intersect8interv(c(1,10),c(pi,10*pi)) 
} 
% 
\description{ 
   
  computes and returns the interval (vector of length 2 or 0) 
  which is the intersection of two given intervals.\cr 
  Null intervals are indicated by \samp{numeric(0)}. 
   
} 
% 
\value{ 
   
  A numeric(2) or numeric(0) providing the intersection of the 
  two intervals. 
   
} 
% 
 
\arguments{ 
    \item{int1}{The first interval (numeric(2) or numeric(0)).} 
    \item{int2}{The second interval (numeric(2) or numeric(0)).} 
    \item{monitor}{List providing the monitoring constants, see \code{rbsa0$monitor$v} to know the contents.} 
} 
 
% 
\keyword{miscellaneous} 
\keyword{intersect} 
\keyword{interval} 
