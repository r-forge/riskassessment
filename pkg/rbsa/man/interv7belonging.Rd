% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{interv7belonging} 
% 
\alias{interv7belonging} 
% 
\title{checks if a series of values belong to a series of intervals} 
% 
\usage{ 
  interv7belonging(x,int,monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  interv7belonging(1:5,1:2); 
  interv7belonging(1:5,matrix(1:10,ncol=2)); 
} 
% 
\details{ 
   
  This function is compatible with real infinite values 
   
} 
% 
\description{ 
   
  computes and returns the indicator vector of the positions of 
  values with respect to intervals. 
   
} 
% 
\value{ 
   
  A matrix with rows associated to the \code{x} values and 
  columns associated to the \code{int} intervals giving 
  \code{-2,-1,0,1,2} according to whether \code{x} is less than, 
  equal to the lower bound, inside, equal to the upper bound or 
  greater than the interval. 
   
} 
% 
 
\arguments{ 
    \item{x}{Vector of value(s) to be scrutinized.} 
    \item{int}{Series of interval(s) to be considered. Either a \samp{numeric(2)} or a matrix with two columns. Empty intervals (\samp{numeric(0)} are not admitted.} 
    \item{monitor}{List providing the monitoring constants, see \code{rbsa0$monitor$v} to know the contents.} 
} 
 
% 
\keyword{miscellaneous} 
\keyword{belong} 
\keyword{interval} 
