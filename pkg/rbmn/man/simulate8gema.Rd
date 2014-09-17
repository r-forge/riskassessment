% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{simulate8gema} 
% 
\alias{simulate8gema} 
% 
\title{simulates from a /gema/ object} 
% 
\usage{ 
  simulate8gema(gema,nbs,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  simulate8gema(rbmn0$gema1$v,10,check=TRUE); 
} 
% 
\details{ 
   
  Just the application of the standard formula 
  to a white noise. Variables names are taken 
  from those of \samp{gema$mu}, when these 
  does not exist, standard ones are provided. 
   
} 
% 
\description{ 
   
  returns a matrix of simulated 
  values with the variable in columns and the 
  simulations in rows. 
   
} 
% 
\value{ 
   
  A matrix of size : \samp{nbs x length(gema$mu)} 
   
} 
% 
 
\arguments{ 
    \item{gema}{ The \samp{gema} object.} 
    \item{nbs}{ number of simulations to return.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{generating--matrix} 
\keyword{simulation} 
\section{Future}{ 
Include the checking of the topological order 
} 
