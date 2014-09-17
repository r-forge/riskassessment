% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{form3crop} 
% 
\alias{form3crop} 
% 
\title{removes framing characters from a character string} 
% 
\usage{ 
  form3crop(chaine,bef=rbsa0$sep0$v,aft=bef,
            mxm=Inf,empty=FALSE,
            monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  form3crop('IMPORTANT','IM',' ANT'); 
  form3crop(c('   OUF ',' FOU ',''),' ','',1); 
} 
% 
\description{ 
   
  removes \samp{bef}s before and \samp{aft}s after a character string. 
   
} 
% 
\value{ 
   
  \samp{character} after removings 
   
} 
% 
 
\arguments{ 
    \item{chaine}{The character string to refine. Can be a vector.} 
    \item{bef}{ What to repeatedly remove at the beginning.} 
    \item{aft}{ What to repeatedly remove at the end.} 
    \item{mxm}{ Maximum number of tags to remove.} 
    \item{empty}{ Must remaining empty lines be removed?} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{utilities} 
\keyword{cropping} 
\keyword{format} 
