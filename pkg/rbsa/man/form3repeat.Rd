% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{form3repeat} 
% 
\alias{form3repeat} 
% 
\title{prints a repeated given string} 
% 
\usage{ 
  form3repeat(cha="-",nb=10,imp=FALSE,cr=imp) 
} 
% 
\examples{ 
  form3repeat('-+',20,TRUE) 
} 
% 
\description{ 
   
  Component by component, prints the concatenation 
  of the given string(s) repeated \samp{nb} times. 
   
} 
% 
\value{ 
   
  character string or printing according to \samp{imp} 
   
} 
% 
 
\arguments{ 
    \item{cha}{ The string to repeat} 
    \item{nb}{ Number of repetitions} 
    \item{imp}{ Printing when TRUE or returning (default)} 
    \item{cr}{ Must a line feed be added?} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{repeat} 
