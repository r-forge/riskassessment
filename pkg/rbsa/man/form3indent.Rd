% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{form3indent} 
% 
\alias{form3indent} 
% 
\title{provides indentation of different levels} 
% 
\usage{ 
  form3indent(niv=1,cr=1,ele="",monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  cat(form3indent(2),"Bien `a vous","\n"); 
  cat(form3indent(3),cr=3,"Bien `a vous","\n"); 
} 
% 
\description{ 
   
  returns a character to be used as indentation. 
  The level of indentation is given by \samp{niv*monitor$ind$v} times 
  the character \samp{ele}. New lines can be added first. 
   
} 
% 
\value{ 
   
  a scalar string 
   
} 
% 
 
\arguments{ 
    \item{niv}{ Level of indentation.} 
    \item{cr}{ Number of previous feed lines to first provide?} 
    \item{ele}{ String to repeat.} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{indentation} 
