% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{erreur} 
% 
\alias{erreur} 
% 
\title{issues an error message and concludes accordingly} 
% 
\usage{ 
  erreur(x,...,w=FALSE,monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  erreur(matrix(1:4,2),"This matrix is not symmetric",w=TRUE) 
  erreur("Are you sure of it?",w=TRUE); 
   
} 
% 
\description{ 
   
  when called this function prints x, then displays a message before stopping 
  the process except if it is a warning or if the general constant 
  \samp{monitor$fat$v} is true. 
   
} 
% 
\value{ 
   
  nothing is returned 
   
} 
% 
 
\arguments{ 
    \item{x}{object to be printed before the message. When \samp{void9(x)} nothing is printed. When it is a list, all components of the list are successively printed.} 
    \item{\dots}{pieces of message to display after pasting} 
    \item{w}{ Indicates if it is a simple warning} 
    \item{monitor}{ List of monitoring constants, see \samp{rbsa0$monitor} to know its structure.} 
} 
 
% 
\keyword{error} 
