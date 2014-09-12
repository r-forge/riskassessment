% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{form3line} 
% 
\alias{form3line} 
% 
\title{prints a separator line from a given pattern} 
% 
\usage{ 
  form3line(len=50,pat="-_-",wid=3,
            gind="",pat2="_-_",
            imp=TRUE) 
} 
% 
\examples{ 
  form3line(); 
  form3line(gind="^^^"); 
  form3line(pat="+",wid=1,gind="   ",pat2=NULL); 
} 
% 
\description{ 
   
  Prints a separator line from a given pattern. 
  The line can be composite with the width argument. 
  The pattern can comprise more than one character. 
  General indentation is possible (\samp{gind}). 
  Even lines can have a different pattern(\samp{pat2}). 
   
} 
% 
\value{ 
   
  character string or printing according to \samp{imp} 
   
} 
% 
 
\arguments{ 
    \item{len}{ Line length (without the general indentation).} 
    \item{pat}{ Pattern to use.} 
    \item{wid}{ Number of elementary lines.} 
    \item{gind}{ String to introduce as general indentation.} 
    \item{pat2}{ Pattern to use for the even lines. If NULL, the pattern is common to all lines.} 
    \item{imp}{ Printing when TRUE or returning (FALSE)} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{line} 
\seealso{ 
\code{\link{form3repeat}} 
} 
