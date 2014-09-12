% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{form3frame} 
% 
\alias{form3frame} 
% 
\title{surrounds a character string} 
% 
\usage{ 
  form3frame(chaine,bef="(*)_",aft="",imp=FALSE,cr=imp,
             monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  form3frame('IMPORTANT','<<< ',' >>>'); 
  form3frame('IMPORTANT','<<< ',' >>>',imp=TRUE); 
} 
% 
\description{ 
   
  Adds _bef_ore and _aft_er some characters to a character string 
  (the same for all components when it is a vector). 
   
} 
% 
\value{ 
   
  character string or printing according to \samp{imp}. 
   
} 
% 
 
\arguments{ 
    \item{chaine}{The character string to frame; can be a vector.} 
    \item{bef}{ What to add at the beginning.} 
    \item{aft}{ What to add at the end.} 
    \item{imp}{ Printing when TRUE if not returning} 
    \item{cr}{ Must a line feed be added?} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{cropping} 
