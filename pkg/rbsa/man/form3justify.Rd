% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{form3justify} 
% 
\alias{form3justify} 
% 
\title{formats a character string} 
% 
\usage{ 
  form3justify(chaine,
               nbc=8,
               format=3,
               tronc=TRUE,
               carac="",
               monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  form3justify("vers") 
  form3justify("versification",5) 
  form3justify(letters[1:5],format=2,carac="+"); 
} 
% 
\details{ 
   
  The justification is made component by component, 
  no concatenation between them is done. 
   
} 
% 
\description{ 
   
  Formats character string(s). 
  The main use of this function is to produce 
  aligned columns lists while not printing 
  them at the same time. 
   
} 
% 
\value{ 
   
  a character string 
   
} 
% 
 
\arguments{ 
    \item{chaine}{the character string to be printed, can be a vector.} 
    \item{nbc}{ Desired number of characters for the result; when \samp{chain} is a vector can be a vector of the same length} 
    \item{format}{ Indicates the type of alignment:\cr 0 no aligment (no supplementary character added to reach \samp{nbc})\cr 1 to the left side\cr 2 centered\cr 3 to the right side} 
    \item{tronc}{ If true, no more than \samp{nbc} characters are returned and possibly the string is truncated. In that case, \samp{$} is introduced to indicate the fact.} 
    \item{carac}{ Character to use for enlarging the string} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{justify} 
\section{Future}{ 
Improve the behavior when \samp{nchar(carac)>1}. 
} 
\seealso{ 
\code{\link{text3preparation}} 
} 
