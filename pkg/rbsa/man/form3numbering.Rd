% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{form3numbering} 
% 
\alias{form3numbering} 
% 
\title{provides a systematic numbering of items} 
% 
\usage{ 
  form3numbering(nbn,type="n",bef="-",aft="-",
                 start=1+(type=="n"),
                 monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  form3numbering(27); 
  form3numbering(27,start=1); 
  form3numbering(100,"a"); 
  form3numbering(100,"A"); 
} 
% 
\details{ 
   
  Alphabetical series are "aa","ab",...,"az","ba",... Then starting 
  from the second element means to start from "ab". It is not possible 
  to start from the 0th element.\cr 
  Numerical series are "00","01","02",...,"09","10",... Then one 
  can start from the zeroth element which is "00" but starting from 
  the second element means starting from "01". This inconsistency 
  may look not desirable, but it allows to follow the natural 
  reasoning when working in any of the two logics. 
   
} 
% 
\description{ 
   
  Provides systematic names for a series of items according their 
  number giving them an identical number of characters. 
   
} 
% 
\value{ 
   
  character vector with \samp{nbn} different strings associated to names 
   
} 
% 
 
\arguments{ 
    \item{nbn}{Number of item names to generate from \samp{start}.} 
    \item{type}{ Defines the type of numbering to use. There are three possibilities: \samp{A} for uppercased letters, \samp{a} for lowercased letters and \samp{n} for arabic numbers.} 
    \item{bef}{ To put before the number.} 
    \item{aft}{ To put after  the number.} 
    \item{start}{ The first number to issue (>0).} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{utilities} 
\keyword{format} 
\keyword{numbering} 
\section{Future}{ 
Monitor the number of digits to obtain series of "Z01", "Z02",... ,"Z79", "Z80". 
} 
\seealso{ 
\code{\link{form3names}} 
} 
