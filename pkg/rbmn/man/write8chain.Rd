% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{write8chain} 
% 
\alias{write8chain} 
% 
\title{writes a /chain/ object into a text file} 
% 
\usage{ 
  write8chain(chain,file=NULL,check=rbmn0$check$v) 
} 
% 
\examples{ 
  write8chain(rbmn0$chain1$v,file="toto.txt"); 
} 
% 
\details{ 
   
  See \samp{read8chain} documentation. 
   
} 
% 
\description{ 
   
  writes to the text file \samp{file} 
  a /chain/ object which can be back read with \samp{read8chain}. 
   
} 
% 
\value{ 
   
  nothing but a file is created. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The concerned \samp{chain} object.} 
    \item{file}{file to be created. When NULL the result is sent to the standard output.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{file} 
