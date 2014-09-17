% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{write8nbn} 
% 
\alias{write8nbn} 
% 
\title{writes a /nbn/ object into a text file} 
% 
\usage{ 
  write8nbn(nbn,file,tag1=sssrbsa0$tag1$v,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  write8nbn(rbmn0$nbn1$v,file="toto.txt"); 
} 
% 
\details{ 
   
  See \samp{read8nbn} documentation. 
   
} 
% 
\description{ 
   
  writes to the text file \samp{file} 
  a /nbn/ object which can be back read with \samp{read8nbn}. 
   
} 
% 
\value{ 
   
  nothing but a file is created. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The concerned \samp{nbn} object.} 
    \item{file}{file to be created. When NULL the result is sent to the standard output.} 
    \item{tag1}{A matrix of three tags (by rows): first column to open and second column to close.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{normal--Bayesian--network} 
\keyword{file} 
