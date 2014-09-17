% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{check8chain} 
% 
\alias{check8chain} 
% 
\title{checks a /chain/ object} 
% 
\usage{ 
  check8chain(chain) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  check8chain(rbmn0$chain1$v); 
  \dontrun{check8chain(rbmn0$adja1$v);} 
} 
% 
\details{ 
   
  Looking a the code of this function provides a way to know which 
  are the requirements of a /chain/ object. 
   
} 
% 
\description{ 
   
  checks the consistency of \samp{chain} as a /chain/ object 
  return a non empty \samp{character} with some clues if inconsistency is detected. 
   
} 
% 
\value{ 
   
  \samp{character(0)} or a \samp{character} containing some clue 
  about the discovered inconsistency. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The \samp{chain} object to check.} 
} 
 
% 
\keyword{chain} 
\keyword{check} 
