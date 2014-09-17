% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{check8gema} 
% 
\alias{check8gema} 
% 
\title{checks a /gema/ object} 
% 
\usage{ 
  check8gema(gema) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  check8gema(rbmn0$gema1$v); 
  res <- check8gema(rbmn0$adja1$v); 
  if (is.na(as.logical(res))) { print(res);} 
} 
% 
\details{ 
   
  Looking a the code of this function provides a way to know which 
  are the requirements of a /chain/ object. 
   
} 
% 
\description{ 
   
  checks the consistency of \samp{gema} as a /gema/ object 
  issues a fatal error with some clues if inconsistent. 
   
} 
% 
\value{ 
   
  \samp{character(0)} or a \samp{character} containing some clue 
  about the discovered inconsistency. 
   
} 
% 
 
\arguments{ 
    \item{gema}{ The \samp{gema} object to check.} 
} 
 
% 
\keyword{check} 
\keyword{generating--matrix} 
