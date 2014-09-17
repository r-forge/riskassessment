% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{check8mn} 
% 
\alias{check8mn} 
% 
\title{checks a /mn/ object} 
% 
\usage{ 
  check8mn(mn) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  check8mn(rbmn0$mn1$v); 
  check8mn(rbmn0$adja1$v); 
} 
% 
\details{ 
   
  Looking a the code of this function provides a way to know which 
  are the requirements of a /mn/ object. But rouhghly speaking, it is a list 
  with two (or three) names components. \samp{$mu} for the expectation (when 
  possible covariables are nought); \samp{gamma} for the (conditional) 
  variance matrix (and \samp{rho} for the regression coefficients of 
  the covariables). All this structure must be consistently named or dimnamed. 
   
} 
% 
\description{ 
   
  checks the consistency of \samp{mn} as a /mn/ object, 
  returns some clues if inconsistent. 
   
} 
% 
\value{ 
   
  \samp{character(0)} or a \samp{character} containing some clue 
  about the discovered inconsistency. 
   
} 
% 
 
\arguments{ 
    \item{mn}{ The \samp{mn} object to check.} 
} 
 
% 
\keyword{check} 
\keyword{multinormal} 
