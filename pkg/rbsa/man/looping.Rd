% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{looping} 
% 
\alias{looping} 
\alias{bc} 
\alias{bd} 
\alias{bf} 
% 
\title{sequences for easy looping} 
% 
\usage{ 
  bc(nb) 
  bd(n1,n2) 
  bf(x) 
} 
% 
\examples{ 
  ## 
  ## bc 
  bc(0); 
  bc(5); 
  bc(pi); 
  bc(4*pi); 
  ## 
  ## bd 
  xx <- 1:5; 
  for (ii in 1:6) { print(c(xx[bd(1,ii-1)],10,xx[bd(ii,5)]));} 
  ## 
  ## bf 
  bf(0); 
  bf(5); 
  bf(character(0)); 
  bf(letters); 
} 
% 
\details{ 
   
  nb is rounded before use 
   
} 
% 
\description{ 
   
   (bc) 
  This function returns \samp{1:nb} when \samp{nb > 0} and 
  \samp{numeric(0)} otherwise. 
  Quite useful to prevent starting 
  a loop of length nought 
  \cr (bd)  
  This function returns \samp{n1:n2} when \samp{n1<=n2} and 
  \samp{numeric(0)} otherwise. 
  Quite useful when some insertion must be done within 
  a sequence 
  \cr (bf)  
  This function returns \samp{1:length(x)} when \samp{length(x) > 0} and 
  \samp{numeric(0)} otherwise. 
  Quite useful to prevent starting 
  a loop of length nought 
   
} 
% 
\value{ 
   
   (bc) 
  \samp{1:nb} if \samp{nb > 0} 
  else \samp{numeric(0)}. 
  \cr (bd)  
  \samp{n1:n2} if \samp{n1<n2} 
  else \samp{numeric(0)}. 
  \cr (bf)  
  \samp{1:length(x)} if \samp{length(x) > 0} 
  else \samp{numeric(0)}. 
   
} 
% 
 
\arguments{ 
    \item{nb}{length of the loop} 
    \item{n1}{first element} 
    \item{n2}{second element} 
    \item{x}{vector} 
} 
 
% 
\keyword{iteration} 
\keyword{miscellaneous} 
\keyword{miscellaneous} 
