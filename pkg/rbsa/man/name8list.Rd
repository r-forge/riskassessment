% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{name8list} 
% 
\alias{name8list} 
% 
\title{returns the same list but with names} 
% 
\usage{ 
  name8list(lili,monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  uu <- list(list(1:3,4:6), 
  B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))), 
  list(a=1:3,b=letters,list(array(1:8,c(2,2,2))))); 
  str(uu); 
  str(name8list(uu)); 
} 
% 
\details{ 
   
  The new names are obtained with \samp{form3names}. 
   
} 
% 
\description{ 
   
  Explores the branches of a list, returning the same valued list 
  but introducing systematic names when some of the components are not named. This is a 
  way to comply one of the two properties of a rbsa-list (see 
  \samp{rbsa7list9}). 
   
} 
% 
\value{ 
   
  The resulting named list. 
   
} 
% 
 
\arguments{ 
    \item{lili}{ The list structure to be possibly named.} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{input/output} 
\keyword{list} 
\keyword{name} 
