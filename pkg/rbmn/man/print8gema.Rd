% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{print8gema} 
% 
\alias{print8gema} 
% 
\title{standard print function for a /gema/ object.} 
% 
\usage{ 
  print8gema(gema,what="ml",ordering=NULL,
             digits=3,printed=TRUE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8gema(rbmn0$gema1$v,check=TRUE); 
  print8gema(rbmn0$gema2$v,"m",check=TRUE); 
  print8gema(rbmn0$gema3$v,"l",digit=1,check=TRUE); 
  print8gema(rbmn0$gema4$v,printed=FALSE,check=TRUE); 
} 
% 
\description{ 
   
  prints a /gema/ object completely 
  or a part of it according to \samp{what} specification. 
   
} 
% 
\value{ 
   
  The \samp{gema} is printed or a matrix having a number of rows equal to 
  the number of nodes is returned 
  binding the elements which are precised in the argument \samp{what}. 
   
} 
% 
 
\arguments{ 
    \item{gema}{\samp{gema} object to be printed.} 
    \item{what}{a \samp{character(1)}; when comprising "m" the expectations are printed, "l" the linear combinations are printed.} 
    \item{ordering}{ Nodes are given following the indices of "ordering" if \samp{numeric} or the names if it is \samp{character}. \samp{NULL} means the identity permutation. Repetitions or missing nodes are accepted.} 
    \item{digits}{ when not null, the number of digits for rounding.} 
    \item{printed}{ \samp{TRUE} to issue a printing, if not the prepared matrix is returned.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{generating--matrix} 
\keyword{print} 
