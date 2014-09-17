% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{chain2gema} 
% 
\alias{chain2gema} 
% 
\title{transforms a /chain/ to a /gema/} 
% 
\usage{ 
  chain2gema(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  identical(chain2gema(rbmn0$chain2$v,check=TRUE)$mu,rbmn0$gema2$v$mu); 
  print(chain2gema(rbmn0$chain2$v,check=TRUE)$li-rbmn0$gema2$v$li); 
} 
% 
\description{ 
   
  From a \samp{chain} object 
  returns the \samp{gema} using a closed formulae.\cr 
  Much precised than to use the /nbn/ way. 
   
} 
% 
\value{ 
   
  The corresponding \samp{gema} object. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ the \samp{chain} object to be transformed.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{generating--matrix} 
