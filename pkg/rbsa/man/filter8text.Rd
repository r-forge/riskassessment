% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{filter8text} 
% 
\alias{filter8text} 
% 
\title{filters components of a character.} 
% 
\usage{ 
  filter8text(text,pattern,remove=TRUE,
              exact=FALSE,how="a",lower=FALSE,
              monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  filter8text(rbsa0$text2$v,"little",exact=FALSE,lower=TRUE,remove=FALSE) 
  filter8text(rbsa0$text2$v,"On",exact=8,lower=TRUE,remove=FALSE) 
  filter8text(rbsa0$text2$v,"On",        lower=TRUE,remove=FALSE) 
  filter8text(rbsa0$text2$v,"On",exact=8,lower=TRUE,remove=TRUE) 
} 
% 
\description{ 
   
  For each component of a character vector, checks with function 
  \samp{belong9} if it satisfies the conditions given by the other 
  arguments, the component is removed (or selected). 
   
} 
% 
\value{ 
   
  A \samp{character} comprising the non-removed (or selected) components 
  of the initial components. 
   
} 
% 
 
\arguments{ 
    \item{text}{ The \samp{character} to be filtered.} 
    \item{pattern}{(\samp{character}) the character string(s) to be found in each component.} 
    \item{remove}{ Must the conforming components be removed (or selected)?} 
    \item{exact}{ When exact, one component must be strictly identical, if not a subtring is sufficient.} 
    \item{how}{ Indicates what to do when \samp{length(sch)>1}. The choice are \samp{v}: a logical vector gives back each check independently; \samp{1}: returns \samp{TRUE} when at least one of the component belongs to the series \samp{ch} and \samp{a} when all components must comply to get TRUE.} 
    \item{lower}{ Must the comparisons being done after case lowering?} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{utilities} 
\keyword{filter} 
\keyword{text} 
\seealso{ 
\code{\link{belong9}} 
} 
