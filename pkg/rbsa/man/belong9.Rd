% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{belong9} 
% 
\alias{belong9} 
% 
\title{indicates inclusion of character string} 
% 
\usage{ 
  belong9(sch,ch,exact=FALSE,how="a",lower=FALSE,
          monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  belong9('a','non'); 
  belong9('o',c('non','oui')); 
  belong9('o',c('non','oui'),6); 
  belong9('o',c('non','oui'),8); 
  belong9('O',c("oui")); 
  belong9('O',c("oui"),lower=TRUE); 
  belong9(c('o','n'),c('non','oui'),6,how='v'); 
  belong9(c('o','n'),c('non','oui'),8,how='v'); 
  belong9(c("o","oui"),c('non','oui')); 
  belong9(c("o","oui"),c('non','oui'),how="v"); 
  belong9(c("A[SEX]","A[AGE]"),c("A[AGE]","A[SEX]"),how="a") 
} 
% 
\details{ 
   
  More possibilities exists when \samp{exact} is \samp{6} or \samp{8}. 
  Namely that the substring must be at the end or at the beginning of the 
  reference chain. 
   
} 
% 
\description{ 
   
  Checks if some string(s) belong(s) to a series of strings. 
  When \samp{exact} is FALSE, returns TRUE if the 
  character string \samp{sch} is included at least once 
  into the character string \samp{ch}.\cr 
  \samp{sch} can be a vector, in that case, the check is made for each 
  of its components. According to \samp{how} the returned 
  value is vectorial or scalar. When \samp{sch} is zero length then 
  \samp{TRUE} is returned. When \samp{ch} is zero length 
  then \samp{FALSE} is returned except when \samp{length(sch)} is zero. 
   
} 
% 
\value{ 
   
  A logical vector with length of \samp{sch}; or \samp{TRUE} or \samp{FALSE} when 
  \samp{how} is \samp{1} or \samp{a}. 
   
} 
% 
 
\arguments{ 
    \item{sch}{The character string(s) to be found.} 
    \item{ch}{The character string(s) to investigate.} 
    \item{exact}{ When exact, one component must be strictly identical, if not a subtring is sufficient. See also the \bold{Details} section for two other possibilities.} 
    \item{how}{ Indicates what to do when \samp{length(sch)>1}. The choice are \samp{v}: a logical vector gives back each check independently; \samp{1}: returns \samp{TRUE} when at least one of the component belongs to the series \samp{ch} and \samp{a} when all components must comply to get TRUE.} 
    \item{lower}{ Must the comparisons being done after case lowering?} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{utilities} 
\keyword{belong} 
