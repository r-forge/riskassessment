% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{form3join} 
% 
\alias{form3join} 
% 
\title{formats a series of names or a similar list} 
% 
\usage{ 
  form3join(vcara,none="-",
            OPA="{",CPA="}",
            opa="(",cpa=")",sep="+",
            imp=FALSE,cr=imp) 
} 
% 
\examples{ 
  form3join(letters[1:4]) 
  form3join(NULL); 
  form3join(NULL,NULL); 
  form3join(NULL,character(0)); 
  form3join(NULL,""); 
} 
% 
\description{ 
   
  returns a scalar character of the components 
  of a \samp{character} surrounded by any kind of parenthesis 
  (each and the whole) 
  separated with the same separator.\cr 
  Easy to undestand when applying the examples. 
   
} 
% 
\value{ 
   
  A character string or nothing when imp is TRUE 
   
} 
% 
 
\arguments{ 
    \item{vcara}{Character vector to be considered.} 
    \item{none}{ The internal result when \samp{vcara} is length zero. When it is \samp{character(0)} then the global parenthesis are not given contrary when it is \samp{""}.} 
    \item{OPA}{ The opening parenthesis to surround the entire list.} 
    \item{CPA}{ The closing parenthesis to surround the entire list.} 
    \item{opa}{ The opening parenthesis to surround each component.} 
    \item{cpa}{ The closing parenthesis to surround each component.} 
    \item{sep}{ The symbol to separate each component.} 
    \item{imp}{ Must the result be printed (with cat) or returned?} 
    \item{cr}{ Must a line feed be added at the end?} 
} 
 
% 
\keyword{print} 
\keyword{format} 
\keyword{format} 
\keyword{join} 
\seealso{ 
\code{\link{form3split}} 
} 
