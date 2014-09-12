% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{text3translate} 
% 
\alias{text3translate} 
% 
\title{translate words} 
% 
\usage{ 
  text3translate(words,translation,
                 left=TRUE,eline=TRUE) 
} 
% 
\examples{ 
  tra <- c(a="A",e="E",i="I",o="O",u="U") 
  text3translate(letters,tra); 
  text3translate(letters,tra,FALSE); 
  text3translate(letters,tra,FALSE,FALSE); 
  text3translate(rbsa0$text3$v,c(The="xxx",the="xxx")); 
} 
% 
\details{ 
   
  When a word has no translation, it is left as it is 
  when \samp{left} is \samp{TRUE}, suppressed if not. 
  Initial spacing is not respected, ending blanks are 
  suppressed and no repeated blanks are separating the 
  final words. 
   
} 
% 
\description{ 
   
  returns a \samp{character()} deduced from 
  \samp{words} replacing some of the words 
  by the translation proposed into \samp{translation}. 
  A component of \samp{words} can comprise several 
  common words. 
   
} 
% 
\value{ 
   
  The resulting \samp{character} after translation. 
   
} 
% 
 
\arguments{ 
    \item{words}{ A \samp{character()} of words .} 
    \item{translation}{ A named \samp{character} vector containing the translations through the names: \samp{names(translation)[i]} becomes \samp{translation[i]}.} 
    \item{left}{ When \samp{TRUE} non translated words are left.} 
    \item{eline}{ Indicates if resulting empty lines must be left.} 
} 
 
% 
\keyword{text} 
\keyword{translate} 
