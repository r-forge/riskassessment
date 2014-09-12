% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{text3replace} 
% 
\alias{text3replace} 
% 
\title{translating some words} 
% 
\usage{ 
  text3replace(text,translate,lowering=FALSE,
               b.accepted=NULL,a.accepted=NULL,
               b.rejected=NULL,a.rejected=NULL,
               b.line=TRUE,e.line=TRUE) 
} 
% 
\examples{ 
  let <- c("a","e","i","o","u","y"); 
  tra <- matrix(c(let,tolower(let)),ncol=2); 
  text3replace(letters,tra); 
  text3replace(c("Il etait une fois","un petit et rouge chaperon"),tra); 
  text3replace(c("Il etait une fois","un petit et rouge chaperon"),tra,b.accepted=" "); 
} 
% 
\details{ 
   
  The words to translate cannot be extended upon two successive lines.\cr 
  The translations are done sequentially along the rows of \samp{translate} 
  so modifying their order can produce different results.\cr 
  For obvious reason, \samp{b.accepted} and \samp{b.rejected} cannot be 
  non \samp{NULL} simultaneously; the same for \samp{a.accepted} and 
  \samp{a.rejected}.\cr 
  Resulting empty lines are not removed neither already present empty lines. 
   
} 
% 
\description{ 
   
  from a text, 
  gives it back after replacing some words (in fact sequences of characters) 
  by others. The translation can be conditionned by the surrounding 
  characters of the sequence. 
   
} 
% 
\value{ 
   
  The translated \samp{text} 
   
} 
% 
 
\arguments{ 
    \item{text}{ A \samp{character} vector containing the text (a component, a line).} 
    \item{translate}{ A two-columns character matrix with, row by row, a word to translate and its translations. The second column can comprise \samp{""} not the first.} 
    \item{lowering}{ Must a lowering being applied before the selection of the words to translate?} 
    \item{b.accepted}{ A \samp{character} providing the surrounding characters before the words to determine the acceptance of the translation.} 
    \item{a.accepted}{ A \samp{character} providing the surrounding characters after the words to determine the acceptance of the translation.} 
    \item{b.rejected}{ A \samp{character} providing the surrounding characters before the words to determine the rejection of the translation.} 
    \item{a.rejected}{ A \samp{character} providing the surrounding characters after the words to determine the rejection of the translation.} 
    \item{b.line}{ Must words at the beginning of a line be translated?} 
    \item{e.line}{ Must words at the end of a line be translated?} 
} 
 
% 
\keyword{replace} 
\keyword{text} 
\seealso{ 
\code{\link{text3acceptance}} 
} 
