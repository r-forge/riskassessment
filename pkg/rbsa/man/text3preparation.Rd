% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{text3preparation} 
% 
\alias{text3preparation} 
% 
\title{Analyses and prepares a /text/} 
% 
\usage{ 
  text3preparation(text,preparation="rbR",
                   content=c(1,length(text)),
                   ccom="#",
                   llength=50) 
} 
% 
\examples{ 
  text3preparation(rbsa0$text2$v[1:3],preparation="j",llength=10) 
  uu <- c("Il etait une fois un petit et rouge chaperon", 
  "qui voulait aller voir sa mere-grand"); 
  text3preparation(uu,"j",llength=20); 
  text3preparation(uu,"j",llength=80); 
  text3preparation( c(" Je veux   voir  "," et re-voir  "),"rbBc") 
  text3preparation(c("# Je veux   voir  "," et re-voir  "),"rbBc") 
} 
% 
\description{ 
   
  from a text returns another text after some 
  transformations.\cr 
  Not intended for the standard user. 
   
} 
% 
\value{ 
   
  The transformed text, that is a \samp{character}. 
   
} 
% 
 
\arguments{ 
    \item{text}{ A \samp{character} vector containing the text to prepare.} 
    \item{preparation}{A \samp{character(1)} whose characters indicates the actions to perform (in this order): \cr\samp{r} to remove the starting characters of \samp{ccom}. \cr\samp{b} to remove 'blank' characters at both ends of lines. \cr\samp{B} to remove repeated 'blank' characters within lines. \cr\samp{R} to remove empty lines. \cr\samp{w} to return the first word of the first line delimited with either blank, \samp{=} of \samp{<-}. \cr \cr The following options are hierarchized (for instance \samp{j} is equivalent to \samp{cvVSj}). \cr\samp{c} to concatanate lines (between lists if any) into a unique line; one space is added between two initial lines. \cr\samp{v} to return the vector with one word one component from the concatenated line of \samp{c}. \cr\samp{V} the same as \samp{v} but eliminated repeated words. \cr\samp{S} the same as \samp{v} but sorting the found words. \cr\samp{j} to gather the words obtained after \samp{v} into text components having less that \samp{llength} characters or only one word.} 
    \item{content}{ Indicates which component of \samp{text} to prepare. Usually \samp{numeric(2)} to indicate the interval of lines to consider. when \samp{numeric(1)}, only this line. When \samp{0} or \samp{diff(content)<0} \samp{character(0)} is returned.} 
    \item{ccom}{A \samp{character(1)} indicating which character(s) at the beginning of lines must possibly be removed (the character(s), not the complete line). } 
    \item{llength}{Maximum number of characters for a line (except when it comprises only one word).} 
} 
 
% 
\keyword{preparation} 
\keyword{text} 
\section{Future}{ 
Other functionalities could be added. 
} 
