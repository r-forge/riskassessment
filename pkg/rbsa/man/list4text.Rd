% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{list4text} 
% 
\alias{list4text} 
% 
\title{Analyses and prepares a list from a /text/} 
% 
\usage{ 
  list4text(text,content=c(1,length(text)),ccom="#",
            lndel=c("{","}"),lvdel=c("<<",">>")) 
} 
% 
\examples{ 
  list4text(c("{a}<<Il etait une fois>>", 
  "{b}<<   un petit et rouge chaperon>>{c}", 
  "<< pas bon>>", 
  "{d}    <<et le m'echant loup...>>")) 
  list4text(c("{a}","<<","pour voir",">>")); 
} 
% 
\details{ 
   
  Useless lines can exist before and after the list, and also 
  between the list items. 
   
} 
% 
\description{ 
   
  from a text with convenient tags 
  returns a named list. Labelling tags must be 
  in first position of the lines, possibly after removing 
  some precised character. 
   
} 
% 
\value{ 
   
  A named list. 
   
} 
% 
 
\arguments{ 
    \item{text}{ A \samp{character} vector containing the text from which to extract the list.} 
    \item{content}{ Indicates which components of \samp{text} to consider. Usually \samp{numeric(2)} to indicate the interval of lines to consider. When \samp{numeric(1)}, only this line. When \samp{0} or \samp{diff(content)<0} a \samp{list(0)} is returned.} 
    \item{ccom}{Character(s) at the beginning of lines to possibly be removed. When \samp{character(0)} no removing is performed.} 
    \item{lndel}{Starting and ending delimiters for the name of the item (at the beginning of the line).} 
    \item{lvdel}{Starting and ending delimiters for the description of the item.} 
} 
 
% 
\keyword{list} 
\keyword{text} 
