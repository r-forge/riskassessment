% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{parse8text} 
% 
\alias{parse8text} 
% 
\title{returns the parsed content of a text} 
% 
\usage{ 
  parse8text(text,item1=c("{","}"),item2=c("<<",">>"),
             numb="#",bull="*",lsep="-") 
} 
% 
\examples{ 
  parse8text(c("{a}","<<","pour voir",">>")); 
  uu <- c("1rst paragraph","","2d paragraph","", 
  "{#} <<un>>","{#}","<<deux>>","","3rd and last paragraph"); 
  parse8text(uu); 
  vv <- c("1rst paragraph","","2d paragraph","", 
  "{AA} <<un>>","{BBB}","<<deux>>","","3rd and last paragraph"); 
  parse8text(vv); 
  parse8text(rbsa0$text4$v); 
} 
% 
\details{ 
   
  Each item of a list must comprise two values, framed with \samp{item1} and 
  \samp{item2}. When the first value is \samp{numb}, it is a numbered 
  item; when the first value is \samp{bull}, it is a bullet item; 
  if not it is a labelled item.\cr 
  Each component, paragraphs and items are supposed to be proposed on 
  non overlapping lines.\cr 
  Successive items are considered to belong to the list of items knowing 
  that empty lines (comprising zero characters) are first eliminated 
  (a line with a blank is not empty and will be considered as a paragraph). 
  Also are eliminated lines starting with \samp{lsep}, their role is to 
  separate distinct paragraphs and lists.\cr 
  When the braces for items are not consistent, no error is reported but the 
  staff is interpreted as part of a paragraph.\cr 
  When two list items have got identical labels, an error is reported. 
   
} 
% 
\description{ 
   
  from a text comprising paragraphs and items 
  finds the different components and returns them by means of a list. 
  When the component is a paragraph, it is a \samp{character}, 
  When the component is an item list, it is a named list.\cr 
  This function is not intended for standard users. 
   
} 
% 
\value{ 
   
  A named \samp{list}. The names for paragraphs start with \samp{P}, those 
  for item lists with \samp{L}. 
   
} 
% 
 
\arguments{ 
    \item{text}{ The \samp{text} to be parsed. For the moment just a \samp{character} vector.} 
    \item{item1}{ \samp{character(2)} the pair of tags to use to define the first value of an item. When \samp{numb} interpreted as an enumeration, when \samp{*} interpreted as an itemized list, if not a description list. The first character of \samp{item1[1]} must start at the beginning a line and the two braces must be on the same line.} 
    \item{item2}{ \samp{character(2)} the pair of tags to use to define the second value of an item.} 
    \item{numb}{ \samp{character(1)} code to indicate automatically numbered items.} 
    \item{bull}{ \samp{character(1)} code to indicate bullet items.} 
    \item{lsep}{ \samp{character(1)} Each line starting with \samp{lsep} is considered a tagging line to separate two paragraphs or two item lists. They can be used to separate a paragraph and an item list but are useless. Separating lines within list items are not considered as separating. Successive separating lines are considered as a unique separating line. They are eliminated in the resulting list.} 
} 
 
% 
\keyword{parsing} 
\keyword{text} 
