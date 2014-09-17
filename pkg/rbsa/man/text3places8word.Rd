% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{text3places8word} 
% 
\alias{text3places8word} 
% 
\title{returns the places of one word} 
% 
\usage{ 
  text3places8word(text,word,
                   column=c(1,Inf),
                   which=c(1,Inf)
                   ) 
} 
% 
\examples{ 
  text3places8word(letters,"j"); 
  text3places8word(letters,"J"); 
  text3places8word(c("Il etait une fois","un petit et rouge chaperon"),"et"); 
  text3places8word(c("Il etait une fois","un petit et rouge chaperon"),"et",which=c(2,3)); 
  text3places8word(c("Il etait une fois","un petit et rouge chaperon"),"et",which=-c(1,3)); 
  text3places8word(c("# Il etait une fois"," #un petit et rouge chaperon"),"#"); 
  text3places8word(c("# Il etait une fois"," #un petit et rouge chaperon"),"#",column=c(1,2)); 
  text3places8word(c("# Il etait une fois"," #un petit et rouge chaperon"),"#",column=c(2,2)); 
  # overlapping pattern 
  text3places8word("aaaa","aa"); 
} 
% 
\details{ 
   
  The word cannot be extended upon two successive lines but the same line 
  can have more than one word. Be aware that overlapping patterns are not 
  all detected (see one of the examples). 
   
} 
% 
\description{ 
   
  from a text, 
  gives back the positions of a word indicated by 
  the associated intervals as defined in the function \samp{text3interval}. 
  The search can be constrained to specific columns. The output can 
  also be restricted. 
   
} 
% 
\value{ 
   
  A four column matrix, each row corresponding to a word place with the help of 
  an interval.\cr 
  For negative values of \samp{which}, the order of occurences is reversed: the last 
  found will be in the first row of the matrix output. 
   
} 
% 
 
\arguments{ 
    \item{text}{ A \samp{character} vector containing the text (a component, a line).} 
    \item{word}{ \samp{character(1)} the word to be found.} 
    \item{column}{ The columns where the first character of the word must found. \samp{c(1,1)} means that it must be at the very start of a line. \samp{c(10,12)} means that it must start on the 10th, 11th or 12th column of a line.} 
    \item{which}{Which occurences of \samp{word} (not the line numbers) must be returned defined by the number of the first one and the number of the last one.\cr So \samp{c(2,2)} will designate the second and only the second; \samp{c(1,5)} will ask for the first five. When the components are both negative, the numbering is done from the end, so \samp{c(-1,-1)} means the last one and \samp{c(-1,-3)} asks for the last three ones given starting from the last. } 
} 
 
% 
\keyword{position} 
\keyword{text} 
\keyword{word} 
\section{Future}{ 
Think of a way to introduce "end of line" as a possible word. 
Improve the case of overlapping patterns. 
} 
