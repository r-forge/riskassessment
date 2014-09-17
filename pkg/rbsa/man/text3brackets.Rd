% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{text3brackets} 
% 
\alias{text3brackets} 
% 
\title{returns the content of pairs of brackets} 
% 
\usage{ 
  text3brackets(text,bra=c("{","}"),
                col1=c(1,Inf),col2=c(1,Inf),
                rembra=TRUE,
                which=c(1,Inf)) 
} 
% 
\examples{ 
  text3brackets(paste(letters,collapse=""),c("j","u")); 
  text3brackets(c(" juste {un","deux ou trois} suffiront !")); 
} 
% 
\details{ 
   
  Tags cannot be upon two successive lines. The same line 
  can have more than one tag. The result is built by an analysis 
  of the result provided by \samp{text3places8word}.\cr 
  When an opening tag is discovered, the following opening 
  tag is not considered before a closing tag has been encountered 
  for the previous one. 
  So the sequence \samp{"\{ toto \{tut\} bof\} \{deux\}"} will gives two 
  contents, respectively \samp{" toto \{tut"} and \samp{"deux"}.\cr 
  When an opening tags remains not consistently closed a fatal 
  error is issued with some indication. 
   
} 
% 
\description{ 
   
  from a text, 
  gives back the contents of a couple of tags (opening and 
  closing tags) indicated by \samp{bra} under the constraint 
  that the first character of each delimiter be in the column 
  interval of each line indicated with \samp{col1} (respectively 
  \samp{col2}). 
   
} 
% 
\value{ 
   
  A \samp{list} of texts. Portion within two braces are eliminated. 
   
} 
% 
 
\arguments{ 
    \item{text}{ A \samp{character} vector containing the text (a component, a line).} 
    \item{bra}{ \samp{character(2)} the pair of tags to use.} 
    \item{col1}{Positions within a line where the opening bracket has to be found.} 
    \item{col2}{Positions within a line where the closing bracket has to be found.} 
    \item{rembra}{Must the brackets be removed?} 
    \item{which}{Which occurence of \samp{tag} (not the line numbers) must be returned defined by the the number of the first one and the number of the last one.} 
} 
 
% 
\keyword{brackets} 
\keyword{text} 
\section{Future}{ 
Think to a way to introduce "end of line" as a possible tag. 
} 
