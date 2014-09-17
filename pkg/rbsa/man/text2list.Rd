% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{text2list} 
% 
\alias{text2list} 
\alias{list2text} 
% 
\title{transforms a character into a list (of lists) of characters, and conversely} 
% 
\usage{ 
  text2list(text,
            tags=rbsa0$tag1$v,
            sep=rbsa0$sep0$v,
            rsep="no_action",
            stag=c("/","/"),
            comment="#",
            monitor=rbsa0$monitor$v
            ) 
  list2text(lili,
            tags=rbsa0$tag1$v,
            stag=c("/",";","/"),
            comment="#",
            comments=character(0),
            monitor=rbsa0$monitor$v
            ) 
} 
% 
\examples{ 
  ## 
  ## text2list 
  text <- c("<<A>>", 
  "[[a]]/*/v 1*un deux trois", 
  "[[b]]/*/v 1*2*3", 
  "un uno one", 
  "deux dos two", 
  "trois tres three", 
  "<<B>>", 
  "[[a]] un deux trois", 
  "[[b]] un  uno  one", 
  " deux dos two", 
  "trois tres three", 
  "<<C>> 1 2 3"); 
  text2list(text); 
  list2text(text2list(text)); 
  ## 
  ## list2text 
  list2text(rbsa0$lis1$v); 
  text2list(list2text(rbsa0$lis1$v)); 
} 
% 
\details{ 
   
   (text2list) 
  It is compulsory to tag each level of the lists, this implies that 
  when starting a new list, a character vma (see \samp{text2vma} 
  for the details: vma means vector or matrix or array) is provided meaning 
  that this is the final level for this branch, or a new sublist 
  is started with the corresponding tag, or a new component of the 
  list is given at a level less or equal to the present.\cr 
  Separator between different character components is given by the 
  \samp{sep} argument or indicated by the separator tag (\samp{stag}), 
  in the last case, it can be different from a leaf list to another.\cr 
  Be aware that before reading a line for a character translation, 
  all starting and ending spaces are eliminated.\cr 
  \cr(list2text) 
  The character strings of the structure must not comprise 
  the \samp{rbsa0$sep0$v} constant but this global constant can be 
  conveniently modified. The same for \samp{rbsa0$sep1$v}.\cr 
  Use is made of the general constant \samp{monitor$chk$v} for the checking. 
   
} 
% 
\description{ 
   
   (text2list) 
  from a conveniently character vector comprising tags, returns nested lists. 
  Most often such a character is obtained by reading a file with 
  the function \samp{file2text}, this explains why it is convenient 
  to speak about 'lines' rather about 'components of the character 
  vector'.\cr 
  All lists are named lists and the tags give the names of their 
  component. The maximum number of nested levels of lists is given by the 
  number of rows of the matrix \samp{tags}. Its corresponding two columns 
  providing the opening and closing sequences of the tags. Final lists 
  contain \samp{character} vectors, each component of them being 
  on the same line and/or on the following line (before a new tag).\cr 
  All tags must start at the very beginning of a line. All separator 
  tags must be used sticked to the list tag.\cr 
  Lines starting with a \samp{comment} are first eliminated. 
  \cr (list2text)  
  The reverse operation of \samp{text2list}. The list must be a 
  rsbsa-list, that is complying 
  some properties : all components of the [sub-]lists must be either 
  a list or a [named] character vector/matrix/array. The number of nested list 
  must not be greater than the number of rows in matrix \samp{tags}. 
  Every list component must be named.\cr 
  The idea is to get a character compatible with \samp{text2list} to produce back 
  the object \samp{lili}.\cr 
  Some comments are added to the content of the list by the function itself and/or 
  according to the whish of the user, this is why an escaping character 
  for comments is asked. 
   
} 
% 
\value{ 
   
   (text2list) 
  a list [of lists [of lists [...] ] ] of character (possibly named) vectors 
  or matrices or arrays. 
  \cr (list2text)  
  The resulting character. 
   
} 
% 
 
\arguments{ 
    \item{text}{ character vector to be transformed into a list.} 
    \item{lili}{ The list structure to be transformed into a \samp{character}.} 
    \item{tags}{ Character matrix with two columns indicating the opening and closing tags of the lists at different levels. Its row numbers gives the maximum number of levels. Opening tags must be different.} 
    \item{sep}{ Character sequence used to split the character vectors of every line. Notice that \samp{LF} is always considered as a separator.} 
    \item{rsep}{ Indicates if repetitions of \samp{sep} must be considered as significant or not and which null value to introduce. If \samp{no_action} then the repetitions will be ignored if not \samp{rsep} component(s) will be introduced.} 
    \item{stag}{ Two character strings indicating the tag to define different \samp{sep} for a given [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{list2file} function.} 
    \item{comment}{ Set a characters indicating that the line is a comment line when found in the first positions.} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
    \item{comments}{Comments that the user want to be added at the beginning of the file.} 
} 
 
% 
\keyword{input/output} 
\keyword{list} 
\keyword{text} 
\keyword{input/output} 
\section{Future}{ 
Extend the possibility of the list by allowing 
contents at the intermediate levels (not only 
at the final levels as it is presently). 
} 
\seealso{ 
\code{\link{text2file}} 
\code{\link{file2list}} 
} 
