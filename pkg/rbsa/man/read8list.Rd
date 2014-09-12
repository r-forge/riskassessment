% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{read8list} 
% 
\alias{read8list} 
% 
\title{reads a file and transforms it in a list (of lists) of characters} 
% 
\usage{ 
  read8list(file,path=getwd(),bullet=")BULLETS(",
            clean=TRUE,ended=")STOP(",comment="#",
            skip=matrix(c(")SKIPPING(",")READING("),1),
            include=c(")FILE("),
            tags=rbsa0$tag1$v,
            sep=rbsa0$sep0$v,
            rsep="no_action",
            stag=c("/","/"),
            monitor=rbsa0$monitor$v
            ) 
} 
% 
\examples{ 
  sink("rbsa.list.txt") 
  cat(rbsa0$text5$v,sep="\n"); 
  sink(); 
  read8list("rbsa.list.txt"); 
  # deleting the created file 
  unlink("rbsa.list.txt"); 
  # deleting the intermediate file 
  unlink(rbsa0$fin$v); 
} 
% 
\details{ 
   
  An intermediary file is generated (and not deleted) to be read by 
  \samp{file2list} function; its name is obtained from the global constant 
  \samp{rbsa0$fin$v}.\cr 
  The numbering of the levels of the list is determined by cycling the 
  global constant \samp{rbsa0$tyl$v} (run the example to have an idea of what this means).\cr 
  Only the first three arguments plus the tags are used by this function, all others are 
  directly passed to \samp{file2list}.\cr 
  For the moment, the numbering of the different levels does not take 
  into account the level hierarchy, this must be offered in 
  future versions of this function.\cr 
   
} 
% 
\description{ 
   
  Reads a file and transforms it in a list (of lists) of characters 
  It is a slight generalization of \samp{file2list}.\cr 
  It is worth explaining why such a function was written. 
  The obvious one is that not always names for the components of the 
  list are natural. (In future version, it could be good to mixed with 
  and without names). Another reason is that the contraint of 'value 
  only at the final level is not always natural. \samp{read8list} adds 
  itself 'null' components (named \samp{rbsa0$l00$v}) 
  to allow it. Finally, levels can be skipped 
  and missing levels are introduced.\cr 
  So the generalization is that 
  the components of the list have not to be given 
  in the file but are determined by the function from an additionnal 
  line (everywhere in the file) tagged with the third argument 
  starting at the first character. 
  This line contains the list of bullets, giving then their number. 
  Notice that all proposed bullets must be different from 
  \samp{rbsa0$l00$v}. 
  But everything has to be paid: now the constraint is 
  that every component must have a non empty content. 
   
} 
% 
\value{ 
   
  a list [of lists [of lists [...] ] ] of character (possibly named) vectors 
  or matrices or arrays. 
   
} 
% 
 
\arguments{ 
    \item{file}{ file to be read and transformed into a list.} 
    \item{path}{ Directory containing the file.} 
    \item{bullet}{Indicates the bullets to be considered. This tags must be at the very beginning of a line. But the bullets can be preceeded by spaces.} 
    \item{clean}{Indicates if starting and ending spaces of every lines must be eliminated at first.} 
    \item{ended}{ To indicate the line from which to stop the reading.} 
    \item{comment}{At the beginning of a line, it indicates that this line must not be considered. More than one commenting character can be considered when it is a vector. For instance \samp{c("#","\%")} means that lines starting with an hash or a percent are comment lines.} 
    \item{skip}{ To indicate set(s) of lines to be skipped. Must be a character matrix where the two columns correspond respectively to the opening and closing tags, and where each row is associate to a couple of tags. Tags are considered successively following the order of these matrix rows.} 
    \item{include}{ Tags to indicate a file (including possible path) by a \samp{character(1)} to include at this point its contents as a text file with the same tags specifications. Including files can be recursive. } 
    \item{tags}{ Character matrix with two columns indicating the opening and closing tags of the lists at different levels. Its row number gives the maximum number of levels. Opening tags must be different.} 
    \item{sep}{ Character sequence used to split the components of character vectors provided within the same line. Notice that \samp{LF} is always considered as a separator.} 
    \item{rsep}{ Indicates if repetitions of \samp{sep} must be considered as significant or not and which null value to introduce. If \samp{no_action} then the repetitions will be ignored if not \samp{rsep} component(s) will be introduced.} 
    \item{stag}{ Two character strings indicating the tag to define different \samp{sep} for a given [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{list2file} function.} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{input/output} 
\keyword{list} 
\keyword{reading} 
\seealso{ 
\code{\link{list2file}} 
} 
