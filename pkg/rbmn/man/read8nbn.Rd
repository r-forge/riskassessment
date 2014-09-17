% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{read8nbn} 
% 
\alias{read8nbn} 
% 
\title{reads from a text file a /nbn/ object.} 
% 
\usage{ 
  read8nbn(file,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(rbmn0$nbn1$v); 
  write8nbn(rbmn0$nbn1$v,file="toto.txt"); 
  print8nbn(read8nbn("toto.txt")); 
  unlink("toto.txt"); 
} 
% 
\details{ 
   
  The function \samp{sssread8list} is called to read the file so standard 
  tags \samp{<<...>>} and \samp{[[...]]} can be replaced according the 
  user preferences.\cr 
  Comments and more things can be introduced in the file, see 
  the \samp{sssread8list} documentation for full details.\cr 
  For the moment, if the implicit graph proposed in the file 
  comprises loops, this is not detected and provokes an 
  infinite looping of the function! 
   
} 
% 
\description{ 
   
  reads from the text file \samp{file} 
  following a precised format an object describing 
  in the natural way a normal Bayesian network.\cr 
  For a /nbn/ with \samp{n} nodes, the file comprises 
  \samp{n} main fields each one associated to a different 
  node. The order is supposed to be the choosen order; 
  it must be a topological one, if not a topological 
  order will be found and imposed. Names of the nodes are 
  the field names.\cr 
  Each node field comprises four possible subfields. \samp{mu}, 
  the conditional expectation of the node when all its parents 
  has got a zero value; default value \samp{0}. 
  \samp{sigma}, the conditional standard deviation of the node; 
  default value \samp{1}. \samp{parents}, the list of possible parents 
  (indicated by their names); default value \samp{NULL}. \samp{regcoef}, 
  values of the regression associated to the parents in the same order; 
  default values: \samp{1} for each. 
   
} 
% 
\value{ 
   
  a /nbn/ object, for the moment a mere list. 
   
} 
% 
 
\arguments{ 
    \item{file}{file to be read.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{normal--Bayesian--network} 
\keyword{file} 
\section{Future}{ 
Checks cycles to avoid infinite loops 
} 
