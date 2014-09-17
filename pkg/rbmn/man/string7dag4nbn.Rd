% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{string7dag4nbn} 
% 
\alias{string7dag4nbn} 
% 
\title{provides so-called string model of a /nbn/} 
% 
\usage{ 
  string7dag4nbn(nbn,sep=";",check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  string7dag4nbn(rbmn0$nbn1$v,check=TRUE); 
  string7dag4nbn(rbmn0$nbn4$v,sep=",",check=TRUE); 
} 
% 
\description{ 
   
  returns a \samp{character(1)} 
  describing the dag of the nbn under 
  the string form. 
   
} 
% 
\value{ 
   
  A \samp{character(1)}. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The nbn.} 
    \item{sep}{Separation sign between parents after the conditioning sign (\samp{|}).} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{DAG} 
\keyword{normal--Bayesian--network} 
\section{Future}{ 
Do it through two new functions 'on-work': \samp{adja2string7dag} and \samp{string7dag2adja} 
} 
