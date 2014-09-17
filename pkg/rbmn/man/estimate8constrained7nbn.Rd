% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{estimate8constrained7nbn} 
% 
\alias{estimate8constrained7nbn} 
% 
\title{estimates the parameters of a nbn with equality constraints} 
% 
\usage{ 
  estimate8constrained7nbn(nbn,smarc,data,
                           imp=0,nite=10,eps=10^-5,
                           check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(rbmn0$nbn5$v); 
  data(boco); 
  print8nbn(estimate8nbn(rbmn0$nbn5$v,boco)); 
  print8nbn(estimate8constrained7nbn(rbmn0$nbn5$v,rbmn0$arc5$v,boco)); 
} 
% 
\details{ 
   
  No constrained regression coefficients doesn't require to be included in 
  \samp{smarc}, the function do it by itself.\cr 
  The score to assess the differences between two successive 
  estimations is quite raw (see the code). 
   
} 
% 
\description{ 
   
  Estimations of the parameters of a /nbn/ is done when there 
  are some equality constraints onto the regression coefficients.\cr 
  Constant terms (\samp{mu}) and conditional standard deviations (\samp{sigma}) 
  are supposed free (that is not constrained with equalities).\cr 
  Equality constraints are given by \samp{smarc}, a list of matrices 
  with two columns, indicating each the series of arcs having equal 
  regression coefficients.\cr 
  The first aim of this function was to estimate specific 
  crossed Bayesian networks but can be applied to any 
  design of constraints; however no check is made about 
  the consistency of the set of constraints. 
   
} 
% 
\value{ 
   
  The resulting /nbn/ object with the estimated parameters. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{\samp{nbn} object.} 
    \item{smarc}{List of Matrices with two columns indicating the tails (1rst column) and the heads (2d column) of the arcs having a common parameter. It is checked that these arcs are indeed included in \samp{nbn}. Nodes must be indicated by their names (not their number).} 
    \item{data}{Data frame to be used for the estimation. It must comprise all necessary nodes (not only those involved in \samp{smarc} but also the remaining parents of \samp{smarc[,2]}. Usually, all used variables are centred but this is not required.} 
    \item{imp}{When \samp{0} nothing displayed. When \samp{1} the number of iterations is displayed. When \samp{2} the successive values of the criterion are also displayed. } 
    \item{nite}{Maximum number of iterations.} 
    \item{eps}{relative difference in successive scores needed to stop the iterations.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{estimation} 
\keyword{normal--Bayesian--network} 
