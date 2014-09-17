% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{generate8grid7crossed} 
% 
\alias{generate8grid7crossed} 
% 
\title{creates a grid crossed nbn with covariates} 
% 
\usage{ 
  generate8grid7crossed(
                        nrow=3,ncol=3,xrow=1,xcol=1,
                        rhorow=matrix(0.5,nrow-1,ncol),
                        rhocol=matrix(0.5,ncol-1,nrow),
                        xrhorow=matrix(0.5,nrow,xrow),
                        xrhocol=matrix(0.5,ncol,xcol),
                        sigmas=matrix(1,nrow,ncol)
                        ) 
} 
% 
\examples{ 
  print8nbn(generate8grid7crossed()$nbn,check=TRUE); 
  print8nbn(generate8grid7crossed(rhorow=matrix(1:2,2,3))$nbn,check=TRUE); 
  rr <- matrix(1:2,2,3); 
  rc <- matrix(10*(1:2),2,3); 
  xrr <- matrix(1:3,3); 
  xrc <- matrix(100*(1:3),3); 
  uu <- generate8grid7crossed(rhorow=rr,rhocol=rc,xrhorow=xrr,xrhocol=xrc); 
  print8nbn(uu$nbn,check=TRUE); 
} 
% 
\details{ 
   
  Let \samp{Y(i,j)} the node of the i-th row and j-th column of the grid; 
  \samp{i} varies from \samp{1} to \samp{nrow} and \samp{j} varies from \samp{1} 
  to \samp{ncol}. The children of \samp{Y(i,j)} are \samp{Y(i+1,j)} and \samp{Y(i,j+1)} 
  when these child nodes exist.\cr 
  So called covariate nodes are \samp{XR(h)} (\samp{h} varying from \samp{1} to \samp{xrow} 
  and \samp{XC(k)} (\samp{k} varying from \samp{1} to \samp{xcol}. The possible children of each 
  \samp{XR(h)} are the \samp{nrow} nodes \samp{Y(i,1)} and those of each \samp{XC(k)} are 
  the \samp{ncol} nodes \samp{Y(1,j)}.\cr 
  No more children are possible: \samp{(nrow-1)*(ncol-1)} within the \samp{Y}s, 
  \samp{xrow*nrow} from the \samp{XR}s and \samp{xcol*ncol} from the \samp{XC}s.\cr 
  All marginal expectations are null. Marginal variances of the covariates are one. 
   
} 
% 
\description{ 
   
  Creates crossed \samp{nbn} following a grid of \samp{nrow} rows and \samp{ncol} 
  columns; plus \samp{xrow} covariables associated to the first column nodes 
  and \samp{xcol} covariables associated to the first row nodes. 
   
} 
% 
\value{ 
   
  A list comprising the following components. (i) \samp{$nbn}: the resulting \samp{nbn} object; 
  (ii) \samp{vari}: names of the variables (in a matrix); 
  (iii) \samp{rcova}: names of the row covariates; 
  (iv) \samp{ccova}: names of the column covariates; 
  (v) \samp{$xy}: a two column matrix proposing convenient positions for the nodes 
  to draw the associated DAG; 
  (vi) \samp{$cova2vari}: the matrix of arcs going from a covariate to a variable; 
  (vii) \samp{$vari2cova}: the matrix of arcs going from a variable to a covariate; 
  (viii) \samp{$row2row}: the matrix of arcs going from a variable to another variable 
  having a greater row number; 
  (ix) \samp{$col2col}: the matrix of arcs going from a variable to another variable 
  having a greater column number. 
   
} 
% 
 
\arguments{ 
    \item{nrow}{ Number of rows for the grid (>1).} 
    \item{ncol}{ Number of columns for the grid (>1).} 
    \item{xrow}{ Number of covariabes for the rows (>=0).} 
    \item{xcol}{ Number of covariates for the columns (>=0).} 
    \item{rhorow}{ Regression coefficients between \samp{Y(i,j)} and \samp{Y(i+1,j)}.} 
    \item{rhocol}{ Regression coefficients between \samp{Y(i,j)} and \samp{Y(i,j+1)}.} 
    \item{xrhocol}{ Regression coefficients between \samp{XR(h)} and \samp{Y(i,1)}.} 
    \item{xrhorow}{ Regression coefficients between \samp{XC(k)} and \samp{Y(1,j)}.} 
    \item{sigmas}{ Conditional standard deviations (not variances!) of the \samp{Y} nodes.} 
} 
 
% 
\keyword{crossed} 
\keyword{generation} 
