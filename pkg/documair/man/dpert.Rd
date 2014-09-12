% 
% created by /documair/ at 14_09_12@10:34 
% 
\name{dpert} 
% 
\alias{dpert} 
\alias{ppert} 
\alias{qpert} 
\alias{rpert} 
% 
\title{The (Modified) PERT Distribution} 
% 
\usage{ 
  dpert(x,min=-1,mode=0,max=1,shape=4,log=FALSE) 
  ppert(q,min=-1,mode=0,max=1,shape=4,lower.tail=TRUE,log.p=FALSE) 
  qpert(p,min=-1,mode=0,max=1,shape=4,lower.tail=TRUE,log.p=FALSE) 
  rpert(n,min=-1,mode=0,max=1,shape=4) 
} 
% 
\examples{ 
  curve(dpert(x,min=3,mode=5,max=10,shape=6), from = 2, to = 11, lty=3) 
  curve(dpert(x,min=3,mode=5,max=10), from = 2, to = 11, add=TRUE) 
  curve(dpert(x,min=3,mode=5,max=10,shape=2), from = 2, to = 11, add=TRUE,lty=2) 
  legend(x = 8, y = 2, c("Default","shape:2","shape:6"), lty=1:3) 
} 
% 
\details{ 
   
  The PERT distribution is a beta distribution extended to the domain \samp{[min, max]} with mean 
  \deqn{\mu=\frac{min+shape\times mode+max}{shape+2}}{mu = (min + shape * mode + max)/(shape + 2)} 
  The underlying beta distribution is specified by \eqn{\alpha_{1}}{shape1} and \eqn{\alpha_{2}}{shape2} defined as 
  \deqn{\alpha_{1}=\frac{(\mu-min)(2\times mode-min-max)}{(mode-\mu)(max-min)}}{shape1=(mu - min)*(2 mode-min-max)/((mode-mu)*(max - min))} 
  \deqn{\alpha_{2}=\frac{\alpha_{1}\times (max-\mu)}{mu-min}}{shape2=shape1*(max - mu)/(mu - min)} 
  If \eqn{\mu=mode}{mu=mode}, \eqn{\alpha_{1}}{shape1} is set to \eqn{1+\nu/2}{1+shape/2}. 
  David Vose proposed a modified PERT distribution with a shape parameter different from 4. 
  The PERT distribution is frequently used to translate expert estimates 
  of the min, max and mode of a random variable in a smooth parametric distribution. 
   
} 
% 
\description{ 
   
  ((just an example extracted from the \pkg{mc2d} package to illustrate the use 
  of reduced documentation with a series of related and aliased functions.))\cr 
  Density, distribution function, quantile function and random generation 
  for the PERT (\emph{aka} Beta PERT) distribution with minimum equals to \samp{min}, mode equals to \samp{mode} 
  and maximum equals to \samp{max}. 
   
} 
% 
\value{ 
   
  \samp{dpert} gives the density, \samp{ppert} gives the distribution function, 
  \samp{qpert} gives the quantile function, and \samp{rpert} generates random deviates. 
   
} 
% 
 
\arguments{ 
    \item{x,q}{Vector of quantiles.} 
    \item{p}{Vector of probabilities.} 
    \item{n}{Number of observations. If length(n) > 1, the length is taken to be the number required.} 
    \item{min}{Vector of minima.} 
    \item{mode}{Vector of modes.} 
    \item{max}{Vector of maxima.} 
    \item{shape}{Vector of scaling parameters. Default value: 4.} 
    \item{log, log.p}{Logical; if \samp{TRUE}, probabilities \samp{p} are given as \samp{log(p)}.} 
    \item{lower.tail}{Logical; if \samp{TRUE} (default), probabilities are \samp{P[X <= x]}, otherwise, \samp{P[X > x]}.} 
} 
 
% 
\keyword{distribution} 
