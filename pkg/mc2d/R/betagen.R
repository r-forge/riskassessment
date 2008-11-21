#<<BEGIN>>
dbetagen <- function(x,shape1,shape2,min=0,max=1,ncp=0,log=FALSE)
#TITLE The Generalised Beta Distribution
#NAME betagen
#KEYWORDS distribution
#DESCRIPTION
#Density, distribution function, quantile function and random generation for the Beta distribution
#defined on the \code{[min, max]} domain with parameters \code{shape1} and \code{shape2} (
#and optional non-centrality parameter \code{ncp}).
#INPUTS
#{x,q}<<Vector of quantiles.>>
#{p}<<Vector of probabilities.>>
#{n}<<Number of observations. If \code{length(n) > 1}, the length is taken to be the number required.>>
#{shape1, shape2}<<Positive parameters of the Beta distribution.>>
#[INPUTS]
#{min}<<Vector of minima.>>
#{max}<<Vector of maxima.>>
#{ncp}<<Non-centrality parameter of the Beta distribution.>>
#{log, log.p}<<Logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.>>
#{lower.tail}<<Logical; if \code{TRUE} (default), probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}.>>
#DETAILS
#\deqn{x \sim  betagen(shape1, shape2, min, max, ncp)}{x ~ betagen(shape1, shape2, min, max, ncp)}
#if
#\deqn{\frac{x-min}{max-min}\sim beta(shape1,shape2,ncp)}{(x-min)/(max-min)~beta(shape1,shape2,ncp)}
#These functions use the \code{\link{Beta}} distribution functions after correct parametrisation.
#EXAMPLE
#curve(dbetagen(x,shape1=3,shape2=5,min=1,max=6), from = 0, to = 7)
#curve(dbetagen(x,shape1=1,shape2=1,min=2,max=5), from = 0, to = 7, lty=2, add=TRUE)
#curve(dbetagen(x,shape1=.5,shape2=.5,min=0,max=7), from = 0, to = 7, lty=3, add=TRUE)
#SEE ALSO
#\code{\link{Beta}}
#VALUE
#\code{dbetagen} gives the density, \code{pbetagen} gives the distribution function,
#\code{qbetagen} gives the quantile function, and \code{rbetagen} generates random deviates.
#AUTHOR Regis Pouillot
#CREATED 08-04-16
#--------------------------------------------
{
  x <- (x-min)/(max-min)
  ow <- options(warn=-1)
  d <- dbeta(x,shape1=shape1,shape2=shape2,ncp=ncp,log=log)
  options(ow)
  d[max <= min] <- NaN
	if(any(is.na(d))) warning("NaN in dbetagen")
  return(d)}

#<<BEGIN>>
pbetagen <- function(q,shape1,shape2,min=0,max=1,ncp=0,lower.tail = TRUE, log.p = FALSE)
#ISALIAS dbetagen
#--------------------------------------------
{
  q2 <- (q-min)/(max-min)
  ow <- options(warn=-1)
  p <- pbeta(q2,shape1=shape1,shape2=shape2,ncp=ncp,lower.tail=lower.tail,log.p=log.p)
  options(ow)
  p[q == min & q==max] <- 1 #if min==max==q
  p[max < min] <- NaN
	if(any(is.na(p))) warning("NaN in pbetagen")
  return(p)}

#<<BEGIN>>
qbetagen <- function(p,shape1,shape2,min=0,max=1,ncp=0,lower.tail=TRUE,log.p=FALSE)
#ISALIAS dbetagen
#--------------------------------------------
{
  ow <- options(warn=-1)
  q <- qbeta(p,shape1=shape1,shape2=shape2,ncp=ncp,lower.tail=lower.tail,log.p=log.p)
  options(ow)
  q2 <- q2*(max-min)+min
  q2[max < min] <- NaN
	if(any(is.na(q2))) warning("NaN in qbetagen")
  return(q2)}


#<<BEGIN>>
rbetagen <- function(n,shape1,shape2,min=0,max=1,ncp=0)
#ISALIAS dbetagen
#--------------------------------------------
{
  ow <- options(warn=-1)
  r <- rbeta(n,shape1=shape1,shape2=shape2,ncp=ncp)
  options(ow)
  r2 <- r*(max-min)+min
  r2[max < min] <- NaN
  if(any(is.na(r2))) warning("NaN in rbetagen")
  return(r2)
}
