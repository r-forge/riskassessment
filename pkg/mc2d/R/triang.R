#<<BEGIN>>
dtriang <- function(x,min=-1,mode=0,max=1,log=FALSE)
#TITLE The Triangular Distribution
#NAME triangular
#KEYWORDS distribution
#DESCRIPTION
#Density, distribution function, quantile function and random generation
#for the triangular distribution with minimum equal to min, mode equal mode
#and maximum equal to max.
#INPUTS
#{x,q}<<vector of quantiles.>>
#{p}<<vector of probabilities.>>
#{n}<<number of observations. If length(n) > 1, the length is taken to be the number required.>>
#[INPUTS]
#{min}<<vector of minima.>>
#{mode}<<vector of modes.>>
#{max}<<vector of maxima.>>
#{log, log.p}<<logical; if TRUE, probabilities p are given as log(p).>>
#{lower.tail}<<logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].>>
#VALUE
#dtriang gives the density, ptriang gives the distribution function,
#qtriang gives the quantile function, and rtriang generates random deviates.
#AUTHOR Regis Pouillot
#EXAMPLE
#curve(dtriang(x,min=3,mode=5,max=10), from = 2, to = 11)
#CREATED 08-02-20
#--------------------------------------------
{
	quel <- x <= mode
	d <- ifelse(quel,
              2*(x-min)/((mode-min)*(max-min)),
	            2 *(max-x)/((max-mode)*(max-min)))
	d[x < min | x > max] <- 0
	d[mode < min | max < mode] <- NaN
	if(log) d <- log(d)
	if(any(is.na(d))) warning("NaN in dtriang")
  return(d)}

#<<BEGIN>>
ptriang <- function(q,min=-1,mode=0,max=1,lower.tail = TRUE, log.p = FALSE)
#ISALIAS dtriang
#--------------------------------------------
{
	quel <- q <= mode
	p <- ifelse(quel,
              (q-min)^2 / ((mode-min)*(max-min)),
	             1 - ((max-q)^2/((max-mode)*(max-min))))
	p[q < min] <- 0
	p[q > max] <- 1
	p[mode < min | max < mode] <- NaN
  if(!lower.tail) p <- 1-p
  if(log.p) p <- log(p)
	if(any(is.na(p))) warning("NaN in ptriang")
  return(p)}

#<<BEGIN>>
qtriang <- function(p,min=-1,mode=0,max=1,lower.tail=TRUE,log.p=FALSE)
#ISALIAS dtriang
#--------------------------------------------
{
  if(log.p) p <- exp(p)
	if(!lower.tail) p <- 1-p
	quel <- p <= (mode-min)/(max-min)
	q <- ifelse(quel,
              min + sqrt(p*(mode-min)*(max-min)),
              max - sqrt((1-p)*(max-min)*(max-mode)))
	q[p < 0 | p > 1] <- NaN
	q[mode < min | max < mode] <- NaN
	if(any(is.na(q))) warning("NaN in qtriang")
  return(q)}


#<<BEGIN>>
rtriang <- function(n,min=-1,mode=0,max=1)
#ISALIAS dtriang
#--------------------------------------------
{  if(length(n) > 1) n <- length(n)
	return(qtriang(runif(n),min=min,mode=mode,max=max,lower.tail=TRUE,log.p=FALSE))}
