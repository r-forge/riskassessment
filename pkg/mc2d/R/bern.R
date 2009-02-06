#<<BEGIN>>
dbern <- function(x,prob=.5,log=FALSE)
#TITLE The Bernoulli Distribution
#NAME bernoulli
#KEYWORDS distribution
#DESCRIPTION
#Density, distribution function, quantile function and random generation
#for the Bernoulli distribution with probability equals to prob.
#INPUTS
#{x,q}<<vector of quantiles.>>
#{p}<<vector of probabilities.>>
#{n}<<number of observations. If length(n) > 1, the length is taken to be the number required.>>
#[INPUTS]
#{prob}<<vector of probabilities of success of each trial.>>
#{log, log.p}<<logical; if TRUE, probabilities p are given as log(p).>>
#{lower.tail}<<logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].>>
#DETAILS
#These fonctions use the corresponding functions from the \code{\link{binomial}} distribution with argument size = 1.
#Thus, 1 is for success, 0 is for failure.
#VALUE
#dbern gives the density, pbern gives the distribution function,
#qbern gives the quantile function, and rbern generates random deviates.
#EXAMPLE
#rbern(n=10,prob=.5)
#rbern(n=3,prob=c(0,.5,1))
#SEE ALSO
#\code{\link{Binomial}}
#AUTHOR Regis Pouillot
#CREATED 08-02-20
#--------------------------------------------
{
  return(dbinom(x, 1, prob=prob, log = log))}

#<<BEGIN>>
pbern <- function(q,prob=.5,lower.tail = TRUE, log.p = FALSE)
#ISALIAS dbern
#--------------------------------------------
{
  return(pbinom(q, 1, prob=prob, lower.tail = lower.tail, log.p = log.p))
  }

#<<BEGIN>>
qbern <- function(p,prob=.5,lower.tail=TRUE, log.p = FALSE)
#ISALIAS dbern
#--------------------------------------------
{
  return(qbinom(p, 1, prob=prob, lower.tail = lower.tail, log.p = log.p))
  }


#<<BEGIN>>
rbern <- function(n,prob=.5)
#ISALIAS dbern
#--------------------------------------------
{
  return(rbinom(n, 1, prob=prob))
  }
