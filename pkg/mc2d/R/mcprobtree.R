#<<BEGIN>>
mcprobtree <- function(mcswitch, mcvalues, type=c("V","U","VU","0"), nsv=ndvar(), nsu=ndunc(),nvariates=1, outm="each", seed=NULL)
#TITLE Creates a Stochastic mcnode Object using a Probability Tree
#DESCRIPTION
#If one node should be built using a mixed of \code{mcstoc} functions/nodes, \code{mcprobtree} creates this \code{\link{mcnode}}
#object from one node specifying the \code{mcstoc} functions/nodes to pick and a list of the different \code{mcstoc} functions/nodes.
#KEYWORDS methods
#INPUTS
#{mcswitch}<<A \code{mcnode} including the \code{mcstoc} functions/mcnodes to pick.>>
#{mcvalues}<<A named list of \code{mcnode}, \code{mcdata} functions or \code{mcstoc} functions, or a combination of those objects.>>
#[INPUTS]
#{type}<<The type of \code{mcnode} to be built. By default, a \code{"V"} node. see \code{\link{mcnode}} for details.>>
#{nsv}<<The number of simulations in the variability dimension of the final node.>>
#{nsu}<<The number of simulations in the uncertainty dimension of the final node.>>
#{nvariates}<<The number of variates of the final \code{mcnode}.>>
#{outm}<<The default output of the \code{mcnode} for multivariates nodes. see \code{\link{outm}}.>>
#{seed}<<The random seed used for the evaluation. If \code{NULL} the \code{seed} is unchanged.>>
#VALUE
#An \code{mcnode} object.
#DETAILS
#\code{mcswitch} may be:
#{*}<<a \code{"0"} mcnode to build any type of node.>>
#{*}<<a \code{"V"} mcnode to build a \code{"V"} or a \code{"VU"} mcnode.>>
#{*}<<a \code{"U"} mcnode to build a \code{"U"} or a \code{"VU"} mcnode.>>
#{*}<<a \code{"VU"} mcnode to build a \code{"VU"}.>>
#\code{mcswitch} will be recycled correctly if needed. (see \code{\link{mcdata}})
#
#The elements in \code{mcvalues} should be of same type and dimension
#as specified in \code{type}, \code{nsv}, \code{nsu} and \code{nvariates}. The name
#should correspond to the values in \code{mcswitch}, specified as character (See Examples).
#These elements will
#be evaluated only if needed : if the corresponding value is not present in \code{mcswitch},
#the element will not be evaluated.
#
#EXAMPLE
## A mixture of normal (Prob=0.75) and uniform (Prob=0.25)
#conc1 <- mcstoc(rnorm,type="U",mean=10,sd=2)
#conc2 <- mcstoc(runif,type="U",min=8,max=12)
#whichdist <- mcstoc(rbern,type="U",prob=0.25) 
#concbis <- mcprobtree(whichdist,list("0"=conc1,"1"=conc2),type="VU")
#SEE ALSO
#\code{\link{mcdata}}, \code{\link{mcstoc}}, \code{\link{switch}}.
#EXAMPLE
#AUTHOR Regis Pouillot
#CREATED 08-06-1
#--------------------------------------------
{

  type <- match.arg(type)
  if(!inherits(mcswitch,"mcnode")) stop("mcswitch should be an mcnode")

  stoc <- as.list(substitute(mcvalues))
  choixstoc <- as.numeric(names(stoc))                                          # OK for logical
  stoc <- substitute(mcvalues)
  choixswitch <- unique(mcswitch)
  if(!all(choixswitch %in% choixstoc)) stop("some values of mcswitch are not present in mcvalues")

  typem <- attr(mcswitch,"type")

  if((type == "V" && typem == "U" )   ||
     (type == "U" && typem == "V" )   ||
     (type != "VU" && typem == "VU" ) ||
     (type == "0" && typem != "0" ))     stop("Incompatible type and type of mcswitch")
  
  mcswitch <- mcdata(mcswitch,type=type,nsv=nsv,nsu=nsu,nvariates=nvariates)
  res <- mcdata(NA,type=type,nsv=nsv,nsu=nsu,nvariates=nvariates)

  for(i in choixswitch){
    whichswitch <- mcswitch == i
    nbcall <- which(choixstoc == i)
    thecall <- eval(stoc[[nbcall]])[whichswitch]                                          #because stoc[[1]] is list
    res[whichswitch] <- thecall
    }
  class(res) <- "mcnode"
  attr(res,"type") <- type
  attr(res,"outm") <- outm
  return(res)}



