#<<BEGIN>>
dimmcnode <- function(x)
#TITLE Dimension of mcnode and mc Objects
#DESCRIPTION
# Provides the dimension (i.e. the number of simulations in the variability dimension,
# the number of simulations in the uncertainty dimension and the 
# maximum number of variates of a \code{mcnode} or a \code{mc} object.
#KEYWORDS utilities
#INPUTS
#{x}<<a \code{mcnode} or a \code{mc} object.>>
#VALUE
#A vector of three scalars: the dimension of variability (1 for \code{"0"} and \code{"U" mcnode}), 
# the dimension of uncertainty (1 for \code{"0"} and \code{"V" mcnode}) and
# the number of variates (the macimal number of variates for an \\code{mc} object.
#NOTE
#This function does not test if the object is correctly built. See \code{\link{is.mcnode}} and \code{\link{is.mc}} .
#EXAMPLE
#data(total)
#dimmcnode(xVUM2)
#dimmc(total)
#AUTHOR Regis Pouillot
#CREATED 07-08-01
#REVISED 07-08-01
#--------------------------------------------
{
  if(!inherits(x,"mcnode")) stop("x is not an mcnode object")
  y <- dim(x)
  names(y) <- c("nsv","nsu","nvariates")
  return(y)}

#<<BEGIN>>
dimmc <- function(x)
#ISALIAS dimmcnode
#--------------------------------------------
{
  if(!inherits(x,"mc")) stop("x is not an mc object")
  lesdim <- sapply(x,dimmcnode)
  y <- apply(lesdim,1,max)
  names(y) <- c("nsv","nsu","max variates")
  return(y)}

#<<BEGIN>>
typemcnode <- function(x,index=FALSE)
#TITLE Provides the Type of a mcnode Object
#DESCRIPTION
# Provide the type of a \code{mcnode} object from its class.
#KEYWORDS utilities
#INPUTS
#{x}<<a \code{mcnode} object>>
#[INPUTS]
#{index}<<if \code{TRUE} give the index of the type rather than the type.>>
#VALUE
# \code{"0", "V","U" or "VU"} or the corresponding index if \code{index=TRUE}.</>
#\code{NULL} if none of this element is found.
#NOTE
#This function does not test if the object is correct. See \code{\link{is.mcnode}}.
#EXAMPLE
#data(total)
#typemcnode(total$xVUM2)
#AUTHOR Regis Pouillot
#CREATED 07-08-01
#REVISED 07-08-01
#--------------------------------------------
#
{
  if(!inherits(x,"mcnode")) stop("x is not an mcnode object")
  type <- attr(x,"type")
  if(index) return(which(c("0", "V","U","VU")==type)) else return(type)
}


#<<BEGIN>>
is.mc <- function(x)
#TITLE Tests mc and mcnode Objects
#DESCRIPTION
# \code{is.mc} tests \code{mc} objects and \code{is.mcnode} tests \code{mcnode} objects.
#KEYWORDS utilities
#INPUTS
#{x}<<An \code{mc} or a \code{mcnode} object.>>
#VALUE
# \code{TRUE} or \code{FALSE}
#DETAILS
# \code{is.mc} tests if \code{x} is a list of \code{mcnode},
#each elements being of compatible dimension.
#It tests if the class \code{"mc"} is affected to the object.</>
# \code{is.mcnode} tests if \code{x} is an array of numeric or logical,
# if it has a "type" attribute and compatible dimensions,
# and if the class \code{"mcnode"} is affected to the object.
#EXAMPLE
#data(total)
#is.mcnode(xVU)
#is.mcnode(total)
#is.mc(total)
#AUTHOR Regis Pouillot
#CREATED 07-08-01
#REVISED 07-08-01
#--------------------------------------------
#
{
  if (!inherits(x, "mc")) return(FALSE)
  x <- unclass(x)
  if(!is.list(x)) return(FALSE)
  mcn <- sapply(x,is.mcnode)
  if(!all(mcn)) return(FALSE)
  nsim <- sapply(x, dim)
  if(!all(nsim[1,] %in% c(1,max(nsim[1,])))) return(FALSE)
  if(!all(nsim[2,] %in% c(1,max(nsim[2,])))) return(FALSE)
  return(TRUE)}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<BEGIN>>
is.mcnode <- function(x)
#ISALIAS is.mc
#--------------------------------------------
#
{ if(!inherits(x,"mcnode")) return(FALSE)
  type <- typemcnode(x)
  if(is.null(type)) return(FALSE)
  x <- unclass(x)
  if(!is.numeric(x) && !is.logical(x)) return(FALSE)
  dimx <- dim(x)
  if(type == "0" && (!is.array(x) || dimx[1]!=1 && dimx[2]!=1)) return(FALSE)
  if(type == "V" && (!is.array(x) || dimx[2]!=1)) return(FALSE)
  if(type == "U" && (!is.array(x) || dimx[1]!=1)) return(FALSE)
  if(type == "VU" && !is.array(x)) return(FALSE)
  return (TRUE)}

