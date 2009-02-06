#<<BEGIN>>
is.na.mcnode <- function(x)
#NAME NA.mcnode
#TITLE Finite, Infinite, NA and NaN Numbers in mcnode.
#DESCRIPTION
# is.na, is.nan, is.finite and is.infinite return a logical mcnode 
#of the same dimension as x.
#INPUTS
#{x}<<A mcnode object.>>
#VALUE
#A logical mcnode object.
#SEE ALSO
#\code{\link{is.finite}}, \code{\link{NA}}
#KEYWORDS NA
#EXAMPLE
#x <- log(mcstoc(rnorm,nsv=1001))
#x
#is.na(x)
#AUTHOR Regis Pouillot
#CREATED 07-25-11
#REVISED 07-08-01
#--------------------------------------------
#
{
  y <- NextMethod()
  attr(y,"type") <- attr(x,"type")
  attr(y,"outm") <- attr(x,"outm")
  class(y) <- "mcnode"
  return(y)}

#<<BEGIN>>
is.nan.mcnode <- function(x)
#ISALIAS is.na.mcnode
#--------------------------------------------
{
  y <- NextMethod()
  attr(y,"type") <- attr(x,"type")
  attr(y,"outm") <- attr(x,"outm")
  class(y) <- "mcnode"
  return(y)}

#<<BEGIN>>
is.finite.mcnode <- function(x)
#ISALIAS is.na.mcnode
#--------------------------------------------
{
  y <- NextMethod()
  attr(y,"type") <- attr(x,"type")
  attr(y,"outm") <- attr(x,"outm")
  class(y) <- "mcnode"
  return(y)}

#<<BEGIN>>
is.infinite.mcnode <- function(x)
#ISALIAS is.na.mcnode
#--------------------------------------------
{
  y <- NextMethod()
  attr(y,"type") <- attr(x,"type")
  attr(y,"outm") <- attr(x,"outm")
  class(y) <- "mcnode"
  return(y)}


  
