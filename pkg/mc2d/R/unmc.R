#<<BEGIN>>
unmc <- function(x, drop=TRUE)
#TITLE Unclasses the mc or the mcnode Object
#DESCRIPTION
#Unclasses the \code{mc} object in a list of arrays 
#or the \code{mcnode} object in an array.  
#KEYWORDS manip
#INPUTS
#{x}<<A \code{mc} or a \code{mcnode} object.>>
#[INPUTS]
#{drop}<<Should the dimensions of size 1 be dropped (see \code{\link{drop}}).>>
#VALUE
#if x is an \code{mc} object: a list of arrays. If \code{drop=TRUE}, a list of vectors, matrixes and arrays.
#if x is an \code{mcnode} object: an array. If \code{drop=TRUE}, a vector, matrix or array.
#EXAMPLE
#data(total)
### A vector
#unmc(total$xV, drop=TRUE)
### An array
#unmc(total$xV, drop=FALSE)
#AUTHOR Regis Pouillot
#CREATED 07-08-01
#REVISED 07-08-01
#--------------------------------------------
{
  unmcnode <- function(y){
    attr(y,"type") <- NULL
    attr(y,"outm") <- NULL
    y <- unclass(y)
    if(drop) y <- drop(y)
    return(y)}

  if(is.mc(x)){
    x <- lapply(x,unmcnode)
    x <- unclass(x)
    return(x)}
 
  return(unmcnode(x))
    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

