

#A REVOIR
#<<BEGIN>>
mcapply <- function(x, margin=c("all","var","unc","variates"), fun, ...)
#TITLE Apply Functions Over mc or mcnode Objects
#DESCRIPTION
# Apply a function on all values or over a given dimension of an mcnode object. May be used for all mcnode of an mc object.
#KEYWORDS misc
#INPUTS
#{x}<<A mc or a mcnode object.>>
#[INPUTS]
#{margin}<<The dimension on which applying the function.
#Maybe "all" (default) to apply the function on all values,
#"var" to apply the function on the variability dimension,
#"unc" to apply the function on the uncertainty dimension, or
#"variates" to apply the function on the variates.>>
#{fun}<<The function to be applied.
#When applied to a vector of length n, fun should return a vector of length n or 1.>>
#{\dots}<<Optionnal arguments to fun.>>
#VALUE
#If fun returns a function of length n or if margin=="all", the returned mcnodes are of type and dimension of x.
#In other cases, the type of mcnode is changed.
#SEE ALSO
#\code{\link{apply}}, \code{\link{mc}}, \code{\link{mcnode}}.
#EXAMPLE
#data(total)
#xVUM
#mcapply(xVUM,"unc",sum)
#mcapply(xVUM,"var",sum)
#mcapply(xVUM,"all",sum)
#mcapply(xVUM,"variates",sum)
#mcapply(total,"all",exp)
#AUTHOR Regis Pouillot
#CREATED 08-03-17
#--------------------------------------------
{
  margin <- match.arg(margin)

  if(is.mc(x)) {
    y <- lapply(x, mcapply, margin=margin, fun=fun, ...)
    class(y) <- "mc"
    return(y)}

  if(!is.mcnode(x)) stop("x should be an mcnode")

  typen <- attr(x,"type")
  dimn <-  dim(x)

  dime <- switch(margin, "unc" = c(1,3), "var" = c(2,3), "all" = c(1,2,3), "variates" = c(1,2))
  nlo <-  switch(margin, "unc" = prod(dimn[c(1,3)]), "var" = prod(dimn[c(2,3)]), "all" = prod(dimn), "variates" = prod(dimn[c(1,2)]))

  dat <- apply(x, dime, fun, ...)
  nl <- length(dat)
  oneres <- nl == nlo        # fun give one scalar
  if(nl != length(x) && !oneres) stop("fun(x) with length(x) = n should return a vector of length n or 1")

  nsu <- ifelse(margin=="unc" && oneres,1,dimn[2])
  nsv <- ifelse(margin=="var" && oneres,1,dimn[1])
  nva <- ifelse(margin=="variates" && oneres,1,dimn[3])

  if(typen=="0") ntype <- "0"
  
  else if(typen=="V"){
    if(margin=="var" && oneres) ntype <- "0"
    else ntype <- "V"}

  else if(typen=="U"){
    if(margin=="unc" && oneres) ntype <- "0"
    else ntype <- "U"}

  else if(margin=="unc" && oneres) ntype <- "V"
  else if(margin=="var" && oneres) ntype <- "U"
  else ntype <- "VU"

  return(mcdata(as.vector(dat),type=ntype,nsv=nsv,nsu=nsu,nvariates=nva))
}

