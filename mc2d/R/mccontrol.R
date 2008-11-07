#<<BEGIN>>
ndvar <- function(n)
#NAME mc.control
#TITLE Sets or Gets the Default Number of Simulations.
#DESCRIPTION
# Sets or retrieves the default number of simulations.
#KEYWORDS misc
#INPUTS
#{n}<<Number of simulations.>>
#DETAILS
#\code{ndvar()} gets and \code{ndvar(n)} sets the default number of simulation in the 1D simulations
#or the number of simulation in the variability dimension in the 2D simulations.</>
#\code{ndunc()} gets and \code{ndunc(n)} sets the number of simulations in the uncertainty dimension
#in the 2D simulations.</>
#\code{n} is rounded to its ceiling value.</>
#The default values when loaded are 1001 for \code{ndvar} and 101 for \code{ndunc}.
#VALUE
#The current value, AFTER modification if \code{n} is present (!= \code{options}).
#EXAMPLE
#(oldvar <- ndvar())
#(oldunc <- ndunc())
#mcstoc(runif,type="VU")
#ndvar(12)
#ndunc(21)
#mcstoc(runif,type="VU")
#ndvar(oldvar)
#ndunc(oldunc)
#AUTHOR Regis Pouillot
#CREATED 08-01-25
#--------------------------------------------
{
  if(!exists("mc.control", envir=.BaseNamespaceEnv))
    assign("mc.control",list(nsv=1001,nsu=101),envir=.BaseNamespaceEnv )
  x <- get("mc.control", envir=.BaseNamespaceEnv)
   if(!is.list(x) || is.null(x$nsv) || is.null(x$nsu))
    assign("mc.control",list(nsv=1001,nsu=101),envir=.BaseNamespaceEnv )
  if(!missing(n)){
    if (n > 0) x$nsv <- ceiling(n)
        else stop("Invalid n")
    x$nsv <- n
    assign("mc.control",x, envir=.BaseNamespaceEnv)}
  return(x$nsv)}

#<<BEGIN>>
ndunc <- function(n)
#ISALIAS ndvar
#--------------------------------------------
{
  if(!exists("mc.control", envir=.BaseNamespaceEnv))
    assign("mc.control",list(nsv=1001,nsu=101),envir=.BaseNamespaceEnv )
  x <- get("mc.control", envir=.BaseNamespaceEnv)
   if(!is.list(x) || is.null(x$nsv) || is.null(x$nsu))
    assign("mc.control",list(nsv=1001,nsu=101),envir=.BaseNamespaceEnv )
  if(!missing(n)){
    if (n > 0) x$nsu <- ceiling(n)
        else stop("Invalid n")
    x$nsu <- n
    assign("mc.control",x, envir=.BaseNamespaceEnv)}
  return(x$nsu)}
