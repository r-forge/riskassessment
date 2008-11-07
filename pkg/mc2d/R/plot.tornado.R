#<<BEGIN>>
plot.tornado <- function(x,which=1,name=NULL,stat=c("median","mean"),xlab="method",ylab="",...)
#TITLE Draws a Tornado chart.
#DESCRIPTION
# Draws a Tornado chart as provided by \code{tornado}.
#KEYWORDS hplot
#INPUTS
#{x}<<A \code{\link{tornado}} object or a \code{\link{tornadounc}} object.>>
#[INPUTS]
#{which}<<Which output to print -for multivariates output-.>>
#{name}<<Vector of name of input variables.
#If NULL, the name will be given from the name of the elements.>>
#{stat}<<The name (or the number of column) of the statistics of the output to be considered.>>
#{xlab}<<Label of the x axis. if "method", use the correlation method used in the \code{tornado} object.>>
#{ylab}<<Label of the y axis.>>
#{\dots}<<Further arguments to be passed to the \code{plot} function.>>
#VALUE
# NULL
#DETAILS
#A point is drawn at the estimate
#and the segment reflects the uncertainty around this estimate.
#SEE ALSO
#\code{\link{tornado}}
#EXAMPLE
#AUTHOR Regis Pouillot
#CREATED 07-08-01
#REVISED 07-08-01
#--------------------------------------------
#
{
  val <- x$value[[which]]
  if(is.null(val)) stop("Invalid value for which")
  nc <- ncol(val)
  nr <- nrow(val)
  if(!is.null(name)) {colnames(val) <- (rep(name,nc))[1:nc]}

  if(xlab=="method") xlab <- c("Spearman's rho statistic","Kendall's tau statistic","Pearson correlation")[pmatch(x$method,c("spearman","kendall","pearson"))]

	plot(c(-1.5,1),c(1,nc),type="n",axes= FALSE, xlab=xlab,ylab=ylab,...)
  axis(1,at=c(-1,-0.5,0,0.5,1))

  stat <- match.arg(stat)
  stat <- ifelse(stat=="mean" && nr!=1, 2 ,1)
  val <- val[,order(abs(val[stat,])),drop=FALSE]
	if(nr==1){
		segments(0,1:nc,val,1:nc,lwd=4)
    }

  else {
    points(val[stat,],1:nc)
    if(nr>3){
        val <- apply(val[3:nr,],2,range)
        segments(val[1,],1:nc,val[2,],1:nc,lwd=4)
        }
    }

		abline(v=0)
		text(-1.4,1:nc,labels=paste(colnames(val),sep=":"),cex=.8)
		}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<BEGIN>>
plot.tornadounc <- function(x,which=1, stat=2, name=NULL, xlab="method", ylab="",...)
#ISALIAS plot.tornado
#--------------------------------------------
#
{
  x$value <- list(x$value[[which]][stat,,drop=FALSE])
	plot.tornado(x,which=1, name=name, xlab=xlab,ylab=ylab,...)
 }

