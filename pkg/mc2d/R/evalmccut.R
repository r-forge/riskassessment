#<<BEGIN>>
evalmccut <- function(model, nsv = ndvar(), nsu = ndunc(), seed = NULL,  ind = "index", progress.bar=TRUE)
#TITLE Evaluates a 2 Dimension Monte Carlo Model in a loop.
#NAME mccut
#DESCRIPTION
#\code{evalmccut} evaluates a 2-dimensional Monte Carlo model using a loop, calculates and stores statistics in the uncertainty dimension
#for further analysis.
#It allows to evaluate very high dimension models using (unlimited?) time instead of (limited) memory.</>
#\code{mcmodelcut} builds a \code{mcmodelcut} object that can be sent to \code{evalmccut}.
#KEYWORDS methods
#INPUTS
#{model}<<a \code{mcmodelcut} object obtained using \code{mcmodelcut} function or (directly) a valid call including three blocks.
#See Details and Examples for the structure of the call.>>
#{x}<<a call or an expression (if \code{is.expr=TRUE}) including three blocks. See Details and Examples for the structure of the call.>>
#[INPUTS]
#{nsv}<<The number of simulations for variability used in the evaluation.>>
#{nsu}<<The number of simulations for uncertainty used in the evaluation.>>
#{seed}<<The random seed used for the evaluation. If \code{NULL} the \code{seed} is unchanged.>>
#{ind}<<The variable name used in \code{model} to refers to the uncertainty. see Details and Example.>>
#{progress.bar}<<if \code{TRUE}, a progress bar is provided during the evaluation.>>
#{is.expr}<< \code{FALSE} to send a call,  \code{TRUE} to send an expression (see \code{\link{mcmodel}} examples)>>
#{lim}<<A vector of values used for the quantile function (uncertainty dimension).>>
#{digits}<<Number of digits in the print.>>
#{\dots}<<Additional arguments to be passed in the final print function.>>
#VALUE
#An object of class \code{mccut}. This is a list including statistics evaluated within the third block. Each list consists of all the \code{nsu}
#values obtained. The \code{print.mccut} method print the median, the mean, the \code{lim} quantiles estimated on each statistics on the
#uncertainty dimension.
#NOTE
#The methods and functions available on the \code{mccut} object is function of the statistics evaluated within the third block:
#{*}<<a \code{\link{print.mccut}} is available as soon as one statistic is evaluated within the third block;>>
#{*}<<a \code{\link{summary.mccut}} and a \code{\link{tornadounc.mccut}} are available if a \code{\link{summary.mc}}
#is evaluated within the third block;>>
#{*}<<\code{\link{converg}} may be used if a \code{\link{summary.mc}} is evaluated within the third block;>>
#{*}<<a \code{\link{plot.mccut}} is available if a \code{\link{plot.mc}} is evaluated within the third block.
#(Do not forget to use the argument \code{draw = FALSE} in the third block);>>
#{*}<<a \code{\link{tornado}} is available if a \code{\link{tornado}} is evaluated within the third block.>>
#DETAILS
#This function should be used for high dimension MC2D simulations, when the memory limits of \R are attained.
#The use of a loop will take (lots of) time, but less memory.</>
#\code{x} (or \code{model} if a call is used directly in \code{evalmccut}) should be built as three blocks, separated by \code{\{}.
#{#}<<The first block is evaluated (step 1) once (and only once) before the first loop.>>
#{#}<<The second block, which should lead to a \code{mc} object, is evaluated using \code{nsu = 1} (step 2).>>
#{#}<<The third block is evaluated on the \code{mc} object. All statistics are stored (step 3).>>
#{#}<<The steps 2 and 3 are repeated \code{nsu} times. At each iteration, the values of the loop index (from 1 to \code{nsu})
#is given to the variable specified in \code{ind}.>>
#{#}<<Finally, the \code{nsu} statistics are returned in an invisible object of class \code{mccut}.>>
#Understanding this, the call should be built like this:
#\code{{{block 1}{block 2}{block 3}}}
#{#}<<The first block (maybe empty) is an expression that will be evaluated only once.
#This block should evaluate all \code{"V" mcnode}. It may evaluate \code{"0" mcnode} and \code{"U" mcnode}
#that will be sent in the second and third block by column, and, optionnaly, some other codes
#(even \code{"VU" mcnode}, sent by columns) that can not be evaluated if \code{ndunc=1}
#(e.g. sampling without replacement in the uncertainty dimension).>>
#{#}<<The second block is an expression that leads to the \code{mc} object.
#It must end with an expression as \code{mymc <- mc(...)}. The variable specified as \code{ind}
#may be helpful to refer to the uncertainty dimension in this step >>
#{#}<<The last block should build a list of statistics refering to the \code{mc}
#object. The function \code{summary} should be used if a summary, a tornado on uncertainty (\code{\link{tornadounc.mccut}}) or a convergence diagnostic
#\code{\link{converg}} is needed,
#the function \code{\link{plot.mc}} should be used if a plot is needed, the function \code{\link{tornado}} should be used if a tornado is needed.
#Moreover, any other function that leads to a
#vector, a matrix, or a list of vector/matrix of statistics evaluated from the \code{mc} object may be used. list are time consuming.>>
#IMPORTANT WARNING: do not forget to affect the results, since the print method provide only
#a summary of the results while all data may be stored in an \code{mccut} object.
#NOTE
#The seed is set at the beginning of the evaluation. Thus, the complete similarity of two evaluations
#is not certain, depending of the structure of your model. Moreover, with a similar seed, the simulation will not be equal to
#the one obtains with \code{evalmcmod} since the random samples will not be obtained in the same order.
#In order to avoid conflict between \code{model} evaluation and the function, the function uses upper case variables.
#Do not use upper case variables in your model.</>
#The function should be re-adapted if a new function to be applied on \code{mc} objects is written.
#SEE ALSO
#\code{\link{evalmcmod}}
#EXAMPLE
#modEC3 <- mcmodelcut({
#
### First block:
###  contains all the 0, V and U nodes.
#    { cook <- mcstoc(rempiricalD, type = "V", values = c(0, 1/5,
#        1/50), prob = c(0.027, 0.373, 0.6))
#      serving <- mcstoc(rgamma, type = "V", shape = 3.93, rate = 0.0806)
#      conc <- mcstoc(rnorm, type = "U", mean = 10, sd = 2)
#      r <- mcstoc(runif, type = "U", min = 5e-04, max = 0.0015)
#    }
### Second block
###  Leads to the mc object
#    {
#      expo <- conc * cook * serving
#      dose <- mcstoc(rpois, type = "VU", lambda = expo)
#      risk <- 1 - (1 - r)^dose
#      res <- mc(conc, cook, serving, expo, dose, r, risk)
#    }
### Third block
###  Leads to a list of statistics: summary, plot, tornado
###  or any function leading to a vector (et), a list (minmax), a matrix or a data.frame (summary)
#    {
#      list(
#        sum = summary(res),
#        plot = plot(res, draw=FALSE),
#        minmax = lapply(res,range),
#        et =  sapply(res,sd)
#     )
#    }
#})
#
#evalmccut(modEC3, nsv = 101, nsu = 11, seed = 666, progress.bar=TRUE)
#AUTHOR Regis Pouillot
#CREATED 08-04-16
#REVISED 08-04-16
#--------------------------------------------
#
{

  # DEFINE SOME FUNCTIONS
  # DEFINEOUT build the empty list that will fill the results

  DEFINEOUT <- function(ARG){
    LAFUNC <- function(ARGJBD){
      if(inherits(ARGJBD,"tornado")) ARGJBD <- ARGJBD$value
      if(is.list(ARGJBD)) return(DEFINEOUT(ARGJBD))
      if(is.data.frame(ARGJBD)) ARGJBD <- as.matrix(ARGJBD)
      if(is.array(ARGJBD)){
         DIMS <- dim(ARGJBD)
         if(inherits(ARGJBD,"mcnode")){
            DIMS <- c(DIMS[1],nsu,DIMS[3])
            NOM <- vector(mode="list",length=3)}
         else {if(length(DIMS) > 2)                                              #Other arrays = matrix
                stop("Array > 2 dimensions are not supported in this function")
          DIMS <- c(DIMS[1],nsu,DIMS[2])
          NOM <- list(rownames(ARGJBD),NULL,colnames(ARGJBD))}
      }
      else {
        DIMS <- c(length(ARGJBD),nsu,1)
        NOM <- list(names(ARGJBD),NULL,NULL)
      }
      return(array(NA,dim=DIMS,dimnames=NOM))
      }
    OUT <- lapply(ARG,LAFUNC)
    names(OUT) <- names(ARG)
    class(OUT) <- class(ARG)
    return(OUT)
  }

  # FONCSPEC is a tricky "autocall" function to fill the results
  FONCSPEC <- function(STAT, PREC, LOOP){
    if(inherits(STAT,"tornado")) {STAT <- STAT$value}
    if(is.list(PREC)){
      PREC <- mapply(FONCSPEC,STAT,PREC,MoreArgs=list(LOOP=LOOP),SIMPLIFY=FALSE)
    }
    else PREC[,LOOP,] <- STAT
    return(PREC)
    }

OLDV <- ndvar()
OLDU <- ndunc()
RESULTAT <- try({                                                                # Debut du try
  ndvar(nsv)
  ndunc(nsu)
  if(!is.null(seed)) set.seed(seed)

  #Analyse expression
  NBEXPR <- length(model[[1]])
  if(NBEXPR != 4) stop("The expression should include three blocks")

  #Evaluate the first block and stock the U and VU mcnode
  LSBEG <- ls()
  eval(model[[1]][[2]])
  LSEND <- ls()
  NEW <- LSEND[!(LSEND %in% LSBEG)]                                             # New objects built
  NEW <- NEW[sapply(NEW,function(x) inherits(get(x),"mcnode"))]                 # New mcnode built
  TYPE <- sapply(NEW, function(x) attr(get(x),which="type"))
  OUTM <- sapply(NEW, function(x) attr(get(x),which="outm"))
    cat("'0' mcnode(s) built in the first block:",NEW[TYPE == "0"],"\n")
    cat("'V' mcnode(s) built in the first block:",NEW[TYPE == "V"],"\n")
    cat("'U' mcnode(s) built in the first block:",NEW[TYPE == "U"],"\n")
    cat("'VU' mcnode(s) built in the first block:",NEW[TYPE == "VU"],"\n")
    cat("The 'U' and 'VU' nodes will be sent column by column in the loop\n")

  QUEL <- (TYPE == "U" | TYPE == "VU")
  OUTM <- OUTM[QUEL]
  NN    <- sum(QUEL)
  TORIG <- TYPE[QUEL]                                                           # Type of Origin
  TORIG1D <- ifelse(TORIG %in% c("0","U"),"0","V")                              # Corresponding Type in 1D
  NORIG <- NEW[QUEL]                                                            # Name of the node to save
  NSAVED <- paste("SAVE",NORIG,sep="")                                          # New names

  for(i in 1:NN)  assign(NSAVED[i], get(NORIG[i]))                              # Save the nodes

  # Prepare the loop
  ndunc(1)                                                                       # nsu = 1
  if(progress.bar){
    windows(width = 3, height = 0.7)
    par(mar = c(0.5, 0.5, 2, 0.5))
    plot(c(0,nsu),c(0,1),xlim=c(0,nsu),ylim=c(0,1),type="n",main="Uncertainty",
		axes=FALSE,xlab="",ylab="",frame.plot=TRUE,xaxs = "i", yaxs = "i", xaxt = "n",
            	yaxt = "n")}

# First simulation
  assign(ind,1)

  # Get the first row of the stocked nodes
  NODE <- vector(mode="list",length=NN)
  for(i in 1:NN) {
    NODE[[i]] <- get(NSAVED[i])[,1,,drop=FALSE]                                 # Can not be put in an mapply function
    attr(NODE[[i]],which="type") <- TORIG[1]                                    # First simulation: keep the original "type"
    attr(NODE[[i]],which="outm") <- OUTM[i]
    class(NODE[[i]])  <- "mcnode"
    assign(NORIG[i],NODE[[i]]) }                                                 # And assign them to their original name


  # Evaluate the model
    eval(model[[1]][[3]])

    NOM <- as.character(model[[1]][[3]][[length(model[[1]][[3]])]][[2]])        # name of the mc second block
    MCOBJ <- get(NOM)
    if(!is.mc(MCOBJ)) stop("The second block result is not an mc object")

    TYPEORI <- sapply(MCOBJ,attr,which="type")                                  #Original type
    TYPENEW <- ifelse(TYPEORI %in% c("0","U"),"0","V")                          #New type

 #Evaluate the model with original type and keep the first results
    PREMS <- eval(model[[1]][[4]])

 #Evaluate the model with new types
    NODE <- mapply("attr<-",NODE,"type",TORIG1D,SIMPLIFY=FALSE)                 # Assign the new type
    for(i in 1:NN) assign(NORIG[i],NODE[[i]])                                   # And assign them to their original name

    MCOBJ[] <- mapply("attr<-",MCOBJ,"type",TYPENEW,SIMPLIFY=FALSE)
    assign(NOM,MCOBJ)

    PREMSPRIM <- eval(model[[1]][[4]])

 #Define the final structure and stock the first simu
    SORTIE <- DEFINEOUT(PREMSPRIM)
    SORTIE <- mapply(FONCSPEC,PREMSPRIM,SORTIE,MoreArgs=list(LOOP=1),SIMPLIFY=FALSE)

# Other Simulations
  for(LOOP in 2:nsu){
    if(progress.bar) polygon(c(LOOP-2, LOOP-2, LOOP, LOOP), c(1, 0, 0, 1), col = "black")

    assign(ind,LOOP)

    for(i in 1:NN) {
      NODE[[i]][] <- get(NSAVED[i])[,LOOP,,drop=FALSE]
      assign(NORIG[i],NODE[[i]])
    }

    eval(model[[1]][[3]])
    MCOBJ <- get(NOM)
    MCOBJ[] <- mapply("attr<-",MCOBJ,which="type",value=TYPENEW,SIMPLIFY=FALSE)
    assign(NOM,MCOBJ)

    STAT <- eval(model[[1]][[4]])
    SORTIE <- mapply(FONCSPEC,STAT,SORTIE,MoreArgs=list(LOOP=LOOP),SIMPLIFY=FALSE)
    }

  if(progress.bar) dev.off()

  # Post Production : Affecte les classes + petites modifications pour qq fonctions connues

  for(JBD in 1:length(SORTIE)){
    if(inherits(PREMS[[JBD]],"summary.mc")) {
      LESTYPES <- sapply(PREMS[[JBD]],"attr","type")
      SORTIE[[JBD]] <- mapply("attr<-",SORTIE[[JBD]],"type",LESTYPES,SIMPLIFY=FALSE)
      class(SORTIE[[JBD]]) <- c("summary.mccut")
      }

    else if(inherits(PREMS[[JBD]],"mcnode")) {
      attr(SORTIE[[JBD]],"type") <- attr(PREMS[[JBD]],"type")
      attr(SORTIE[[JBD]],"outm") <- attr(PREMS[[JBD]],"outm")
      class(SORTIE[[JBD]]) <- c("mcnode")
      }

    else if(inherits(PREMS[[JBD]],"tornado")) {
      PREMS[[JBD]]$value <- SORTIE[[JBD]]
      SORTIE[[JBD]] <- PREMS[[JBD]]
      class(SORTIE[[JBD]]) <- c("tornado.mccut")
      }
      
    else if(inherits(PREMS[[JBD]],"plotmc")){
      TYPEPLOT <- sapply(PREMS[[JBD]],"attr",which="type")
      SORTIE[[JBD]] <- mapply("attr<-",SORTIE[[JBD]],"type",TYPEPLOT,SIMPLIFY=FALSE)
      class(SORTIE[[JBD]]) <- c("plot.mccut")}

  }

class(SORTIE) <- "mccut"

return(SORTIE)}, silent=TRUE)     # Fin du try
ndvar(OLDV)
ndunc(OLDU)
if(inherits(RESULTAT,"try-error")) stop(RESULTAT,call. = FALSE)
return(invisible(RESULTAT))
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>






















