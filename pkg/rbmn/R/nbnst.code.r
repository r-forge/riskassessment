
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
estimate8nbn <- function(nbn,data,check=rbmn0$check$v) 
#TITLE estimating the /nbn/ parameters
#DESCRIPTION 
# From a /nbn/ to describe the DAG, and a data.frame
# containing the necessary observations, returns the /nbn/ with
# all its parameters newly ML estimated.
#DETAILS
# No constraints are put on the parameters.
#KEYWORDS 
#INPUTS
#{nbn}<< The initial /nbn/.>>
#{data}<<The data frame comprising all /nbn/ nodes.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting /nbn/ with the estimated parameters.
#EXAMPLE
# data(boco);
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(rbmn0$nbn5$v);
# print8nbn(estimate8nbn(rbmn0$nbn5$v,boco));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_04_22
#REVISED 13_04_24
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn' is not valid!");
    }
    sssobject9(data,"data.frame");
    if (length(union(names(nbn),names(data)))!=length(data)) {
      ssserreur(list(names(data),names(nbn)),
             "Not all nodes of 'nbn' were found in 'data'!");
    }
  }
  # constant
  nona <- names(nbn);
  # looping onto the node set
  for (nn in sssbf(nbn)) {
    YY <- nona[nn];
    if (length(nbn[[nn]]$parents)>0) {
      XX <- paste(nbn[[nn]]$parents,collapse="+");
    } else {
      XX <- "1";
    }
    FF <- paste(YY,"~",XX);
    flm <- lm(as.formula(FF),data=data);
    ano <- anova(flm);
    # 13_05_02 modif performed to obtain /bnlearn/ results
    #nbn[[nn]]$sigma <- sqrt(ano["Residuals",3]);
    nbn[[nn]]$sigma <- sqrt(ano["Residuals",2]/sum(ano[,1]));
    # end of 13_05_02 modification
    nbn[[nn]]$mu <- coefficients(flm)[1];
    nbn[[nn]]$parents <- names(coefficients(flm)[-1]);
    nbn[[nn]]$regcoef <- coefficients(flm)[-1];
    names(nbn[[nn]]$regcoef) <- NULL;
  }
  # returning
  nbn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
estimate8arcs <- function(nbn,marc,data,check=rbmn0$check$v)
#TITLE estimates a common regression coefficient to a set of arcs
#DESCRIPTION 
# Supposing known all parameters but one in a /nbn/ object, the function
# provides the esmate for the missing one which is supposed common to
# arcs described in the argument \samp{marc}. Constant term is included
# in the optimization.
#DETAILS
# This is mainly an ancillary function of \samp{estimate8constrained7nbn}\cr
# Two coefficient are updated: the regression coefficient and
# the estimate of the standard deviation.
#KEYWORDS 
#INPUTS
#{nbn} <<\samp{nbn} object.>>
#{marc} <<Matrix with two columns indicating the tails (1rst column) and the
#         heads (2d column) of the arcs having a common parameter. It is
#         checked that these arcs are indeed included in \samp{nbn}.
#         Nodes must be indicated by their names (not their number).>>
#{data} <<Data frame to be used for the estimation. It must
#        comprise all necessary nodes (not only those involved
#        in \samp{marc} but also the remaining parents of \samp{marc[,2]}.
#        Usually, all used variables are centred but this is not
#        required.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# the resulting /nbn/ object with the common parameter.
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 13_04_24
#REVISED 13_04_25
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn' is not valid!");
    }
    sssobject9(data,"data.frame");
    if (length(union(names(nbn),names(data)))!=length(data)) {
      ssserreur(list(names(data),names(nbn)),
             "Not all nodes of 'nbn' were found in 'data'!");
    }
    sssobject9(marc,"matrix");
    if (ncol(marc)!=2) {
      ssserreur(dim(marc),"'marc' must be a 2-columns matrix");
    }
  }
  # initializing the data frame
  don <- as.data.frame(matrix(NA,0,2));
  www <- numeric(0);
  nda <- nrow(data);
  # looping onto the set of arcs to build the estimating data frame
  # of the targetted parameter
  for (aa in sssbc(nrow(marc))) {
    # getting the part to add
    tail <- marc[aa,1]; head <- marc[aa,2];
    noeud <- nbn[[head]];
    if (!(tail %in% noeud$parents)) {
      stop(paste(tail,"is not a parent of",head));
    }
    reco <- noeud$regcoef; names(reco) <- noeud$parents;
    reco <- reco[setdiff(noeud$parents,tail)];
    XX <- data[[tail]];
    YY <- data[[head]];
    for (pp in names(reco)) {
      YY <- YY - reco[pp]*data[[pp]];
    }
    newi <- paste(head,sssbc(nda),sep=".");
    dos <- matrix(c(XX,YY),ncol=2,
                  dimnames=list(newi,NULL));
    # adding it to the data.frame
    don <- rbind(don,dos);
    www <- c(www,rep(noeud$sigma^-2,nda));
  }
  dimnames(don)[[2]] <- c("X","Y");
  # estimating the parameter
  elm <- lm(Y ~ -1+X,data=don,weights=www);
  # reporting the estimation in the /nbn/
  for (aa in sssbc(nrow(marc))) {
    # incorporating the new regression coefficient
    tail <- marc[aa,1]; head <- marc[aa,2];
    ou <- which(nbn[[head]]$parents==tail);
    nbn[[head]]$regcoef[ou] <- coefficients(elm);
    # computing the new residual sum of squares
    #cat("<",tail,"->",head,">\n"); browser();
    rss <- sum((
           data[,head] -
           as.matrix(data[,nbn[[head]]$parents,drop=FALSE]) %*%
           matrix(nbn[[head]]$regcoef,ncol=1))^2);
    # incorporating the estimation of the new sigma
    # 13_05_02 modif performed to obtain /bnlearn/ results
    #nbn[[head]]$sigma <- sqrt(rss/(nda-length(nbn[[head]]$parents)));
    nbn[[head]]$sigma <- sqrt(rss/(nda-1));
    # end of 13_05_02 modification
  }
  # returning
  nbn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
estimate8constrained7nbn <- function(nbn,smarc,data,
                                    imp=0,nite=10,eps=10^-5,
                                    check=rbmn0$check$v)
#TITLE estimates the parameters of a nbn with equality constraints
#DESCRIPTION 
# Estimations of the parameters of a /nbn/ is done when there
# are some equality constraints onto the regression coefficients.\cr
# Constant terms (\samp{mu}) and conditional standard deviations (\samp{sigma})
# are supposed free (that is not constrained with equalities).\cr
# Equality constraints are given by \samp{smarc}, a list of matrices
# with two columns, indicating each the series of arcs having equal
# regression coefficients.\cr
# The first aim of this function was to estimate specific
# crossed Bayesian networks but can be applied to any 
# design of constraints; however no check is made about
# the consistency of the set of constraints.  
#DETAILS
# No constrained regression coefficients doesn't require to be included in
# \samp{smarc}, the function do it by itself.\cr
# The score to assess the differences between two successive
# estimations is quite raw (see the code).
#KEYWORDS 
#INPUTS
#{nbn} <<\samp{nbn} object.>>
#{smarc} <<List of Matrices with two columns indicating the tails (1rst column) and the
#         heads (2d column) of the arcs having a common parameter. It is
#         checked that these arcs are indeed included in \samp{nbn}.
#         Nodes must be indicated by their names (not their number).>>
#{data} <<Data frame to be used for the estimation. It must
#        comprise all necessary nodes (not only those involved
#        in \samp{smarc} but also the remaining parents of \samp{smarc[,2]}.
#        Usually, all used variables are centred but this is not
#        required.>>
#[INPUTS]
#{imp}<<When \samp{0} nothing displayed. When \samp{1} the number of iterations is displayed.
#       When \samp{2} the successive values of the criterion are also displayed. >>
#{nite}<<Maximum number of iterations.>>
#{eps}<<relative difference in successive scores needed to stop the iterations.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting /nbn/ object with the estimated parameters.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(rbmn0$nbn5$v);
# data(boco);
# print8nbn(estimate8nbn(rbmn0$nbn5$v,boco));
# print8nbn(estimate8constrained7nbn(rbmn0$nbn5$v,rbmn0$arc5$v,boco));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 13_04_25
#REVISED 14_08_15
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn' is not valid!");
    }
    sssobject9(data,"data.frame");
    if (length(union(names(nbn),names(data)))!=length(data)) {
      ssserreur(list(names(data),names(nbn)),
             "Not all nodes of 'nbn' were found in 'data'!");
    }
    sssobject9(smarc,"list",mensaje="'smarc must be list of matrices");
    for (sli in sssbf(smarc)) {
      sssobject9(smarc[[sli]],"matrix",speci=matrix(c(NA,2,NA,2),2),
              mensaje="Each component of 'smarc' must be a 2-columns matrix");
      sssobject9(smarc[[sli]],"character",mensaje="'smarc' must be a character matrix");
      if (length(union(smarc[[sli]],names(nbn)))>length(names(nbn))) {
        ssserreur(smarc[[sli]],"'smarc' component number",sli,"contains names which are not nodes of 'nbn'");
      }
    }
  }
  sep <- " / ";
  # detecting and checking the proposed arcs for equality
  nona <- names(nbn);
  aaa <- adja4nbn(nbn,check=FALSE);
  for (a1 in sssbf(smarc)) {
    ma <- smarc[[a1]];
    for (a2 in sssbc(nrow(ma))) {
      tail <- ma[a2,1]; head <- ma[a2,2];
      if (aaa[tail,head] == 0) {
        ssserreur(paste(tail,"->",head),message="This arc is not included in the /nbn/");
      } else {
        aaa[tail,head] <- aaa[tail,head] + 1;
      }
    }
  }
  # adding the remaining arcs alone in the list
  for (ii in sssbf(nona)) { for (jj in sssbf(nona)) {
    if (aaa[ii,jj] == 1) {
      smarc[[length(smarc)+1]] <- matrix(c(nona[ii],nona[jj]),1);
    }
  }}
  # centring every node
  mimi <- sapply(data,mean);
  for (nn in nona) {
    data[[nn]] <- data[[nn]] - mimi[nn];
  }
  # starting the estimation without constraint
  nbna <- estimate8nbn(nbn,data);
  nite <- round(max(nite,1));
  if (imp>1) { cat("estimate8constrained7nbn:",sep,sep="");}
  # iterating for the estimation
  nbite <- 0; dsco <- 2*eps;
  while ((nbite < nite) & (dsco > eps)) {
    nbite <- nbite+1;
    # looping onto the parameters
    for (pp in sssbf(smarc)) {
      nbna <- estimate8arcs(nbna,smarc[[pp]],data);
    }
    # calculating the score
    if (nbite > 1) {
      dsco <- diff8nbn(nbna,nbnn);
      if (imp>1) { cat(dsco,sep,sep="");}
    }
    # updating the /nbn/
    nbnn <- nbna;
  }
  if (imp>1) { cat("\n");}
  if (imp>0) { cat("nb.ite =",nbite,"(",dsco,")\n");}
  # reintroducing the mean in every node
  for (nn in nona) {
    mu <- mimi[nn];
    mu <- mu - sum(mimi[nbnn[[nn]]$parents]*nbnn[[nn]]$regcoef);
    nbnn[[nn]]$mu <- mu;
  }
  # returning
  nbnn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
