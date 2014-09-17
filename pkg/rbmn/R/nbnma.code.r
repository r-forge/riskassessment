
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
topo4nbn <- function(nbn,ord=NULL,check=rbmn0$check$v)
#TITLE topological order of a /nbn/
#DESCRIPTION Returns one of the orders of the nodes
# such as the parents of any node are less ranked than
# it when \samp{is.null(ord)}.\cr
# If not check that the proposed order
# is either a right permutation (\samp{is.numeric(ord)}) or
# a vector of node names providing a topological order
# (\samp{is.character(ord)}).
#DETAILS
# When \samp{!is.null(ord)} the order must be a permutation, if
# not an error is issued.
#KEYWORDS 
#INPUTS
#{nbn} <<\samp{nbn} object for which the order must be computed
#        or checked.>>
#[INPUTS]
#{ord} <<\samp{NULL} or an order to test as a permutation or
#        a vector of names.>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# When no topological order, an error is issued! \cr
# When \samp{is.null{ord}}, a \samp{integer} giving a permutation 
# of the nodes of the /nbn/ providing
# a topological order.\cr
# When \samp{!is.null{ord}}, \samp{TRUE} or \samp{FALSE}
# to indicate that it is a topological order or not.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# names(rbmn0$nbn4$v)[topo4nbn(rbmn0$nbn4$v)];
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 12_01_14
#REVISED 13_07_05
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8nbn(nbn,topo=TRUE);
    if (!sssvoid9(cheche)) {
      ssserreur(nbn,c(cheche,"Not a valid /nbn/!"));
    }
  }
  # basic constants
  nn <- length(nbn);
  nam <- names(nbn);
  # checking
  for (ii in sssbc(nn)) {
    if (length(union(nam,nbn[[ii]]$parents))!= nn) {
      ssserreur(list(nam,nbn[[ii]]$parents),
             message="Unknown Parents");
    }
  }
  # processing
  if (is.null(ord)) {
    # getting a topological order
    res <- numeric(0);
    while (length(res) < nn) {
      for (ii in sssbc(nn)) {
        if (!(ii %in% res)) {
          pare <- nbn[[ii]]$parents;
          # if its parents are already included, it can be added
          if (length(union(nam[res],pare))==length(res)) {
            res <- c(res,ii);
          }
        }
      }
    }
  } else {
    # checking the proposed order
    # translating 'ord' as numeric if character
    if (is.character(ord)) { nord <- sssposi9(ord,nam);
    } else { nord <- ord;}
    # checking it is a permutation
    if (!all(sort(nord)==1:nn)) {
        ssserreur(ord,message="'ord' is not a permutation.")
    }
    # checking the order
    res <- TRUE; 
    parents <- character(0);
    for (ii in sssbc(nn)) {
      pare <- nbn[[nord[ii]]]$parents;
      if (length(union(pare,parents))>length(parents)) {
        res <- FALSE;
      }
    parents <- union(parents,nam[ii]);
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
normalize8nbn <- function(nbn,mu=0,sigma=1,check=rbmn0$check$v)
#TITLE normalizes a /nbn/
#DESCRIPTION returns a \samp{nbn} with a
# given expectation and variance through
# a transformation leaving the correlation
# unchanged.
#DETAILS
#KEYWORDS
#INPUTS
#{nbn}<< The \samp{nbn} object to transform.>>
#[INPUTS]
#{mu} << Imposed expectations. When \samp{NULL}
#        nothing is changed. When of length one,
#        this value is given to all the node
#        expectations. If not the complete vector
#        of expectations.>>
#{sigma} << The same as \samp{mu} but for the
#        standard deviations.>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# The transformed \samp{nbn}.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(normalize8nbn(rbmn0$nbn1$v));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_12_19
#REVISED 11_12_19
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8nbn(nbn);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"/nbn/: not a valid /nbn/");
    }
    sssobject9(   mu,"numeric",1,mensaje="Not a valid 'mu'");
    sssobject9(sigma,"numeric",1,mensaje="Not a valid 'sigma'");
  }
  # number of nodes
  nn <- length(nbn);
  nna <- names(nbn);
  # developping the mu and sigma
  if (length(mu)==1) {
    mu <- rep(mu,nn);
  }
  if (length(sigma)==1) {
    sigma <- rep(sigma,nn);
  }
  # going to the gema form
  gema <- nbn2gema(nbn);
  # getting the mn form
  mn <- gema2mn(gema);  
  # computing the actual mu and sigma
  mu0 <- mn$mu;
  sigma0 <- sqrt(diag(mn$gamma));
  # transforming accordingly the gema
  if (!is.null(mu)) {
    gema$mu <- mu;
  }
  if (!is.null(sigma)) {
    if (!all(sigma>0)) {
      ssserreur(sigma,message="asked standard deviation(s) are not positive");
    }
    gema$li <- diag(sigma/sigma0,nrow=nn) %*% gema$li;
  }
  names(gema$mu) <- nna;
  dimnames(gema$li) <- list(nna,NULL);
  # back to the nbn
  res <- gema2nbn(gema);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbn4nbn <- function(nbn,check=rbmn0$check$v)
#TITLE From a /nbn/ computes the 'normalized' /nbn/
#DESCRIPTION returns a /nbn/ object with the 
# same structure as \samp{nbn} but all \samp{$mu}
# are put to zero, all \samp{$sigma} to one as well
# as \samp{$regcof}.
#DETAILS
# This normalization makes easy the study of the   
# /nbn/ structure.
#KEYWORDS
#INPUTS
#{nbn}<< The \samp{nbn} object to transform.>>
#[INPUTS]
#{check} << Must a check of the argument be performed?>>
#VALUE
# The resulting \samp{nbn}.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(nbn4nbn(rbmn0$nbn4$v));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE To be integrated into 'normalize8nbn' since it is a kind of normalization
#AUTHOR J.-B. Denis
#CREATED 12_02_17
#REVISED 12_02_17
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8nbn(nbn);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"/nbn/: not a valid /nbn/");
    }
  }
  # modification
  for (no in sssbf(nbn)) {
    nbn[[no]]$mu <- 0;
    nbn[[no]]$sigma <- 1;
    nbn[[no]]$regcoef[] <- 1;
  }
  # returning
  nbn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
adja4nbn <- function(nbn,topo=FALSE,check=rbmn0$check$v) 
#TITLE adjacency matrix of a /nbn/
#DESCRIPTION returns a dimnamed matrix
# indicating with 1 an arc from row to column nodes
# (0 everywhere else); i.e. the adjacency matrix.
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn}<< The initial \samp{nbn} object.>>
#[INPUTS]
#{topo} <<Must the topological ordering of the nodes be checked?>>
#{check} << Must a check of the argument be performed?>>
#VALUE
# A dimnamed matrix
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# adja4nbn(rbmn0$nbn4$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_07_27
#REVISED 14_08_12
#--------------------------------------------
{
  # checking
  if (check) {
    cheche <- check8nbn(nbn);
    if (!sssvoid9(cheche)) {
      ssserreur(cheche,"Not a valid /nbn/");
    }
  }
  # getting the parentship matrix
  nbno <- length(nbn);
  nbna <- names(nbn);
  res <- matrix(0,nbno,nbno);
  dimnames(res) <- list(from=nbna,to=nbna);
  for (nn in sssbf(nbn)) {
    if (length(nbn[[nn]]$parents) > 0) {
      res[match(nbn[[nn]]$parents,nbna),nn] <- 1;
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
adja2nbn <- function(adja) 
#TITLE standardized /nbn/ from an adjacency matrix
#DESCRIPTION returns a \samp{nbn} object
# with O/1 regression coefficients having \samp{adja} as
# adjacency matrix.
#DETAILS
#KEYWORDS 
#INPUTS
#{adja}<< The initial adjacency matrix.>>
#[INPUTS]
#VALUE
# The corresponding standardized \samp{nbn} object.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8nbn(adja2nbn(adja4nbn(rbmn0$nbn3$v,check=FALSE)),check=TRUE);
#REFERENCE
#SEE ALSO adja4nbn
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_05_02
#REVISED 13_05_02
#--------------------------------------------
{
  # checking
  sssobject9(adja,"matrix",mensaje="'adja' must be a matrix");
  if (nrow(adja)!=ncol(adja)) {
    ssserreur(adja,"'adja' must be a square matrix");
  }
  # getting the parentship matrix
  nbno <- nrow(adja);
  nbna <- dimnames(adja)[[1]];
  res <- vector("list",nbno);
  names(res) <- nbna;
  for (nn in sssbf(res)) {
    res[[nn]]$mu <- 0;
    res[[nn]]$sigma <- 1;
    res[[nn]]$parents <- nbna[which(adja[,nn]==1)];
    res[[nn]]$regcoef <- rep(1,length(res[[nn]]$parents));
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rm8nd4adja <- function(adja,nodes)
#TITLE  removes somes nodes from an adjacency matrix
#DESCRIPTION 
# Eliminates from the adjacency matrix (\samp{adja})all \samp{nodes}
# not breaking the existing links.\cr
# Important: the node order in \samp{adja} must be topological.
#DETAILS
# When a node is removed, all its parents become parents of all its children.
#KEYWORDS
#INPUTS
#{adja} <<The relation matrix to be consider (same format as those
#        provided by the function \samp{adja4nbn}. Must be in topological
#        order, roots first.>>
#{nodes} <<Numeric or character vector providing the node numbers
#          to use for the generation of the subset.>>
#[INPUTS]
#VALUE
# The reduced adjacency matrix.
#EXAMPLE
# rm8nd4adja(rbmn0$adja4$v,"1.1");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE remove the topological order constraint
#AUTHOR J.-B. Denis
#CREATED 12_07_27
#REVISED 12_07_27
#--------------------------------------------
{
# checking
nbno <- nrow(adja);
# the topological order
masque <- outer(sssbc(nbno),sssbc(nbno),">=");
if (sum(masque*adja^2) > 0) {
  ssserreur(adja,message="Not a topological order");
}
# < to be finished >
# getting constants
nano <- dimnames(adja)[[1]];
if (is.character(nodes)) {
  nodes <- match(nodes,nano);
}
nodes <- sort(unique(nodes),decreasing=TRUE);
nbs <- length(nodes);
# degenerate case
if (nbno*nbs == 0) {
  return(matrix(0,0,0));
}
# removing starting from the leaves
for (nod in nodes) {
  nbc <- nrow(adja);
  des <- sssbc(nbno)[adja[nod,]!=0];
  asc <- sssbc(nbno)[adja[,nod]!=0];
  if (length(des)*length(asc)>0) {
    adja[asc,des] <- 1;
  }
  adja <- adja[-nod,]; adja <- adja[,-nod];
}
# returning
adja;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
generate8nbn <- function(rnn=c(3,7),ppar=0.5,rreg=c(-1,1),
                         rmu=c(0,0),rsig=c(0,1),
                         nona=sssform3names(max(rnn)))
#TITLE returns a randomly built /nbn/ object.
#DESCRIPTION
# generates a random \samp{nbn} object within some precised limits
# given by the arguments.
#DETAILS
# To obtain systematic results, you have to call \samp{set.seed}
# before this function.\cr
# Node numbers are uniformly drawn. Parent numbers are
# independently drawn from all ancestors with the probability
# associated to the considered node. Regression coefficient are uniformly drawn.
# Conditional expectations and standard deviations are uniformly drawn.\cr
# All range arguments can be given one value instead of two, to precise the
# unique value to use.
#KEYWORDS 
#INPUTS
#[INPUTS]
#{rnn} <<Range of the number of nodes.>>
#{ppar} <<Probabilities (not a range) of the parent occurrence for
# each ancestor of every node. Can be a vector,
# cycled as necessary.>>
#{rreg} <<Range of regression coefficients.>>
#{rmu}  <<Range of the conditional expectations.>>
#{rsig} <<Range of the conditional standard deviations.>>
#{nona} <<Proposed names for the maximum number of nodes, only the
#         necessary first ones will be used.>>
#VALUE
# a /nbn/ object, with nodes in topological order.
#EXAMPLE
# set.seed(1234)
# print8nbn(generate8nbn());
# print8nbn(generate8nbn());
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_04_21
#REVISED 13_04_24
#--------------------------------------------
{
  # checking
  # < to be done >
  # dealing with unique values
  if (length(rnn)==1) { rnn <- c(rnn,rnn);}
  if (length(rreg)==1) { rreg <- c(rreg,rreg);}
  if (length(rmu)==1) { rmu <- c(rmu,rmu);}
  if (length(rsig)==1) { rsig <- c(rsig,rsig);}
  # getting the node number.
  nn <- floor(runif(1,rnn[1],rnn[2]+1));
  # getting the node names
  nona <- nona[sssbc(nn)];
  # getting the probabilities of parents
  pp <- ppar;
  while (length(pp) < nn) { pp <- c(pp,ppar);}
  pp <- pp[1:nn];
  # building the /nbn/
  res <- vector("list",0);
  for (ii in sssbc(nn)) {
    mu <- runif(1,rmu[1],rmu[2]);
    sigma <- runif(1,rsig[1],rsig[2]);
    regcoef <- numeric(0);
    parents <- character(0);
    for (jj in sssbc(ii-1)) {
      if (rbinom(1,1,pp[ii]) > 0) {
        parents <- c(parents,nona[jj]);
        regcoef <- c(regcoef,runif(1,rreg[1],rreg[2]));
      }
    }
    res[[ii]] <- list(mu=mu,sigma=sigma,
                      parents=parents,
                      regcoef=regcoef);
  }
  names(res) <- nona;
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
arc7nb4nbn <- function(nbn,each=FALSE,check=rbmn0$check$v) 
#TITLE returns the number(s) of arcs of a /nbn/
#DESCRIPTION returns the arc numbers of 
# of a /nbn/ object; either globally or the number
# of parents of each node.
#DETAILS
# Parents associated with a zero regression coefficient
# are not excluded in the counting.
#KEYWORDS 
#INPUTS
#{nbn}<< The \samp{nbn} object to consider.>>
#[INPUTS]
#{each} << When \samp{TRUE}, returns a named vector of the
# number of parents of each node. If not the total number of arcs.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# Either a number or a named vector of numbers (names being the node names).
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# arc7nb4nbn(rbmn0$nbn5$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE To complete with a broad description of properties
#       returning a list and associating a printing function.
#AUTHOR J.-B. Denis
#CREATED 13_04_22
#REVISED 13_04_22
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn' is not valid!");
    }
  }
  # getting the arc number for each node
  nbno <- length(nbn);
  res <- rep(0,nbno);
  for (nn in sssbf(nbn)) {
    res[nn] <- length(nbn[[nn]]$parents);
  }
  # finalizing
  if (each) {
    names(res) <- names(nbn);
  } else {
    res <- sum(res);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
