
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
generate8chain <- function(rnn=c(3,7),proo=0.5,rcor=c(-1,1),
                         rmu=c(0,0),rsig=c(0,1),
                         nona=sssform3names(max(rnn)),check=rbmn0$check$v)
#TITLE generation of a /chain/ /nbn/
#DESCRIPTION [randomly] generates a /chain/ /nbn/ for the structure
# and for the regressions coefficients.
#DETAILS
# Proposed ranges can be a unique value, implying no randomness
# in the value.\cr
# Roots are placed according to \samp{proo} probabilities, then collider
# are placed in between with uniform probability on the possibles nodes.
#KEYWORDS 
#INPUTS
#[INPUTS]
#{rnn} <<Range of the number of nodes.>>
#{proo} <<Probabilit[y|ies] that the successive and acceptable nodes be colliders. Can be a vector.>>
#{rcor} <<Range of the correlations between neighbour nodes.>>
#{rmu}  <<Range of the expectations.>>
#{rsig} <<Range of the standard deviations.>>
#{nona} <<Proposed names for the maximum number of nodes, only the
#         necessary first ones will be used.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# A /chain/ coding list is returned.
#EXAMPLE
# set.seed(1234);
# print8chain(generate8chain(check=TRUE));
# print8chain(generate8chain(check=TRUE));
# print8chain(generate8chain(rnn=10,rcor=0.5,check=TRUE));
# print8chain(generate8chain(rnn=10,rcor=0.5,check=TRUE));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE split the function into the generation of the structure and
# the generation of the regression coefficients.
#AUTHOR J.-B. Denis
#CREATED 13_07_03
#REVISED 13_07_03
#--------------------------------------------
{
  # checking
  if (check) {
    sssobject9(rnn,"integer",c(1,2),c(1,Inf),mensaje="'rnn' is not acceptable");
    sssobject9(proo,"numeric",-1,c(0,1),mensaje="'proo' is not acceptable");
    sssobject9(rcor,"numeric",c(1,2),c(-1,1),mensaje="'rcor' is not acceptable");
    sssobject9(rmu,"numeric",c(1,2),mensaje="'rmu' is not acceptable");
    sssobject9(rsig,"numeric",c(1,2),c(0,Inf),mensaje="'rsig' is not acceptable");
    sssobject9(nona,"character",mensaje="'nona' is not acceptable");
  }
  # dealing with unique values
  if (length(rnn)==1) { rnn <- c(rnn,rnn);}
  if (length(rcor)==1) { rcor <- c(rcor,rcor);}
  if (length(rmu)==1) { rmu <- c(rmu,rmu);}
  if (length(rsig)==1) { rsig <- c(rsig,rsig);}
  # getting the node number.
  nn <- floor(runif(1,rnn[1],rnn[2]+1));
  # getting the node names
  nona <- nona[sssbc(nn)];
  # getting the probabilities of downstream
  pp <- proo;
  while (length(pp) < nn) { pp <- c(pp,proo);}
  pp <- pp[1:nn];
  # building the /chain/
  # miscellaneous part
  res <- vector("list",0);
  res$names <- nona;
  res$mu <- runif(nn,rmu[1],rmu[2]);
  res$sigma <- runif(nn,rsig[1],rsig[2]);
  res$corre <- runif(nn-1,rcor[1],rcor[2]);
  # placing roots first
  root <- rbinom(nn,1,pp);
  for (ii in sssbc(nn-1)) {
    if (root[ii]==1) { root[ii+1] <- 0;}
  }
  # at least one root
  if (sum(root)==0) {
    root[sample.int(nn,1)] <- 1;
  }
  res$roots <- nona[root==1];
  root <- which(root==1);
  # placing colliders second
  res$colliders <- character(0);
  for (ii in sssbc(length(root)-1)) {
    r1 <- root[ii]; r2 <- root[ii+1];
    if (r2-r1<2) { stop("Erreur in generate8chain");}
    qui <- sample.int(r2-r1-1,1)+r1;
    res$colliders <- c(res$colliders,nona[qui]);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8chain <- function(chain,digits=3,check=rbmn0$check$v)
#TITLE prints a /chain/ object
#DESCRIPTION prints a /chain/ object.
#DETAILS
# See \samp{nbn2chain} code for some details about the
# definition of a /chain/.
#KEYWORDS 
#INPUTS
#{chain} << The \samp{chain} object to print.>>  
#[INPUTS]
#{digits} << when not null, the number of digits for rounding the
# numerical values.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# nothing but something is printed
#EXAMPLE
# print8chain(rbmn0$chain1$v,check=TRUE);
# print8chain(rbmn0$chain2$v,check=TRUE);
# print8chain(rbmn0$chain3$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_18
#REVISED 12_01_30
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # node number
  nn <- length(chain$names);
  orien <- aux2(chain)/2+1.5;
  cat("#-------------------------\n");
  for (node in sssbc(nn)) {
    nna <- chain$names[node];
    nona <- paste("(",nna,")",sep="");
    if (nna %in% chain$roots) {
      nona <- paste("<--(",nna,")-->",sep="");
    }
    if (nna %in% chain$colliders) {
      nona <- paste("-->)",nna,"(<--",sep="");
    }
    cat(sssform3justify(nona,nbc=10,format=2));
    cat("  ",round(chain$mu[node],digits),"              (",
             round(chain$sigma[node],digits),")",sep="");
    cat("\n");
    if (node < nn) {
      cat(sssform3justify(c("^","v")[orien[node]],nbc=10,format=2));
      cat("     <",round(chain$corre[node],digits),">",sep="");
      cat("\n");
    }
  }
  cat("#-------------------------\n");
  # returning
  invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check8chain <- function(chain)
#TITLE checks a /chain/ object
#DESCRIPTION checks the consistency of \samp{chain} as a /chain/ object
# return a non empty \samp{character} with some clues if inconsistency is detected.
#DETAILS
# Looking a the code of this function provides a way to know which
# are the requirements of a /chain/ object.
#KEYWORDS 
#INPUTS
#{chain} << The \samp{chain} object to check.>>  
#[INPUTS]
#VALUE
# \samp{character(0)} or a \samp{character} containing some clue
# about the discovered inconsistency.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# check8chain(rbmn0$chain1$v);
# \dontrun{check8chain(rbmn0$adja1$v);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_07_26
#REVISED 13_07_26
#--------------------------------------------
{
  # checking the component names
  compo <- names(rbmn0$chain1$v);
  if (!setequal(compo,names(chain))) {
    return("The names of the chain are not 'names(rbmn0$chain1$v)'");
  }
  # checking the lengths
  nbno <- length(chain$names);
  res <- character(0);
  if (nbno != length(chain$mu)) { res <- c(res,"'$mu' has got a bad length");}
  if (nbno != length(chain$mu)) { res <- c(res,"'$sigma' has got a bad length");}
  if ((nbno-1) != length(chain$corre)) { res <- c(res,"'$corre' has got a bad length");}
  if ((nbno-1)/2 < length(chain$colliders)) { res <- c(res,"'$colliders' has got a bad length");}
  if ((nbno+1)/2 < length(chain$roots)) { res <- c(res,"'$roots' has got a bad length");}
  if (length(res) > 0) { return(res);}
  # checking the correlations
  if (any(abs(chain$corre)>1)) { return("Some correlation doesn't make sense");}
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
topo4chain <- function(chain,check=rbmn0$check$v)
#TITLE returns a topological order of a /chain/ or checks a proposed order.
#DESCRIPTION From a \samp{chain} object
# returns one of the possible topological orders,
# through a permutation when \samp{is.null(ord)}.
# If not \samp{ord} must be a proposed order to be
# checked given as a permutation if \samp{is.numeric(ord)}
# or a vector of ordered names if \samp{is.character(ord)}.
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< the \samp{chain} object to be considered.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# a permutation vector of the nodes of the /nbn/
#        or a named character with the nodes not having
#        their parents before them; when it is of
#        length zero this means that the check 
#        was successful.
#EXAMPLE
# topo4chain(rbmn0$chain2$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Add the 'ord' option!
#AUTHOR J.-B. Denis
#CREATED 12_01_31
#REVISED 12_01_31
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # getting the permutation
  ooo <- aux2(chain,TRUE);
  res <- ooo;
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
state4chain <- function(chain,check=rbmn0$check$v)
#TITLE returns the states of each node of a chain
#DESCRIPTION From a \samp{chain} object
# returns a named character precising the role of each node:
# "r" for root, "c" for collider, "t" for transmitter and
# "l" for leaf.
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< the \samp{chain} object to be considered.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# a character of the states named with node names.
#EXAMPLE
# state4chain(rbmn0$chain1$v,check=TRUE);
# state4chain(rbmn0$chain3$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_31
#REVISED 12_01_31
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # identifying
  nn <- length(chain$names);
  etat <- rep("t",nn);
  etat[c(1,nn)] <- "l";
  if (length(chain$colliders) > 0) {
      etat[sssposi9(chain$colliders,chain$names)] <- "c";
  }
  etat[sssposi9(chain$roots,chain$names)] <- "r";
  # naming
  names(etat) <- chain$names;
  # returning
  etat;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
marginal4chain <- function(chain,check=rbmn0$check$v)
#TITLE returns marginal expectations and standard deviations of a chain
#DESCRIPTION From a \samp{chain} object
# returns a list with two components: \samp{$mu} and \samp{$sigma}
# vectors of marginal expectations and standard deviations (not a /mn/
# object since the correlations are lost; for that use \samp{chain2mn}).
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< the \samp{chain} object to be considered.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# a list with the two components \samp{$mu} and \samp{$sigma}.
#EXAMPLE
# marginal4chain(rbmn0$chain2$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_31
#REVISED 12_01_31
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # getting a topological order
  # and the status of each node
    nn <- length(chain$names);
    ooo <- topo4chain(chain,check=FALSE);
    etat <- state4chain(chain,check=FALSE);
  # following this order computing
  # progressively expectations and 
  # standard deviations
    mu <- sigma <- rep(NA,nn);
    orien <- aux2(chain);
    for (uu in ooo) {
	if (etat[uu]=="r") {
            # root, no modification
	    mu[uu] <- chain$mu[uu];
	    sigma[uu] <- chain$sigma[uu];
	} else {
          if (etat[uu]=="c") {
              # collider: the two neighbours are parents
              sigma[uu] <- chain$sigma[uu]/aux0(chain$corre[(uu-1):uu]);              
              mu[uu] <- chain$mu[uu] + 
                        chain$corre[uu-1]*sigma[uu]/sigma[uu-1]*mu[uu-1] +
                        chain$corre[uu]  *sigma[uu]/sigma[uu+1]*mu[uu+1];
	  } else {
              # other cases: only one parent
              if (uu>1) { if (orien[uu-1]==1) {
                  # parent upward
                  sigma[uu] <- chain$sigma[uu]/aux0(chain$corre[uu-1]);              
                  mu[uu] <- chain$mu[uu] + 
                            chain$corre[uu-1]*sigma[uu]/sigma[uu-1]*mu[uu-1];

	      }}
              if (uu<nn) { if (orien[uu]==-1) {
                  # parent downward
                  sigma[uu] <- chain$sigma[uu]/aux0(chain$corre[uu]);              
                  mu[uu] <- chain$mu[uu] + 
                            chain$corre[uu]*sigma[uu]/sigma[uu+1]*mu[uu+1];
	      }}
	  }
	}
    }
  if (any(is.na(mu))) {
      ssserreur(chain,message="either the chain or the algo");
  }
  # returning
  list(mu=mu,sigma=sigma);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain2nbn <- function(chain,check=rbmn0$check$v)
#TITLE transforms a /chain/ to a /nbn/
#DESCRIPTION From a \samp{chain} object
# returns the \samp{nbn} translation.
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< the \samp{chain} object to be transformed.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The corresponding \samp{nbn} object.
#EXAMPLE
# print8nbn(chain2nbn(rbmn0$chain2$v,check=TRUE),ordering=names(rbmn0$nbn2$v),check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_01_19
#REVISED 13_07_04
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # constants
  nna <- chain$names;
  nn <- length(nna);
  ooo <- aux2(chain,TRUE); # topological order
  rho <- aux2(chain); # arc orientations
  avan <- c(0,rho)== 1; # parent before
  apre <- c(rho,0)==-1; # parent after
  aaa <- sort(sssposi9(chain$roots,chain$names));
  # transforming
  res <- vector("list",nn);
  sigma <- marginal4chain(chain,check=FALSE)$sigma;
  mu <- chain$mu;
  for (nnu in ooo) {
    papa <- character(0); # parent specification
    coef <- numeric(0);   # regression coefficients
    # only non roots must be filled
    if (!(nnu %in% aaa)) {
      corri <- 1;
      if (avan[nnu] & apre[nnu]) {
        corri <- aux0(chain$corre[(nnu-1):nnu]);
      } else {
        if (avan[nnu]) { corri <- aux0(chain$corre[nnu-1]);}
        if (apre[nnu]) { corri <- aux0(chain$corre[nnu  ]);}
      }
      if (avan[nnu]) {
        # the node before is a parent
        papa <- nna[nnu-1];
        coef <- chain$sigma[nnu] / sigma[nnu-1] /
                corri * chain$corre[nnu-1];
      }
      if (apre[nnu]) {
        # the node after is a parent
        papa <- c(papa,nna[nnu+1]);
        coef <- c(coef,chain$sigma[nnu] / sigma[nnu+1] /
                       corri * chain$corre[nnu]);
      }
    }
    res[[nnu]]$parents <- papa;
    res[[nnu]]$regcoef <- coef;
    res[[nnu]]$mu <- chain$mu[nnu];             
    res[[nnu]]$sigma <- chain$sigma[nnu];
  }
  names(res) <- nna;
  # reordering the nodes
  ooo <- topo4chain(chain,check=FALSE);
  res <- res[ooo];
  # testing possible NaNs
  for (ii in sssbf(res)) {
    if (any(is.na(res[[ii]]$regcoef))) {
      stop("Dectected Numerical Inaccuracy in 'chain2nbn'");
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain2gema <- function(chain,check=rbmn0$check$v)
#TITLE transforms a /chain/ to a /gema/
#DESCRIPTION From a \samp{chain} object
# returns the \samp{gema} using a closed formulae.\cr
# Much precised than to use the /nbn/ way.
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< the \samp{chain} object to be transformed.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The corresponding \samp{gema} object.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# identical(chain2gema(rbmn0$chain2$v,check=TRUE)$mu,rbmn0$gema2$v$mu);
# print(chain2gema(rbmn0$chain2$v,check=TRUE)$li-rbmn0$gema2$v$li);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_02_06
#REVISED 12_02_06
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # useful constants
  nna <- chain$names;
  nn <- length(nna);
  pare <- aux2(chain,parents=TRUE);
  # initializing
  res <- list(mu=marginal4chain(chain,check=FALSE)$mu,
              li=diag(nrow=nn));
  names(res$mu) <- nna;
  dimnames(res$li) <- list(nna,NULL);
  # computing linear combination
  ## for correlations colliding structure
  for (no in sssbc(nn)) {
    if (pare[no] == -1) {
      # a colliding node
      res$li[no,no-1] <- chain$corre[no];
      res$li[no,no] <- aux0(chain$corre[c(no,no+1)]);
      res$li[no,no+1] <- chain$corre[no+1];
    } else {
      if (pare[no] > 0) {
        # a standard node
        if (pare[no] < no) { rho <- chain$corre[no-1];
                    } else { rho <- chain$corre[no];}
        res$li[no,pare[no]] <- rho;
        res$li[no,no] <- aux0(rho);
      }
    }
  }
  ## for correlations sequential segments
  if (length(chain$colliders)>0) {
    ccc <- sssposi9(chain$colliders,nna);
  } else {
    ccc <- numeric(0);
  }
  aaa <- sssposi9(chain$roots,nna);
  ava <- aaa[-length(aaa)];
  apr <- aaa[-1];
  for (cc in sssbf(ccc)) {
    pivo <- res$li[ccc[cc],ccc[cc]];
    ba <- ava[cc]:ccc[cc];
    bp <- ccc[cc]:apr[cc];
    res$li[ba,ba] <- aux1(chain$corre[ba[-length(ba)]],FALSE);
    res$li[bp,bp] <- aux1(chain$corre[bp[-length(bp)]],TRUE);
    res$li[ccc[cc],ccc[cc]] <- pivo;
  }
  ## without forgetting endind sequential segments
  if (aaa[1] > 1) {
    aa <- 1:aaa[1];
    res$li[aa,aa] <- aux1(chain$corre[1:(aaa[1]-1)],TRUE);
  }
  if (aaa[length(aaa)] < nn) {
    aa <- aaa[length(aaa)]:nn;
    res$li[aa,aa] <- aux1(chain$corre[aaa[length(aaa)]:(nn-1)],FALSE);
  }
  #
  ## adding standard deviations
  masig <- marginal4chain(chain,check=FALSE)$sigma;
  res$li <- res$li * masig;
  # reordering the nodes
  ooo <- topo4chain(chain,check=FALSE);
  res$mu <- res$mu[ooo];
  res$li <- res$li[ooo,ooo];
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain2cor <- function(chain,check=rbmn0$check$v)
#TITLE computes the correlation matrix of a chain
#DESCRIPTION returns the correlation
# matrix of a /chain/ object from closed expressions.
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< The chain object to consider.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The correlation matrix. It is not
# sorted to respect a topological order
# contrary to \samp{chain2mn} function.
#EXAMPLE
# chain2cor(rbmn0$chain3$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_02_01
#REVISED 11_02_02
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  nn <- length(chain$mu);
  res <- matrix(0,nn,nn);
  dimnames(res) <- list(chain$names,chain$names);
  # finding the milestones
  etat <- state4chain(chain,check=FALSE);
  sto <- sort(c(1,nn,which("c"==etat)));
  # introducing the blocks
  for (bb in sssbc(length(sto)-1)) {
    deb <- sto[bb]; fin <- sto[bb+1];
    res[sssbd(deb,fin),sssbd(deb,fin)] <- aux5(chain$corre[sssbd(deb,fin-1)]);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain2mn <- function(chain,order=TRUE,check=rbmn0$check$v)
#TITLE computes the distribution of a chain
#DESCRIPTION returns the /mn/ object
# associated to a /chain/ object. Much better
# to use this function that the general function
# \samp{nbn2mn} since closed formulae are applied.
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< The chain object to consider.>>
#[INPUTS]
#{order} << Must a topological order be imposed?>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting /mn/ object. Following the
# convention of \samp{mn} objects, a topological
# order is given to it. This is necessary to retrieve
# the associate /nbn/.
#EXAMPLE
# print8mn(chain2mn(rbmn0$chain1$v,check=TRUE),check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Think about imposing any order for the result...
#AUTHOR J.-B. Denis
#CREATED 11_12_05
#REVISED 13_07_30
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
    sssobject9(order,"logical",1,mensaje="'order' is not acceptable");
  }
  # getting the marginal parameters
  margi <- marginal4chain(chain,check=FALSE);
  # getting the multivariable parameters
  corre <- chain2cor(chain,check=FALSE);
  # computing the variance matrix
  gamma <- outer(margi$sigma,margi$sigma,"*") * corre;
  # naming the components
  names(margi$mu) <- chain$names;
  dimnames(gamma) <- list(chain$names,chain$names);
  # getting a topological order
  if (order) {
    ooo <- topo4chain(chain,check=FALSE);
  } else {
    ooo <- sssbf(margi$mu);
  }
  # returning
  list(mu=margi$mu[ooo],
       gamma=gamma[ooo,ooo]);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain2pre <- function(chain,corre=FALSE,check=rbmn0$check$v)
#TITLE computes the precision of a chain
#DESCRIPTION returns the precision matrix
# of a chain, that is the inverse of its
# variance (correlation) matrix. Much better
# to use this function that 
# \samp{solve(chain2mn(chain)$gamma)} since
# exact formulae are applied.
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< The chain object to consider.>>
#[INPUTS]
#{corre} <<To get the inverse of the correlation
#          matrix instead of.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# A dimnamed matrix
#EXAMPLE
# chain2pre(rbmn0$chain2$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_02_02
#REVISED 11_02_06
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
    sssobject9(corre,"logical",1,mensaje="'corre' is not acceptable");
  }
  # building the inverse of the correlation matrix
  # constants
  nn <- length(chain$names);
  ## first a long sequential chain
  res <- aux6(chain$corre);
  ## then the 3x3 blocks for colliders
  coli <- sort(which("c"==state4chain(chain,check=FALSE)));
  for (cc in coli) {
    ends <- c(cc==2,cc==nn-1);
    ou <- (cc-1):(cc+1);
    res[ou,ou] <- aux7(chain$corre[ou[-3]],ends);
  }
  # adding the variance components
  if (!corre) {
    sisi <- marginal4chain(chain,check=FALSE)$sigma;
    res <- res / outer(sisi,sisi,"*");
  }
  # dimnaming
  dimnames(res) <- list(chain$names,chain$names);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
inout4chain <- function(chain,check=rbmn0$check$v)
#TITLE reduces a chain to its inputs and outputs
#DESCRIPTION From a \samp{chain} returns the 
# reduced \samp{chain} comprising only inputs 
# (that is root nodes) and outputs (that is
# colliders and ends which are not roots)
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< The chain object to consider.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting chain
#EXAMPLE
# print8chain(inout4chain(rbmn0$chain2$v,check=TRUE),check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_02_07
#REVISED 11_02_07
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # determining the node to keep
  eta <- state4chain(chain,check=FALSE);
  kee <- chain$names[which(eta!="t")];
  nk <- length(kee);
  # building the resulting chain
  res <- vector("list",0);
  res$names <- kee;
  res$roots <- chain$roots;
  res$colliders <- chain$colliders;
  if (nk < 2) {
    res$mu <- chain$mu;
    res$sigma <- chain$sigma;
    res$corre <- chain$corre;
  } else {
    mn <- chain2mn(chain);
    mn$mu <- mn$mu[kee];
    mn$gamma <- mn$gamma[kee,kee,drop=FALSE];
    res$mu <- mn$mu;
    res$sigma <- sqrt(diag(mn$gamma));
    co <- cor4var(mn$gamma);
    res$corre <- diag(co[-1,-nk,drop=FALSE]);
    res <- aux4(res);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rev8chain <- function(chain,check=rbmn0$check$v)
#TITLE reverses the nodes of a chain
#DESCRIPTION returns the chain obtained
# after reversing its node order
#DETAILS
#KEYWORDS 
#INPUTS
#{chain}<< The chain object to consider.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting chain
#EXAMPLE
# print8chain(rbmn0$chain2$v,check=TRUE);
# print8chain(rev8chain(rbmn0$chain2$v,check=TRUE),check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_02_07
#REVISED 11_02_07
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8chain(chain);
    if (length(che)>0) {
      print(che);
      stop("The provided 'chain' is not valid!");
    }
  }
  # building it
  res <- vector("list",0);
  res$names <- rev(chain$names);
  res$roots <- chain$roots;
  res$colliders <- chain$colliders;
  res$mu <- rev(chain$mu);
  res$sigma <- rev(chain$sigma);
  res$corre <- rev(chain$corre);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbn7chain9 <- function(nbn,order=FALSE,check=rbmn0$check$v)
#TITLE Checks if a given /nbn/ is a /chain/
#DESCRIPTION returns \samp{TRUE} [the order] or
# \samp{FALSE} [NULL] according that \samp{nbn}
# is a chain of not [according to \samp{order}].
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn}<< The nbn object to consider.>>
#[INPUTS]
#{order} << When \samp{FALSE} the answer to the
# question is returned with \samp{TRUE} or \samp{FALSE}.\cr
# When \samp{TRUE} the chain order
# of the nodes is returned if it is a /chain/
# else \samp{NULL}.>>
#{check} << Must a check of the arguments be performed?>>
#VALUE
# A \samp{logical(1)} when \samp{order} si \samp{TRUE} if not
#  the resulting chain order versus NULL.
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# nbn7chain9(rbmn0$nbn1$v,check=TRUE);
# nbn7chain9(rbmn0$nbn4$v,check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_02_14
#REVISED 11_02_14
#--------------------------------------------
{
  # checking
  if (check) {
    che <- check8nbn(nbn);
    if (length(che)>0) {
      print(che);
      stop("The provided 'nbn' is not valid!");
    }
    sssobject9(order,"logical",1,mensaje="'order' is not acceptable");
  }
  # constants
  nn <- length(names(nbn));
  # degenerate case
  if (nn == 1) {
    if (order) {
      return(1);
    } else {
      return(TRUE);
    }
  }
  # symmetrical relationship matrix
  rela <- adja4nbn(nbn,check=FALSE);
  rela <- rela + t(rela);
  # checking obvious cases
  res <- TRUE;
  nbr <- apply(rela,1,sum);
  if (sum(nbr == 1)!= 2) { res <- FALSE;}
  if (sum(nbr == 2)!= nn-2) { res <- FALSE;}
  if (!res) {
    if (order) {
      return(NULL);
    } else {
      return(FALSE);
    }
  }
  # looking for a chain between nodes
  anc <- which(nbr==1)[1];
  orde <- anc;
  mauvais <- FALSE;
  while ((length(orde)<nn) & !mauvais) {
    nou <- which(rela[anc,]==1);
    if (length(nou) != 1) {
      mauvais <- TRUE;
    } else {
      rela[anc,nou] <- rela[nou,anc] <- 0;
      orde <- c(orde,nou);
      anc <- nou;
    }
  }
  #
  if (mauvais) {
    if (order) {
      res <- NULL;
    } else {
      res <- FALSE;
    }
  } else {
    if (!order) {
      res <- TRUE;
    } else {
      res <- orde;
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbn2chain <- function(nbn,check=rbmn0$check$v)
#TITLE transforms a /nbn/ into a /chain/
#DESCRIPTION returns the chain obtained
# from \samp{nbn} which is supposed to a chain.
# If it is not a chain, an error is issued.
#DETAILS
# It is advised to use \samp{nbn7chain9} before
# calling this function.
#KEYWORDS 
#INPUTS
#{nbn}<< The /nbn/ object to consider.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#VALUE
# The resulting chain
#EXAMPLE
# rbmn0 <- complete8rbmn0(rbmn0);
# print8chain(nbn2chain(rbmn0$nbn2$v,check=TRUE),check=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_02_14
#REVISED 11_02_15
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
  # getting one of the two possible chain orders
  ooo <- nbn7chain9(nbn,TRUE);
  # building the chain except for correlations
  res <- vector("list",0);
  res$names <- names(nbn)[ooo];
  res$sigma <- sapply(nbn,function(a){a$sigma;})[ooo];
  res$mu    <- sapply(nbn,function(a){a$mu;})[ooo];
  nbpa <- sapply(nbn,function(a){length(a$parents);})
  nbpa <- nbpa[ooo];
  res$roots <- res$names[which(nbpa==0)];
  res$colliders <- res$names[which(nbpa==2)];
  res$corre <- rep(0.5,length(res$names)-1);
  # getting a topological order
  too <- topo4chain(res,check=FALSE);
  res$corre <- rep(0.5,length(res$names)-1);
  sig <- rep(NA,length(res$names));
  # adding the correlations
  if (length(res$names)>1) {
    for (nnu in sssbf(res$names)) {
      nnn <- too[nnu];
      nna <- res$names[nnn];
      if (!(nna %in% res$roots)) {
        papa <- nbn[[nna]]$parents;
        # computing the necessary standard deviations
        sb <- res$sigma[nnn];
        coco <- sasa <- numeric(0);
        for (pn in sssbf(papa)) {
          pp <- papa[pn];
          reco <- nbn[[nna]]$regcoef[pn];
          if (nnn > 1) {if (res$names[nnn-1]==pp) {
            sa <- sig[nnn-1];
            sasa <- c(sasa,sa);
            coco <- c(coco,reco);
          }}
          if (nnn < length(res$names)) {if (res$names[nnn+1]==pp) {
            sa <- sig[nnn+1];
            sasa <- c(sasa,sa);
            coco <- c(coco,reco);
          }}
        }
        # getting the marginal variance
        sb <- sqrt(sum(coco^2*sasa^2)+sb^2);
        sig[nnn] <- sb;
        # computing the correlation(s)
        for (pn in sssbf(papa)) {
          pp <- papa[pn];
          reco <- nbn[[nna]]$regcoef[pn];
          if (nnn > 1) {if (res$names[nnn-1]==pp) {
            sa <- sig[nnn-1];
            res$corre[nnn-1] <- reco * sa / sb;
          }}
          if (nnn < length(res$names)) {if (res$names[nnn+1]==pp) {
            sa <- sig[nnn+1];
            res$corre[nnn] <-  reco * sa / sb;
          }}
        }
      } else {
        # conditional and marginal variances are equal
        sig[nnn] <- res$sigma[nnn];
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
