
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bnfit2nbn <- function(bn.fit) 
#TITLE transforms a /bn.fit/ of /bnlearn/ package to a /nbn/
#DESCRIPTION returns a \samp{nbn} object
# from a Gaussian \samp{bn.fit} object of /bnlearn/
# package.
#DETAILS
# If \samp{bn.fit} is not pertinent, a fatal error
# is issued.
#KEYWORDS 
#INPUTS
#{bn.fit}<< The object to be transformed.>>
#[INPUTS]
#VALUE
# A list following the \samp{nbn} specification
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_05_11
#REVISED 12_05_11
#--------------------------------------------
{
  # checking
  if (class(bn.fit) != "bn.fit") {
    ssserreur(bn.fit,message="This object is not of class 'bn.fit'");
  }
  # initializing the resulting object
  res <- vector("list",length(bn.fit));
  names(res) <- names(bn.fit);
  # filling each node
  for (nn in sssbf(res)) {
    sigma <- bn.fit[[nn]]$sd;
    bnc <- bn.fit[[nn]]$coefficients;
    mui <- which(names(bnc)=="(Intercept)");
    if (length(mui)!=1) {
      ssserreur(list(nn,bn.fit[[nn]]),
             message="unexpected node of 'bn.fit'");
    }
    mu <- bnc[mui];
    parents <- names(bnc)[-mui];
    regcoef <- bnc[-mui];
    res[[nn]] <- list(mu=mu,sigma=sigma,parents=parents,regcoef=regcoef);
  }
  # providing the topological order
  tor <- topo4nbn(res);
  res <- res[tor];
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2nbn <- function(bn) 
#TITLE transforms a /bn/ of /bnlearn/ package to a /nbn/
#DESCRIPTION returns a \samp{nbn} object
# from a DAG (\samp{bn} object) of /bnlearn/
# package. O and 1 coefficients are introduced...
#DETAILS
#KEYWORDS 
#INPUTS
#{bn}<< The object to be transformed.>>
#[INPUTS]
#VALUE
# A list following the \samp{nbn} specification
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_05_02
#REVISED 13_05_02
#--------------------------------------------
{
  # checking
  if (class(bn) != "bn") {
    ssserreur(bn.fit,message="This object is not of class 'bn'");
  }
  nono <- bn$nodes
  # initializing the resulting object
  res <- vector("list",length(nono));
  names(res) <- names(nono);
  # filling each node
  for (nn in names(res)) {
    sigma <- 1;
    mu <- 0;
    parents <- nono[[nn]]$parents;
    regcoef <- rep(1,length(parents));
    res[[nn]] <- list(mu=mu,sigma=sigma,parents=parents,regcoef=regcoef);
  }
  # providing a topological order
  tor <- topo4nbn(res);
  res <- res[tor];
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbn2bnfit <- function(nbn,onlydag=FALSE,check=rbmn0$check$v) 
#TITLE transforms a /nbn/ to a /bn.fit/ of /bnlearn/ package
#DESCRIPTION returns a \samp{bn.fit} object
# from a Gaussian \samp{nbn} object of /rbmn/
# package.
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn}<< The object to be transformed.>>
#[INPUTS]
#{check} << Must a check of the arguments be performed?>>
#{onlydag} << Indicates if only the DAG must be computed. In that
# case a /bn/ object of /bnlearn/ is returned.>>
#VALUE
# The resulting \samp{bn.fit} (or \samp{bn}) object.
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_11_29
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
  }
  # creating the dag
  mod <- string7dag4nbn(nbn,":");
  dag <- model2network(mod);
  if (onlydag) { return(dag);}
  # incorporating the parameters
  prodis <- vector("list",0);
  for (nn in sssbf(nbn)) {
    para <- vector("list",0);
    para$coef <- c(nbn[[nn]]$mu);
    for (pp in sssbf(nbn[[nn]]$parents)) {
      para$coef <- c(para$coef,nbn[[nn]]$regcoef[pp]);
    }
    names(para$coef) <- c("(Intercept)",nbn[[nn]]$parents);
    para$sd <- nbn[[nn]]$sigma;
    prodis[[nn]] <- para;
  }
  names(prodis) <- names(nbn);
  # creating the bnfit
  res <- custom.fit(dag,dist=prodis);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
