
rbsb3k("reset");
 g4n3k("reset");

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
prr3k <- function(whi)
#TITLE  assigns the prr constants
#DESCRIPTION
# defines or returns the constants used within /prr/. 
# The performed action depends on the argument.
#DETAILS
#PKEYWORDS helpful
#KEYWORDS misc
#INPUTS
#{whi}    <<a \code{character(1)} indicating either to reset or
#           to return the names or the current values. The three possible
#           values are \code{RESET}, \code{reset}, \code{names}, \code{definitions} or \code{values}.>>
#[INPUTS]
#VALUE
# When \code{whi=="RESET"} or \code{whi=="reset"}
# nothing (but the assignments of 
# the layer(s) \code{prr} are performed).\cr
# When \code{whi=="names"} the names as a character vector are returned.\cr
# When \code{whi=="definitions"} the definitions as a named character vector are returned.\cr
# When \code{whi=="values"} the values through a named list are returned.\cr
#EXAMPLE
## First assign the standard values
# prr3k("RESET");
## to get the short labels
# prr3k("names");
## to obtain the current values
# prr3k("values");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_14
#REVISED 10_11_29
#--------------------------------------------
{
# checking
if (!(whi %in% c("RESET","reset","names","definitions","values"))) {
    print.default(whi);
    stop("prr3k does not accept this argument");
}
#
# definition of the different constants
sc <- md <- character(0);
#
sc["mck"]     <- "Systematic checks?"; 
sc["mwa"]     <- "Must warning be simple warning?"; 
sc["rwa"]     <- "Warning messages issued by some functions"; 
sc["type"]    <- "Default type for the nodes";
sc["tag1"]    <- "Tags for file representing a list";
sc["tag2"]    <- "Tags for easyprogramming expressions";
sc["tdis"]    <- "Possible types of prr distributions";
#
sc["support"]   <- "Default support for the nodes";
sc["cV"]      <- "Default cV for the nodes";
sc["cU"]      <- "Default cU for the nodes";
sc["round"]   <- "Default for rounding";
sc["default"] <- "Name for the default block into files";
#
sc["probab1"] <- "Example 1 of /probab/";
sc["probab2"] <- "Example 2 of /probab/";
sc["probab3"] <- "Example 3 of /probab/";
sc["probab4"] <- "Example 4 of /probab/";
#
sc["distri1"] <- "Example 1 of /distri/";
sc["distri2"] <- "Example 2 of /distri/";
sc["distri3"] <- "Example 3 of /distri/";
sc["distri4"] <- "Example 4 of /distri/";
#
sc["assess1"] <- "Example 1 of /assess/";
sc["assess2"] <- "Example 2 of /assess/";
sc["assess3"] <- "Example 3 of /assess/";
sc["assess4"] <- "Example 4 of /assess/";
#
sc["nod1"]    <- "Example 1 of /nod/";
sc["nod2"]    <- "Example 2 of /nod/";
sc["nod3"]    <- "Example 3 of /nod/";
sc["nod4"]    <- "Example 4 of /nod/";
sc["nod5"]    <- "Example 5 of /nod/";
#
sc["chain1"]  <- "Example 1 of /chain/";
sc["chain2"]  <- "Example 2 of /chain/";
sc["chain3"]  <- "Example 3 of /chain/";
#
# resetting prevous constants
#
if (whi=="RESET") {
    rbsb3k("RESET");
    g4n3k ("RESET");
}
#
# returning the names
#
if (whi=="names") { return(names(sc));}
#
# returning the definitions
#
if (whi=="definitions") { return(sc);}
#
# returning the values
#
if (whi=="values") {
    res <- vector("list",0);
    for (ii in bf(sc)) {
        noco <- names(sc)[[ii]];
        eee <- paste("res[[\"",noco,"\"]] <- prr.",noco,";",sep="");
        eval(parse(text=eee));
    }
    return(res);
}
# 
# defining the basic constants
#
    prr.mck        <- TRUE;
    prr.mwa        <- TRUE;
    prr.rwa        <- rbsb.cha0;
    prr.type       <- "beta";
    prr.support    <- c(-Inf,Inf); 
    prr.cV         <- 0;
    prr.cU         <- 0;
    prr.round      <- 3;
    prr.default    <- "default";
    prr.tag1       <- matrix(c("<<",">>",
                               "((","))"),
                             ncol=2,byrow=TRUE);
    prr.tag2       <- matrix(c("{{","}}"),
                             ncol=2,byrow=TRUE);
    prr.tdis       <- c("normal","lognormal","beta","categorical");
#
# assigning the basic constants
# 
    assign("prr.mck",      prr.mck,       pos=".GlobalEnv");
    assign("prr.mwa",      prr.mwa,       pos=".GlobalEnv");
    assign("prr.rwa",      prr.rwa,       pos=".GlobalEnv");
    assign("prr.type",     prr.type,      pos=".GlobalEnv");
    assign("prr.support",  prr.support,   pos=".GlobalEnv");
    assign("prr.cV",       prr.cV,        pos=".GlobalEnv");
    assign("prr.cU",       prr.cU,        pos=".GlobalEnv");
    assign("prr.round",    prr.round,     pos=".GlobalEnv");
    assign("prr.default",  prr.default,   pos=".GlobalEnv");
    assign("prr.tag1",     prr.tag1,      pos=".GlobalEnv");
    assign("prr.tag2",     prr.tag2,      pos=".GlobalEnv");
    assign("prr.tdis",     prr.tdis,      pos=".GlobalEnv");
#
# /probab/ constants
#
    prr.probab1    <- new("probab",family="beta",
                          pafixe=c(1,2,1,1.5));
    prr.probab2    <- new("probab",family="normal",
                          pafixe=c(0,Inf));
    prr.probab3    <- new("probab",family="lognormal",
                          pafixe=c(0,1,1,100));
    prr.probab4    <- new("probab",family="categorical",
                          pafixe=0:5);
#
    assign("prr.probab1",  prr.probab1,   pos=".GlobalEnv");
    assign("prr.probab2",  prr.probab2,   pos=".GlobalEnv");
    assign("prr.probab3",  prr.probab3,   pos=".GlobalEnv");
    assign("prr.probab4",  prr.probab4,   pos=".GlobalEnv");
#
# /distri/ constants
    prr.distri1    <- new("distri",probab=prr.probab1,nbdraw=50,
                          parand=matrix(c(1.25,50),ncol=2));
    prr.distri2    <- new("distri",probab=prr.probab2,nbdraw=18,
                          parand=matrix(c(10,18),ncol=2));
    prr.distri3    <- new("distri",probab=prr.probab3,nbdraw=25,
                          parand=matrix(c(10,90),ncol=2));
    prr.distri4    <- new("distri",probab=prr.probab4,nbdraw=8,
                          parand=matrix(c(2,60),ncol=2));
#
#
    assign("prr.distri1",  prr.distri1,   pos=".GlobalEnv");
    assign("prr.distri2",  prr.distri2,   pos=".GlobalEnv");
    assign("prr.distri3",  prr.distri3,   pos=".GlobalEnv");
    assign("prr.distri4",  prr.distri4,   pos=".GlobalEnv");
#
# /assess/ constants
#
    prr.assess1    <- new("assess",x="1.23", cV=50,cU=60);
    prr.assess2    <- new("assess",x="100",cV=5, cU=10);
    prr.assess3    <- new("assess",x="abs({{prr.nod1}}*{{prr.nod2}})",
                                   cV=0,cU=0);
    prr.assess4    <- new("assess",x="2",  cV=50,cU=10);
#
    assign("prr.assess1",  prr.assess1,   pos=".GlobalEnv");
    assign("prr.assess2",  prr.assess2,   pos=".GlobalEnv");
    assign("prr.assess3",  prr.assess3,   pos=".GlobalEnv");
    assign("prr.assess4",  prr.assess4,   pos=".GlobalEnv");
#
# /nod/ constants
#
    prr.nod1       <- new("nod",name="prr.nod1",
                                probab=prr.probab1,
                                assess=prr.assess1,
                                format=c(3,0,0)
                         );
    prr.nod2       <- new("nod",name="prr.nod2",
                                probab=prr.probab2,
                                assess=prr.assess2,
                                format=c(1,0,0)
                         );
    prr.nod3       <- new("nod",name="prr.nod3",
                                probab=prr.probab2,
                                assess=prr.assess3,
                                format=c(1,-1,-1)
                         );
    prr.nod4       <- new("nod",name="prr.nod4",
                                probab=prr.probab4,
                                assess=prr.assess4,
                                format=c(1,0,0)
                         );
    prr.nod5       <- new("nod",name="prr.nod5",
                                probab=new("probab",
                                           family="lognormal",
                                           pafixe=c(0,1,1,Inf)),
                                assess=new("assess",x="10000",
                                           "cV"=20,"cU"=50),
                                format=c(0,0,0)
                         );
#
    assign("prr.nod1",     prr.nod1,      pos=".GlobalEnv");
    assign("prr.nod2",     prr.nod2,      pos=".GlobalEnv");
    assign("prr.nod3",     prr.nod3,      pos=".GlobalEnv");
    assign("prr.nod4",     prr.nod4,      pos=".GlobalEnv");
    assign("prr.nod5",     prr.nod5,      pos=".GlobalEnv");
#
# /chain/ constants
#
    prr.chain1     <- new("chain",des=new("des",name="prr.chain1"),
                                  nos=list(prr.nod1)
                         );
    prr.chain2     <- new("chain",des=new("des",name="prr.chain2"),
                                  nos=list(prr.nod1,prr.nod2)
                         );
    prr.chain3     <- new("chain",des=new("des",name="prr.chain3"),
                                  nos=list(prr.nod1,prr.nod2,prr.nod3)
                         );
#
    assign("prr.chain1",   prr.chain1,    pos=".GlobalEnv");
    assign("prr.chain2",   prr.chain2,    pos=".GlobalEnv");
    assign("prr.chain3",   prr.chain3,    pos=".GlobalEnv");
#
# returning nothing
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ubeta <- function(mu,coefvar,support=c(0,1),trunc=support) 
#TITLE adapted draws of a Beta distribution
#DESCRIPTION
# This function slightly generalizes the basic \code{rbeta}
# function, allowing translation and dilatation with \code{support}
# and truncation with \code{trunc}.
# Also the retained parameterization with expectation \code{mu}
# and coefficient of variation \code{coefvar} encompasses some
# degenerate cases.\cr
# When the intersection of \code{support} and \code{trunc} is
# empty, an error is issued.\cr
# The number of returned draws is \code{max(length(mu),length(coefvar))}. When both
# lengths are not equal, the smaller one must be equal to one, if not an
# error is issued.\cr
# NA value are possible in \code{mu} and \code{coefvar},
# then missing values are returned for the considered draws.\cr
# Be aware that giving some of the \code{mu} either equal to \code{support[1]} or
# equal to \code{support[2]} will imply draws fixed to this value if it belongs to
# the truncation interval (\code{NA} otherwise). So an extreme value for \code{mu}
# means that it is sure, not compensated by \code{coefvar}. We advise not
# to introduce such degenerate cases.
#DETAILS
# The proposed parameterization is given by the following relationships with
# the standard \code{a} and \code{b} parameters when \code{support=c(0,1)}.\cr
# \code{mu = a / (a+b)} and \code{coefvar = 200 / (a+b)},\cr
# \code{a = 200 * (mu / coefvar)} and \code{b = 200 * ((1-mu)/coefvar)}.\cr
# When \code{mu} is equal to \code{support[1]} or \code{support[2]}, this is
# a degenerate case considered as fixed with \code{coefvar} is null whatever is
# the provided value.
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{mu}<<numeric vector of the expectation of the beta, must be in
# the interval defined by \code{support}.>>
#{coefvar}<<numeric vector of the coefficient of variation. Must be comprised
# between 0 and 100.>>
#[INPUTS]
#{support} << The two limits of the interval for which the beta
# is defined (equivalent to a translation and dilatation).
#          The support interval is common for all draws. >>
#{trunc} <<The two limits to which distribution is truncated.
#          The truncation interval is common for all draws. 
# In fact, it is computed as the intersection with \code{support}
# which cannot be empty.>> 
#VALUE
# A vector of the drawn values, possibly containing \code{NA}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# ubeta(rep(0.8,20),2);
# ubeta(c(rep(0.8,19),NA),2,support=c(-10,20));
# ubeta(c(rep(0.8,19),NA),100*runif(20),support=c(-10,10),trunc=c(0.5,1.5));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# Some day, this function might be introduced in another package like /rbsb/
#AUTHOR J.-B. Denis
#CREATED 10_10_27
#REVISED 10_11_23
#--------------------------------------------
{
    #
    # number of simulations and putting parameters to the same length
    nbs <- max(length(mu),length(coefvar));
    # no simulation
    if (nbs == 0) { return(numeric(0));}
    #
    if (length(mu)!=length(coefvar)) {
        if (min(length(mu),length(coefvar))!=1) {
            erreur(list(length(mu),length(coefvar)),
                   "ubeta: non accepted length of the parameters");
        }
        if (length(mu)==1) { mu <- rep(mu,nbs);
        } else { coefvar <- rep(coefvar,nbs);}
    }
    #
    # dealing with the scheme of the missing values
    miva <- is.na(mu+coefvar);
    if (nbs==sum(miva)) {
        # only missing values
        return(rep(NA,nbs));
    }
    miva <- which(miva);
    mu[miva] <- median(mu,na.rm=TRUE);
    coefvar[miva] <- median(coefvar,na.rm=TRUE);
    #
    # checking of the arguments
    # the support
    check4tyle(support,rbsb.numer,2,message="ubeta: provided support is bad");
    if (diff(support) < 0) { erreur(support,"ubeta: not accepted support");}
    # the truncation support
    check4tyle(trunc,rbsb.numer,2,message="ubeta: provided truncation support is bad");
    if (diff(trunc) < 0) { erreur(trunc,"ubeta: not accepted truncation support");}
    # computing the restricted truncation support to the support
    trunc[1] <- max(trunc[1],support[1]);
    trunc[2] <- min(trunc[2],support[2]);
    if (diff(trunc) < 0) {
        erreur(list(support,trunc),
               "ubeta: not compatible support and trunc");}
    # the parameters
    if (!all(mu>=support[1]) | !all(mu<=support[2])) {
        erreur(list(range(mu),support),
                    "ubeta: some mu values are not into support");
    }
    if (!all(coefvar>=0)) {
        erreur(range(coefvar),
                    "ubeta: some coefvar values are strictly negative");
    }
    #
    # imposing the null dispersion for extreme values
    exva <- which((mu==support[1])|(mu==support[2]));
    coefvar[exva] <- 0;
    #
    # adding missing values when fixed outside the truncation support
    if (trunc[1]==trunc[2]) {
        # everybody is fixed
        fixed <- rep(TRUE,nbs);
    } else {
        fixed <- (coefvar==0);
    }
    mivap <- which(fixed & ((mu < trunc[1]) | (mu>trunc[2])));
    miva <- c(miva,mivap);
    #
    # reminding the fixed components
    fixed <- which(fixed);
    #
    # determining the non random draws
    noran <- union(miva,fixed);
    rando <- setdiff(1:nbs,noran);
    nbss <- length(rando);
    #
    # initializing the results
    res <- mu;
    #
    # Finally drawing wihtout any degenerate cases
    if (nbss > 0) {
        # pseudo random draws must be done
        xx <- (mu[rando]-support[1])/diff(support);
        cv <- coefvar[rando];
        if (all(support==trunc)) {
            # the standard rbeta can be used (no truncation)
            rr <- rbeta(nbss,200/cv*xx,200/cv*(1-xx));
            rr <- support[1] + (support[2]-support[1])*rr; 
        } else {
            # due to truncation, the repartition function must be used
            tru01 <- (trunc - support[1] ) / diff(support);
            aa <- 200/cv*xx; bb <- 200/cv*(1-xx);
            rr <- xx;
            for (jj in bc(nbss)) {
                rb <- pbeta(tru01,aa[jj],bb[jj]);
                rr[jj] <- runif(1,rb[1],rb[2]);
            }
            rr <- qbeta(rr,200/cv*xx,200/cv*(1-xx));
            rr <- support[1] + (support[2]-support[1])*rr;
            if (any((rr<trunc[1])|(rr>trunc[2]))) {
                # due to numerical errors some values
                # are outside the truncation support, another
                # calculation is proposed for them
                quels <- which((rr<trunc[1])|(rr>trunc[2]));
                prr.rwa <- c(prr.rwa,"ubeta");
                erreur(NULL,w=TRUE,length(quels),
                       "from 'ubeta'",
                       "random draws were outside the truncation support and",
                       "the precautionary algorithm was applied:",
                       "be very cautious about the provided results");
                for (jj in quels) {
                    rb <- pbeta(trunc,aa[jj],bb[jj]);
                    bornes <- qbeta(rb,200/cv[jj]*xx[jj],200/cv[jj]*(1-xx[jj]));
                    bornes <- support[1] + (support[2]-support[1])*bornes;
                    if (bornes[1]>=trunc[2]) {
                        bornes <- trunc;
                    } else {
                        if (bornes[2]<=trunc[1]) {
                            bornes <- trunc;
                        } else {
                            bornes[1] <- max(bornes[1],trunc[1]);
                            bornes[2] <- min(bornes[2],trunc[2]);
                        }
                    }
                    # precaution
                    if (diff(bornes) <= 0) {
                        rapport("'ubeta' bornes were badly computed");
                    }
                    rr[jj] <- runif(1,bornes[1],bornes[2]);
                }
                # no need for transformation
            }
        }
        # replacing drawn values
        res[rando] <- rr;
    }
    # replacing the NAs
    res[miva] <- NA;
    #
    # returning
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
unormal <- function(mu,coefvar,trunc=c(-Inf,Inf)) 
#TITLE adapted draws of a normal distribution
#DESCRIPTION
# This function slightly generalizes the basic \code{rnorm}
# function, allowing truncation with \code{trunc}.\cr
# The number of returned draws is 
# \code{max(length(mu),length(coefvar))}. When both
# lengths are not equal, the smaller one must be equal to one, if not an
# error is issued.\cr
# \code{NA} value are possible in \code{mu} and \code{coefvar},
# then missing values are returned for the considered draws.
#DETAILS
# To comply with /prr/ point of view, the second parameter of the
# normal distribution is given through the variation coefficient.
# The relation with more standard sigma is given by\cr
#    sigma = (coefvar/100)*abs(mu) and\cr
#  coefvar = (100*sigma) / abs(mu).
#  Notice that
# as the standard deviation is given through the variation coefficient,
# expectation values too close to zero can be problematic. This is why, such
# values are discarded and replaced by \code{NA}. A local constant is used for
# the test.
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{mu}<<numeric vector of the expectation of the normal.>>
#{coefvar}<<numeric vector of the coefficient of variation.
# Cannot be negative.>>
#[INPUTS]
#{trunc} <<The two limits to which distribution is truncated.
#          The truncation interval is common for all draws. >> 
#VALUE
# A vector of the drawn values, possibly containing \code{NA}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# unormal(rep(10,20),2);
# unormal(c(rep(10,19),NA),2);
# unormal(c(rep(10,19),NA),2,trunc=c(8,12));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# Some day, this function might be introduced in another package like /rbsb/
#AUTHOR J.-B. Denis
#CREATED 10_10_28
#REVISED 10_11_29
#--------------------------------------------
{
    epsi <- 10^-20;
    #
    # number of simulations and putting parameters to the same length
    nbs <- max(length(mu),length(coefvar));
    # no simulation
    if (nbs == 0) { return(numeric(0));}
    #
    if (length(mu)!=length(coefvar)) {
        if (min(length(mu),length(coefvar))!=1) {
            erreur(list(length(mu),length(coefvar)),
                   "unormal: non accepted length of the parameters");
        }
        if (length(mu)==1) { mu <- rep(mu,nbs);
        } else { coefvar <- rep(coefvar,nbs);}
    }
    #
    # imposing NA for too small values
    mu[abs(mu/coefvar) < epsi] <- NA;
    #
    # dealing with the scheme of the missing values
    miva <- is.na(mu+coefvar);
    if (nbs==sum(miva)) {
        # only missing values
        return(rep(NA,nbs));
    }
    miva <- which(miva);
    mu[miva] <- median(mu,na.rm=TRUE);
    coefvar[miva] <- median(coefvar,na.rm=TRUE);
    #
    # checking of the arguments
    # the truncation support
    check4tyle(trunc,rbsb.numer,2,message="unormal: provided trunc is bad");
    if (diff(trunc) < 0) { erreur(trunc,"unormal: not accepted truncation support");}
    # positivity of the coefficient of variation
    if (!all(coefvar>=0)) {
        erreur(range(coefvar),
                    "unormal: some coefvar values are negative");
    }
    #
    # determining the non random draws
    rando <- setdiff(1:nbs,miva);
    nbss <- length(rando);
    xx <- mu[rando]; 
    sig <- coefvar[rando]/100*abs(xx);
    #
    # initializing the results
    res <- mu;
    #
    # Finally drawing
    if (nbss > 0) {
        if (all(c(-Inf,Inf)==trunc)) {
            # the standard rnorm can be used (no truncation)
            rr <- rnorm(nbss,xx,sig);
        } else {
            # due to truncation, the repartition function must be used
            rr <- xx;
            for (jj in bc(nbss)) {
                rb <- pnorm(trunc,xx[jj],sig[jj]);
                rr[jj] <- runif(1,rb[1],rb[2]);
            }
            rr <- qnorm(rr,xx,sig);
            if (any((rr<trunc[1])|(rr>trunc[2]))) {
                # due to numerical errors some values
                # are outside the truncation support, another
                # calculation is proposed for them
                quels <- which((rr<trunc[1])|(rr>trunc[2]));
                prr.rwa <- c(prr.rwa,"unormal");
                erreur(NULL,w=TRUE,length(quels),
                       "From 'unormal':",
                       "random draws were outside the truncation support and",
                       "the precautionary algorithm was applied:",
                       "be very cautious about the provided results");
                for (jj in quels) {
                    rb <- pnorm(trunc,xx[jj],sig[jj]);
                    bornes <- qbeta(rb,xx[jj],sig[jj]);
                    if (bornes[1]>=trunc[2]) {
                        bornes <- trunc;
                    } else {
                        if (bornes[2]<=trunc[1]) {
                            bornes <- trunc;
                        } else {
                            bornes[1] <- max(bornes[1],trunc[1]);
                            bornes[2] <- min(bornes[2],trunc[2]);
                        }
                    }
                    # precaution
                    if (diff(bornes) <= 0) {
                        rapport("'unormal' bornes were badly computed");
                    }
                    rr[jj] <- runif(1,bornes[1],bornes[2]);
                }
            }
        }
        # replacing drawn values
        res[rando] <- rr;
    }
    # replacing the NAs
    res[miva] <- NA;
    #
    # returning
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ulnormal <- function(mu,coefvar,support=c(0,+1),trunc=c(0,Inf)) 
#TITLE adapted draws of a log-normal distribution
#DESCRIPTION
# This function slightly generalizes the basic \code{rlnorm}
# function, allowing a different support that \code{[0,+Inf[}
# with \code{support} and truncation with \code{trunc}.\cr
# The number of returned draws is 
# \code{max(length(mu),length(coefvar))}. When both
# lengths are not equal, the smaller one must be equal to one, if not an
# error is issued.\cr
# NA value are possible in \code{mu} and \code{coefvar},
# then missing values are returned for the considered draws.\cr
# Notice that \code{support[2]} is not a bound, only its sign is used
# to know if the support is \code{[support[1],+Inf[} when plus or
# \code{]-Inf,support[1]} when minus.
#DETAILS
# To comply with /prr/ point of view, the second parameter of the
# log-normal distribution is given through the variation coefficient.\cr
# In fact, after making the appropriate transformation, this function
# calls the \code{unormal} function. To make the code safer, also the check
# of the consistency of the parameters is made by \code{unormal} not by
# \code{ulnormal}.\cr
# The remark made for \code{unormal} applies to this function too (since it is 
# called). This can generate missing values (\code{NA}) for too small \code{mu} 
# components.
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{mu}<<numeric vector of the expectation of the normal (i.e. in the log scale).>>
#{coefvar}<<numeric vector of the coefficient of variation (also in the log scale).
# Cannot be negative.>>
#[INPUTS]
#{support} <<Defines the support of the log-normal distribution. This is
# either  \code{[support[1],+Inf[} when \code{support[2] >= 0} or
# \code{]-Inf,support[1]} when \code{support[2] < 0}.
# Of course, this is not in the log scale.
#          The support semi-interval is common for all draws. >>
#{trunc} <<The two limits (not in the log scale) 
# to which the distribution is truncated.
# It will be restricted to its intersection with the support.
#          The truncation interval is common for all draws. >> 
#VALUE
# A vector of the drawn values, possibly containing \code{NA}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# ulnormal(rep(10,20),2);
# ulnormal(c(rep(10,19),NA),2);
# ulnormal(c(rep(10,19),NA),2,trunc=c(8,12));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# Some day, this function might be introduced in another package like /rbsb/
#AUTHOR J.-B. Denis
#CREATED 10_10_29
#REVISED 10_11_16
#--------------------------------------------
{
    #
    # checking the support
    check4tyle(support,rbsb.numer,2,message="bad support provided to 'ulnormal'");
    if (abs(support[1])==Inf) {
        erreur(support,"The starting/ending constant cannot be +-Inf");
    }
    if (abs(support[2])!=1) {
        erreur(support,"The sign constant must be +-1");
    }
    #
    # providing the necessary transformation functions
    x2l <- function(x) { log((x-support[1])*support[2]);}
    l2x <- function(x) { exp(x)*support[2] + support[1];}
    #
    # calling 'unorm'
    res <- unormal(mu,coefvar,x2l(trunc));
    #
    # going back in the original scale
    res <- l2x(res);
    #
    # returning
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ucateg <- function(mu,coefvar,lim) 
#TITLE draws from a categorical distribution
#DESCRIPTION
# Following the description given in /prr/ document, returns draws
# from a so-called categorical distribution defined by the \code{lim}
# vector of limits. The number of limits must be greater or equal to 3.
# The returned values are in between the two extreme limits.\cr
# The number of returned draws is 
# \code{max(length(mu),length(coefvar))}. When both
# lengths are not equal, the smaller one must be equal to one, if not an
# error is issued.\cr
# NA value are possible in \code{mu} and \code{coefvar},
# then missing values are returned for the considered draws.
#DETAILS
# Extensive use is made of the function \code{ubeta}.
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{mu}<<numeric vector of the expectations. From it the central limit is choosen.>>
#{coefvar}<<numeric vector of coefficients of variation. Either a common value
# for every draws, or a value for each of the draws. Cannot be negative.>>
#{lim} <<vector of the ordered limits; at least three limits.
#          The limits are common for all draws. >>
#[INPUTS]
#VALUE
# A vector of the drawn values, possibly containing \code{NA}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# set.seed(1234);  
# ucateg(round(10*runif(25)),30,1:10);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# Some day, this function might be introduced in another package like /rbsb/
#AUTHOR J.-B. Denis
#CREATED 10_11_16
#REVISED 10_11_16
#--------------------------------------------
{
    #
    # number of simulations and putting parameters to the same length
    nbs <- max(length(mu),length(coefvar));
    # no simulation
    if (nbs == 0) { return(numeric(0));}
    #
    if (length(mu)!=length(coefvar)) {
        if (min(length(mu),length(coefvar))!=1) {
            erreur(list(length(mu),length(coefvar)),
                   "unormal: non accepted length of the parameters");
        }
        if (length(mu)==1) { mu <- rep(mu,nbs);
        } else { coefvar <- rep(coefvar,nbs);}
    }
    #
    # dealing with the scheme of the missing values
    miva <- is.na(mu+coefvar);
    if (nbs==sum(miva)) {
        # only missing values
        return(rep(NA,nbs));
    }
    miva <- which(miva);
    mu[miva] <- median(mu,na.rm=TRUE);
    coefvar[miva] <- median(coefvar,na.rm=TRUE);
    #
    # checking the limits
    check4tyle(lim,rbsb.numer,c(3,Inf),message="bad limit 'lim' in 'ucateg'");
    if (!all(diff(lim)>0)) {
        stop(lim,"Non ordered limits in 'ucateg'");
    }
    #
    # getting the central limits
    x2k <- function(x) {
        ulim <- 2:(length(lim)-1);
        uu <- outer(x,lim[ulim],function(a,b) {abs(a-b);});
        vv <- apply(uu,1,min);
        (uu==vv) %*% ulim;        
    }
    # just calling ubeta to get the output through some normalization
    eten <- 2; 
    kkk <- x2k(mu);
    nmu <- lim[kkk];
    bbi <- lim[pmax(kkk-eten,1)];
    bbs <- lim[pmin(kkk+eten,length(lim))];
    nmu <- (nmu-bbi) / (bbs-bbi);
    res <- ubeta(nmu,coefvar)
    res <- res * (bbs-bbi) + bbi;
    #
    # returning
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
###########################################
########
#((((((( NEW S4 CLASS probab
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8probab <- function(object)
#TITLE  checks a /probab/
#DESCRIPTION
#   This function checks /probab/ objects
#DETAILS
# It is the validity method for /probab/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The probab object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_23
#REVISED 10_11_23
#--------------------------------------------
{
    if (class(object)!="probab") {
        erreur(class(object),"This object is not of class 'probab'");
    }
    #
    res <- character(0);
    #
    # checking the proposed family
    if (!("family" %in% slotNames(object))) {
        res <- c(res,"This supposed /probab/ has no slot @family!");
    } else {
        if (exists("prr.tdis")) {
        # this condition has been introduced to allow the definition
        # of the prototype in the 'setClass' instruction
            res <- c(res,check4tyle(object@family,"character",1,prr.tdis,
                     "object@family not accepted",FALSE));
        }
    }
    #
    # checking the  parameters according to the family
    #
    if (!("pafixe" %in% slotNames(object))) {
        res <- c(res,"This supposed /probab/ has no slot @pafixe!");
    } else {
      npafix <- length(object@pafixe);
      if (object@family=="normal") {
        if (npafix != 2) {
          erreur(object@pafixe,"For 'normal', '@pafixe' must have TWO values");
        }
        if (diff(object@pafixe) <= 0) {
          erreur(object@pafixe,"For 'normal', '@pafixe' must define an INTERVAL");
        }
      }
      if (object@family=="lognormal") {
        if (npafix != 4) {
          erreur(object@pafixe,"For 'lognormal', '@pafixe' must have FOUR values");
        }
        if (is.infinite(object@pafixe[1])) {
          erreur(object@pafixe[3:4],"For 'lognormal', '@pafixe[1]' must be FINITE");
        }
        if (object@pafixe[2]^2 != 1) {
          erreur(object@pafixe[3:4],"For 'lognormal', '@pafixe[2]' must be +/-1");
        }
        if (diff(object@pafixe[3:4]) <= 0) {
          erreur(object@pafixe[3:4],"For 'lognormal', '@pafixe[3:4]' must define an INTERVAL");
        }
      }
      if (object@family=="beta") {
        if (npafix != 4) {
          erreur(object@pafixe,"For 'beta', '@pafixe' must have FOUR values");
        }
        if (any(is.infinite(object@pafixe))) {
          erreur(object@pafixe[3:4],"For 'beta', '@pafixe' must be FINITE");
        }
        if (diff(object@pafixe[1:2]) <= 0) {
          erreur(object@pafixe[1:2],"For 'beta', '@pafixe[1:2]' must define an INTERVAL");
        }
        if (diff(object@pafixe[3:4]) <= 0) {
          erreur(object@pafixe[3:4],"For 'beta', '@pafixe[3:4]' must define an INTERVAL");
        }
        if ((object@pafixe[3] < object@pafixe[1]) | (object@pafixe[2] < object@pafixe[4]) ) {
          erreur(object@pafixe,"for a beta /probab/ the truncation is not in the support");
        }
      }
      if (object@family=="categorical") {
        if (npafix < 3) {
          erreur(object@pafixe,"For a categorical /probab/ at least THREE limits are required");
        }
        if (any(diff(object@pafixe) < 0)) {
          erreur(object@pafixe,"For a categorical /probab/ limits are not increasing");
        }
      }
    }
    #
    # returning
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=prr.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("probab", representation(
         # specify the structural part of /distri/ objects
         family = "character",    # The family of the probabbution
                                  # (must be in the prr.tdis list)
         pafixe = "numeric"       # Fixed parameters depending of the family
                              ),
                    prototype(family="beta",
                              pafixe=c(0,1,0,1)
                         ),
                validity=valid8probab
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8probab <- function(x,...,empha=0)
#TITLE  prints a /probab/ object
#DESCRIPTION 
# prints, with or more emphasis, a /probab/ object.
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<probab object to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the \code{print} function.>>
#{empha} << degree of emphasis, between 0 and 10.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# prr3k("RESET"); # needed only for R checking, to be forgotten
# print(prr.probab1);
# print(prr.probab1,empha=2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_11_23
#REVISED 10_11_24
#--------------------------------------------
{
# some checks
if (prr.mck) {
    #
    check4valid(valid8probab(x));
    check4tyle(empha,"numeric",1,c(0,10));
    #
}
#
# printing
if (empha == 0) {
    # strict printing
    cat("/probab/",
        "of @family '",x@family,"'",sep="");
    cat(" and @pafixe =",x@pafixe,
        "\n");
} else {
    if (empha == 10) {
        # deluxe printing
        tit <- paste("/probab/ object of family '",x@family,"'",sep="");
        form3title(tit,"box");
        fait <- FALSE;
        if (x@family=="normal") {
            fait <- TRUE;
            if (all(x@pafixe[1:2]==c(-Inf,Inf))) {
                form3title(" (without any truncation)",box="no",laft=0);
            } else {
                form3title(paste(" Truncation on [",
                                 x@pafixe[1],",",
                                 x@pafixe[2],"]"),
                           box="no",laft=0);
            }
        }
        if (x@family=="lognormal") {
            fait <- TRUE;
            if (x@pafixe[2]==1) {
                support <- c(x@pafixe[1],Inf);
            } else {
                support <- c(-Inf,x@pafixe[1]);
            }
            form3title(paste("    Support on [",
                             support[1],",",
                             support[2],"]",sep=""),
                       box="no",laft=0);
            if (all(x@pafixe[3:4]==support)) {
                form3title(" (without any truncation)",box="no",laft=0);
            } else {
                form3title(paste(" Truncation on [",
                                 x@pafixe[3],",",
                                 x@pafixe[4],"]"),
                           box="no",laft=0);
            }
        }
        if (x@family=="beta") {
            fait <- TRUE;
            form3title(paste("   Support on [",
                             x@pafixe[1],",",
                             x@pafixe[2],"]",sep=""),
                       box="no",laft=0);
            if (all(x@pafixe[3:4]==x@pafixe[1:2])) { 
                form3title(" (without any truncation)",box="no",laft=0);
            } else {
                form3title(paste("Truncation on [",
                                 x@pafixe[3],",",
                                 x@pafixe[4],"]",sep=""),
                           box="no",laft=0);
            }
        }
        if (x@family=="categorical") {
            fait <- TRUE;
            nil <- length(x@pafixe)-2;
            form3title(paste("There are",nil,"internal limits"),
                       box="no",laft=0);
            form3title(paste("Limits are (",
                             x@pafixe[1],") ",
                             paste(x@pafixe[2:(1+nil)],collapse="|"),
                             " (",x@pafixe[2+nil],")",
                             sep=""),
                       box="no",laft=0);
        }
        if (!fait) {
          rapport(paste("print8probab: x@family =",x@family,"not done!"));
        }
    } else {
        if (empha >= 2) {
            form3repeat("=",45,TRUE);
        }
        if (empha >= 3) {
            form3line(1," ",1);
        }
        # standard printing
        tit <- paste("/probab/ object");
        nemph <- max(0,empha-2);
        form3titre(tit,nemph);
        cat("\n           Family = '",x@family,"'\n",sep="");
        cat(  "          @pafixe =",x@pafixe,"\n");
        if (empha >= 3) {
            form3line(1," ",1);
        }
        if (empha >= 2) {
            form3repeat("=",45,TRUE);
        }
    }
}
#
# returning nothing
invisible();
  }
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "probab"), print8probab);


###########################################
###########################################
########
#((((((( NEW S4 CLASS distri
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8distri <- function(object)
#TITLE  checks a /distri/
#DESCRIPTION
#   This function checks /distri/ objects
#DETAILS
# It is the validity method for /distri/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The distri object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_10_21
#REVISED 10_11_23
#--------------------------------------------
{
    if (class(object)!="distri") {
        erreur(class(object),"This object is not of class 'distri'");
    }
    #
    res <- character(0);
    #
    # checking the proposed probab
    #
    if (!("probab" %in% slotNames(object))) {
        erreur(class(object),"This supposed /distri/ has no slot @probab!");
    } else {
        rrr <- valid8probab(object@probab);
        if (!identical(rrr,TRUE)) {
          erreur(rrr,"This supposed /distri/ has got a bad @probab");
        }
    }
    #
    # checking the number of draws 
    #
    if (!("nbdraw" %in% slotNames(object))) {
        erreur(class(object),"This supposed /distri/ has no slot @nbdraw!");
    } else {
        res <- check4tyle(object@nbdraw,"numeric",1,c(0,Inf));
    }
    #
    # checking the @parand
    #
    if (!("parand" %in% slotNames(object))) {
        res <- c(res,"This supposed /distri/ has no slot @parand!");
    } else {
      if (!is.matrix(object@parand)) {
        erreur(object@parand," '@parand' must be a matrix");
      }
      if (ncol(object@parand) != 2) {
        erreur(object@parand," '@parand' must have two columns");
      }
      if ((min(object@parand[,2]) <   0) |
          (max(object@parand[,2]) > 100)) {
        erreur(range(object@parand[,2]),"The 'coefvar' must be between 0 and 100");
      }
    }
    #
    # some further consistencies about the @parand part with respect to @probab
    #
    npafix <- length(object@probab@pafixe);
    #
    if (object@probab@family=="normal") {
      # no check to perform
    }
    #
    if (object@probab@family=="lognormal") {
      if (!all(((range(object@parand[,1])-log(object@probab@pafixe[1]))*object@probab@pafixe[2])>0)) {
        erreur(list(range(object@parand[,1]),log(object@probab@pafixe[1])),"For lognormal proposed 'mu' not in the support");
      }
      if (object@probab@pafixe[2]==1) {
        if(!all((range(object@parand[,1])-object@probab@pafixe[3])>0)) {
          erreur(list(range(object@parand[,1]),object@probab@pafixe[3]),
                 "Not all the mu in the support for a 'lognormal' /distri/");
        }
      } else {
        if(!all((range(object@parand[,1])-object@probab@pafixe[3])<0)) {
          erreur(list(range(object@parand[,1]),object@probab@pafixe[3]),
                 "Not all the mu in the support for a 'lognormal' /distri/");
        }
      }
    }
    #
    if (object@probab@family=="beta") {
      if (!all(abs(belong2interval(object@parand[,1],object@probab@pafixe[1:2]))<=1)) {
        erreur(list(range(object@parand[,1]),object@probab@pafixe[1:2]),"For beta proposed 'mu' not in the support");
      }
    }
    #
    if (object@probab@family=="categorical") {
      # no check to perform
    }
    #
    # returning
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=prr.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("distri", representation(
         #
         # in the future some adaptation could undertaken
         # in order to tackle the i.i.d. case more straightforwardly
         # (useless repetitions of the same values in @parand)
         #
         # Even if some "NA" can exist in @parand, /distri/ object
         # are numerically specified which is not the case of /nod/
         # a more complicated description of a random variable since
         # it is defined with until three random variables.
         #
         #
         probab   = "probab",    # The structural part of the /distri/
         nbdraw   = "numeric",   # The number of draws (for i.i.d. cases)
                                 # 0 if not
         parand   = "matrix"     # The random part of the /distri/
                                 # when @nbdraw > 0, only the first line
                                 # is used and considered as constant if not
                                 #    the number of lines gives the number
                                 #                   of draws when drawing
                              ),
                    prototype(probab= new("probab",family="beta",pafixe=c(0,1,0,1)),
                              nbdraw=10,
                              parand=matrix(c(0.5,30),ncol=2)
                         ),
                validity=valid8distri
        );

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8distri <- function(x,...,empha=0)
#TITLE  prints a /distri/ object
#DESCRIPTION 
# prints, with or more emphasis, a /distri/ object.
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<distri object to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the \code{print} function.>>
#{empha} << degree of emphasis, between 0 and 10.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# prr3k("RESET"); # needed only for R checking, to be forgotten
# print(prr.distri1);
# print(prr.distri1,empha=2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_10_21
#REVISED 10_11_24
#--------------------------------------------
{
# some checks
if (prr.mck) {
    #
    check4valid(valid8distri(x));
    check4tyle(empha,"numeric",1,c(0,10));
    #
}
#
# printing
if (x@nbdraw == 0) {
  nbd <- nrow(x@parand);
} else {
  nbd <- x@nbdraw;
}
#
rparand <- rep("",ncol(x@parand));
for (jj in bf(rparand)) {
  rparand[jj] <- form3liste(range(x@parand[,jj]),OPA="[",CPA="]",
                        opa="",cpa="",sep=",");
}
if (empha == 0) {
    # strict printing
    cat("/distri/",
        "of family  =",x@probab@family,
        "with",nbd,"draws. @parand in ");
    form3liste(rparand,OPA="",CPA="",opa="",cpa="",sep="-",imp=TRUE,cr=FALSE);
    cat(" and @probab@pafixe =",x@probab@pafixe,
        "\n");
} else {
    if (empha == 10) {
        # deluxe printing
        tit <- paste("/distri/ object of family",x@probab@family);
        form3title(tit,"box");
        form3titre(paste("A total of",nbd,"draws"),1);
        if (x@nbdraw==0) {
            form3title(paste("     mu in",rparand[1]),box="no",laft=0);
            form3title(paste("coefvar in",rparand[2]),box="no",laft=0);
        } else {
            form3title(paste("        mu =",x@parand[,1]),box="no",laft=0);
            form3title(paste("   coefvar =",x@parand[,2]),box="no",laft=0);
        }
        fait <- FALSE;
        if (x@probab@family=="normal") {
            fait <- TRUE;
            if (all(x@probab@pafixe[1:2]==c(-Inf,Inf))) {
                form3title(" (without any truncation)",box="no",laft=0);
            } else {
                form3title(paste(" Truncation on [",
                                 x@probab@pafixe[1],",",
                                 x@probab@pafixe[2],"]"),
                           box="no",laft=0);
            }
        }
        if (x@probab@family=="lognormal") {
            fait <- TRUE;
            if (x@probab@pafixe[2]==1) {
                support <- c(x@probab@pafixe[1],Inf);
            } else {
                support <- c(-Inf,x@probab@pafixe[1]);
            }
            form3title(paste("    Support on [",
                             support[1],",",
                             support[2],"]",sep=""),
                       box="no",laft=0);
            if (all(x@probab@pafixe[3:4]==support)) {
                form3title(" (without any truncation)",box="no",laft=0);
            } else {
                form3title(paste(" Truncation on [",
                                 x@probab@pafixe[3],",",
                                 x@probab@pafixe[4],"]"),
                           box="no",laft=0);
            }
        }
        if (x@probab@family=="beta") {
            fait <- TRUE;
            form3title(paste("   Support on [",
                             x@probab@pafixe[1],",",
                             x@probab@pafixe[2],"]",sep=""),
                       box="no",laft=0);
            if (all(x@probab@pafixe[3:4]==x@probab@pafixe[1:2])) { 
                form3title(" (without any truncation)",box="no",laft=0);
            } else {
                form3title(paste("Truncation on [",
                                 x@probab@pafixe[3],",",
                                 x@probab@pafixe[4],"]",sep=""),
                           box="no",laft=0);
            }
        }
        if (x@probab@family=="categorical") {
            fait <- TRUE;
            nil <- length(x@probab@pafixe)-2;
            form3title(paste("There are",nil,"internal limits"),
                       box="no",laft=0);
            form3title(paste("Limits are (",
                             x@probab@pafixe[1],") ",
                             paste(x@probab@pafixe[2:(1+nil)],collapse="|"),
                             " (",x@probab@pafixe[2+nil],")",
                             sep=""),
                       box="no",laft=0);
        }
        if (!fait) {
          rapport(paste("print8distri: family =",x@probab@family,"not done!"));
        }
    } else {
        if (empha >= 2) {
            form3repeat("=",45,TRUE);
        }
        if (empha >= 3) {
            form3line(1," ",1);
        }
        # standard printing
        tit <- paste("/distri/ object");
        nemph <- max(0,empha-2);
        form3titre(tit,nemph);
        cat("     Family       =",x@probab@family,"\n");
        cat("        with",nbd,"draws.\n");
        cat("        @parand in ",
            form3liste(rparand,OPA="",CPA="",opa="",cpa="",sep="-",imp=FALSE)
           );
        cat("\n        @probab@pafixe =",x@probab@pafixe,"\n");
        if (empha >= 3) {
            form3line(1," ",1);
        }
        if (empha >= 2) {
            form3repeat("=",45,TRUE);
        }
    }
}
#
# returning nothing
invisible();
  }
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "distri"), print8distri);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rdistri <- function(distri,nbr=1)
#TITLE simulates with a /distri/ object
#DESCRIPTION 
# From the necessary numerical values simulates independent
# draws from any /distri/ distribution.
#DETAILS
#KEYWORDS
#PKEYWORDS nod
#INPUTS
#{distri} <<The /distri/ object to simulate with.>>
#[INPUTS]
#{nbr} <<Number of repetition for the simulation.>>
#VALUE
# a vector of the simulated values
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# rdistri(prr.distri1);
# rdistri(prr.distri2);
# rdistri(prr.distri3);
# rdistri(prr.distri4);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_17
#REVISED 10_11_24
#--------------------------------------------
{
#
# checking
if (prr.mck) {
    check4valid(valid8distri(distri),"From 'rdistri'");
}
#
# preparing the constant case
if (distri@nbdraw > 0) {
  uu <- numeric(0);
  for (cc in bc(ncol(distri@parand))) {
    uu <- cbind(uu,rep(distri@parand[,cc],distri@nbdraw));
  }
  distri@parand <- uu;
} else {
  uu <- distri@parand;
}
#
# preparing the repetitions
if (nbr>1) {
  for (repet in bc(nbr-1)) { 
    distri@parand <-rbind(distri@parand,uu);
  }
}
#
# simulating according to the type of distribution
fait <- FALSE;
if (distri@probab@family=="normal") {
  res <- unormal(distri@parand[,1],distri@parand[,2],
                 distri@probab@pafixe[1:2]
                );
  fait <- TRUE;
}
#
if (distri@probab@family=="lognormal") {
  res <- ulnormal(distri@parand[,1],distri@parand[,2],
                  distri@probab@pafixe[1:2],distri@probab@pafixe[3:4]
                 );
  fait <- TRUE;
}
#
if (distri@probab@family=="beta") {
  res <- ubeta(distri@parand[,1],distri@parand[,2],
               distri@probab@pafixe[1:2],distri@probab@pafixe[3:4]
              );
  fait <- TRUE;
}
#
if (distri@probab@family=="categorical") {
  res <- ucateg(distri@parand[,1],distri@parand[,2],
                distri@probab@pafixe
               );
  fait <- TRUE;
}
#
if (!fait) {
  rapport(paste("rdistri: family =",distri@probab@family,"not done!"));
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8distri <- function(distri,nbs=100,tit=deparse(substitute(distri)),
                        ...)
#TITLE plots the density of a /distri/.
#DESCRIPTION 
# Plots the numerically approximated distribution of a /distri/ distribution
# defined through a call to \code{plot(density())}.
# The approximation is made with the help of \code{nbs} simulations
# repeating the drawings in \code{rdistri}.
#DETAILS
#KEYWORDS
#PKEYWORDS probability
#INPUTS
#{distri} <<The distri object to get the repartition.>>
#[INPUTS]
#{nbs} <<Number of simulations to perform to approximate the density.>>
#{tit} <<Title for the graph.>>
#{\dots} <<To be passed to the standard 'plot' function.>>
#VALUE
# Nothing but a plot is issued.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# plot8distri(prr.distri1);
# plot8distri(prr.distri2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_19
#REVISED 10_11_23
#--------------------------------------------
{
#
# not all checks, some are deferred to 'rdistri'
# checking
if (prr.mck) {
  check4tyle(nbs,"numeric",1,c(1,Inf),"Number of simulations into 'plot8distri'");
  check4tyle(tit,"character",1,message="Title into 'plot8distri'");
}
#
# getting the simulations
simu <- rdistri(distri,nbr=nbs);
#
# plotting
if (!exists("xlim")) {
  xlim <- range(simu);
}
if (sum(!is.na(simu))==0) {
  erreur(distri,"Only NA values were obtained, no plot!",w=TRUE);
} else {
  plot(density(simu),main=tit,cex=0.5,xlim=xlim,...);
}
#
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
###########################################
########
#((((((( NEW S4 CLASS assess
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8assess <- function(object)
#TITLE  checks an /assess/
#DESCRIPTION
#   This function checks /assess/ objects
#DETAILS
# It is the validity method for /assess/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The assess object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_10_21
#REVISED 10_10_21
#--------------------------------------------
{
    if (class(object)!="assess") {
        erreur(class(object),"This object is not of class 'assess'");
    }
    #
    res <- character(0);
    #
    if (!("x" %in% slotNames(object))) {
        res <- c(res,"This supposed /assess/ has no slot @x!");
    } else {
        res <- c(res,check4tyle(object@x,"character",1,NULL,
                 "object@x not accepted",FALSE));
    }
    #
    #
    if (!("cV" %in% slotNames(object))) {
        res <- c(res,"This supposed /assess/ has no slot @cV!");
    } else {
        res <- c(res,check4tyle(object@cV,"numeric",1,
                 c(0,100),
                 "object@cV not accepted",FALSE));
    }
    #
    if (!("cU" %in% slotNames(object))) {
        res <- c(res,"This supposed /assess/ has no slot @cU!");
    } else {
        res <- c(res,check4tyle(object@cU,"numeric",1,
                 c(0,100),
                 "object@cU not accepted",FALSE));
    }
    #
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=prr.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("assess", representation(
         x    ="character", # central value of the node
                            # in fact an easyp expression with tags,
                            # so "12" is acceptable, as well as "{{p}}*(1-{{p}})"
                            # where "p" must be the name of a previous node
         cV   ="numeric",   # Variability coefficient of the node
         cU   ="numeric"    # Uncertainty coefficient of the node
                              ),
                prototype(x="0.3",cV=40,cU=0),
                validity=valid8assess
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8assess <- function(x,...,empha=0)
#TITLE  prints an /assess/ object
#DESCRIPTION 
# prints, with or more emphasis, an /assess/ object.
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<assess object to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the \code{print} function.>>
#{empha} << degree of emphasis, between 0 and 10.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# prr3k("RESET"); # needed only for R checking, to be forgotten
# print(prr.assess1);
# print(prr.assess1,empha=2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_10_21
#REVISED 10_11_24
#--------------------------------------------
{
# some checks
if (prr.mck) {
    #
    check4valid(valid8assess(x));
    check4tyle(empha,"numeric",1,c(0,10));
    #
}
#
# printing
if (empha == 0) {
    # minimal printing
    cat("/assess/",
        "with x =",paste("'",x@x,"'",sep=""),
        "cV =",x@cV,
        "cU =",x@cU,
        "\n");
} else {
    if (empha >= 3) {
        form3line(1," ",1);
    }
    if (empha >= 2) {
        form3repeat("=",45,TRUE);
    }
    # standard printing
    tit <- paste("/assess/ object");
    nemph <- max(0,empha-2);
    form3titre(tit,nemph);
    cat("\n          Central value = '",x@x,"'\n",sep="");
    cat(  "         Variability c. =",x@cV,"\n");
    cat(  "         Uncertainty c. =",x@cU,"\n");
    if (empha >= 3) {
        form3line(1," ",1);
    }
    if (empha >= 2) {
        form3repeat("=",45,TRUE);
    }
} 
#
# returning nothing
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "assess"), print8assess);


###########################################
###########################################
########
#((((((( NEW S4 CLASS nod
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8nod <- function(object)
#TITLE  checks a /nod/
#DESCRIPTION
#   This function checks /nod/ objects
#DETAILS
# It is the validity method for /nod/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The nod object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_14
#REVISED 10_11_23
#--------------------------------------------
{
    if (class(object)!="nod") {
        erreur(class(object),"This object is not of class 'nod'");
    }
    #
    res <- character(0);
    #
    if (!("name" %in% slotNames(object))) {
        res <- c(res,"This supposed /nod/ has no slot @name!");
    } else {
        res <- c(res,check4tyle(object@name,"character",1));
    }
    #
    if (!("probab" %in% slotNames(object))) {
        res <- c(res,"This supposed /nod/ has no slot @probab!");
    } else {
        rrr <- valid8probab(object@probab);
        if (!(identical(rrr,TRUE))) {
            res <- c(res,rrr);
        }
    }
    #
    if (!("assess" %in% slotNames(object))) {
        res <- c(res,"This supposed /nod/ has no slot @assess!");
    } else {
        rrr <- valid8assess(object@assess);
        if (!(identical(rrr,TRUE))) {
            res <- c(res,rrr);
        }
    }
    #
    if (!("format" %in% slotNames(object))) {
        res <- c(res,"This supposed /nod/ has no slot @format!");
    } else {
        res <- c(res,check4tyle(object@format,"integer",3,
                 c(-Inf,Inf),
                 "object@format not accepted",FALSE));
    }
    #
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=prr.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("nod", representation(
         name     = "character", # the name of the node
         probab   = "probab",    # main characteristics of the distribution
         assess   = "assess",    # assessment given by the expert completing @probab
         format   = "numeric"    # [1] number of decimals to round 'X'
                                 # (Inf or < 0 means no rounding)
                                 # [2] number of decimals to round 'm',
                                 # (Inf means no rounding,
                                 #  < 0 means no output)
                                 # [3] same for 'CV" as 'm'.
                              ),
                prototype(probab=new("probab",family="beta",
                                              pafixe=c(10,20,10,20)
                                    ),
                          assess=new("assess",x="20",cV=50,cU=10),
                          format=c(2,0,-1)
                          ),
                validity=valid8nod
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8nod <- function(x,...,empha=0)
#TITLE  prints a /nod/ object
#DESCRIPTION 
# prints, with or more emphasis, a /nod/ object.
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nod object to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the \code{print} function.>>
#{empha} << degree of emphasis, between 0 and 10.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# prr3k("RESET"); # needed only for R checking, to be forgotten
# print(prr.nod1);
# print(prr.nod1,empha=2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_09_14
#REVISED 10_10_21
#--------------------------------------------
{
# some checks
if (prr.mck) {
    #
    check4valid(valid8nod(x));
    check4tyle(empha,"numeric",1,c(0,10));
    #
}
#
# getting the parents
papa <- parents5nod(x);
if (length(papa)==0) {
  papa <- "{NO parents}";
} else {
  papa <- paste(papa,collapse=",");
  papa <- paste(prr.tag2[1],papa,prr.tag2[2],sep="");
}
#
# printing
if (empha == 0) {
    # minimal printing
    cat("/nod/",x@name,"\n");
    form3repeat(" ",8,TRUE,FALSE);
    print8probab(x@probab,empha=empha);
    form3repeat(" ",8,TRUE,FALSE);
    print8assess(x@assess,empha=empha);
    form3repeat(" ",8,TRUE,FALSE);
    cat("/format/ =",x@format);
    cat("  ");
    cat(papa);
    cat("\n");
} else {
    if (empha >= 2) {
        form3repeat("=",45,TRUE);
    }
    if (empha >= 3) {
        form3line(1," ",1);
    }
    # standard printing
    tit <- paste("/nod/ named",x@name);
    nemph <- max(0,empha-2);
    form3titre(tit,nemph);cat("\n");
    form3repeat(" ",4,TRUE,FALSE);
    print8probab(x@probab,empha=1);
    form3repeat(" ",4,TRUE,FALSE);
    print8assess(x@assess,empha=1);
    form3repeat(" ",4,TRUE,FALSE);
    form3titre("/format/",0);cat("\n");
    cru <- c("'X'","'m'","'CV'");
    for (ii in 1:3) {
      cat(form3justifie(cru[ii],12,3,FALSE));
      if (x@format[ii] < 0) {
        cat(" will not be printed\n");
      } else {
        cat(" will be printed with",round(x@format[ii]),"decimal(s)\n");
      }
    }
    form3repeat(" ",6,TRUE,FALSE);
    cat("<parents>:",papa,"\n");
    if (empha >= 3) {
        form3line(1," ",1);
    }
    if (empha >= 2) {
        form3repeat("=",45,TRUE);
    }
} 
#
# returning nothing
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "nod"), print8nod);

###########################################
###########################################
########
#((((((( NEW S4 CLASS chain
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8chain <- function(object)
#TITLE  checks a /chain/
#DESCRIPTION
#   This function checks /chain/ objects
#DETAILS
# It is the validity method for /chain/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The chain object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_14
#REVISED 10_09_16
#--------------------------------------------
{
    #
    if (class(object)!="chain") {
        erreur(class(object),"This object is not of class 'chain'");
    }
    #
    res <- character(0);
    #
    if (!("des" %in% slotNames(object))) {
        res <- c(res,"This supposed /chain/ has no slot @des!");
        return(res);
    } else {
        rr <- valid8des(object@des);
        if (!(identical(rr,TRUE))) {
            res <- c(res,rr);
        }
    }
    #
    if (!("nos" %in% slotNames(object))) {
        res <- c(res,"This supposed /chain/ has no slot @nos!");
        return(res);
    } else {
        no <- character(0);
        for (ii in bf(object@nos)) {no <- c(no,object@nos[[ii]]@name);}
        if (!is.list(object@nos)) {
            res <- c(res,"For this supposed /chain/, @nos is not a list");
        } else {
            # checking the names of the nodes
            if (length(unique(no)) != length(no)) {
                erreur(no,"inconsistent names",w=TRUE);
                res <- c(res,"this /chain@nos/ has got no or missing names");
            }
            # checking each node by itself
            for (nn in bf(object@nos)) {
                rr <- valid8nod(object@nos[[nn]]);
                if (!(identical(rr,TRUE))) {
                    res <- c(res,rr);
                }
            }
            # checking the parentship of each node
            for (nn in bf(object@nos)) {
                aspar <- parents5nod(object@nos[[nn]]);
                if (length(aspar)>0) {
                    if (nn == 1) {
                        res <- c(res,paste("The first node asked for",
                                           paste(aspar,collapse=" / "),
                                           "as parents!"));
                    } else {
                        avpar <- no[1:(nn-1)];
                        mipar <- setdiff(aspar,avpar);
                        if (length(mipar)>0) {
                            res <- c(res,paste("For the node (",
                                               no[nn],
                                               "), the parent(s) ",
                                               paste(mipar,collapse=" / "),
                                               " have not being introduced",
                                               " as previous node(s)",sep="")
                                    );
                        }
                    }
                }
            }
        }
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=prr.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("chain", representation(
         des ="des",    # description of the chain
         nos ="list"    # list of nodes
                              ),
                prototype(des=new("des",name="Empty chain",
                                        time=now("d"),
                                        comm="Created by /prr/"),
                          nos=vector("list",0)),
                validity=valid8chain
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8chain <- function(x,...,empha=0)
#TITLE  prints a /chain/ object
#DESCRIPTION
# prints, with or more emphasis, a /chain/ object.
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<chain object to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the \code{print} function.>>
#{empha} << degree of emphasis, 0 for one line printing.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# prr3k("RESET"); # needed only for R checking, to be forgotten
# print(prr.chain1);
# print(prr.chain1,empha=2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_09_14
#REVISED 10_11_24
#--------------------------------------------
{
# some checks
if (prr.mck) {
    check4valid(valid8chain(x));
    check4tyle(empha,"numeric",1,c(0,10));
}
if (empha %in% 0:1)  { wdes <- "n";}
if (empha %in% 2:3)  { wdes <- "ndr";}
if (empha %in% 4:10) { wdes <- "a";}
#
# printing the description
print8des(x@des,what=wdes,empha=empha);
#
# printing the nodes
nbnd <- length(x@nos);
form3titre(paste("This chain have got",nbnd,"node(s)"),empha=empha);
cat("\n");
#
for (nn in bc(nbnd)) {
    form3repeat(" ",4,TRUE,FALSE);
    print8nod(x@nos[[nn]],empha=empha);
}
#
# returning nothing
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "chain"), print8chain);

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2nod <- function(li)
#TITLE transforms a consistent list into a new nod object
#DESCRIPTION 
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of a /nod/ object.
# The main use of this function is to generate chains read from text files
# with the function \code{read8chain}.
#DETAILS
# Non used components of the list are not considered, this
# is the case for a possible \code{$name}.
#KEYWORDS
#PKEYWORDS nod
#INPUTS
#{li} <<The list to be transformed into a /nod/ object.>>
#[INPUTS]
#VALUE
# The generated 'nod' object
#EXAMPLE
# prr3k("RESET"); # (only for R checking)
# print(list2nod(nod2list(prr.nod1)));
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_15
#REVISED 10_09_15
#--------------------------------------------
{
# checking
if (prr.mck) {
    check4tyle(li,"list",-11,message="The first argument must be a list");
}
# checking the names
nano <- c("name","family","pafixe","x","cV","cU","format");
if (length(unique(c(names(li),nano))) != length(names(li))) {
    erreur(list(names(li),nano),"Some necessary slots for a /nod/ are missing");
}
#
# getting the different components
name <- paste(as.character(li$name),collapse=" ");
family <- paste(as.character(li$family,collapse=" "));
pafixe <- as.numeric(li$pafixe)
x <- paste(as.character(li$x),collapse=" ");
cV <- as.numeric(li$cV);
cU <- as.numeric(li$cU);
format <- round(as.numeric(li$format));
# building the nod
res <- new("nod",name=name,
                 probab=new("probab",family=family,pafixe=pafixe),
                 assess=new("assess",x=x,cV=cV,cU=cU),
                 format=format
          );
# checking the result
if (prr.mck) {check4valid(valid8nod(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nod2list <- function(nod)
#TITLE transforms a /nod/ into a consistent list
#DESCRIPTION 
# Transforms a /nod/ into a consistent list
# doing the necessary translations to make things
# easy for the end user.
#DETAILS
# According to \code{prr.mck}, the argument \code{nod}
# is checked or not.
#KEYWORDS
#PKEYWORDS nod
#INPUTS
#{nod} <<The nod object to be transformed into a list.>>
#[INPUTS]
#VALUE
# The generated list.
#EXAMPLE
# prr3k("RESET"); # (only for R checking)
# nod2list(prr.nod3);
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_15
#REVISED 10_09_16
#--------------------------------------------
{
# checking
if (prr.mck) {check4valid(valid8nod(nod));}
# preparing
nano <- c("name","family","pafixe","x","cV","cU","format");
res <- vector("list",length(nano));
names(res) <- nano;
# getting the different components
res[["name"]] <- nod@name;
res[["family"]] <- nod@probab@family;
res[["pafixe"]] <- nod@probab@pafixe;
res[["x"]] <- nod@assess@x;
res[["cV"]] <- nod@assess@cV;
res[["cU"]] <- nod@assess@cU;
res[["format"]] <- nod@format;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2chain <- function(li,chaind=1,nod=bf(li)[-1])
#TITLE transforms a consistent list into a new chain object
#DESCRIPTION 
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of a /des/ object and /nod/ objects.
# The main use of this function is to generate chains read from text files
# with the function \code{read8chain}.
#DETAILS
# Be aware that the node must be given in a compatible order
# with the sequential construction of the chain, that is 
# parents before children.
#KEYWORDS
#PKEYWORDS chain
#INPUTS
#{li} <<The list to be transformed into a chain object.
# The chaind-th component must be the description of the bn;
# the nod components are supposed to be the nodes.>>
#[INPUTS]
#{chaind} << the number of the \code{li} component to be
#         interpreted as the description of the chain.>>
#{nod} << the numbers of the \code{li} to be
#         considered as defining a node (in the right order).>>
#VALUE
# The generated /chain/ object
#EXAMPLE
# prr3k("RESET"); # (only for R checking)
# print(list2chain(chain2list(prr.chain1)));
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_15
#REVISED 10_11_29
#--------------------------------------------
{
# checking
if (prr.mck) {
  check4tyle(li,"list",c(1,Inf),message="A list is asked for");
  check4tyle(chaind,"numeric",1,c(1,length(li)),message="Not a component number of the list");
  check4tyle(nod,"numeric",c(0,Inf),c(1,length(li)),message="Not component numbers of the list");
  if (chaind %in% nod) {
    erreur(list(chaind,nod),"The description cannot be also a node!");
  }
}
#
# getting the chain in its different components
#
# interpreting the description
lides <- li[[chaind]];
for (uu in bf(lides)) {
    if (names(lides)[uu] == "comm") { what <- rbsb.vma["v"];
    } else { what <- rbsb.vma["c"];}
    lides[[uu]] <- char2vma(lides[[uu]],what); 
}
lides$name <- names(li)[chaind];
des <- list2des(lides);
# interpreting the nodes
nos <- vector("list",length(nod));
for (ii in bf(nod)) {
    # component of the list
    iii <- nod[ii];
    # getting the sublist
    lino <- li[[iii]];
    # adding its name
    lino$name <- names(li)[iii];
    # building the node
    nos[[ii]] <- list2nod(lino);
}
names(nos) <- names(li[nod]);
#
# creating the chain
res <- new("chain",des=des,nos=nos);
# checking
if (prr.mck) {
    check4valid(valid8chain(res));
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain2list <- function(chain)
#TITLE transforms a /chain/ into a list 
#DESCRIPTION 
# transforms a /chain/ into a list compatible with the
# function \code{list2chain}.
# The main use of this function is to be a part of
# the function \code{write8chain}.
#DETAILS
#KEYWORDS
#PKEYWORDS chain
#INPUTS
#{chain} <<The chain to be transformed.>>
#[INPUTS]
#VALUE
# The generated list.
#EXAMPLE
# prr3k("RESET"); # (only for R checking)
# chain2list(prr.chain1);
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_16
#REVISED 10_09_25
#--------------------------------------------
{
# checking
if (prr.mck) {
    check4valid(valid8chain(chain),message="From 'chain2list'");
}
# preparing
res <- vector("list",0);
# getting the description
res[[chain@des@name]] <- des2list(chain@des);
# getting the nodes
for (nn in bf(chain@nos)) {
    res[[chain@nos[[nn]]@name]] <- nod2list(chain@nos[[nn]]);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8chain <- function(fi)
#TITLE gets a /chain/ from a text file
#DESCRIPTION
#  Produces a /chain/ from a text file.
# The text file must follow the structure compatible
# with \code{file2list}. The first component of the file
# must be the description of the chain; the remaining 
# components are supposed to be the nodes (see the
# the text file examples).\cr
# \code{write8chain} onto \code{prr.chain1} can be used 
# to get an example of such a text file (see the examples).\cr
# See also the "Details" section for further possibility.
#DETAILS
# The tags used to decode the file are given by the \code{prr.tag1}
# constant. Its values can be modified by the user if necessary.
#KEYWORDS
#PKEYWORDS chain file
#INPUTS
#{fi} <<The file name to be considered.>>
#[INPUTS]
#VALUE
# The generated /chain/ object.
#EXAMPLE
# prr3k("RESET"); # (only for R checking)
# write8chain(prr.chain1,"tutu.txt");
# print(read8chain("tutu.txt"),empha=2);
# unlink("tutu.txt");
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_15
#REVISED 10_11_24
#--------------------------------------------
{
# reading the list
li <- file2list(fi,
                tags=prr.tag1,
                path="");
# transforming into a chain
res <- list2chain(li);
## No check already performed at the end of list2chain
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
write8chain <- function(chain,fi,ap=FALSE)
#TITLE writes a /chain/ onto a file
#DESCRIPTION 
# writes a /chain/ object to a text file to be
# readable with \code{read8bn}
#DETAILS
#KEYWORDS
#PKEYWORDS chain
#INPUTS
#{chain} <<The /chain/ to be written.>>
#{fi} <<name of the file to be written.
#       When \code{rbsb.cha0} no file is
#       written but a character is returned.>>
#[INPUTS]
#{ap} <<Must a possible existing file be appended?>>
#VALUE
# According to \code{fi} (1) nothing and a file is created or modified
# or (2) a character with the content of the file.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# write8chain(prr.chain1,rbsb.cha0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_15
#REVISED 10_11_24
#--------------------------------------------
{
# some checks
if (prr.mck) {
    check4valid(valid8chain(chain));
    check4tyle(fi,"character",c(0,1),message="Must indicate one file");
    check4tyle(ap,"logical",1,message="Must indicate if appending");
}
#
# creating the list
lili <- chain2list(chain);
#
# adapting the list for a standard use of list2file
#
# writing the file and returning
list2file(lili,fi,
               tags=prr.tag1,
               ap=ap);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
parents5nod <- function(nod)
#TITLE extracts the parents of a /nod/
#DESCRIPTION 
# returns the parents (from the \code{nod@x} slot)
# of a given node.
#DETAILS
#KEYWORDS
#PKEYWORDS nod
#INPUTS
#{nod} <<The /nod/ to be considered.>>
#[INPUTS]
#VALUE
# A character with the name of the parents. When there 
# are no parents: \code{character(0)}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# parents5nod(prr.nod2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_17
#REVISED 10_09_17
#--------------------------------------------
{
# some checks
if (prr.mck) {
    check4valid(valid8nod(nod));
}
#
# getting the parents
uu <- easyp3cut(nod@assess@x,pth=prr.tag2);
res <- unique(uu$blo[uu$typ==1]);
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
support5probab <- function(probab)
#TITLE extracts the parents of a /probab/
#DESCRIPTION 
# returns the support 
# of a given /probab/.
#DETAILS
# \code{@pafixe} is interpreted according to the family
#KEYWORDS
#PKEYWORDS probab
#INPUTS
#{probab} <<The /probab/ to be considered.>>
#[INPUTS]
#VALUE
# A \code{numeric(2)} defining the support.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# support5probab(prr.probab1);
# support5probab(prr.probab2);
# support5probab(prr.probab3);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_25
#REVISED 10_11_25
#--------------------------------------------
{
# some checks
if (prr.mck) {
    check4valid(valid8probab(probab));
}
#
# according to the family
if (probab@family=="beta") {
  res <- intersect4interval(probab@pafixe[1:2],probab@pafixe[3:4]);
}
if (probab@family=="normal") {
  res <- probab@pafixe;
}
if (probab@family=="lognormal") {
  if (probab@pafixe[2] == 1) {
    su <- c(probab@pafixe[1],Inf);
  } else {
    su <- c(-Inf,probab@pafixe[1]);
  }
  res <- intersect4interval(su,probab@pafixe[3:4]);
}
if (probab@family=="categorical") {
  res <- probab@pafixe[c(1,length(probab@pafixe))];
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
x5nod <- function(nod,X)
#TITLE computes the x values for a node
#DESCRIPTION 
# x values for the node \code{nod} are returned
# to prepare a simulation of size \code{nrow(X)}.
# Possible parents are extracted from the data frame
# \code{X}, these must exist.
#DETAILS
# No checks about the existence of the parent are
# made in this very basic function.
#KEYWORDS
#PKEYWORDS nod
#INPUTS
#{nod} <<The concerned /nod/ object.>>
#{X} <<Data frame of the previous simulated nodes in columns.
#      When \code{nod} is an ancestor node (= no parents), it
#      must be provided for the number of rows.>>
#[INPUTS]
#VALUE
# a vector of numeric values, the length of
#  which is the row number of \code{X}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# x5nod(prr.nod1,as.data.frame(matrix(NA,15,0)));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_18
#REVISED 10_11_24
#--------------------------------------------
{
#
# no check
#
# computing the x values for this node
uu <- easyp3cut(nod@assess@x,pth=prr.tag2);
vv <- easyp3stickback(uu,matrix(c("X[,'","']"),ncol=2));
ligne <- paste("res <-",vv);
eval(parse(text=ligne));
if (length(res)==1) { res <- rep(res,nrow(X));}
# 
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simu8nod <- function(nod,X=100,add=TRUE)
#TITLE simulates values for a node
#DESCRIPTION 
# From the data frame \code{X} where the possible
# parents must exist, a simulation is done for the
# node \code{nod}. The number of values to simulate
# is given by the X row number, so it must be present
# (even with zero columns) for ancestor nodes.\cr
# The function returns an augmented  data frame with a new column 
# (or more according to \code{nod@format})
# in last position(s) containing the simulated values.
#DETAILS
#KEYWORDS
#PKEYWORDS chain
#INPUTS
#{nod} <</nod/ object to be simulated.>>
#[INPUTS]
#{X} <<Data frame of the previous simulated nodes in columns.
#      For ancestor nodes can have zero columns or more
#      simply be an integer indicating the desired number
#      of simulations, as it is in the default.>>
#{add} <<Must the matrix \code{X} be augmented, or only the
#      the new columns returned?>>
#VALUE
# Data frame with the simulated and retained column(s)
# possibly as an augmentation of the inputted \code{X}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# X1 <- simu8nod(prr.nod1,11);
# X2 <- simu8nod(prr.nod2,X1);
# X3 <- simu8nod(prr.nod3,X2);
# simu8nod(prr.nod4,14);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_17
#REVISED 10_11_24
#--------------------------------------------
{
#
# managing the 'X' argument first
if (is.numeric(X)) { X <- as.data.frame(matrix(0,abs(round(X[1])),0));}
#
# checking
if (prr.mck) {
    check4tyle(X,"data.frame",-1,message="From 'simu8nod': a data.frame is expected");
    check4tyle(add,"logical",1,message="From 'simu8nod': a logical is expected");
    check4valid(valid8nod(nod),message="From 'simu8nod': a /nod/ is expected");
    papa <- parents5nod(nod);
    mipa <- setdiff(papa,names(X));
    if (length(mipa)>0) {
        erreur(list(nod,names(X)),
               paste("These parents:",mipa,"are missing")
              );
    }
}
# getting the name
nam <- nod@name;
#
# computing the x values for this node (using previous nodes)
xv <- x5nod(nod,X);
nbs <- length(xv);
#
# computing sub-nodes in turn according the type of node
#
fami <- nod@probab@family;
# <-m->
sbn.md <- new("distri",
              probab=new("probab",family=fami,pafixe=nod@probab@pafixe),
              nbdraw=0,
              parand=cbind(xv,rep(nod@assess@cU,nbs))
             );
sbn.mv <- rdistri(sbn.md);
# <-CV->
sbn.CVd <- new("distri",
               probab=new("probab",family="beta",pafixe=c(0,100,0,100)),
               nbdraw=0,
               parand=cbind(rep(nod@assess@cV,nbs),
                            rep(nod@assess@cU,nbs))
              );
sbn.CVv <- rdistri(sbn.CVd);
# <-X->
sbn.Xd <- new("distri",
              probab=new("probab",family=fami,pafixe=nod@probab@pafixe),
              nbdraw=0,
              parand=cbind(sbn.mv,sbn.CVv)
             );
sbn.Xv <- rdistri(sbn.Xd);
#
# rounding as prescribed
if (nod@format[1] >= 0) { sbn.Xv  <- round(sbn.Xv, nod@format[1]);}
if (nod@format[2] >= 0) { sbn.mv  <- round(sbn.mv, nod@format[2]);}
if (nod@format[3] >= 0) { sbn.CVv <- round(sbn.CVv,nod@format[3]);}
#
# preparing the output
if (!add) { X <- data.frame(matrix(NA,nrow=nbs,ncol=0));}
#
if (nod@format[3] >= 0) {
  X[[paste(nam,".CV",sep="")]] <- sbn.CVv;
}
if (nod@format[2] >= 0) {
  X[[paste(nam,".m",sep="")]] <- sbn.mv;
}
X[[nam]] <- sbn.Xv;
#
# returning
X;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rr8nod <- function(nod,x=NULL,add=TRUE)
#TITLE computes the central value of a node
#DESCRIPTION 
# From the named vector \code{x} where the possible
# parents must exist, the deterministic application of
# the central value is done for the
# node \code{nod}.
#DETAILS
#KEYWORDS
#PKEYWORDS chain
#INPUTS
#{nod} <</nod/ object to be simulated.>>
#[INPUTS]
#{x} <<named vector of the previous computed nodes,
# can be anything for the ancestor nodes.>>
#{add} <<Must the vector \code{x} be augmented, or only the
#      the new value returned?>>
#VALUE
# Named vector with the computed and retained node
# possibly as an augmentation of the inputted \code{x}.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# x1 <- rr8nod(prr.nod1);
# x2 <- rr8nod(prr.nod2,x1);
# x3 <- rr8nod(prr.nod3,x2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_29
#REVISED 10_11_29
#--------------------------------------------
{
#
# checking
if (prr.mck) {
    check4tyle(add,"logical",1,message="From 'rr8nod': a logical is expected");
    check4valid(valid8nod(nod),message="From 'rr8nod': a /nod/ is expected");
    papa <- parents5nod(nod);
    if (length(papa)>0) {
        check4tyle(x,"nnumeric",-1,message="From 'rr8nod': when parents are present in the node, a named numeric is expected for 'x'");
        mipa <- setdiff(papa,names(x));
        if (length(mipa)>0) {
          erreur(list(nod,names(X)),
                 paste("From rr_nod, these parents:",mipa,"are missing")
                );
        }
    }
}
#
# computing the x values for this node (using previous nodes)
papa <- parents5nod(nod);
if (length(papa)>0) {
  X <- as.data.frame(t(as.matrix(x)));
} else {
  X <- as.data.frame(1);
}
xv <- x5nod(nod,X);
names(xv) <- nod@name;#
# preparing the output
if (add) {
  xv <- c(x,xv);
}
#
# returning
xv;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simu8chain <- function(chain,nbs=100)
#TITLE simulates values for a given chain
#DESCRIPTION 
# A simulation is done with the
# object \code{chain}. The number of values to simulate
# is given by \code{nbs}. 
# The function returns a data frame with simulations
# in rows and variables in columns. According to
#  the \code{@format} of the nodes,
#  more or less [random] variables are
# stored for each node.
#DETAILS
#KEYWORDS
#PKEYWORDS chain
#INPUTS
#{chain} <</chain/ object to be simulated.>>
#[INPUTS]
#{nbs} <<An integer indicating the desired number
#VALUE
# returns a list with two components
#{$df} << A data frame with all the simulated (and kept) variables.>>
#{$rr} << A named vector with the results of a deterministic application of
# the chain (equivalent to the original Risk Ranger).>>
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# simu8chain(prr.chain1,nbs=5);
# simu8chain(prr.chain2,nbs=5);
# simu8chain(prr.chain3,nbs=5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_19
#REVISED 10_11_29
#--------------------------------------------
{
#
# checking
if (prr.mck) {
    check4valid(valid8chain(chain),message="From 'simu8chain': a /chain/ is expected");
    check4tyle(nbs,rbsb.integ,1,c(0,Inf),message="From 'simu8chain': a non-negative integer is expected");
}
#
# starting the resulting data frame and vector
SX <- as.data.frame(matrix(0,nbs,0));
SR <- rbsb.nnu0;
#
# increasing it each mode in turn
for (jbd in bf(chain@nos)) {
    SX <- simu8nod(chain@nos[[jbd]],X=SX);
    SR <- rr8nod(chain@nos[[jbd]],x=SR)
}
#
# returning
list(df=SX,rr=SR);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8nod <- function(nod,nbs=1000,tit=NULL,...)
#TITLE plots the density of an ancestor /nod/.
#DESCRIPTION 
# Plots the numerically approximated distribution of a /nod/
# not depending on parents through a call to \code{plot(density())}.
# The approximation is made with the help of \code{nbs} simulations.
#DETAILS
# The central value \code{nod@assess@x} is drawn, as are the limits of the support.
#KEYWORDS
#PKEYWORDS probability
#INPUTS
#{nod} <<The /nod/ object of which the repartition is looked for>>
#[INPUTS]
#{nbs} <<Number of simulations to perform to approximate the density.>>
#{tit} <<When \code{NULL}, a standard title is used.>>
#{\dots} <<To be passed to the standard 'plot' function.>>
#VALUE
# Nothing but a plot is issued.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# plot8nod(prr.nod1);
# plot8nod(prr.nod2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_19
#REVISED 10_11_25
#--------------------------------------------
{
#
# checking
if (prr.mck) {
  check4valid(valid8nod(nod),message="the proposed node to plot8nod is not valid");
  check4tyle(nbs,"numeric",1,c(50,Inf),"not a sensible 'nbs' proposed to plot8nod");
  if (!is.null(tit)) {
    check4tyle(tit,"character",1,message="bad title for plot8nod");
  }
}
if (length(parents5nod(nod)) > 0) {
  erreur(nod,"This /nod/ has got parents and cannot be simulated in an isolated fashion");
}
#
# getting the simulations (only for X)
nod@format[2:3] <- -10;
simu <- simu8nod(nod,X=nbs);
#
# setting up the title
if (is.null(tit)) {
  ti1 <- paste("/nod/",nod@name);
  ti1 <- paste(ti1,paste("(",nod@probab@family,")",sep=""));
  ti1 <- paste(ti1,paste(" x= ",nod@assess@x,sep=""));
  ti1 <- paste(ti1,paste(" cV= ",nod@assess@cV,sep=""));
  ti1 <- paste(ti1,paste(" cU= ",nod@assess@cU,sep=""));
  # computing some statistics
  st1 <- mean(simu[[1]],na.rm=TRUE);
  st2 <- sd(simu[[1]],na.rm=TRUE);
  st3 <- median(simu[[1]],na.rm=TRUE);
  st4 <- diff(quantile(simu[[1]],c(0.25,0.75)));
  ti2 <- paste("(m,s):",round(st1,2),round(st2,2));
  ti2 <- paste(ti2,"(M,IQ):",round(st3,2),round(st4,2));
  tit <- paste(ti1,"\n",ti2,sep="");  
}
#
# plotting
su <- support5probab(nod@probab);
su <- intersect4interval(su,range(simu[[1]]));
plot(density(simu[[1]]),main=tit,cex=0.5,xlim=su,...);
abline(v=nod@assess@x,lty=2);
abline(v=su,lwd=3);
#
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
mplot8nod <- function(nod,nbs=1000,x=NULL,
                      cV=seq(5,95,length=4),
                      cU=seq(5,95,length=4),...)
#TITLE simulates and plots densities of a /nod/ varying its parameters
#DESCRIPTION 
# Plots a matrix of plots containing each a /nod/ distribution.
# This is defined through a \code{nod} object whose \code{@assess} slot
# is modifyed with the values indicated in the three \code{x,cV,cU}
# arguments.\cr
# The plots varies in row-column in one of the three way: (cv,cu|x fixed),
# (x,cu|cv fixed) or (x,cv|cu fixed) indicated by the fact
# fact that one of the \code{x}, \code{cv} or \code{cu} argument
# is provided with only one value.
#DETAILS
# The matrix of plots is done by successive call to \code{plot8nod}.
# The adjustment of \code{par(mfrow)} is done within the
# function. To generate a graph file, \code{pdf}
# or a similar function must be called beforehand, as well as 
# \code{dev.off} afterwards.
#KEYWORDS
#PKEYWORDS plot
#INPUTS
#[INPUTS]
#{nod} <<The node defining the distribution except for \code{@assess} values.>>
#{nbs} <<Number of values to simulate to numerically obtain
# a density graph.>>
#{x} <<central value(s) for the distribution.
# When \code{NULL}, will be put to \code{as.numeric(nod@assess@x)}.>>
#{cV} <<variability value(s) for the distribution.>>
#{cU} <<uncertainty value(s) for the distribution.>>
#{\dots} <<To be passed to the \code{plot8distri} function.>>
#VALUE
# Nothing but a matrix of plots is issued.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# mplot8nod(prr.nod2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_20
#REVISED 10_11_25
#--------------------------------------------
{
#
# Most of the checks are deferred to 'rdistri'
# through 'plot8distri'
if (prr.mck) {
  check4valid(valid8nod(nod));
  check4tyle(nbs,"numeric",1,c(50,Inf));
  if (!is.null(x)) {check4tyle( x,rbsb.numer,c(1,Inf));}
  check4tyle(cV,rbsb.numer,c(1,Inf));
  check4tyle(cU,rbsb.numer,c(1,Inf));
}
#
if (is.null(x)) { x <- as.numeric(nod@assess@x);}
#
# preparing the rows and columns of the plot matrix
fait <- FALSE;
if (length(x)==1) {
    # cV in columns, cU in rows
    nc <- length(cV);
    nl <- length(cU);
    xv <- rep(x,nc*nl);
    cVv <- rep(cV,nl);
    cUv <- rep(cU,each=nc);
    fait <- TRUE;
} else {
    if (length(cV)==1) {
	# x in columns, cU in rows
	nc <- length(x);
	nl <- length(cU);
	xv <- rep(x,nl);
	cVv <- rep(cV,nc*nl);
	cUv <- rep(cU,each=nc);
        fait <- TRUE;
    } else { if (length(cU)==1) {
	# x in columns, cV in rows
	nc <- length(x);
	nl <- length(cV);
	xv <- rep(x,nl);
	cVv <- rep(cV,each=nc);
	cUv <- rep(cU,nc*nl);
        fait <- TRUE;
    }}
}
if (!fait) {
    erreur(list(length(x),length(cV),length(cU)),
           "mplot8nod: one of these must be of length one");
}
#
# plotting
par(mfrow=c(nl,nc));
for (kk in bc(nc*nl)) {
    nod@assess@x <- as.character(xv[kk]);
    nod@assess@cV<-cVv[kk];
    nod@assess@cU<-cUv[kk];
    plot8nod(nod,nbs=nbs,...);
}
#
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
chain2fileexpert <- function(chain,file,comment=2)
#TITLE writes from a /chain/ a file to be completed by an expert
#DESCRIPTION 
# From a complete chain, write (with possibly many comments)
# a file to be completed by the assessement of expert(s). Only 
# necessary of components of \code{@assess} of every node are proposed
#DETAILS
# The values of the original /chain/ are recalled in the comments
# when \code{comment} is greater than zero.
#KEYWORDS
#PKEYWORDS assess
#INPUTS
#{chain} <<The /chain/ serving as template.>>
#{file} <<name of the file to be written.
#       When \code{rbsb.cha0} no file is
#       written but a character is returned.>>
#[INPUTS]
#{comment} << Indicates the level of the comment
# to introduce in the created file. [0] no comment,
# [1] just the date of the creation, [2] as [1] plus some
# brief explanations, [3] verbose.
#VALUE
# The content of the file is returned or written to the designated file.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# chain2fileexpert(prr.chain3,rbsb.cha0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_26
#REVISED 10_11_26
#--------------------------------------------
{
# some checks
if (prr.mck) {
    check4valid(valid8chain(chain));
    check4tyle(file,"character",c(0,1),message="Must indicate one file");
    check4tyle(comment,"numeric",1,c(0,3),message="indicates the type of comments");
}
#
# functions of use
ajout <- function(quoi,indent=1) {
if (is.numeric(quoi)) {
  if (quoi < 0) {
    res <- c(res,"#---------------------------------------------");
  } else {
    for (ii in bc(quoi)) {
      res <- c(res,"#");
    }
  }
} else {
  res <- c(res,paste("#",paste(rep(" ",indent),collapse=""),quoi,sep=""));
}
res;
}
#
# Starting the resulting file
res <- character(0);
#
# Heading of the file
if (comment > 0) {
  res <- ajout(1);
  res <- ajout(paste("File created on",now('dm'),"by the function 'chain2fileexpert'"));
  res <- ajout(1);
  res <- ajout(paste("The original /chain/ was described in a variable named",
              deparse(substitute(chain))));
  if (length(file) == 1) {
    res <- ajout(paste("(To be written onto the file",deparse(substitute(file)),
                       ")",sep=""),4);
  } else {
    res <- ajout("(Not to be written into a file but to be returned)",4);
  }
  res <- ajout(2);
}
#
# brief indication about how to complete the file
if (comment > 1) {
  res <- ajout("In every line starting with '((' for a given node",2);
  res <- ajout("The expert must give some assessed value",2);
  res <- ajout("Those with '((x))' are central values of the nodes",4);
  res <- ajout("Those with '((cV))' are variabilities of the nodes",4);
  res <- ajout("Those with '((cU))' are uncertainties of the nodes",4);
  res <- ajout("(central values can be anything)",6);
  res <- ajout("(variabilities and uncertainties must belong to [0,100])",6);
  if (comment > 2) {
    res <- ajout("(  0 means no variability or uncertainty)",8);
    res <- ajout("(100 means extreme variability or uncertainty)",8);
    res <- ajout(1);
    res <- ajout("Values to assess are in between dashed lines",2);
    res <- ajout(1);
  }
  res <- ajout(2);
}
if (comment > 2) {
  res <- ajout("Only ancestor nodes have central values, the other",2);
  res <- ajout("nodes have their central values deduced from their",2); 
  res <- ajout("parent nodes and the way with which they are linked to",2);
  res <- ajout(2);
}
if (comment > 1) {
  res <- ajout("Proposed standard values are suggested but it is always",2);
  res <- ajout("the responsability of the expert to add assessed values",2);
  res <- ajout(2);
  res <- ajout("It is also possible to modify the name of the chain",1);
  res <- ajout("and add some '((defi))nition' and '((comme))nt'.",1);
  res <- ajout(2);
  res <- ajout("# Here is the identification of the chain to assess",0);
  if (comment > 2) {
    res <- ajout("#   The name between '<<' and '>>'",0);
    res <- ajout("#   The definition after '((defi))'",0);
    res <- ajout("#   The comment after '((comm))'",0);
  }
  res <- ajout(1);
}
#
# identifying the chain
res <- ajout(-1);
res <- c(res,
         paste(prr.tag1[1,1],chain@des@name,prr.tag1[1,2],sep="")
        );
if (chain@des@defi == "undefined") {
  chain@des@defi <- rbsb.cha0;
}
res <- c(res,
         paste(prr.tag1[2,1],"defi",prr.tag1[2,2],
               " ",chain@des@defi,sep="")
        );
res <- c(res,
         paste(prr.tag1[2,1],"comm",prr.tag1[2,2],
               " ",chain@des@comm,sep="")
        );
res <- ajout(-1);
#
# each node in turn
if (comment > 1) {
  res <- ajout(1);
  res <- ajout("Here is/are the assessment for each node in turn",3);
  res <- ajout(1);
}
for (nn in bf(chain@nos)) {
  pp <- parents5nod(chain@nos[[nn]]);
  nbpp <- length(pp);
  if (comment > 2) {
    res <- ajout(paste("For the node number",nn,"named",
                 paste("'",chain@nos[[nn]]@name,"'",sep="")),1);
    if (nbpp==0) {
      res <- ajout("[without any parent]",3);
    } else {
      pp <- paste(pp,collapse=", ");
      pp <- paste("(",pp,")",sep="");
      res <- ajout(paste("[the parent of which is/are: ",pp,"]",sep=""),3);
    }
  }
#
if (comment > 0) {
  if (nbpp==0) {
    rre <- paste("x = ",chain@nos[[nn]]@assess@x,",",sep="");
  } else {
    rre <- "";
  }
  rre <- paste(rre," cV = ",chain@nos[[nn]]@assess@cV," & ",sep="");
  rre <- paste(rre," cU = ",chain@nos[[nn]]@assess@cU,sep="");
  res <- ajout(
               paste("Suggested values found in the pattern chain:",
                     rre
                    )
              );
  res <- ajout(-1);
}
res <- c(res,
         paste(prr.tag1[1,1],chain@nos[[nn]]@name,prr.tag1[1,2],sep="")
        );
if (nbpp==0) {
  res <- c(res,paste(prr.tag1[2,1],"x",prr.tag1[2,2],sep=""));
}
res <- c(res,paste(prr.tag1[2,1],"cV",prr.tag1[2,2],sep=""));
res <- c(res,paste(prr.tag1[2,1],"cU",prr.tag1[2,2],sep=""));
res <- ajout(-1);
}
if (comment > 0) {
  res <- ajout(1);
  res <- ajout("The End",3);
}
#
# writing the file and returning
char2file(res,file);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expert4chain <- function(file,chain)
#TITLE reads the expert assessement of an expertise and
# builds the corresponding chain
#DESCRIPTION 
# From a file completed by the assessement of expert(s), and
# an existing chain, this function constitutes a chain.
# All assessment must be done.
#DETAILS
# Usually the expert assessment file comes from a call
#  to \code{chaine2fileexpert}. Nodes can be in any order
# but the first component must be devoted to the chain description.\cr
# Tags to be used for the file are \code{prr.tag1}.
#KEYWORDS
#PKEYWORDS assess chain
#INPUTS
#{file} <<name of the file where must be extracted the expert assessemnt.>>
#{chain} <<The /chain/ serving as template for the new chain.
# It must be completely consistent with the file assessment.>>
#[INPUTS]
#VALUE
# The resulting /chain/.
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# ## chain2fileexpert(prr.chain3,"tutu");
# ## file 'tutu' must be completed by hand 
# ## cha <- expert4chain("tutu",prr.chain);
# ## print(cha);
# ## unlink("tutu");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_29
#REVISED 10_11_29
#--------------------------------------------
{
# some checks
if (prr.mck) {
    check4valid(valid8chain(chain));
    check4tyle(file,"character",c(0,1),message="Must indicate one file");
}
#
# reading the file
lili <- file2list(file,tags=prr.tag1);
#
# checking the consistency
if (prr.mck) {
  if (length(lili[-1]) != length(chain@nos)) {
    erreur(list(chain,lili),"The numbers of nodes are different between 'chain' and 'lili'");
  }
  for (nn in bf(lili[-1])) {
    if (!(chain@nos[[nn]]@name %in% names(lili))) {
      erreur(list(chain@nos[[nn]]@name,names(lili)),
             "This node is not in the proposed 'lili' list");
    }
  }
}
#
# building the resulting chain
res <- chain;
#
# The chain itself
res@des@name <- names(lili)[1];
if (!isvide(lili[[1]]$defi)) {
  res@des@defi <- paste(lili[[1]]$defi,collapse=" ");
}
if (!isvide(lili[[1]]$comm)) {
  res@des@comm <- paste(lili[[1]]$comm,collapse=" ");
}
#
# each node in turn
for (nn in bf(lili[-1])) {
  nono <- chain@nos[[nn]]@name;
  lil <- lili[[nono]];
  nbp <- length(parents5nod(chain@nos[[nn]]));
  if (nbp == 0) {
    xx <- lil$x;
    if (is.null(xx)) {
      erreur(list(chain@nos[[nn]],lil),
             "No 'x' component for this ancestor node");
    } else {
      res@nos[[nn]]@assess@x <- as.character(xx[1]);
    }
  }
  #
  ccV <- lil$cV;
  if (is.null(ccV)) {
    erreur(list(chain@nos[[nn]],lil),
           "No 'cV' component for this ancestor node");
  } else {
    res@nos[[nn]]@assess@cV <- as.numeric(ccV[1]);
  }
  #
  ccU <- lil$cU;
  if (is.null(ccU)) {
    erreur(list(chain@nos[[nn]],lil),
           "No 'cU' component for this ancestor node");
  } else {
    res@nos[[nn]]@assess@cU <- as.numeric(ccU[1]);
  }
}
#
# checking
if (prr.mck) {
  check4valid(valid8chain(res));
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
get8nam <- function(nam)
#TITLE returns the name of a node from the name of a subnode
#DESCRIPTION
# Mainly of use for \code{look4simu}.
#DETAILS
#KEYWORDS
#PKEYWORDS name
#INPUTS
#{nam} <<\code{character(1)} of the subnode name.>>
#[INPUTS]
#VALUE
# A list with two components:
#{$nam} <<the node name.>>
#{$quoi} <<the type of the subnod \code{m|CV|X}.>>
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# get8nam("foo.CV");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_29
#REVISED 10_11_29
#--------------------------------------------
{
# constants
CV <- ".CV";
m <- ".m";
nbc <- nchar(CV);
nbm <- nchar(m);
#
# checking
check4tyle(nam,"character",1,message="From 'get8nam': must be character(1)");  
#
# computing the x values for this node
nbl <- nchar(nam);
quoi <- "X";
if (nbl > nbc) {
  xxx <- substr(nam,nbl-nbc+1,nbl);
  if (identical(xxx,CV)) {
    nam <- substr(nam,1,nbl-nbc);
    quoi <- "CV";
  }
}
if ((quoi=="X") & (nbl > nbm)) {
  xxx <- substr(nam,nbl-nbm+1,nbl);
  if (identical(xxx,m)) {
    nam <- substr(nam,1,nbl-nbm);
    quoi <- "m";
  }
}
# 
# returning
list(nam=nam,quoi=quoi);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
look4simu <- function(simu,what="ub",how="SP",who="A",selec=NULL)
#TITLE makes some standard statistical investigations about a simulated /chain/
#DESCRIPTION 
# From a simulation made about a /chain/ stored as a \code{list}
# as done by the function \code{simu8chain} returns / prints /
# plots standards results for continuous variables.
#DETAILS
# (see the code.)
#KEYWORDS
#PKEYWORDS simulation
#INPUTS
#{simu} <<\code{list} resulting from a \code{simu8chain} call. The two
# expected components are \code{$df} and \code{$rr}.>>
#[INPUTS]
#{what} << Indicates which kind of statistics to investigate: \code{u} for
# univariate, \code{b} for bivariate.>>
#{how} << Indicates what to do: \code{s} to return the statistics,
# \code{S} to print the statistics, \code{p} or \code{P} to make
# standard plots.>>
#{who} << Which kind of subnode must be handled: \code{A} for All, \code{X} for X, \code{m} for m and \code{C} for CV.>>
#{selec} << Vector containing the names of the node to include, \code{NULL} means all nodes.>>
#VALUE
# According of \code{how}, only printing and plots and/or statistics
#EXAMPLE
# prr3k("RESET"); # For R checking compliance
# simul <- simu8chain(prr.chain3);
# look4simu(simul); 
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_11_29
#REVISED 10_11_29
#--------------------------------------------
{
# some checks
if (prr.mck) {
    check4tyle(simu,"list",2,message="From 'look4simu': simu must be a list with two components");
    check4tyle(simu$df,"data.frame",-1,message="From 'look4simu': simu$df must be a data frame");
    check4tyle(simu$rr,"nnumeric",-1,message="From 'look4simu': simu$rr must be a named numeric");
}
#
# Identifying which kind of subnodes must be retained
sbn <- rbsb.cha0;
if (expr3present("A",who)) {
  sbn <- c("X","CV","m");
} else {
  if (expr3present("C",who)) { sbn <- c(sbn,"CV");}
  if (expr3present("X",who)) { sbn <- c(sbn,"X");}
  if (expr3present("m",who)) { sbn <- c(sbn,"m");}
}
#
# making the required selection for the data frame
dafra <- as.data.frame(matrix(NA,nrow(simu$df),0));
for (ii in bf(simu$df)) {
  # name of the considered variable
  mama <- names(simu$df)[ii];
  nana <- get8nam(mama);
  if ((is.null(selec)) |
      (!is.null(selec) & (nana$nam %in% selec))) {
     # getting the corresponding node
     if (nana$quoi %in% sbn) {
       dafra[[mama]] <- simu$df[[mama]];
     }
  }
}
res <- vector("list",0);
#
# statistics
if (expr3present("s",tolower(how))) {
  if (expr3present("u",what)) {
    SU <- df2ustat(dafra);
    if (expr3present("s",how)) { res$SU <- SU;}
    if (expr3present("S",how)) { print(SU);}
  }
  #
  if (expr3present("b",what)) {
    SU <- df2bstat(dafra);
    if (expr3present("s",how)) { res$SB <- SU;}
    if (expr3present("S",how)) { print(SU);}
  }
}
#
# plots
if (expr3present("p",tolower(how))) {
  if (expr3present("u",what)) {
    for (vv in names(dafra)) {
      plot(density(dafra[[vv]]),main=vv);
      abline(v=simu$rr[vv],lw=3,lty=2);
    }
  }
  #
  if (expr3present("b",what)) {
    nana <- names(dafra);
    for (v1 in bf(nana)) { for (v2 in bf(nana)) { if (v1<v2) {
      plot(dafra[[v1]],dafra[[v2]],
           main="",xlab=nana[v1],ylab=nana[v2]);
      abline(v=simu$rr[nana[v1]],lw=1,lty=2);
      abline(h=simu$rr[nana[v2]],lw=1,lty=2);
    }}}
  }
}
# returning
if (length(res)==0) { return(invisible());}
res;
dafra
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


