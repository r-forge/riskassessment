
rs003k("reset");

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rsba3k <- function(whi)
#TITLE  assigns the rsba constants
#DESCRIPTION (ba)
# defines or returns the constants used within /rbsbba/. 
# The performed action depends on the argument.
#DETAILS
# see the details in \code{rs003k} NEVERTHELESS
# be aware that changing some constants like \code{rbsb.nom?}
# will possibly imply inconsistency with the constants of 
# other layers, like \code{rbsb.gn?},  \code{rbsb.bn?}, 
# \code{rbsb.dn?}...
#PKEYWORDS helpful
#KEYWORDS misc
#INPUTS
#{whi}    <<a character(1) indicating either to reset or
#           to return the names or the current values. The three possible
#           values are \code{RESET}, \code{reset}, \code{names}, \code{definitions} or \code{values}.>>
#[INPUTS]
#VALUE
# When \code{whi=="RESET"} nothing (but the assignments are
# performed for all layers \code{rs00} and \code{rsba}).
# When \code{whi=="reset"} nothing (but the assignments of 
# the layer \code{rsba} are performed).
# When \code{whi=="names"} the names as a character vector.
# When \code{whi=="definitions"} the definitions as a named character vector.
# When \code{whi=="values"} the values through a named list.
#EXAMPLE
## First assign the standard values
# rsba3k("RESET");
# print(rbsb.nom1);
## to get the short labels
# rsba3k("names");
## to obtain the current values
# rsba3k("values");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_09_30
#REVISED 10_07_06
#--------------------------------------------
{
# checking
if (!(whi %in% c("RESET","reset","names","definitions","values"))) {
    print.default(whi);
    stop("rsba3k does not accept this argument");
}
#
if (whi=="RESET") { rs003k("RESET");}
#
# definition of the different constants
sc <- md <- character(0);
sc["nom0"] <- "null /nom/"; 
sc["nom1"] <- "Example 1 of /nom/"; 
sc["nom2"] <- "Example 2 of /nom/"; 
sc["nom3"] <- "Example 3 of /nom/"; 
sc["nom4"] <- "Example 4 of /nom/"; 
sc["nom5"] <- "Example 5 of /nom/"; 
sc["nom6"] <- "Example 6 of /nom/"; 
sc["nom7"] <- "Example 6 of /nom/"; 
sc["ion0"] <- "null /ion/"; 
sc["ion1"] <- "exemple 1 of /ion/"; 
sc["ion2"] <- "exemple 2 of /ion/"; 
sc["ion3"] <- "exemple 3 of /ion/"; 
sc["arc0"] <- "null /arc/"; 
sc["pam0"] <- "null /pam/"; 
sc["pam1"] <- "Example 1 of /pam/"; 
sc["win0"] <- "null /win/"; 
sc["win1"] <- "Example of /win/'"; 
sc["win2"] <- "Example of /win/ compatible with 'rbsb.daf2'"; 
sc["win3"] <- "Example of /win/ compatible with 'rbsb.daf3' from 'rbsb.daf2'"; 
sc["alk0"] <- "null /alk/"; 
sc["alk1"] <- "Example 1 of /alk/ (without parent)"; 
sc["alk2"] <- "Example 2 of /alk/ (with parents)"; 
sc["alk3"] <- "Example 3 of /alk/ (with parents)"; 
sc["alk4"] <- "Example 4 of /alk/ (categoric variable)"; 
sc["pta0"] <- "null /pta/"; 
sc["pta1"] <- "Example 1 of /pta/"; 
sc["pgr0"] <- "current /pgr/"; 
sc["pos0"] <- "current /pos/"; 
sc["tlk"] <- "different types of links"; 
sc["ltn"] <- "names of the types of links"; 
sc["lta"] <- "slot names of alk"; 
sc["l_a"] <- "defines the needed arguments for the different ltypes"; 
sc["f_d"] <- "The different families of distributions"; 
sc["all"] <- "Symbol defining all the variates from a node"; 
sc["who"] <- "all surrounde = WHOle"; 
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
    for (ii in sjl(sc)) {
        noco <- names(sc)[[ii]];
        eee <- paste("res[[\"",noco,"\"]] <- rbsb.",noco,";",sep="");
        eval(parse(text=eee));
    }
    return(res);
}
# some very big constants
    ##############################################################
    # STARTING THE DEFINITION OF ltypes
    ##############################################################
    #
    # argNO indicates the arguments which will be fixed by rebastaba
    # argno indicates the arguments which will be computed by rebastaba
    # argyes indicates the arguments that the user can provide but have default values
    # argYES indicates the arguments that the user must provide
    # non mentioned arguments are irrelevant or forbidden (e.g. the
    #                                           ltransfo for Bernoulli)
    # specific treatment is given to lred and lcod which can always be
    # deduced from lpod
    # After a while, I decided that the default values must be given
    # through examples ! (examples can be found afterwards)
    # the component $rep was introduced because some probabilities of
    # the 'miscellaneous' family can be repeatable.
    rbsb.lta <- c("ldes","ltype","lpara","lrep","lnat","lvar",
                  "lparent","lpod","lred","lcod","ltransfo","ldaf",
                  "lwin","lfunct","lcomp");  
    #================================================================
    ### BE AWARE THAT rbsb.lta MUST BE IDENTICAL TO slotNames("alk")
    ###    but a this time, it is not accessible (in R check).
    #================================================================
    rbsb.fam_dis <- c("conti_scalar","discr_scalar","conti_vector","discr_vector",
                  "categoric","data_based","miscellaneous");
    names(rbsb.fam_dis) <- c("c_s","d_s","c_v","d_v",
                  "cat","dab","mis");
    rbsb.f_d <- rbsb.fam_dis;
    #
    rbsb.tlk <- list(
	###
	normal=list(
	  defi="Classical univariate normal distribution.",
	  fami="conti_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lnat"),
	  argno=c("lparent"),
	  argyes=c("ltransfo","lrep","lred","lcod","lvar"),
	  argYES=c("lpara","lpod"),
	  par=list(mu="expectation",sigma="standard deviation"),
	  ltransfo="(|3|)",lrep=1,
	  lvar="",
	  lnat="conti",
	  lparent=rbsb.cac
	  ),
	###
	uniform=list(
	  defi="Classical univariate uniform distribution.",
	  fami="conti_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lnat"),
	  argno=c("lparent"),
	  argyes=c("ltransfo","lrep","lred","lcod","lvar"),
	  argYES=c("lpara","lpod"),
	  par=list(a="lower bound",b="upper bound"),
	  ltransfo="(|3|)",lrep=1,
	  lvar="",lnat="conti",lparent=rbsb.cac
	  ),
	###
	beta=list(
	  defi="Classical univariate beta distribution.",
	  fami="conti_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lnat"),
	  argno=c("lparent"),
	  argyes=c("ltransfo","lrep","lred","lcod","lvar"),
	  argYES=c("lpara","lpod"),
	  par=list(a="lower bound",b="upper bound"),
	  ltransfo="(|3|)",lrep=1,
	  lvar="",lnat="conti",lparent=rbsb.cac
	  ),
	###
	Bernoulli=list(
	  defi="Binary variable taking values 1 or 0",
	  fami="discr_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lnat","lpod","lred","lcod","ltransfo"),
	  argno=c("lparent"),
	  argyes=c("lrep","lvar"),
	  argYES=c("lpara"),
	  par=list(p="probability of 1"),
	  ltransfo="",lrep=1,
	  lpod=list(0:1),lred=list(0:1),lcod=list(0:1),
	  lvar="",lnat="integ",lparent=rbsb.cac
	  ),
	###
	binomial=list(
	  defi=c("Classical univariate distribution of number of successes.",
		 "Notice that 'lpod' must be given by the user since /rebastaba/",
		 "cannot know in advance which can be the highest value of",
		 "the binomial size which can be inherited from other node",
		 "values."),
	  fami="discr_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lnat"),
	  argno=c("lparent"),
	  argyes=c("lrep","lred","lcod","lvar"),
	  argYES=c("lpara","lpod"),
	  par=list(n="size",p="probability of success"),
	  ltransfo="",lrep=1,
	  lvar="",lnat="integ",lparent=rbsb.cac
	  ),
	###
	Dirac=list(
	  defi="Classical Dirac distribution: the variable can take only one value.",
	  fami="miscellaneous",rep=TRUE,bugs=TRUE,
	  argNO=rbsb.cha0,
	  argno=c("lparent"), 
	  argyes=c("ltransfo","lrep","lred","lcod","lnat","lvar"), # lnat must be limited to 'conti' or 'integ'
	  argYES=c("lpara","lpod"),
	  par=list(k="The value (either 'conti' or 'integ'!)"),
	  ltransfo="",lrep=1,
	  lvar="",lnat="conti",lparent=rbsb.cac
	  ),
	###
	multinomial=list(
	  defi=c("Multivariate generalisation of the binomial distribution.",
		    "n trials are performed and each can result in 1 of k",
		    "possibilities, k being given by the size of the vector",
		    "of probabilities. The variables are x1,...,xk and they",
		    "always sum to n.",
		 "Notice that 'lpod' must be given by the user since /rebastaba/",
		 "cannot know in advance which can be the highest value of",
		 "the multinomial size which can be inherited from other node",
		 "values."),
	  fami="discr_vector",rep=FALSE,bugs=FALSE,
	  argNO=character(0),
	  argno=c("lnat","lrep","lparent"),
	  argyes=c("lred","lcod","lvar"),
	  argYES=c("lpara","lpod"),
	  ltransfo="",lparent=rbsb.cac,
	  par=list(n="size",p="probability vector (will be normalized) {its length gives the dimension}"),
	  # lpod="ax<-c(0,argu[['lpara']]$n);nx<-length(argu[['lpara']]$p);argu[['lpod']]<-vector('list',nx);argu[['lpod']][1:nx]<-list(ax);", This cannot be computed through /rebastaba/ since the upper value can depend of the parents
	  lnat="argu[['lnat']]<-rep('integ',length(argu[['lpara']]$p));",
	  lrep=0
	  ),
	###
	Dirichlet=list(
	  defi=c(paste("Multivariate generalisation of the beta distribution.",
		 "Mostly used to generate probability vectors for",
		 "multinomial or numcat. It is a multivariate distribution.",
		 "Intuitively, it helps to see it as the random cutting of",
		 "a segment of size one into q components."),
		 paste("Notice that 'lpod' is naturally given by [0,1] but the",
		 "user can restrict it differently for each of the q components.",
		 "In fact, for the moment it is compulsory"),
		 paste("The parameterization is not the usual one. The idea is to",
		       "help the user distinguishing the expectation (given",
		       "with the normalized ai) and the variance (conversely",
		       "related with the sum of the ai, denoted by A)")),
	  fami="conti_vector",rep=FALSE,bugs=FALSE,
	  argNO=character(0),
	  argno=c("lparent","lrep","lnat"),
	  argyes=c("lred","lcod","lvar"),
	  argYES=c("lpod","lpara"),
	  ltransfo="",lparent=rbsb.cac,
	  par=list(A="sum of ai",a="vector (will be normalized) of the ai {its length gives the dimension}"),
	  #lpod="nx<-length(argu[['lpara']]$a);argu[['lpod']]<-vector('list',nx);argu[['lpod']][1:nx]<-0:1;", ((For the moment, new8alk is not adapted to accept no lpod at all, so it is also compulsory))
	  lnat="argu[['lnat']]<-rep('conti',length(argu[['lpara']]$a));",
	  lrep=0
	  ),
	###
	numcat=list(
	  defi=c(paste("'numcat' for CATegoric variable with probability vector/table",
		 "NUMerically determined... Awaiting for a better denomination!",
		"Some kind of empirical distribution for categorical variables",
		"the associated",
		"variable is not integer but a factor. It is not considered as",
		"a multivariate node. The values are the levels of the factor.",
		"When there is no parent, the p parameter is a simple vector",
		"with as many probabilities as levels in the factor.",
		"When there is a categoric parent, the p parameter must be a",
		"matrix with as many rows that categories in the parent; each",
		"row being a different probability vector (also normalized).",
		"When there are several categoric parents, the p parameters must",
		"also been a matrix but the row are associated to the combination",
		"of the categories of the parents using indexing developped in",
		"unex function (see also examples or codes for more details).",
		"When there are two parents proposed in order 'A' then 'B'",
		"The probabilities of the child must be given varying indices",
		"of 'A' faster than those of 'B'..."),
		paste("This means that when there are more than one parent, the matrix",
		"must be constructed as an array, the child node being in last",
		"position."),
		paste("From this, it follows that the parents cannot be deduced from",
		"parameters definition and that THEY HAVE TO BE GIVEN BY lparent argument"),
		paste("When the parent(s) is(are) not categoric, 'parcat', 'easyp' and 'program'",
		"ltypes must be used.")),
	  fami="categoric",rep=FALSE,bugs=FALSE,
	  argNO=c("lnat"),
	  argyes=c("lvar","lred","lcod","lparent"),
	  argYES=c("lpara","lpod"),
	  lnat="categ",
	  par=list(p="the probability vector [or  matrix] (they will be normalized)"),
	  lparent=character(0)
	  ),
	###
	parcat=list(
	  defi=c(paste("'parcat' for CATegoric variable with probability vector",
		 "PARametrically determined... Awaiting for a better denomination!",
		"Some kind of empirical distribution for categorical variables",
		"but conversely to 'numcat' the probability table is always a vector",
		"not a matrix or a multidimensional array. The associated",
		"variable is not integer but a factor. It is not considered as",
		"a multivariate node. The values are the levels of the factor.",
		"When there is no parent, the p parameter is a numeric vector.",
		"In any case, the probability vector must have got as many probabilities",
		"as levels in the factor. A convenient parent for such nodes are",
		"Dirichlet nodes, but this is not an obligation.",
		"As the probabilities are normalized before use, they must not",
		"sum to one ; nevertheless they must not be negative."),
		paste("From this, it follows that the parents are deduced from",
		"the p parameter definition...")),
	  fami="categoric",rep=FALSE,bugs=FALSE,
	  argNO=c("lnat"),
	  argno=c("lparent"),
	  argyes=c("lvar","lred","lcod"),
	  argYES=c("lpara","lpod"),
	  lnat="categ",
	  par=list(p="the probability vector (they will be normalized)"),
	  lparent=rbsb.cac
	  ),
	###
	score=list(
	  defi=c("'score' is a very specific kind of link which transforms",
		 "a 'categ' node into a scalar numeric node.",
		 "This is why, the needed information reduce to the name,",
		 "the unique parent (which must be a 'categ' node) and the",
		 "vector of scores to associate to each category of the",
		 "parent and the possible domain..."),
	  fami="conti_scalar",rep=FALSE,bugs=FALSE,
	  argNO=c("lnat","lvar"),
	  argyes=c("lred","lcod"),
	  argYES=c("lpara","lparent","lpod"),
	  lnat="conti",
	  par=list(scores="the numerical values to give for the variable"),
	  lparent=character(0)
	  ),
	###
	easyp=list(
	  defi=c("Handy(?) programing with the parents. The user can easily introduce",
		"sums, products of parents variables. In fact every R compatible",
		"expressions are allowed. It is suggested that the result will be",
		"checked on some simulations.",
		"One can asked if there is some difference with the",
		"'Dirac' type of node? In fact not, except that the user is supposed",
		"not to introduce any randomness when defining the node",
		"conditionally to the parents",
		"Be aware that in case of syntax errors, it is not that easy to",
		"see where it is, so introduce 'easyp' nodes once at once."),
	  fami="miscellaneous",rep=TRUE,bugs=FALSE,
	  argNO=character(0),
	  par=list(pro="easyprogramming"),
	  argno=c("lparent"),
	  argyes=c("lrep","lvar","ltransfo","lred","lcod"),
	  argYES=c("lnat","lpod","lpara"),
	  lvar="",lrep=1,
	  ltransfo="",lparent=rbsb.cac
	  ),
	###
	empidata=list(
	  defi=c("To allow the distribution to be drawn from available data sets"),
	  fami="data_based",rep=FALSE,bugs=FALSE,
	  argNO=c("lnat","lvar"),
	  argno=c("lrep","lparent"),
	  argyes=c("lwin","lred","lcod"),
	  argYES=c("lpod","ldaf"),
	  lparent=rbsb.cac,
	  lwin=-1,
	  lrep=0
	  ),
	###
	program=list(
	  defi=c("Facility to introduce already prepared and tested simulation functions.",
		"Be aware that you must follow the way the generation function is called",
		"within rebastaba and that the parentship introduced is not checked!"),
	  fami="miscellaneous",rep=FALSE,bugs=FALSE,
	  argNO=character(0),
	  argno=c("lrep"),
	  argyes=c("lparent","lred","lcod"),
	  argYES=c("lvar","lnat","lpod","lfunct","lrep")
		)
	);
#
#   getting the list of defined types of links
#
    rbsb.ltn <- names(rbsb.tlk);
#
# preparing the matrix for 'help8ltype'
#
rbsb.tlk_argu <- matrix("-",length(rbsb.tlk),3+length(rbsb.lta));
dimnames(rbsb.tlk_argu) <- list(rbsb.ltn,c("family","rep?","bugs?",rbsb.lta));
#
for (jbd in sjl(rbsb.tlk)) {
    if (!(rbsb.tlk[[jbd]]$fami %in% rbsb.fam_dis)) {
        erreur(list(names(rbsb.tlk)[jbd],rbsb.tlk[[jbd]]$fami),
               "This family is not registrated in rbsb.fam_dis!",w=TRUE);
        rapport("Defining (1) the 'rbsb.tlk' constant in 'rsba.code.r'");
    } else {
        rbsb.tlk_argu[jbd,1] <- 
           names(rbsb.fam_dis)[which(rbsb.tlk[[jbd]]$fami==rbsb.fam_dis)];
    }
    rbsb.tlk_argu[jbd,2] <- rbsb.tlk[[jbd]]$rep;
    rbsb.tlk_argu[jbd,3] <- rbsb.tlk[[jbd]]$bugs;
    for (jd in rbsb.tlk[[jbd]]$argNO) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rbsb.tlk)[jbd],"in argNO"),w=TRUE);
            rapport("Defining (2) the 'rbsb.tlk' constant in 'rsba.code.r'");
        }
        rbsb.tlk_argu[jbd,jd] <- "NO";
    }
    for (jd in rbsb.tlk[[jbd]]$argno) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rbsb.tlk)[jbd],"in argno"),w=TRUE);
            rapport("Defining (3) the 'rbsb.tlk' constant in 'rsba.code.r'");
        }
        rbsb.tlk_argu[jbd,jd] <- "no";
    }
    for (jd in rbsb.tlk[[jbd]]$argyes) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rbsb.tlk)[jbd],"in argyes"),w=TRUE);
            rapport("Defining (4) the 'rbsb.tlk' constant in 'rsba.code.r'");
        }
        rbsb.tlk_argu[jbd,jd] <- "yes";
    }
    for (jd in rbsb.tlk[[jbd]]$argYES) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(list(names(rbsb.tlk)[jbd],jd),paste("bad argument for",names(rbsb.tlk)[jbd],"in argYES"),w=TRUE);
            rapport("Defining (5) the 'rbsb.tlk' constant in 'rsba.code.r'");
        }
        rbsb.tlk_argu[jbd,jd] <- "YES";
    }
    rbsb.tlk_argu[jbd,c("ldes","ltype")] <- "YES";
    rbsb.tlk_argu[jbd,c("lcomp")] <- "no";
}
#
    ##############################################################
    # ENDING THE DEFINITION OF ltypes
    ##############################################################
    rbsb.l_a <- rbsb.tlk_argu;
    rbsb.all <- "-";
    rbsb.who <- paste(rbsb.cpt["variables","opening"],
                           rbsb.all,
                           rbsb.cpt["variables","closing"],
                           sep="");
#
#
#
# loading the standard values
#
if (tolower(whi)=="reset") {
    assign("rbsb.nom0",new("nom",x=rbsb.lis0),pos=".GlobalEnv");
    assign("rbsb.nom1",new("nom",x=list(A="",B="",C="")),pos=".GlobalEnv");
    assign("rbsb.nom2",new("nom",x=list(A="",B=letters[1:3])),pos=".GlobalEnv");
    assign("rbsb.nom3",new("nom",x=list(A=letters[1:3],B="",C=c("1","2"))),pos=".GlobalEnv");
    assign("rbsb.nom4",new("nom",x=list(a="",b="")),pos=".GlobalEnv");
    assign("rbsb.nom5",new("nom",x=list(a="",B="")),pos=".GlobalEnv");
    assign("rbsb.nom6",new("nom",x=list(V1="",V2="",V3="",
                                        F1="",F2="",F3="")),pos=".GlobalEnv");
    assign("rbsb.nom7",new("nom",x=list(UN=c("one","two","three"),
                                        DEUX=c("uno","dos","tres","cuatro"),
                                        TROIS="")),
                                 pos=".GlobalEnv");
    assign("rbsb.ion0", new(
                      "ion",nn=character(0),vn=character(0),nvn=character(0),
                      nk=numeric(0),ij=numeric(0),vk=numeric(0),
                      iden=character(0)),pos=".GlobalEnv");
    assign("rbsb.ion1", new(
                      "ion",nn=rep("C",2),vn=c("1","2"),nvn=c("C[1]","C[2]"),
                      nk=rep(3,2),ij=1:2,vk=5:6,
                      iden="nv"),pos=".GlobalEnv");
    assign("rbsb.ion2", new(
                      "ion",nn=c("A","B","C"),vn=rep("-",3),nvn=c("A[-]","B[-]","C[-]"),
                      nk=1:3,ij=rep(-1,3),vk=rep(0,3),
                      iden="n"),pos=".GlobalEnv");
    assign("rbsb.ion3", new(
                      "ion",nn=c("A","A","B"),vn=c("a","c",""),nvn=c("A[a]","A[c]","B"),
                      nk=c(1,1,2),ij=c(1,3,1),vk=c(1,3,4),
                      iden="nv"),pos=".GlobalEnv");
    assign("rbsb.arc0",new("arc",nbn=0,fle=matrix(NA,0,3)),pos=".GlobalEnv");
    assign("rbsb.pam0",new("pam",rlt=matrix(NA,0,0)),pos=".GlobalEnv");
    assign("rbsb.pam1",new("pam",rlt=matrix(c(rep(0,10),1,1,0,rep(c(0,0,0,0,1),2),1,0),5)),pos=".GlobalEnv");
    assign("rbsb.win0", new("win",nat=rbsb.nch0,swg=rbsb.nnu0,skk=rbsb.num0,
                                  sdi=rbsb.num0,snb=rbsb.num0,
                                  rwg=rbsb.num0,rkk=rbsb.num0,
                                  rty=c("NULL","NULL"),rmo=rbsb.cha0,
                                  rk2=rbsb.num0),pos=".GlobalEnv");
    assign("rbsb.win1", new("win",nat=structure(rep("conti",3), .Names = letters[1:3]),
                                  rty=c("1","systematic")),pos=".GlobalEnv");
    assign("rbsb.win2", new("win",
                    nat=structure(c("categ",rep("conti",2)),.Names=c("SEX","AGE","HGT")),
                    swg=rbsb.nnu0,skk=rbsb.num0,sdi=rbsb.num0,snb=rbsb.num0,
                                  rty=c("1","systematic"),rmo="AGE",
                                  rwg=rbsb.num0,rkk=rbsb.num0,
                                  rk2=numeric(0)),pos=".GlobalEnv");
    assign("rbsb.win3", 
           new("win",nat=structure(c("categ",rep("conti",3)), .Names = c("SEX","AGE","HGT","WGT")),
                     swg=structure(c(100,1,1),
                                   .Names = c("A[SEX]","A[AGE]","A[HGT]")),
                                  skk=2,sdi=c(0,10),snb=c(1,Inf),
                                  rty=c("0","random"),rmo=character(0),
                                  rwg=rep(1,4),rkk=1,
                                  rk2=numeric(0)),pos=".GlobalEnv");
    assign("rbsb.alk0",new("alk",ldes=new("des",name="AA"),
                                                      ltype="normal",lpara=list(mu=0,sigma=1),
                                                      lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                      lpod=list(c(-3,3)),ltransfo=character(0),
                                                      ldaf=rbsb.daf0,lwin=rbsb.win0,lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.alk1",new("alk",ldes=char2des("my pretty alk"),
                                                 ltype="normal",lpara=list(mu=10,sigma=1),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-14,14)),ltransfo="(|1|)",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.alk2",new("alk",ldes=char2des("simple alk with two parents"),
                                                 ltype="normal",lpara=list(mu="{{A}}",sigma="abs({{B[a]}})"),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-10,50)),ltransfo="(|1|)",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.alk3",new("alk",ldes=char2des("simple alk with all the variable of one parent"),
                                                 ltype="normal",lpara=list(mu="{{C[1]}}+{{C[2]}}",sigma=2),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-10,50)),ltransfo="(|1|)",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.alk4",new("alk",ldes=char2des("Cat"),
                                                 ltype="numcat",lpara=list(p=c(0.4,0.2,0.4)),
                                                 lrep=0,lvar="",lparent=character(0),
                                                 lpod=list(letters[1:3]),lnat="categ",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.pta0",new("pta",name="Null pta",
                                               vam="A",vac=character(0),
                                               vad=character(0),vav=character(0),
                                      kkk=2,pro=array(1:3,dim=3,dimnames=list(A=c("a","b","c")))
                                 ),pos=".GlobalEnv");
    assign("rbsb.pta1",new("pta",name="P.Table",vam=c("A","B"),vac=character(0),vad=character(0),kkk=2,pro=array(rep(0.05,20),dim=c(4,5),dimnames=list(A=letters[1:4],B=LETTERS[1:5]))),pos=".GlobalEnv");
    assign("rbsb.pgr0",new("pgr"),pos=".GlobalEnv");
    assign("rbsb.pos0",new("pos",
                                    posi=matrix(NA,0,4),
                                    view=c(0,0),
                                    zoom=c(0,0,0,1)
                                                ),pos=".GlobalEnv");
    assign("rbsb.tlk",rbsb.tlk,pos=".GlobalEnv");
    assign("rbsb.ltn",rbsb.ltn,pos=".GlobalEnv");
    assign("rbsb.lta",rbsb.lta,pos=".GlobalEnv");
    assign("rbsb.l_a",rbsb.l_a,pos=".GlobalEnv");
    assign("rbsb.f_d",rbsb.f_d,pos=".GlobalEnv");
    assign("rbsb.all",rbsb.all,pos=".GlobalEnv");
    assign("rbsb.who",rbsb.who,pos=".GlobalEnv");
}
#
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
###########################################
########
#((((((( NEW S4 CLASS nom
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8nom <- function(object)
#TITLE  checks a /nom/
#DESCRIPTION (ba)
#   This function checks /nom/ objects
#DETAILS
# It is the validity method for /nom/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The nom object to be validated.>>
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
#CREATED 09_09_30
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="nom") {
        erreur(NULL,paste("This object is not of class 'nom' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    if (!("x" %in% slotNames(object))) {
        res <- c(res,"This supposed /nom/ has no slot @x!");
        return(res);
    }
    #
    # except when empty a named list is expected for @x
    if (length(object@x)>0) {
        if (is.null(names(object@x))) {
            res <- c(res,"slot x of nom must be a NAMED list");
        } 
    }
    # no duplicatd node names
    if (sum(duplicated(names(object@x)))>0) {
        res <- c(res,"Node names must be unique");
    }
    # no empty node names
    if ("" %in% names(object@x)) {
        res <- c(res,"Node names must not be empty");
    }
    # for the variable names
    for (jbd in sjl(names(object@x))) {
        # at least one variable 
        if (length(object@x[[jbd]])==0) {
            res <- c(res,"A node (jbd) has got no variable name, '' must be introduced at minimum");
        }
        # no duplication within a node
        if (sum(duplicated(object@x[[jbd]]))>0) {
            res <- c(res,"Some of these variable names are duplicated");
        }
        # no empty node except when unique
        if (("" %in% object@x[[jbd]])&(length(object@x[[jbd]])>1)) {
            res <- c(res,"Variable names must not be empty except when the variable is unique");
        }
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("nom", representation(
         x="list" # list of variable names of each node, the component names
                   # are the node names when there is no variable name,
		   # must be "" NOT character(0) for the length giving
                   # giving the number of variables in any cases...
                              ),
               prototype(x=rbsb.lis0),
               validity=valid8nom
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8nom <- function(x,...,what="v",type=0)
#TITLE  prints the node-variable names
#DESCRIPTION (ba)
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{what} << what to print ('v'=node:variable names;
#          'nv'=node[variable] names;
#         'n' only node names)
#{type} << type of printing: 
#              (0) everything on the same line;
#              (1) a node, a line.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.nom2);
# print(rbsb.nom2,what='nv');
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_08
#REVISED 10_06_10
#--------------------------------------------
{
# some checks
if (rbsb.mck) {
    #
    che <- valid8nom(x);
    if (!identical(che,TRUE)) {
        erreur(che,"This /nom/ is not valid");
    }
    #
    types <- 0:1;
    if (!(type %in% types)) {
        erreur(list(types,type),"Bad argument");
    }
    #
    forma <- c("v","n","nv");
    if (!(what %in% forma)) {
        erreur(list(forma,what),"Bad argument");
    }
}
#
if (nbnv(x) > 0) {
    for (ii in names(x@x)) {
        if (what=="n") {
            cat(" ",ii);
        }
        if (what=="v") {
            cat(" ",ii);
            cat(form3liste(x@x[[ii]],none=character(0),OPA="[",CPA="]",opa="",cpa="",sep="|"));
        }
        if (what=="nv") {
            cat("  ");
            if (identical(x@x[[ii]],"")) { cat(ii);
            } else {
                cat(form3liste(x@x[[ii]],none=character(0),OPA="",CPA="",opa=paste(ii,"[",sep=""),cpa="]",sep=","));
            }
        }
    if (type==1) { if (ii!=names(x@x)[nbnv(x)]) { cat("\n");}}
    }
}
cat("\n");
#
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
length8nom <- function(x)
#TITLE  returns the length of a 'nom' object
#DESCRIPTION (ba)
# provides the number of nodes of a \code{/nom/}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{x}<<the 'nom' object to be measured.>>
#[INPUTS]
#VALUE
# The number of nodes in 'x'
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# length(rbsb.nom1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_29
#REVISED 09_04_29
#--------------------------------------------
{
# returning
length(x@x);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "nom"), print8nom);
setMethod("length",signature(x = "nom"), length8nom);


###########################################
###########################################
########
#((((((( NEW S4 CLASS ion
########
###########################################
###########################################



#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8ion <- function(object)
#TITLE  checks a /ion/
#DESCRIPTION (ba)
#   This function checks /ion/ objects
#DETAILS
# It is the validity method for /ion/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The ion object to be validated.>>
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
#CREATED 09_10_01
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="ion") {
        erreur(NULL,paste("This object is not of class 'ion' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    #
    # each slot must have the same lenght
    nl <- length(object@nn);
    if (length(object@vn) != nl) { res <- c(res,"length of @vn  is different from this of @nn");}
    if (length(object@nvn)!= nl) { res <- c(res,"length of @nvn is different from this of @nn");}
    if (length(object@nk) != nl) { res <- c(res,"length of @nk  is different from this of @nn");}
    if (length(object@ij) != nl) { res <- c(res,"length of @ij  is different from this of @nn");}
    if (length(object@vk) != nl) { res <- c(res,"length of @vk  is different from this of @nn");}
    #
    # all variate names must be different
    if (length(object@nvn) != length(unique(object@nvn))) {
        res <- c(res,paste(object@nvn,"repetitions between variate names"));
    }
    
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
# see the comment into nv2ion for some explanation...
#
setClass("ion", representation(
         nn="character",  # node names
         vn="character",  # variable names
         nvn="character", # node[variable] names
         nk="numeric",    # node numbers
         ij="numeric",    # variable numbers within node
         vk="numeric",    # variable numbers
         iden="character" # identification of the inputs
                              ),
               prototype(nn=character(0),vn=character(0),nvn=character(0),
                         nk=numeric(0),ij=numeric(0),vk=numeric(0),
                         iden=character(0)),
               validity=valid8ion
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
length8ion <- function(x)
#TITLE  returns the length of a 'ion' object
#DESCRIPTION (ba)
# provides the length of a \code{/ion/}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{x}<<the 'ion' object to be measured.>>
#[INPUTS]
#VALUE
# The number of items in 'x'
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# length(rbsb.ion0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_28
#REVISED 09_04_28
#--------------------------------------------
{
# returning
length(x@nn);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8ion <- function(x,how="n")
#TITLE  prints a 'ion' object
#DESCRIPTION (ba)
# print associated to a \code{/ion/} object.
#DETAILS
#KEYWORDS print
#PKEYWORDS 
#INPUTS
#{x}<<the 'ion' object to be printed.>>
#[INPUTS]
#{how} << the way to make the printing:
#               'n' for node names
#               'v' for variable names
#               'i' for node numbers
#               'j' for nested variable numbers
#               'k' for variable numbers
#               'a' for everything.>>
#VALUE
# Nothing but a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.ion0);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_28
#REVISED 09_04_28
#--------------------------------------------
{
# some checks
che <- valid8ion(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /ion/ is not valid");
}
#
if (expr3present("a",how)) { how<-"nvijk";}
if (length(x)==0) { return(invisible());}
# constituting the necessary matrix
nr <- character(0); ma <- matrix(0,0,length(x@nn));
if (expr3present("n",how)) {
    nr <- c(nr,"nn");
    ma <- rbind(ma,x@nn);
}
if (expr3present("v",how)) { if (length(x@vn)>0) {
    nr <- c(nr,"vn");
    ma <- rbind(ma,x@vn);
}}
if (expr3present("i",how)) {
    nr <- c(nr,"nk");
    ma <- rbind(ma,x@nk);
}
if (expr3present("j",how)) {
    nr <- c(nr,"k.j");
    ma <- rbind(ma,x@ij);
}
if (expr3present("k",how)) { if (length(x@vk)>0) {
    nr <- c(nr,"vk");
    ma <- rbind(ma,x@vk);
}}
dimnames(ma) <- list(nr,x@nvn);
print(ma);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "ion"), print8ion);
setMethod("length",signature(x = "ion"), length8ion);

###########################################
###########################################
########
#((((((( NEW S4 CLASS arc
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8arc <- function(object)
#TITLE  checks a /arc/
#DESCRIPTION (ba)
#   This function checks /arc/ objects
#DETAILS
# It is the validity method for /arc/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The arc object to be validated.>>
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
#CREATED 09_10_03
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="arc") {
        erreur(NULL,paste("This object is not of class 'arc' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    # fatal
    if (nrow(object@fle)>0) {
        if (max(object@fle[,1:2]) > object@nbn) {
            res <- c(res,"Inconsistency of arcs with the node number (@nbn)!");
        }
        if (any(object@fle[,1:2] <= 0)) {
            res <- c(res,"A @fle matrix must not have non positive value in its first two columns!");
        }
    }
    if (ncol(object@fle) != 3) { res <- c(res,"A @fle matrix must have 3 columns!");}
    # warning
    if (nrow(object@fle)>0) {
        sco <- object@fle[,1] + object@nbn*object@fle[,2];
        if (length(sco) > length(unique(sco))) {
            res <- c(res,"duplicated arcs in @fle");
        }
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
setClass("arc",representation(
    nbn="numeric",       #  number of items
    fle="matrix"),        #  matrix with 3 columns
                         #  each row giving the dep. and arr. items
                         #  plus the decoration to apply to the arc.
               prototype(nbn=0,fle=matrix(NA,0,3)),
               validity=valid8arc
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8arc <- function(x,...)
#TITLE  prints an /arc/
#DESCRIPTION (ba)
# prints the matrix defining arcs
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<arc object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.arc0);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_10_03
#REVISED 09_10_03
#--------------------------------------------
{
# some checks
che <- valid8arc(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /arc/ is not valid");
}
# printing
cat("There are",x@nbn,"nodes and",nrow(x@fle),"arcs defined by\n");
print(x@fle);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "arc"), print8arc);



###########################################
###########################################
########
#((((((( NEW S4 CLASS pam
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8pam <- function(object)
#TITLE  checks a /pam/
#DESCRIPTION (ba)
#   This function checks /pam/ objects
#DETAILS
# It is the validity method for /pam/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The pam object to be validated.>>
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
#CREATED 09_10_03
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="pam") {
        erreur(NULL,paste("This object is not of class 'pam' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    nbn <- nrow(object@rlt);
    if (ncol(object@rlt) != nbn) { res <- c(res,dim(object@rlt),"A pam matrix must be squared!");}
    if (sum((object@rlt==0)|(object@rlt==1)) != nbn*nbn) {
        res <- c(res,"A pam matrix must be binary (0/1 values)!");
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("pam",representation(
    rlt="matrix"),       #  square matrix with as many rows as items
                         #  parents in rows, children in columns
                         #  1 if a ReLaTion, 0 otherwise.
               prototype(rlt=matrix(NA,0,0)),
               validity=valid8pam
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pam <- function(x,...)
#TITLE  prints a /pam/
#DESCRIPTION (ba)
# Nothing more than printing a /pam/ matrix (after checking it).
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<pam object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.pam0);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_10_03
#REVISED 09_10_03
#--------------------------------------------
{
# some checks
che <- valid8pam(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /pam/ is not valid");
}
# printing
cat("This /pam/ matrix is associated to",nrow(x@rlt),"nodes/variates\n");
print(x@rlt);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "pam"), print8pam);



###########################################
###########################################
########
#((((((( NEW S4 CLASS win
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8win <- function(object)
#TITLE  checks a /win/
#DESCRIPTION (ba)
#   This function checks /win/ objects
#DETAILS
# See the code or the associated documentation.
#KEYWORDS classes
#INPUTS
#{object} <<The win object to be validated.>>
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
#CREATED 09_10_03
#REVISED 10_08_11
#--------------------------------------------
{
    #
    # eliminating the NULL case
    #
    if (identical(object@rty,rep("NULL",2))) { return(TRUE);}
    #
    if (class(object)!="win") {
        erreur(NULL,paste("This object is not of class 'win' but '",
               class(object),"'",sep=""));
    }
    res <- character(0);
    #
    # checking the definition of the variables
    #
    rr <- check4tyle(object@nat,"ncharacter",c(1,Inf));
    if (is.character(rr)) { res <- c(res,rr);}
    if (!expr3present(object@nat,rbsb.sna,exact=TRUE,how="a")) {
        res <- c(res,"The nature(s) are not valid");
    }
    nbva <- length(object@nat);
    if (length(unique(names(object@nat)))!=nbva) {
        res <- c(res,"Variable nature names (names(object@nat)) are repeated");
    }
    #
    # checking the selection definition slots
    #
    rr <- check4tyle(object@swg,"nnumeric",  -1,"valid8win",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    if (length(unique(names(object@swg)))!=length(object@swg)) {
        res <- c(res,"Covariate weight names (names(object@swg)) are repeated");
    }
    uu <- expr3extrait(paste(names(object@swg),collapse=" "),
                       rbsb.cpt["variables","opening"],
                       rbsb.cpt["variables","closing"]);
    if (length(uu)>0) {if (length(unique(uu))!=length(uu)) {
        res <- c(res,"Identically named variables from different nodes seems used as parents?");
    }}
    cova <- (length(object@swg)>0);
    rr <- check4tyle(object@skk  ,"numeric",1*cova,paste("win@skk must be numeric of length",1*cova),FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    rr <- check4tyle(object@sdi ,"numeric",2*cova,paste("win@sdi  must be numeric of length",2*cova),FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    rr <- check4tyle(object@snb ,"integer",2*cova,paste("win@snb  must be integer of length",2*cova),FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    if (length(object@swg)>0) {
        if (sum(abs(object@swg))==0) {
            res <- c(res,"In win@swg, not all weights can be zero!");
        }
    }
    if(any(object@swg < 0)) { res <- c(res,"win@swg  cannot be negative");}
    if(any(object@skk < 0)) { res <- c(res,"win@skk  cannot be negative");}
    if(any(object@sdi < 0)) { res <- c(res,"win@sdi cannot be negative");}
    if(any(object@snb <=0)) { res <- c(res,"win@snb cannot be null or negative");}
    if(any(diff(object@sdi) < 0)) { res <- c(res,"win@sdi must be non decreasing");}
    if(any(diff(object@snb) < 0)) { res <- c(res,"win@snb must be non decreasing");}
    #
    # checking the representation definition slots
    #
    rr <- check4tyle(object@rty ,"character", 2,"win@rty  must be character of size 2",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    #            1   2   3
    choix1 <- c("*","0","1");
    #              1       2        3        4         5          6
    choix2 <- c("mean","median","stdev","quantile","random","systematic");
    if (!(object@rty[1] %in% choix1)) {
         res <- c(res,paste("win@rty is not consistent (=",object@rty[1],
                            ") must be among",paste(choix1,collapse="/")));
    }
    if (!(object@rty[2] %in% choix2)) {
         res <- c(res,paste("win@rty is not consistent (=",object@rty[2],
                            ") must be among",paste(choix2,collapse="/")));
    }
    #
    rr <- check4tyle(object@rmo  ,"character",c(0,1),paste("win@rmo must be numeric(0:1)"),FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    if(any(object@rkk < 0)) { res <- c(res,"win@rkk  cannot be negative");}
    # quantile case
    if (object@rty[2] %in% choix2[4]) {
         # a numeric constant is needed in @rk2
         if (length(object@rk2) != 1) { 
             res <- c(res,paste("One numerical constant is needed in @rk2 for @rty[2] =",object@rty[2]));
         } else {
             if ((object@rk2 < 0) | (object@rk2 > 1)) {
                 res <- c(res,paste("@rk2 must be a proba when @rty[2] =",object@rty[2]));
             }
         }
    }
    # systematic case
    if (object@rty[2] %in% choix2[6]) {
         # one or no numeric constant is needed in @rk2
         if (length(object@rk2) > 1) { 
             res <- c(res,paste("At most one numerical constant is needed in @rk2 for @rty[2] =",object@rty[2]));
         } else {
             if (length(object@rk2)==1) { if ((object@rk2 < 1) | (round(object@rk2)!=object@rk2)) {
                 res <- c(res,paste("@rk2 must be a positive integer when @rty[2] =",object@rty[2]));
             }}
         }
    }
    if (length(object@rwg) > 0) {
        rr <- check4tyle(object@rwg,"numeric",nbva,"valid8win",FALSE);
        if (is.character(rr)) { res <- c(res,rr);}
        rr <- check4tyle(object@rkk  ,"numeric",1,paste("win@rkk must be numeric of length",1),FALSE);
        if (is.character(rr)) { res <- c(res,rr);}
    }
    #
    if ((object@rty[2] %in% choix2[1:4]) & 
        (object@rty[1] %in% choix1[1:2]) & 
        (length(object@rmo) > 0)) {
        res <- c(res,paste("No monitoring variable for @rty =",object@rty))
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("win",representation(
                         #
                         # __details in the Rd file__
                         #
                         # defining the variables and their natures
                         #
    nat="character",     # natures of the variables giving also the names
                         #
                         #  "s" defines a window for selection of candidates
                         #
    swg="numeric",       #  named vector of the weights for the involved
                         #  parents' of the node.
    skk="numeric",       #  power coefficient for the distance
    sdi="numeric",       #  minimum and maxima distances accepted
    snb="numeric",       #  minimum and maxima candidate numbers accepted
                         #
                         #  "r" defines the algorithm to apply for 
                         #      representing with one or several
                         #      individuals a given individual
    rty="character",     #  specification of the kind of representation:
                         #    rty[1] the way the representation is done
                         #         '*' each variable independently  
                         #         '0' the real row, the closest to '*'
                         #         '1' a real row selected from '@rmo'
                         #    rty[2] type of prediction:
                         #         'random' a random draw with possibly a 
                         #                  weighting given by the absolute
                         #                  value of pmo when rty[1]=='1'
                         #                  or =='*'.
                         #         'mean' the mean, on pmo when rty[1]=='1'
                         #         'median', on pmo when...
                         #         'systematic', the frequency of the drawing
                         #                  is given by rk2, if it does not
                         #                  exist it is set to one;
                         #                  when rty[1] is '1' or '*', 
                         #                  pmo is considered
                         #                  to defined the order of drawing
                         #                  if not the order of the data basis
                         #                  is used;
                         #         'quantile', as median but for any probability
                         #                  level indicated in rk2
     rmo="character",    #     the monitoring variable
     rk2="numeric",      #     the frequency of the systematic drawing
                         #               (one each every k[2])
                         #           or the quantile for the quantile drawing
                         #               (k[2] is the desired probability)
    rwg="numeric",       #  vector of the weights for the variables
                         #      of the created node.
    rkk="numeric"),      #  power coefficient for the distance calculation
               prototype(nat=structure("conti",.Names="A"),
                         swg=rbsb.nnu0,sdi=rbsb.num0,snb=rbsb.num0,skk=rbsb.num0,
                         rty=c("*","random"),rmo=rbsb.cha0,rk2=rbsb.num0,
                         rwg=rbsb.nnu0,rkk=rbsb.num0
                        ),
               validity=valid8win
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8win <- function(x,...,fo=1)
#TITLE  prints a /win/
#DESCRIPTION (ba)
# prints a /win/ object after checking its validity
#DETAILS
#KEYWORDS print
#PKEYWORDS
#INPUTS
#{x} <<win object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.
# In fact, not used but kept for the consistence with the other
# print functions.>>
#{fo} << format to use, 1 for standard,
#                       0 for reduced.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.win0);
# print(rbsb.win1);
# print(rbsb.win2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_08
#REVISED 10_07_27
#--------------------------------------------
{
# some checks
if (rbsb.mck) {
    che <- valid8win(x);
    if (!identical(che,TRUE)) {
        erreur(che,"This /win/ is not valid");
    }
}
#
# printing
if (identical(fo,1)) {
# the null case
    nsou <- 39;
    if (isvide(x)) {
        form3repete("=",nsou,TRUE);
        cat("            NULL /win/\n");
        form3repete("=",nsou,TRUE);
        return(invisible());
    }
    #
    form3repete("=",nsou,TRUE);
    cat("The node variable(s) is/are (with nature):\n");
    print(x@nat);
    form3repete("-",nsou,TRUE);
    if (isvide(x@swg)) {
        cat("No subsetting, all the data.frame used\n");
    } else {
        cat("Subsetting is done with the parents and weights:\n");
        print(x@swg);
        cat("  (power coefficient is ",x@skk,")\n",sep="");
        cat("Criteria of acceptance are:\n");
        cat("    min/MAX distances:",x@sdi,"\n");
        cat("    min/MAX numbers  :",x@snb,"\n");
    }
    form3repete("-",nsou,TRUE);
    cat("/win/ with a drawing type of",x@rty[2],"\n");
    if (x@rty[2] == "systematic") {
        if (isvide(x@rk2)) { cat("Standard frequency (=1)\n");
        } else { cat("Frequency of",x@rk2,"\n");}
    }
    if (x@rty[2] == "quantile") {
        cat("Probability of",x@rk2,"\n");
    }
    if (x@rty[1] == "*") { 
        cat("   independently for each variable\n   (raw result)\n");
    }
    if (x@rty[1] == "0") { 
        cat("   independently for each variable\n   (represented by the closest individual)\n");
    }
    if (x@rty[1] == "1") { 
        cat("   A real individual drawn\n");
        if(!isvide(x@rmo)) {
            cat("   (according to the monitoring variable)\n",sep="");
        }
    }
    if (isvide(x@rmo)) { cat("No monitoring variable\n");
    } else { cat("The monitoring variable is",x@rmo,"\n");}
    if (length(x@rwg)==0) {
        cat("No weights associated to the variables\n");
    } else {
        cat("The  weights associated to the variables are:\n");
        print(structure(x@rwg,.Names=names(x@nat)));
        cat("  (power coefficient is ",x@rkk,")\n",sep="");
    }
    form3repete("=",nsou,TRUE);
} else {
    if (isvide(x)) {
        cat("NULL /win/\n");
        return(invisible());
    }
    cat("@nat  =  ");
    for (ii in sjl(x@nat)) {
        cat(names(x@nat)[ii],": ",x@nat[ii],";  ",sep="");
    }
    cat("\n");
    cat("@swg  =  ");
    for (ii in sjl(x@swg)) {
        cat(names(x@swg)[ii],": ",x@swg[ii],";  ",sep="");
    }
    cat("\n");
    cat("@skk  = ",x@skk,"\n");
    cat("@sdi  = ",x@sdi,"\n");
    cat("@snb  = ",x@snb,"\n");
    cat("@rty  = ",x@rty,"\n");
    cat("@rmo  = ",x@rmo,"\n");
    cat("@rk2  = ",x@rk2,"\n");
    cat("@rwg  = ",x@rwg,"\n");
    cat("@rkk  = ",x@rkk,"\n");
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
win2list <- function(win)
#TITLE  transforms into a list a /win/
#DESCRIPTION (ba)
# transforms into a list a \code{win} object
#DETAILS
#KEYWORDS misc
#PKEYWORDS win
#INPUTS
#{win} <<the win object to be transformed>>
#[INPUTS]
#VALUE
# a list of vectors
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# win2list(new("win",nat=structure(rep("categ",7),.Names=letters[1:7])));
# win2list(NULL);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_06_25
#REVISED 10_08_13
#--------------------------------------------
{
# degenerate case
if (isvide(win)) { return(rbsb.lis0);}
# some checks
if (rbsb.mck) {
    che <- valid8win(win);
    if (!identical(che,TRUE)) {
        erreur(che,"This /win/ is not valid");
    }
}
# preparing
res <- list(nat=c(names(win@nat),as.character(win@nat)),
            swg=c(names(win@swg),as.character(win@swg)),
            skk=as.character(win@skk),
            sdi=as.character(win@sdi),
            snb=as.character(win@snb),
            rty=win@rty,
            rmo=win@rmo,
            rk2=as.character(win@rk2),
            rwg=as.character(win@rwg),
            rkk=as.character(win@rkk)
           );
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2win <- function(li)
#TITLE  transforms a convenient list into a /win/
#DESCRIPTION (ba)
# transforms a convenient list into a \code{win} object
#DETAILS
#KEYWORDS misc
#PKEYWORDS win
#INPUTS
#{li} <<the list to be transformed>>
#[INPUTS]
#VALUE
# the resulting win
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# list2win(list(nat=c("a","conti"),swg=c(letters[1:7],1:7),
#               skk="0",sdi=as.character(0:1),
#               snb=as.character(3:4),rwg="1",rkk="1",
#               rty=c("*","median")));
# list2win(NULL);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_06_25
#REVISED 10_08_11
#--------------------------------------------
{
# degenerate cases
if (isvide(li)) {return(rbsb.win0);}
# some checks
if (rbsb.mck) {
    if (!is.list(li)) {
        erreur(li,'The argument must be a list');
    }
}
# preparing (either a char to be transformed or a 
#            numeric according to the slot)
nat <- char2vma(li$nat,"V",nat="C");
swg <- char2vma(li$swg,"V",nat="N");
skk <- char2vma(li$skk,"v",nat="N");
sdi <- char2vma(li$sdi,"v",nat="N");
snb <- char2vma(li$snb,"v",nat="N");
rwg <- char2vma(li$rwg,"v",nat="N");
rkk <- char2vma(li$rkk,"v",nat="N");
rty <- char2vma(li$rty,"v",nat="C");
rmo <- char2vma(li$rmo,"v",nat="C");
rk2 <- char2vma(li$rk2,"v",nat="N");
res <- new("win",nat=nat,
                 swg=swg,skk=skk,sdi=sdi,snb=snb,
                 rwg=rwg,rkk=rkk,rty=rty,rmo=rmo,rk2=rk2);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "win"), print8win);


###########################################
###########################################
########
#((((((( NEW S4 CLASS alk
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8alk <- function(object)
#TITLE  checks an /alk/
#DESCRIPTION (ba)
#   This function checks /alk/ objects in
# a light way. For a more complete checking
# one must use \code{check8alk}
#DETAILS
# It is the validity method for /alk/ objects.\cr
# The main idea is that non completed /alk/ must
# avoid the redundancy while completed /alk/ must
# have all slot filled in the standard way.
#KEYWORDS classes
#INPUTS
#{object} <<The alk object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s).
# Sometimes, when the inconsistency seems too
# severe, a fatal error can be issued.
#EXAMPLE
#REFERENCE
#SEE ALSO check8alk
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_04
#REVISED 10_09_06
#--------------------------------------------
{
#########################################
#####  permanent prerequisites          #
#########################################
    if (class(object)!="alk") {
        erreur(NULL,paste("This object is not of class 'alk' but '",
                          class(object),"'",sep=""));
    }
    res <- character(0);
    #
    # an alk must comprise a valid description
    rr <- valid8des(object@ldes);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    # an alk must have a logical lcomp slot
    rr <- check4tyle(object@lcomp,"logical",1,"for /alk/, logical slot @lcomp must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    # the type must be defined
    check4tyle(object@ltype,"character",1,"for /alk/, character slot @ltype must exist",TRUE);
    # the nature must be character even if of length zero
    rr <- check4tyle(object@lnat,"character",-1,"for /alk/, character slot @lnat must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    ###
    nbv <- length(alkvar(object));
    if (object@ltype == "empidata") {
        rr <- valid8win(object@lwin);
        if (is.character(rr)) { res <- c(res,rr);}
        if (identical(object@lwin,rbsb.win0)) {
            res <- c(res,"NULL /win/ not allowed for an data based /alk/");
        }
    } else {
        if(!identical(object@lwin,rbsb.win0)) {
            res <- c(res,"Non data based /alk/ must have a nul @lwin");
        }
    }
    #
    # checking the existence and the consistency of the variable
    # natures for empidata
    if (object@ltype %in% c("empidata")) {
        ddd <- get8daf(object@ldaf,1:5);
        vana <- alkvar(object);
	for (vv in vana) {
	    if (!(vv %in% c(names(ddd),names(rbsb.ena)))) {
		res <- c(res,paste(vv,
                        "is not a variable of the proposed data frame"));
	    } else {
		if (is.factor(ddd[[vv]]) & !(object@lwin@nat[vv] %in% c("categ","cateo"))) {
		    res <- c(res,paste("The nature of the",vv,"th variable (",vv,
			               ") of the associated data frame is not consistent"));
		}
            }
	}
    } 
    if (identical(object@lcomp,TRUE)) {
###################################################
###### when the node is supposed to be completed  #
###################################################
	if (length(object@lvar) != nbv) {
	    res <- c(res,paste("The /alk/ of ltype",
                               object@ltype,
                               "seems not have been properly completed for the slot '@lvar'"));
	}
        # length of lnat
        if(length(object@lnat) != length(object@lvar)) {
            res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),
                     "For completed /alk/ the lengths of @lnat and @lvar must be equal"));
        }
	# checking the dimensions of the declared domains
	if (length(object@lpod) != nbv) {
	    res <- c(res,paste(object@lpod,"In a completed /alk/, Bad @lpod?",nbv,"variable(s) await possible domain(s)"));
	}
	if (length(object@lred) != nbv) {
	    res <- c(res,paste(object@lred,"In a completed /alk/, Bad @lred?",nbv,"variable(s) await representation domain(s)"));
	}
	if (length(object@lcod) != nbv) {
	    res <- c(res,paste(object@lcod,"In a completed /alk/, Bad @lcod?",nbv,"variable(s) await common domain(s)"));
	}
    } else { 
#########################################################
######  when the node is supposed not to be completed   #
#########################################################
        #
        if (object@ltype %in% c("empidata")) {
            # checking specificity of 'empidata' nodes
            if (!isvide(object@lparent)) {
                res <- c(res,"With non completed empidata /alk/ must not comprise non empty 'lparent' slot (included in 'lwin')");
            }
            if (!isvide(object@lvar)) {
                res <- c(res,"With non completed empidata /alk/ must not comprise non empty 'lvar' slot (included in 'lwin')");
            }
            if (!isvide(object@lnat)) {
                res <- c(res,"With non completed empidata /alk/ must not comprise non empty 'lnat' slot (included in 'lwin')");
            }
        } else {
            # checking specificity of non 'empidata' nodes
	    if (length(object@lvar) > 0) {
		if(length(object@lnat) != length(object@lvar)) {
		    res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),
                             "The lengths of lnat and lvar must be equal for multivariate nodes"));
		}
	    } else {
		if(length(object@lnat) != 1) {
		    form3affiche(list(object@ldes,object@lcomp));
		    res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),
                             "The length of lnat must be equal to ONE for scalar and repeated nodes"));
		}
	    }
        }
	# checking the dimensions of the declared domains
	if (length(object@lpod) != nbv) {
            form3affiche(object@ldes@name);
	    res <- c(res,paste(paste(object@lpod,object@lvar,collapse="/"),"Inconsistent lpod and lvar"));
	}
	if (length(object@lred)>0) { if (length(object@lred) != nbv) {
            form3affiche(object@ldes@name);
	    res <- c(res,paste(paste(object@lred,object@lvar,collapse="/"),"Inconsistent lred and lvar"));
	}}
	if (length(object@lcod)>0) { if (length(object@lcod) != nbv) {
            form3affiche(object@ldes@name);
	    res <- c(res,paste(paste(object@lcod,object@lvar,collapse="/"),"Inconsistent lcod and lvar"));
	}}
    }
    #
    # returning
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




###########################################
# Objects from this class can have two states
# (i) such as defined by the user, some slots can be potentially
#     defined (or not well checked, like the existence of the
#     parents). This kind of state allows the constitution of 
#     basis of nodes from them, different bn can be constructed. 
#     A classical case is when competitive formulae have to be
#     compared.
# (ii) completed by complete8alk then all necessary fields are
#     filled with respect to a consistent bn.
# "alk" means Asked LinK.

setClass("alk",representation(
    ldes="des", # the name and human information with respect with the node
    ltype="character",   # type of link (see help8ltype function for details)
    lpara="list", # list of parameters (when repeated the repetition
                  # is through vector of these values)
    lrep="numeric", # indicates if the number of repeated variables
                    # idependently distributed (some ltype cannot be repeated
                    # (numcat, multivariate,...) when it is not the case
                    # must be zero).
    lnat="character", # nature of each variable
    lvar="character", # character(nb_var), "" when not multivariate
    lparent="character", # at the variate level
                         # when a node multivariate is involved, it is 
                         # extended at the variate level
    lpod="list", # as many as variables for multivariate, the same if not
    lred="list", # as lpod
    lcod="list", # as lpod
    ltransfo="character", # done by the way of easyprogramming
                          # when there are several variables, the same
                          # transformation is applied to each of them
                          # so it must be of length one
    ldaf="daf",           # for ltype='empidata'
                          #           
    lwin="win",           # for ltype='empidata'
    lfunct="function",    # R function for ltype='program'
    lcomp="logical"),     # indicates if the alk is completed (in general by /rebastaba/)
                          # within the context of a given bn
    # notice: a key feature of links is its dimension (1 for
    # univariate). This one is no more explicited because it
    # implicitely given by the length of the slot @lvar. This is
    # one of the reasons why "" must be provided when the variable
    # is implicit (not named).
    #
               prototype(ldes=new("des",name="AA"),
                         ltype="normal",
                         lpara=list(mu=0,sigma=1),
                         lrep=0,
                         lnat="conti",
                         lvar="",lparent=character(0),
                         lpod=list(c(-3,3)),
                         ltransfo=character(0),
                         ldaf=rbsb.daf0,
                         lwin=new("win",nat=rbsb.nch0,swg=rbsb.nnu0,
                                        sdi=rbsb.num0,snb=rbsb.num0,skk=rbsb.num0,
                                        rwg=rbsb.num0,rkk=rbsb.num0,
                                        rty=c("NULL","NULL"),rmo=rbsb.cha0,rk2=rbsb.num0
                                 ),
                         lcomp=FALSE),
               validity=valid8alk
);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8alkcomp <- function(alk,des=0,retrait=4)
#TITLE  prints an alk object supposed to be completed
#           for a bn
#DESCRIPTION (ba)
#   This function prints in a interpreted way an alk object
# supposed to be completed.. Must not be called by the user
# who might use directly the \code{print} associated to \code{/alk/}.
#DETAILS The interpretation is hand made for each type of links.
#DETAILS The interpretation is hand made for each type of links.
# The node name is not given because it could be more
# convenient to print it outside this function with some
# adequate format. 
#KEYWORDS print
#PKEYWORDS link bn
#INPUTS
#{alk} <<The alk object.>>
#[INPUTS]
#{des} << indicates if the description slot must be
#        printed. O: no; 1: in a short way; 2: completely.
#        Other values means that nothing is printed.>>
#{retrait} << number of spaces for indentation.>>
#VALUE
# nothing but a print is performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_11_06
#REVISED 09_10_08
#--------------------------------------------
{
# some checking
# some checks
che <- valid8alk(alk);
if (!identical(che,TRUE)) {
    erreur(che,"This /alk/ is not valid");
}
#
if (!alk@lcomp) {
    erreur("print8alkcomp","this alk is not indicated as completed!");
}
ax <- function(x) { form3titre(x,0,retrait);}
lt <- alk@ltype;
nav <- alk@lvar;
ld <- length(alk@lvar);
pod <- alk@lpod;
red <- alk@lred;
cod <- alk@lcod;
if (des == 0) { print(alk@ldes,"n");}
if (des == 1) { print(alk@ldes,"ndo");}
if (des == 2) { print(alk@ldes,"ndotrc");}
ax(paste("link type is",lt));
quoi <- names(rbsb.l_a[lt,rbsb.l_a[lt,]!="-"]);
for (qui in quoi) {
    if (qui == "lpara") {
        nbp <- length(alk@lpara);
        ax(paste("distribution defined with",nbp,
                         "parameter(s)"));
        if (lt %in% c("numcat")) {
            ax("See the probability table below");
        } else {
            for (ip in sj(nbp)) {
                ax(paste(names(alk@lpara[ip]),"=",
                         paste(alk@lpara[[ip]],collapse=" ; ")));
            }
        }
    }
    if (qui == "lvar") {
        if (ld==1) {
            if (nav[1]=="") {
                ax("No specific variable name");
            } else {
                ax(paste("The variable name is:",nav[1]));
            }
        }
        if (ld > 1) {
            ax(paste("The",ld,"variable names are:"));
            form3titre(form3liste(nav,OPA="",CPA="",opa="",cpa="",sep="; "),0,9);
        }
        for (va in 1:length(nav)) {
            nnav <- nav[va];
            ax(paste(nnav,": possible       values:",paste(pod[[va]],collapse="; ")));
            ax(paste(nnav,": representation values:",paste(red[[va]],collapse="; ")));
            ax(paste(nnav,": common         values:",paste(cod[[va]],collapse="; ")));
        }
    }
    if (qui == "ltransfo") {
        if ((length(alk@ltransfo) == 0)||(alk@ltransfo == "")) {
            ax("No transformation");
        } else {
            qq <- easyp3cut(alk@ltransfo,rbsb.cpt);
            if ( (length(qq$typ) == 1) & all(qq$typ == 4) ) {
                ax(paste("rounding with",qq$blo,"decimal(s)"));
            } else { ax(paste("transforming with an easyp =",alk@ltransfo));}
        }
    }
    if (qui == "leasyp") {
        ax("The piece of code to use is:");
        ax(alk@lpara$pro);
    }
    if (qui == "lfunct") {
        ax("The simulating function to use is:");
        print(alk@lfunct);
    }
    #
    if (!isvide(alk@ldaf)) {
    if (qui == "ldaf") {
        if(alk@ldaf@what=="t") {
            ax(paste("The data base is stored into file \"",alk@ldaf@valu,"\"",sep=""));
        }
        if(alk@ldaf@what=="d") {
            ax(paste("The data base is given by the variable of name \"",alk@ldaf@valu,"\"",sep=""));
        }
        if(alk@ldaf@what=="f") {
            ax(paste("The data base is given by the function of name \"",alk@ldaf@valu,"\"",sep=""));
        }
    }}
    #
    if (qui == "lwin") {
        if (length(alk@lwin) == 0) { ax("No selection since no parents");
        } else {
            ax("The selection of a subset is governed by:");
            print(alk@lwin);
        }
    }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8alkuse <- function(alk,des=0,retrait=4)
#TITLE  prints an alk object not supposed to be completed
#           for a bn
#DESCRIPTION (ba)
#   This function prints in a interpreted way an alk object
# supposed not to be completed. Must not be called by the user
# who might use directly the \code{print} associated to \code{/alk/}.
#DETAILS The interpretation is hand made for each type of links.
#KEYWORDS print
#PKEYWORDS link bn
#INPUTS
#{alk} <<The alk object.>>
#[INPUTS]
#{des} << indicates if the description slot must be
#        printed. O: no; 1: in a short way; 2: completely.
#        Other values means nothing is printed.>>
#{retrait} << number of spaces for indentation.>>
#VALUE
# nothing but a print is performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_11_06
#REVISED 09_10_08
#--------------------------------------------
{
# some checking
che <- valid8alk(alk);
if (!identical(che,TRUE)) {
    erreur(che,"This /alk/ is not valid");
}
#
if (alk@lcomp) {
    erreur("print8alkuse","this alk is declared as completed!");
}
ax <- function(x) { form3titre(x,0,retrait);}
lt <- alk@ltype;
ld <- alk@lrep;
if (isvide(ld)) { ld <- 0;} # standard guess
nav <- alk@lvar;
ld <- length(nav);
pod <- alk@lpod;
red <- alk@lred; if (length(red) == 0) { red <- pod;}
cod <- alk@lcod; if (length(cod) == 0) { cod <- red;}
if (des == 0) { print(alk@ldes,"n");}
if (des == 1) { print(alk@ldes,"ndo");}
if (des == 2) { print(alk@ldes,"ndotrc");}
ax(paste("link type is",lt));
quoi <- names(rbsb.l_a[lt,rbsb.l_a[lt,]!="-"]);
for (qui in quoi) {
    if (!isvide(alk@lpara)) {
    if (qui == "lpara") {
        nbp <- length(alk@lpara);
        ax(paste("distribution defined with",nbp,
                         "parameter(s)"));
        if (lt %in% c("numcat")) {
            ax("See the probability table below");
        } else {
            for (ip in sj(nbp)) {
                ax(paste(names(alk@lpara[ip]),"=",
                         paste(alk@lpara[[ip]],collapse=" ; ")));
            }
        }
    }}
    if (!isvide(alk@lvar)) {
    if (qui == "lvar") {
        if (ld == 0) {
            ax(paste("possible       values:",paste(pod[[1]],collapse="; ")));
            ax(paste("representation values:",paste(red[[1]],collapse="; ")));
            ax(paste("common         values:",paste(cod[[1]],collapse="; ")));
        }
        if (ld > 0) {
            ax(paste("variables names are:"));
            form3titre(paste(nav,collapse=" ; "),0,9);
            for (va in 1:length(nav)) {
                nnav <- nav[va];
                ax(paste(nnav,": possible       values:",paste(pod[[va]],collapse="; ")));
                ax(paste(nnav,": representation values:",paste(red[[va]],collapse="; ")));
                ax(paste(nnav,": common         values:",paste(cod[[va]],collapse="; ")));
            }
        }
        if (ld < 0) {
            ax(paste("variables names are 1 till",-ld));
            for (va in 1:(-ld)) {
                ax(paste(va,": possible       values:",paste(pod[[va]],collapse="; ")));
                ax(paste(va,": representation values:",paste(red[[va]],collapse="; ")));
                ax(paste(va,": common         values:",paste(cod[[va]],collapse="; ")));
            }
        }
    }}
    if (!isvide(alk@ltransfo)) {
    if (qui == "ltransfo") {
        if ((length(alk@ltransfo) == 0)||(alk@ltransfo == "")) {
            ax("No transformation");
        } else {
            qq <- easyp3cut(alk@ltransfo,rbsb.cpt);
            if ( (length(qq$typ) == 1) & all(qq$typ == 4) ) {
                ax(paste("rounding with",qq$blo,"decimal(s)"));
            } else { ax(paste("transforming with an easyp =",alk@ltransfo));}
        }
    }}
    #
    if (!isvide(alk@lfunct)) {
    if (qui == "lfunct") {
        ax("The simulating function to use is:");
        print(alk@lfunct);
    }}
    #
    if (!isvide(alk@ldaf)) {
    if (qui == "ldaf") {
        if(alk@ldaf@what=="t") {
            ax(paste("The data base is stored into file \"",alk@ldaf@valu,"\"",sep=""));
        }
        if(alk@ldaf@what=="d") {
            ax(paste("The data base is given by the variable of name \"",alk@ldaf@valu,"\"",sep=""));
        }
        if(alk@ldaf@what=="f") {
            ax(paste("The data base is given by the function of name \"",alk@ldaf@valu,"\"",sep=""));
        }
    }}
    #
    if (!isvide(alk@lwin)) {
    if (qui == "lwin") {
        if (length(alk@lwin) == 0) { ax("No selection since no parents");
        } else {
            ax("The selection of a subset is governed by:");
            print(alk@lwin);
        }
    }}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8alk <- function(x,...,retrait=4,proba=FALSE)
#TITLE  prints the node-variable names
#DESCRIPTION (ba)
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{retrait} << number of spaces for indentation.>>
#{proba} << Must the probability table be printed?
#          (only for numcat nodes)>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(new("alk",ldes=new("des",name="AA"),
#                         ltype="normal",
#                         lpara=list(mu=0,sigma=1),
#                         lrep=0,
#                         lnat="conti",
#                         lvar="",lparent=character(0),
#                         lpod=list(c(-3,3)),
#                         ltransfo=character(0),
#                         ldaf=rbsb.daf0,
#                         lwin=rbsb.win0,
#                         lcomp=FALSE));
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 08_12_25
#REVISED 09_10_04
#--------------------------------------------
{
# some checks
che <- valid8alk(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /alk/ is not valid");
}
# printing
if (x@lcomp) { print8alkcomp(x,retrait=retrait);
} else { print8alkuse(x,retrait=retrait);}
if (proba) { if (x@ltype == "numcat") {
    form3titre("Probability table without interpretation nor normalization",0);
    print(x@lpara$p);
}}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "alk"), print8alk);



###########################################
###########################################
########
#((((((( NEW S4 CLASS pgr
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8pgr <- function(object)
#TITLE  checks a /pgr/
#DESCRIPTION (ba)
#   This function checks /pgr/ objects
#DETAILS
# It is the validity method for /pgr/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The pgr object to be validated.>>
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
#CREATED 09_10_04
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="pgr") {
        erreur(NULL,paste("This object is not of class 'pgr' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    #
    if (length(object@unitscale) != 1) { res <- c(res,"Bad unitscale");}
    if (length(object@cexscale) != 1) { res <- c(res,"Bad cexscale");}
    if (length(object@arrowlength) != 1) { res <- c(res,"Bad arrowlength");}
    if (length(object@sscale) != 1) { res <- c(res,"Bad sscale");}
    if (length(object@diar) != 1) { res <- c(res,"Bad diar");}
    #
    if (length(res)== 0) { res <- TRUE;
    } else {
        res <- c(res,"Incorrect definition of object",class(object));
        erreur(res,w=rbsb.mwa);
    }
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("pgr",representation(
    unitscale="numeric",    #
    padding="numeric",      # to preserve some margins around
                            # extreme values (1 = no margin, 1.1 = 10% of margins)
    cexscale="numeric",     # size for the characters on the plot
    arrowlength="numeric",  # size of the arrow heads
    sscale="numeric",       #
    kzoom="numeric",        # the changing in zoom for magnifying
    diar="numeric"),        # distance of arrow ends to nodes
               prototype(
    unitscale=20,
    padding=1.1,
    cexscale=1,
    arrowlength=0.12,
    sscale=1,
    kzoom=1.2,
    diar=0.05),
               validity=valid8pgr
);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pgr <- function(x,...,what="v",type=0)
#TITLE  prints the node-variable names
#DESCRIPTION (ba)
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} << Further arguments to be passed to the print function.>>
#{what} << what to print ('v'=node-variable names;
#         'n' only node names)
#{type} << type of printing (0) a node a line;
#              (1) everything on the same line.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(new("pgr",
#    unitscale=20,
#    padding=1.1,
#    cexscale=1,
#    arrowlength=0.12,
#    sscale=1,
#    kzoom=1.2,
#    diar=0.05));
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_08
#REVISED 09_04_08
#--------------------------------------------
{
# some checks
che <- valid8pgr(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /pgr/ is not valid");
}
# printing
cat("Printing a /pgr/ object:\n");
cat("   unitscale=",x@unitscale,"\n");
cat("   padding=",x@padding,"\n");
cat("   cexscale=",x@cexscale,"\n");
cat("   arrowlength=",x@arrowlength,"\n");
cat("   sscale=",x@sscale,"\n");
cat("   kzoom=",x@kzoom,"\n");
cat("   diar=",x@diar,"\n");
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "pgr"), print8pgr);



###########################################
###########################################
########
#((((((( NEW S4 CLASS pos
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8pos <- function(object)
#TITLE  checks a /pos/
#DESCRIPTION (ba)
#   This function checks /pos/ objects
#DETAILS
# It is the validity method for /pos/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The pos object to be validated.>>
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
#CREATED 09_10_04
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="pos") {
        erreur(NULL,paste("This object is not of class 'pos' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    #
    nbn <- nrow(object@posi);
    if (nbn > 0) {
        if (ncol(object@posi) != 4) {
            res <- c(res,paste(dim(object@posi),sep="/","@posi must have FOUR columns!"));
        }
        if (!is.numeric(object@view) | (length(object@view) != 2)) {
            res <- c(res,"@view must be numeric(2)!");
        }
        if (!is.numeric(object@zoom) | (length(object@zoom) != 4)) {
            res <- c(res,"@zoom must be numeric(4)!");
        }
        if (object@zoom[4] <= 0) {
            res <- c(res,"Magnifying coefficient of @zomm cannot be negative or nought!");
        }
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("pos",representation(
    posi="matrix",        # 3D coordinates of each node for plotting + 
                          # decoration. That is a (nbn,4) matrix.
    view="numeric",       # the two angles for the projection into R^2
                          # (0,0) means projection into (X,Y)
    zoom="numeric"),      # four values (center point after normalization on [-1,+1] instead
                          # of [min,MAX] and the the magnifying factor). (0,0,0,1) implies
                          # the standard fitted view.
               prototype(posi=matrix(NA,0,4),
                                    view=c(0,0),
                                    zoom=c(0,0,0,1)),
               validity=valid8pos
);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pos <- function(x,...)
#TITLE  prints a /pos/
#DESCRIPTION (ba)
# just defining the print.pos function
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<the pos object to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# uu <- new("pos",posi=matrix(round(10*cos(1:20)),5));
# print8pos(uu);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_10_04
#REVISED 09_10_04
#--------------------------------------------
{
# some checks
che <- valid8pos(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /pos/ is not valid");
}
# printing
cat("Printing a /pos/ object:\n");
cat("   view=",x@view,"\n");
cat("   zoom=",x@zoom,"\n");
cat("   posi= (a matrix with 3D plus decoration code) \n");
print(x@posi);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "pos"), print8pos);



###########################################
###########################################
########
#((((((( NEW S4 CLASS pta
########
###########################################
###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8pta <- function(object)
#TITLE  checks a /pta/
#DESCRIPTION (ba)
#   This function checks /pta/ objects
#DETAILS
# It is the validity method for /pta/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The pta object to be validated.>>
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
#CREATED 09_10_04
#REVISED 10_06_23
#--------------------------------------------
{
    if (class(object)!="pta") {
        erreur(NULL,paste("This object is not of class 'pta' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    #
    if (!("kkk" %in% slotNames(object))) {
        res <- c(res,"This supposed /pta/ has no slot @kkk!");
        return(res);
    }
    if (length(object@vac)+length(object@vam)!=length(dim(object@pro))) {
        cat("MARGINAL:   ",object@vam,"\n");
        cat("CONDITIONAL:",object@vac,"\n");
        cat("DIMENSION:  ",dim(object@pro));
        res <- c(res,paste(object@vam,object@vac,dim(object@pro),"nodes and probability dimensions does not fit[1]!"));
    }
    if (!setequal(union(object@vac,object@vam),names(dimnames(object@pro)))) {
        cat("MARGINAL:   ",object@vam,"\n");
        cat("CONDITIONAL:",object@vac,"\n");
        cat("DIMENSION:  ",names(dimnames(object@pro)),"\n");
        res <- c(res,paste(names(dimnames(object@pro)),"nodes and probability names does not fit[2]!"));
    }
    if ((length(intersect(object@vam,object@vac))!=0) |
        (length(intersect(object@vac,object@vad))!=0) |
        (length(intersect(object@vad,object@vam))!=0)) {
        cat("MARGINAL:   ",object@vam,"\n");
        cat("CONDITIONAL:",object@vac,"\n");
        cat("CONDITIONED:",object@vad,"\n");
        res <- c(res,paste("marginal,conditional and conditioned nodes must be distinct!"));
    }
    if (length(object@vad)!=length(object@vav)) {
        cat("CONDITIONED:",object@vad,"\n");
        cat("VALUES     :",object@vav,"\n");
        res <- c(res,paste("conditioned nodes and proposed values does not fit!"));
    }
    if (any(object@pro < 0)) {
        res <- c(res,paste(object@pro,"some of supposed probabilities are negative!"));
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
# probability table
setClass("pta", representation(
    name="character",    # title or name
    vam="character",     # variable names of the Marginal dimensions
    vac="character",     # variable names of the Conditional dimensions
    vad="character",     # variable names of the conditioneD dimensions
    vav="character",     # variable values of the conditioned dimensions
    kkk="numeric",       # integer used to print the probabilities 
                         #  (see the details section in print8pta)
    pro="array"          # array of the probabilities (can be proportional)
                         ),
               prototype(name="Null pta",
                         vam="A",vac=character(0),
                         vad=character(0),vav=character(0),kkk=2,
                         pro=array(1:3,dim=3,dimnames=list(A=c("a","b","c")))
                        ),
               validity=valid8pta
);


###########################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pta <- function(x,...,comment=11)
#TITLE  prints the node-variable names
#DESCRIPTION (ba)
# prints the node-variable names of x (a 'nom' object)
#DETAILS
# First probabilities are normalized to sum 1 over all marginale
#       (this is to be consistent with the multinomial 
#        parameterizations in most R functions.)
# Then probabilities are printed using the scaling slot @kkk. 
# More precisely, if kkk > 0 then round(p*10^kkk,0) is printed
#                 if kkk = 0 then round(p,3) is printed
#                 if kkk < 0 then round(p,-kkk) is printed
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{comment} << Numeric scalar defining the way to print
#             0: just the probability table as an array
#             1: like 0 plus indications about marginal,
#                conditional and conditioned dimensions.
#             10: just the probability table as data frame
#                 (each line associated to the second 
#                  dimension of the probability array)
#             11: like 10 plus indications about marginal,
#                conditional and conditioned dimensions.
#          >>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(new("pta",name="Null pta",
#         vam="A",vac=character(0),
#         vad=character(0),vav=character(0),
#         kkk=2,
#         pro=array(1:3,dim=3,dimnames=list(A=c("a","b","c")))
#          ));
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 08_07_29
#REVISED 09_10_04
#--------------------------------------------
{
# some checks
che <- valid8pta(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /pta/ is not valid");
}
# printing
# normalizing the probability
pt <- normalize4pta(x);
# some useful constant
didi <- dim(pt@pro);
nana <- dimnames(pt@pro);
nono <- names(nana);
dima <-  as.vector(outer(pt@vam,nono,"==") %*% 
                   matrix(1:length(didi),ncol=1));
dico <-  as.vector(outer(pt@vac,nono,"==") %*% 
                   matrix(1:length(didi),ncol=1));
# possibly printing the headings
if ((comment %% 10) != 0) {
    if (!isvide(pt@name)) {
        cat("pt object:",pt@name,"\n");
    }
    if (length(pt@vam)>0) {
        cat("Marginal    dimensions are: ");
        for (vv in sjl(pt@vam)) {
            cat(pt@vam[vv],"(",didi[dima[vv]]," categories) ",sep="");
        }
        cat("\n");
    } else { cat("NO Marginal dimension\n");}
    if (length(pt@vac)>0) {
        cat("Conditional dimensions are: ");
        for (vv in sjl(pt@vac)) {
            cat(pt@vac[vv],"(",didi[dico[vv]]," categories) ",sep="");
        }
        cat("\n");
    } else { cat("NO Conditional dimension\n");}
    if (length(pt@vad)>0) {
        cat("Conditioned dimensions are:\n");
        for (vv in sjl(pt@vad)) {
            cat("     ",pt@vad[vv]," = ",pt@vav[vv],"\n");
        }
    } else { cat("NO Conditioned dimension\n");}
    if (pt@kkk > 0) {
        cat("probabilities are multiplied by 10^",pt@kkk,"\n",sep="");
    }
}
# printing the proba
if (length(pt@vam) == 0) {
    return();
} else {
    if (comment < 5) {
	ppe <- as.vector(outer(c(pt@vam,pt@vac),
			       nono,
			       "==") %*% matrix(1:length(didi),ncol=1)
			);
        print(aperm(pt@pro),ppe);
    } else {
	ppf <- as.vector(outer(c(pt@vac,pt@vam),
			       nono,
			       "==") %*% matrix(1:length(didi),ncol=1)
			);
        # building the necessary data frame
        nli <- prod(didi[dico]);
        nco <- length(dico) + 1 +prod(didi[dima]);
        ddff <- matrix(":",nli,nco);
        # filling the conditional description
        for (uu in sjl(dico)) {
            if (uu > 1) {
                avant <- prod(didi[dico[1:(uu-1)]]);
            } else { avant <- 1;}
            if (uu < length(dico)) {
                apres <- prod(didi[dico[(uu+1):length(dico)]]);
            } else { apres <- 1;}
            ddff[,uu] <- rep(rep(nana[[dico[[uu]]]],each=avant),apres);
        }
        # filling the probabilities
        ddff[,(length(dico)+2):nco] <- aperm(pt@pro,ppf);
        # adding the identification of the columns
        if (length(dico) > 0) {
            uu <- c(pt@vac,rep("---",nco-length(dico)));
            ddff <- rbind(uu,ddff);
        }
        for (uu in sjl(dima)) {
            if (uu > 1) {
                avant <- prod(didi[dima[1:(uu-1)]]);
            } else { avant <- 1;}
            if (uu < length(dima)) {
                apres <- prod(didi[dima[(uu+1):length(dima)]]);
            } else { apres <- 1;}
            vv <- c(rep("",length(dico)),
                    paste(nono[dima[[uu]]],":",sep=""),
                    rep(rep(nana[[dima[[uu]]]],each=avant),apres));
            ddff <- rbind(vv,ddff);
        }
        # at last printing
        dimnames(ddff) <- list(NULL,NULL);
        print(as.data.frame(ddff,quote=TRUE));
    }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("print",signature(x = "pta"), print8pta);

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
x2nom <- function(x)
#TITLE  returns the "nom" slot of an object
#DESCRIPTION (ba)
# when 'x' is a 'nom' object returns it. Else
# checks if \code{x@nom} exists and returns it, if not
# a fatal error is issued.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{x} <<the bn/gn/dn/gn/.. or nom object>>
#[INPUTS]
#VALUE
# a 'nom' object
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# identical(rbsb.nom0,x2nom(rbsb.nom0));
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_20
#REVISED 09_09_29
#--------------------------------------------
{
if (!is(x,"nom")) {
    if (!("nom" %in% slotNames(x))) {
        str(x);
        erreur(slotNames(x),"'x' is not a 'nom' object or does not have got such a slot!");
    }
    x <- x@nom;
}
# returning
x;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbnv <- function(x,what=-1)
#TITLE  number of nodes/variables for an bn/gn/dn/nom object
#DESCRIPTION (ba)
# According to 'what', returns the number of 
# nodes/variables for an bn/gn/dn/nom object
#DETAILS
# no check is performed.
#KEYWORDS misc
#PKEYWORDS node nb gn dn nom
#INPUTS
#{x} <<the bn/gn/dn/gn or nom object>>
#[INPUTS]
#{what} << 
#  -1: returns the number of nodes
#   0: returns the total number of variables
#   i: returns the number of variables of the i-th node.
# (For convenience 'n' is translated as -1 and 'v' is
# translated as 0).>>
#VALUE
# The number of nodes or variables, accordingly to what
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nbnv(rbsb.nom2); # number of nodes
# nbnv(rbsb.nom2,0); # number of variates  
#COMMENT
# Remember that in most cases the number of
# variables of a dn associated to a bn is one
# more due to the scoring ">?<" variable.\cr
# This function is a fusion of former nbnd and
# nbva functions.
#AUTHOR J.-B. Denis
#CREATED 07_06_13
#REVISED 09_11_25
#--------------------------------------------
{
#
# checking
if (rbsb.mck) {
    check4tyle(what,c("integer","character"),1,"Bad 'what' in 'nbnv'!");
}
x <- x2nom(x);
if (what=="n") { what <- -1;}
if (what=="v") { what <-  0;}
if (!is.numeric(what)) { erreur(what,"'what' is not acceptable!");}
if (what==-1) {
    res <- length(names(x@x));
} else {
    nvs <- sapply(x@x,length);
    if (length(nvs)==0) {nvs <- numeric(0);}
    if (what==0) {
        res <- sum(nvs);
    } else {
        if ((what < -1)|(what > length(nvs))) {
            erreur(list(x,what),"'what' is not a node number of the nom 'x'");
        } else {
            res <- nvs[what];
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2ion <- function(x,nom,kwhat="n",check=TRUE)
#TITLE  returns the /ion/ of a series of
# nodes/variables
#DESCRIPTION (ba)
# This is a central, basic and very general function for
# programming with /rebastaba/, so not 
# that easy to grasp. It is suggested to read the detail section.\cr
# From a series of nodes (or variables) indicated with 'x'
# returns their complete name/index descriptions under a 'ion' object.
# Checking can be desactivated, anyway it is of course conditional 
# to \code{rbsb.mck}...\cr
# Be aware that the proposed order is not respected, nodes/variables are
# sorted!
#DETAILS
# There are different ways to designate a subset of nodes/variables belonging
# to a /nom/ object. Let A[a], A[b], A[c], B, C[1], C[2] be the set of 
# variables of /nom/ \code{uu} given in the example section. Intuitively, we
# coud designate the first node in different ways: (1) as the first node, 
# (2) as the node of name 'A', (3) as the first three variables of \code{uu},
# (4) as the subset of variables 'A[a]', 'A[b]' and 'A[c]'... \code{nv2ion}
# using one of these ways (and more) returns equivalent ways of defining them.\cr
# Said in a different manner, it gives the simultaneous translation
# of one way in every known ways. This could be quite useful for exchanges
# between user and algorithms. Another properties is to give the subsets 
# in a unique way.\cr
# In fact \code{"-"} is \code{rbsb.all}.
#KEYWORDS misc
#PKEYWORDS node variable
#INPUTS
#{x} << indicates one or several subsets of nodes/variables of the second 
# argument (\code{nom}). When they are several subsets, \code{nv2ion} 
# deals with the union of them. The indication can
# be made (1) by names (\code{character}
# interpreted of nodes or variates according to the third argument \code{kwhat};
# (ii) a numeric matrix of two rows giving its columns the [node number, variable number];
# (iii) a numeric giving the index of nodes or variables according
# to the third argument \code{kwhat}.\cr
# See the description section for more insights.\cr
# Additional facilities are given by the extensions of '-'
# (when \code{x} is a character), 0 and -1 (when \code{x} is numeric).
# \code{"-"}, \code{matrix(c(0,-1),2)} and \code{0} are equivalent. 
# Notice that \code{matrix(c(0,0),2)} will keep on the developped set of
# variable names (not using '-').
# >>
#{nom}    <<nom object of reference.>>
#[INPUTS]
#{kwhat} << This argument is used in two different ways
#                 according its values. When 'x' is a single
#                 numeric it indicates if the user wants to specify
#                 a node ("n"/"N") or a variable ("v"). But when it is
#                 a node (either numeric or character), it indicates
#                 if the node is wanted ("n") of the set of variables
#                 of this node ("N").
#{check} <<( when TRUE checking of the argument consistence
#          is performed if rbsb.mck is also TRUE.>>
#VALUE
# A 'ion' object comprising names, indices and identifications.\cr
#{@nn} <<The node names.>>
#{@vn} <<The variable names.>>
#{@nvn}<<The node[variable] names.>>
#{@ij} <<The variable indices within each node.>>
#{@nk} <<The indices at the node level.>>
#{@vk} <<The indices at the variable level.>>
#{@iden} <<The identification of the \code{x} inputs by
# a character vector of the same length and containing
# either 'nn' (-> node as node), or 'nv' (-> node as variable set),
# or 'v' (-> variable level).>>
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nv2ion(1,rbsb.nom2,"n");
# nv2ion(1,rbsb.nom2,"N");
# nv2ion(1,rbsb.nom2,"v");
# nv2ion(0,rbsb.nom2,"n");
# nv2ion("-",rbsb.nom2,"n");
# nv2ion("A",rbsb.nom2,"n");
# nv2ion("B",rbsb.nom2,"n");
# nv2ion("C",rbsb.nom3,"n");
#REFERENCE
#SEE ALSO Before using \code{nv2ion}, it is suggested to run the script \code{rsba.demo.ion.r}.
#CALLING
# uniqueness is attempted but redundancy is not avoided...
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_28
#REVISED 10_06_10
#--------------------------------------------
{
#=====================================================
# STEP 1: investigation of the arguments, checking, preparation
#===================================================== 
# some checking and preparation
if (check) {
    if (rbsb.mck) {check4valid(valid8nom(nom));}
    if (!(kwhat %in% c("n","N","v"))) {
        erreur(kwhat,"what must be 'n', 'N' or 'v'");
    }
}
#
# determining the case and the length of input
if (is.character(x)) {
    if (isvide(x)) { x <- character(0); }
    ca <- "cha"; na <- length(x);
} else {
   if (!is.numeric(x)) {
       erreur(x,"'x' must be numeric (or character)!");
   } else {
       if (is.matrix(x)) {
           if (nrow(x)!=2) {
               erreur(x,"When 'x' is a matrix, TWO rows are expected!");
           } else {
               ca <- "mat"; na <- ncol(x);
           }
       } else {
           ca <- "vec"; na <- length(x);
       }
   }
}
#
# na is the number of asked subsets
# cha is the type under which the subsets are defined
#     *vec*tor, *mat*rix, *cha*racter.
#
# preparing the result as a list
iden <- character(na);
maij <- matrix(NA,2,0);
if (na==0) { return(rbsb.ion0);}
#
lll <- c(0,cumsum(sapply(nom@x,length)));
names(lll) <- NULL;
#=====================================================
# STEP 2: processing each subset in a jbd loop
#===================================================== 
for (jbd in sj(na)) {
    # the ij notation is used as a common standard
    # that is all coding are first translated under the
    # c(i,j) format of the matrix columns, 'i' being the 
    # node number and 'j' the variable number.
    #=====================================================
    # STEP 2.A: the subset is translated into c(n,v) code
    #===================================================== 
    if (ca=="cha") {
        # under character specification
        xx <- x[jbd];
        if (xx==rbsb.all) {
            # the shortcut
            xxx <- c(0,-1);
	    # but this can be modified at the variable level
	    # with the use of 'kwhat'
            if (kwhat=="N") { xxx <- c(0,0);}
        } else {
	    # standard specification
	    nn <- nv2nv(xx);
	    no <- which(nn$nod==names(nom@x));
	    if (length(no)==0) {
                form3affiche(x);
		erreur(list(nom,nn),"Not accepted as node name in 'nom'");
	    }
	    if (xx==nn$nod) {
		# at the node level
		xxx <- c(no,-1);
		# but this can be modified at the variable level
		# with the use of 'kwhat'
		if (kwhat=="N") { xxx <- c(no,0);}
	    } else {
		# at the variable level
		if (no==0) {
		    xxx <- c(0,0);
		} else {
		   if (nn$var==rbsb.all) {
		       xxx <- c(no,0);
		   } else {
		       nv <- which(nn$var==nom@x[[nn$nod]]);
		       if (check) { if (length(nv)==0) {
			   erreur(list(nom,nn),"The variable name was not found");
		       }}
		       xxx <- c(no,nv);
		   }
		}
	    }
	}
    }
    if (ca=="mat") {
        # under matrix column specification
        xx <- x[,jbd];
        xxx <- xx;
    }
    if (ca=="vec") {
        # under scalar specification
        xx <- x[jbd];
        if (kwhat=="v") {
            # at the variable level
            if (xx==0) {
                xxx <- c(0,0);
            } else {
                kn <- sum(lll < xx);
                if (kn == 1) {
                    xxx <- c(1,xx);
                } else {
                    xxx <- c(kn,xx-lll[kn]);
                }
            }
        } else {
            # at the node level
            if (kwhat=="n") {
                # at the stric node level
                xxx <- c(xx,-1);
            } else {
                xxx <- c(xx,0);
            }
        }
    }
    #=====================================================
    # STEP 2.B: checking the obtained code
    #===================================================== 
    if (xxx[1] < -1) {
        erreur(x,"Non acceptable negative node number was found");
    }
    if (xxx[1] > length(nom@x)) {
        erreur(x,"Too high node number was found");
    }
    if (xxx[2] < -1) {
        erreur(x,"Non acceptable negative variable number was found");
    }
    if (xxx[1]>0) { if (xxx[2] > length(nom@x[[xxx[1]]])) {
        erreur(x,"Too high variable number was found");
    }}
    #=====================================================
    # STEP 2.C: performing the identification
    #===================================================== 
    if (xxx[2]==-1) { iden[jbd] <- "nn";}
    if (xxx[2]== 0) { iden[jbd] <- "nv";}
    if (xxx[2] > 0) { iden[jbd] <- "v";}
    if (iden[jbd]=="") {
        rapport("Bad identification in 'nv2ion'");
    }
    #=====================================================
    # STEP 2.D: expanding the different codes
    #===================================================== 
    if (xxx[1] == 0) {
        # nodes are repeated
        if (xxx[2] == 0) {
            # all variables
            xxx <- matrix(NA,2,0);
            for (ii in sjl(nom@x)) {
                lon <- length(nom@x[[ii]]);
                xxx <- cbind(xxx,matrix(c(rep(ii,lon),sj(lon)),
                                        nrow=2,byrow=TRUE));
            }
        } else {
            # all nodes
            if (xxx[2] == -1) {
                xxx <- matrix(c(1:length(nom@x),rep(0,length(nom@x))),
                              2,byrow=TRUE);
            } else {
                rapport("nv2ion: unexpected variable specification");
            }
        }
    } else {
        # a specific node
        if (xxx[2] == 0) {
            xxx <- matrix(c(rep(xxx[1],length(nom@x[[xxx[1]]])),
                          1:length(nom@x[[xxx[1]]])),
                          2,byrow=TRUE);
        } else {
            xxx <- matrix(xxx,2);
        }
    }
    #=====================================================
    # STEP 2.E: cumulating into the (n,v) way
    #===================================================== 
    #
    maij <- cbind(maij,xxx);
    #
} # ending the loop (jbd in sj(na))
if (ncol(maij)==0) { return(rbsb.ion0);}
#=====================================================
# STEP 3: sorting and elimination from the matrix form
#=====================================================
maj <- 2*(10+max(maij[2,]));
sco <- maij[1,]*maj + maij[2,];
ord <- order(sco);
sco <- sco[ord];
qui <- c(TRUE,sco[-1]-sco[-length(sco)]>0);
maij <- maij[,ord[qui],drop=FALSE];
#=====================================================
# STEP 4: filling all the fields with the results
#===================================================== 
res <- list(nn=character(0),vn=character(0),nvn=character(0),
            ij=matrix(NA,2,0),
            nk=numeric(0),vk=numeric(0),iden=character(0));
res$ij <- maij;
for (sd in sj(ncol(maij))) {
    xxx <- res$ij[,sd];
    res$nn <- c(res$nn,names(nom@x)[xxx[1]]);
    res$nk <- c(res$nk,xxx[1]);
    #
    if (xxx[2]>0) {
        res$vn <- c(res$vn,nom@x[[xxx[1]]][xxx[2]]);
        res$vk <- c(res$vk,lll[xxx[1]]+xxx[2]);
    } else {
        res$vn <- c(res$vn,rbsb.all);
        res$vk <- c(res$vk,0);
    }
    #
    res$nvn <- paste(res$nn,
                     rbsb.cpt["variables","opening"],
                     res$vn,
                     rbsb.cpt["variables","closing"],
                     sep="");
}
# adaptation for unamed variables
res$nvn[res$vn==""] <- res$nn[res$vn==""];
#
# dealing with the special case of no nodes
if (length(res$nn) == 0) {
    res$vn <- res$nvn <- character(0);
    res$ij <- matrix(0,2,0);
    res$nk <- res$vk <- numeric(0);
}
#
# returning
new("ion",nn=res$nn,vn=res$vn,nvn=res$nvn,
          nk=res$nk,ij=res$ij[2,],vk=res$vk,
          iden=iden);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nanv <- function(x,what=-1)
#TITLE  names of nodes/variables for an bn/gn/dn/nom object
#DESCRIPTION (ba)
# According to 'what', returns the names of 
# nodes/variables for an bn/gn/dn/nom object
#DETAILS
# No check is made about \code{x}
#KEYWORDS misc
#PKEYWORDS variable node nb gn dn nom
#INPUTS
#{x} <<the bn/gn/dn/gn or nom object>>
#[INPUTS]
#{what} << 
#  -1: returns the names of the nodes
#   0: returns all names of the variables
#   i: returns the names of the variables of the i-th node.
# (For convenience 'n' is translated as -1 and 'v' is
# translated as 0).>>
#VALUE
# The names of nodes or variables, accordingly to what
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nanv(rbsb.nom2) # node names
# nanv(rbsb.nom2,0) # variable names
#COMMENT
# Remember that in most cases the number of
# variables of a dn associated to a bn is one
# more due to the scoring ">?<" variable.\cr
# This function is consistent with nbnv.
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 09_11_25
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(what,c("integer","character"),1,"Bas 'what' in 'nanv'!");
}
x <- x2nom(x);
if (what=="n") { what <- -1;}
if (what=="v") { what <-  0;}
if (!is.numeric(what)) { erreur(what,"'what' is not acceptable!");}
if (what==-1) {
    res <- names(x@x);
} else {
    if (what==0) {
        res <- character(0);
        for (ii in sjl(x@x)) {
            if (isvide(x@x[[ii]])) {
                res <- c(res,names(x@x)[ii]);
            } else {
                res <- c(res,
                         paste(names(x@x[ii]),
                               rbsb.cpt["variables","opening"],
                               x@x[[ii]],
                               rbsb.cpt["variables","closing"],
                               sep="")
                        );
            }
        }
    } else {
        if ((what < -1)|(what > length(x@x))) {
            erreur(list(x,what),"'what' is not a node number of the nom 'x'");
        } else {
            ii <- what;
            if (isvide(x@x[[ii]])) {
                res <- names(x@x)[ii];
            } else {
                res <- paste(names(x@x[ii]),
                             rbsb.cpt["variables","opening"],
                             x@x[[ii]],
                             rbsb.cpt["variables","closing"],
                             sep="");
            }
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2nom <- function(nova)
#TITLE  transforms a character into a 'nom' object
#DESCRIPTION (ba)
# returns a \code{/nom/} whose names/variables comes from \code{nova}.
# Repetitions are possible and eliminated.
#DETAILS
# nodes and variables/nodes are sorted.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nova} <<character.>>
#[INPUTS]
#VALUE
# a 'nom' object with nodes possibly comprising covariables
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print.default(char2nom(LETTERS[1:4]));
# print(char2nom(c("C[b]","A","B[one]","C[b]","C[a]","B[two]")));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 10_07_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(nova,"character",-1);
}
## preparing
nova <- sort(unique(nova));
nono <- nv2nod(nova);
nova <- nv2var(nova);
## getting the x slot
no <- sort(unique(nono));
# first the node names
xx <- as.list(rep("",length(no)));
names(xx) <- no;
# second the variate names
for (nn in no) {
    ou <- which(nn==nono)
    vv <- nova[ou];
    xx[[nn]] <- sort(vv);
}
res <- new("nom",x=xx);
if (rbsb.mck) {check4valid(valid8nom(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nom2char <- function(nom,of="v",sort=FALSE)
#TITLE  transforms a /nom/ into a character
#DESCRIPTION (ba)
# returns a character of the variables or node from a \code{/nom/}.
#DETAILS
# Notice that \code{char2nom(nom)} is equivalent to \code{nv2ion(0,nom,"v")@nvn}.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be transformed.>>
#[INPUTS]
#{of} <<'v' to return the set of variables; 'n' to return the set 
#       of involved nodes.>>
#{sort} <<Must the nodes and names be sorted?>>
#VALUE
# a \code{character}.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nom2char(rbsb.nom3);
# nom2char(rbsb.nom3,"n");
#REFERENCE
#SEE ALSO nom2char
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_06
#REVISED 10_07_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    if (!(of %in% c("v","n"))) {
        erreur(of,"Must be 'v' for 'variables' or 'n' for 'nodes'");
    }
}
## preparing
if (of == "n") {
    # node names 
    res <- names(nom@x);
} else {
    # variable names
    res <- character(0);
    for (ii in sjl(nom@x)) {
        nn <- names(nom@x)[ii];
        if(isvide(nom@x[[ii]])) {
            res <- c(res,nn);
        } else {
            res <- c(res,paste(nn,rbsb.cpt["variables","opening"],
                                  nom@x[[ii]],
                                  rbsb.cpt["variables","closing"],sep=""));
        }
    }
}
#
if (sort) { res <- sort(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2nom <- function(lili)
#TITLE  transforms a list into a /nom/
#DESCRIPTION (ba)
# simply returns \code{nom} with its unique slot \code{@x}
# being equal to \code{lili}. This
# function was introduced for consistency with other
# objects and to prepare future evolutions
#DETAILS
# According to \code{rbsb.mck} the produced object is checked.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{lili} <<The \code{list} object to be transformed.>>
#[INPUTS]
#VALUE
# The resulting \code{nom}.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# list2nom(rbsb.nom3@x);
#REFERENCE
#SEE ALSO nom2list
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_07
#REVISED 10_07_07
#--------------------------------------------
{
## preparing
res <- new("nom",x=lili);
# checking
if (rbsb.mck) {
    check4valid(valid8nom(res));
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nom2list <- function(nom)
#TITLE  transforms a /nom/ into a list
#DESCRIPTION (ba)
# simply returns \code{nom@x} which is a list. This
# function was introduced for consistency with other
# objects and to prepare future evolutions
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be transformed.>>
#[INPUTS]
#VALUE
# The resulting \code{list}.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nom2list(rbsb.nom3);
#REFERENCE
#SEE ALSO list2nom
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_07
#REVISED 10_07_07
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
}
## preparing
res <- nom@x;
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sort8nom <- function(nom,by="Aa")
#TITLE  sorts a /nom/ according to various ways
#DESCRIPTION (ba)
# Nodes and variables of the \code{nom} are sorted
# according the alphabet, the number of variables.
#DETAILS
# Compatible criteria can be used simultaneously.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be sorted.>>
#[INPUTS]
#{by} <<A \code{character(1)} indicating the kind of sorting
#       by the letters in it. \code{a} for sorting the variables
#       within the node in alphabetical order. \code{A} for the nodes
#       and/or \code{S} with respect to their numbers of variates.>>
#VALUE
# The resulting \code{nom}.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# sort8nom(rbsb.nom3,"S");
# print(sort8nom(rbsb.nom7,"Aa"));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_07
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(by,"character",1,"'by' must be 'character(1)'");
}
## preparing
on <- sjl(nom@x);
if (expr3present("S",by)) { on <- order(sapply(nom@x,length));}
if (expr3present("A",by)) { on <- order(names(nom@x));}
res <- vector("list",length(nom@x));
names(res) <- names(nom@x)[on];
al <- expr3present("a",by);
for (ii in sjl(nom@x)) {
    iii <- names(res)[ii];
    if (!al) { res[[ii]] <- nom@x[[iii]];
    } else { res[[ii]] <- sort(nom@x[[iii]]);}
}
res <- new("nom",x=res);
if (rbsb.mck) { check4valid(valid8nom(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
compare8nom <- function(noma,nomb,level="v")
#TITLE  compares two /nom/s
#DESCRIPTION (ba)
# compares two /nom/s either at the node or 
# at the variable level.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{noma} <<The first \code{nom} to be compared.>>
#{nomb} <<The second \code{nom} to be compared.>>
#[INPUTS]
#{level} <<'v' or 'n' to indicate if the comparaison 
#          must be made at the 'variable' or 'node' level.>>
#VALUE
# A list of three /nom/ named \code{$a_b}, \code{$b_a} and
# \code{$a.b} giving the respectively the set differences and the 
# intersection of the choosen items.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# compare8nom(rbsb.nom2,rbsb.nom3);
# compare8nom(rbsb.nom2,rbsb.nom3,"n");
# compare8nom(rbsb.nom5,rbsb.nom3);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_08
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(noma));
    check4valid(valid8nom(nomb));
    check4tyle(level,"character",1,"'level' must be 'character(1)'");
    if (!(level %in% c("v","n"))) {
        erreur(level,"'level' must be 'v' or 'n'");
    }
}
# taking care of the level
if (level == "n") {
    noma <- nom2nom(noma);
    nomb <- nom2nom(nomb);
}
# making the set operations
itema <- nom2char(noma);
itemb <- nom2char(nomb);
res <- vector("list",0);
res[["a_b"]] <- char2nom(setdiff(itema,itemb));
res[["b_a"]] <- char2nom(setdiff(itemb,itema));
res[["a.b"]] <- char2nom(intersect(itema,itemb));
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nom2nom <- function(nom,quoi="n")
#TITLE  transform a /nom/.
#DESCRIPTION (ba)
# Reduces a /nom/ to its node.
#DETAILS
# For the moment, this is the only possiblility;
# further on more ideas can occur.
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{nom} <<The \code{nom} object to be transformed.>>
#[INPUTS]
#{quoi} <<\code{character(1)} indicating the kind of 
#       transformation. \code{n} for removing all the
#       the possible variables, leaving only the nodes.>>
#VALUE
# The resulting \code{nom}.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(nom2nom(rbsb.nom7));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_07_08
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(quoi,"character",1,"'by' must be 'character(1)'");
}
## preparing
if (expr3present("n",quoi)) {
    res <- lapply(sjl(nom@x),function(x){"";});
    names(res) <- names(nom@x);
    res <- new("nom",x=res);
}
# checking
if (rbsb.mck) { check4valid(valid8nom(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
and4nom <- function(nom,nod,var="")
#TITLE  adds one node to a /nom/
#DESCRIPTION (ba)
# returns \code{nom} after adding it a new node.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom node
#INPUTS
#{nom} <<The /nom/ to be completed.>>
#{nod} <<Name for the new node (character(1)).>>
#[INPUTS]
#{var} <<Name(s) for the variable of the new node.>>
#VALUE
# The /nom/ completed
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.nom2);
# print(and4nom(rbsb.nom2,"D","z"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 09_05_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(nod,"character", 1,"The node name (only one) was expected.");
    check4tyle(var,"character",c(1,Inf),"The variable name(s) was expected [At least one].");
    if (nod %in% nanv(nom,"n")) {
        erreur(list(nom,nod),"The node you proposed already exists.");
    }
    if (length(unique(var))!=length(var)) {
        erreur(var,"You proposed duplicated variable names for the same node.");
    }
}
# addition
nom@x[[nod]] <- var;
# returning
nom;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rmnd4nom <- function(nom,nod)
#TITLE  removes one node to a /nom/
#DESCRIPTION (ba)
# removes one (and only one) node to a \code{/nom/}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom node
#INPUTS
#{nom} <<The /nom/ to be restricted.>>
#{nod} <<Name of the name to be removed (character(1)).>>
#[INPUTS]
#VALUE
# The reducede /nom/
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rmnd4nom(rbsb.nom2,"A"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_06
#REVISED 09_05_06
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(nod,"character", 1,"The node name (only one) was expected.");
    if (!(nod %in% nanv(nom,"n"))) {
        erreur(list(nom,nod),"The node you proposed does not exist in that /nom/.");
    }
}
# removing
qui <- which(names(nom@x)!=nod);
nom@x <- nom@x[qui];
# returning
nom;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv3nom <- function(na,nom)
#TITLE  detects the existence of names into nom
#DESCRIPTION (ba)
# Detects the existence of 'na' as a valid node names or
# a valid variable names included into nom
#DETAILS
#KEYWORDS misc
#PKEYWORDS node
#INPUTS
#{na} <<name vector to be looked for>>
#{nom}    <<nom object to make the correspondence.>>
#[INPUTS]
#VALUE
# a numeric: 0 (doesn't exist), 1 (only as node name),
# 2 (only as variable name) and 3 as variable and node name.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nv3nom(c("C[1]","C[10]","C","B"), rbsb.nom2);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_16
#REVISED 09_04_29
#--------------------------------------------
{
# some checking
if (rbsb.mck) {check4valid(valid8nom(nom));}
if (!is.character(na)) {
    erreur(na,"'na' must be a character");
}
# preparing
res <- numeric(length(na));
names(res) <- na;
if (length(nom)>0) {
    var <- nv2ion(0,nom,"v")@nvn;
    nod <- nv2ion(0,nom,"n")@nn;
    for (ii in sjl(na)) {
        no <- na[ii];
        if (no %in% nod) { res[ii] <- res[ii]+1;}
        if (no %in% var) { res[ii] <- res[ii]+2;}
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dim8nd <- function(type,para,parents,nom,var=rbsb.cha0)
#TITLE  computes the dimension of a node
#DESCRIPTION (ba)
# analyzing the type, parametes and parents of
# a node returns its dimension. When it can
# be computed even when var is not informed,
# if this is, it is then checked.
#DETAILS
#KEYWORDS misc
#PKEYWORDS node
#INPUTS
#{type} <<The node type.>>
#{para} <<The list of the node parameters.>>
#{parents} <<The list of parents (character).>>
#{nom} <<The /nom/, to interpret the parent dimensions.>>
#[INPUTS]
#{var} <<The provided 'var' list
#        which is compulsory for some node types.>>
#VALUE
# The reduced /nom/
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# dim8nd("normal",list(mu="{{A}}",sigma="abs({{B}})"),rbsb.cha0,rbsb.nom2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_06_16
#REVISED 09_11_05
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(type,"character",1);
    check4tyle(para,"list",-1);
    check4tyle(parents,"character",-1);
    check4valid(valid8nom(nom));
}
# the algorithm
if (isalkrepeatable(type)) {
    # The node is repeatable
    # The dimension is given by the parameters
    res <- 1;
    for (dd in sjl(para)) {
        dip <- easyps2dim(para[[dd]],nom);
        if (dip != 1) {
            if (res==1) { res <- dip;
            } else {
                if (res!=dip) {
                    erreur(list(res,dip),
                     paste("Different parameters propose different",
		     "dimensions for the node"));
                }
            }
        }
    }
} else {
    # The node is not repeatable
    res <- -1;
    if (type %in% c("parcat","binomial","parcat","Bernoulli",
                    "score")) {
        res <- 1;
    }
    if (type %in% c("empidata","program")) {
        res <- length(var);
    }
    # (only non compulsorily univraite nodes are considerd)
    if (type=="multinomial") { res <- length(para$p); }
    if (type=="Dirichlet")   { res <- length(para$a); }
    # this type is to be made!
    if (res == -1) {
        rapport(paste("This type node (",type,") is not yet considered by /rebastaba/"));
    }
}
# possibly checking with var
if (!isvide(var)) {
    if (res!=length(var)) {
        form3affiche(nom);
        form3affiche(list(type,parents,para));
        erreur(list(var,res),paste("dim8nd found a number of variables",
               "different from the list provided by the user"));
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sort8ion <- function(ion,nom,sort="n",rm.redun=TRUE)
#TITLE  sorts a 'ion' object
#DESCRIPTION (ba) sorts a 'ion' object possibly eliminating
# the redundancies
#DETAILS
# The algorithm does note take care of \code{rbsb.who}, changing
# it implies changing this algorithm...
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{ion}<<The 'ion' to be sorted.>>
#{nom}<<associated 'nom' structure to perform the sorting.>>
#[INPUTS]
#{sort} << the way to make the sorting
#               'n': according to 'nom'
#               'a'; according to the alphabet.>>
#{rm.redun} << Must the redundancies be removed?>>
#VALUE
# The sorted [reduced] 'ion'
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# uu <- rbsb.nom2;
# vv <- nv2ion(0,uu);
# print(sort8ion(vv,uu));
# print(sort8ion(vv,uu,"a"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_29
#REVISED 09_04_29
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4valid(valid8ion(ion));
}
# sorting
if (sort=="a") {
    # alphabetical sorting
    oo <- order(ion@nvn);
} else {
    # natural sorting
    oo <- order((nbnv(nom)+1)*ion@nk+ion@vk);
}
for (ii in slotNames(ion)) { if (length(slot(ion,ii))>0) {
    ax <- slot(ion,ii);
    slot(ion,ii) <- ax[oo];
}}
# eliminating the redundancies at the simple level
if (rm.redun) { if (length(ion)>1) {
    dd <- length(ion);
    rr <- c(ion@nvn[-1]!=ion@nvn[-dd],TRUE);
    if (sum(rr)<length(rr)) {
        for (ii in slotNames(ion)) {
            slot(ion,ii) <- slot(ion,ii)[rr];
        }
    }
}}
# eliminating the redundancies at the global level
rr <- numeric(0);
## finding all global nodes (forgetting rbsb.who)
glo <- which("-"==ion@vn);
## finding all nodes
ano <- unique(ion@nn);
## exploring them
for (no in ano) {
    ## for each component of it
    cno <- which(no==ion@nn);
    if (length(cno) > 1) {
        ## are there global nodes for it
        gno <- intersect(glo,cno);
        if (length(gno)>0) {
            ## only one is sufficient
            rr <- c(rr,setdiff(cno,gno[1]));
        }
       
    }
}
## removing the redundant ones
if (length(rr)>0) {
    kk <- setdiff(1:length(ion),rr);
    for (ii in slotNames(ion)) {
        slot(ion,ii) <- slot(ion,ii)[kk];
    }
}
# returning
ion;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2nod <- function(vvn)
#TITLE  transforms complete variable names into node names
#DESCRIPTION (ba)
# No ckeck is made about the existence of the node names
#DETAILS
# the syntax analysis is minimal, looking for a square bracket
# and eliminating evething from that point !
#KEYWORDS misc
#PKEYWORDS node
#INPUTS
#{vvn} <<vector of variable names>>
#[INPUTS]
#VALUE
# vector of deduced node names
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nv2nod("aa");                   # "aa" is returned
# nv2nod(c("a[e]","az[ee]","b")); # c("a","az","b") is returned
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# This function has been added to get the parents of an alk
# whithout reference to a gn/bn.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_15
#REVISED 09_01_15
#--------------------------------------------
{
sep <- rbsb.cpt["variables","opening"];
un <- function(x) {x[1];}
res <- sapply(strsplit(vvn,"[",fixed=TRUE),un);
if (length(res) == 0) { res <- character(0);}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2var <- function(nvn)
#TITLE  returns the variable name from a node[variable] name
#DESCRIPTION (ba)
# Just removing the node name and square brackets.
# nvn can be a vector. In case there is no variable name,
# the standard "" is returned
#DETAILS
# Variable name can be numeric and is returned as such
#KEYWORDS misc
#PKEYWORDS name var
#INPUTS
#{nvn} << character of the complete variable name>>
#[INPUTS]
#VALUE
# variable name without node name
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nv2var(c("A[5]","B"));
#REFERENCE
#SEE ALSO va3va
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_01_03
#REVISED 08_01_03
#--------------------------------------------
{
res <- character(0);
for (i in sjl(nvn)) {
    nv <- nvn[i];
    if (length(grep(rbsb.cpt["variables","opening"],nv,fixed=TRUE)) == 0) {
        # no variable name
        res <- c(res,"");
    } else {
        if ((grep(rbsb.cpt["variables","opening"],nv,fixed=TRUE) != 1) ||
            (grep(rbsb.cpt["variables","closing"],nv,fixed=TRUE) != 1)) {
            cat("you provide (",nv,") as complete node[variable] name\n",sep="");
            erreur(NULL,"BUT it does not comprise the corresponding parenthesis!");
        }
        nv <- strsplit(nv,rbsb.cpt["variables","opening"],fixed=TRUE)[[1]][2];
        nv <- strsplit(nv,rbsb.cpt["variables","closing"],fixed=TRUE)[[1]][1];
        res <- c(res,nv);
    }
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cycle8pam <- function(pam)
#TITLE  detects if a cycle exists from a pam matrix
#DESCRIPTION (ba)
# This function returns TRUE or FALSE if at least one cycle
# is detected within the graph defined by the pam matrix.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS pam cycle
#INPUTS
#{pam} <<The pam matrix>>
#[INPUTS]
#VALUE
# logical variable (TRUE: at least a cycle, FALSE : no cycle).
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# papa <- new("pam",rlt=matrix(c(0,0,1,1,0,0,0,1,0),3));
# cycle8pam(papa);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 07_10_19
#--------------------------------------------
{
if (rbsb.mck) {check4valid(valid8pam(pam));}
# initialization
nbn <- nrow(pam@rlt);
res <- numeric(0);
nbfait <- -1; 
# starting the stupid loop
while (nbfait < length(res)) {
    nbfait <- length(res);
    for (jbd in sj(nbn)) { 
        papa <- which(pam@rlt[,jbd]==1);
        if (length(unique(c(jbd,papa,res))) == (1+length(res))) {
            res <- c(res,jbd);
        }
    }
}
if ((length(res)) != nbn) { res <- TRUE;
} else { res <- FALSE;}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
explore8pam <- function(pam,iir=sj(nrow(pam@rlt)),jjc=iir)
#TITLE  finds different characteristics of relations
#           between nodes
#DESCRIPTION (ba)
# exploring the parentship matrix 'pam', returns a list
# of different nodesxnodes matrices giving standard
# characteristics of the relationship between them.\cr
# The calculation is done with iin order
# and the result is proposed in iin order.
# The relation are:
#  (i) the indication of the relation (-1=row is descendant,
#      0= none, 1= row is ascendant),
#  (ii) the minimum number of arcs to join row and column
#      (0 means no)
#  (iii) the maximum number of arcs to join row and column
#      (0 means no)
#  (iv) the number of different paths joining row and column
#DETAILS
#KEYWORDS utilities
#PKEYWORDS bn gn
#INPUTS
#{pam} <<The pam object>>
#[INPUTS]
#{iir} << the subset of nodes to be considered
#        as starting nodes. They will be en rows of the
#        resulting matrices.>>
#{jjc} << the subset of nodes to be considered as ending
#        nodes. They will be in columns of the resulting matrices.>>
#VALUE
# a list gathering two lists
#  $des: a list of descriptions explaining what is
#        computed in the differents matrices
#  $rel: the list of the computed matrices
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# explore8pam(rbsb.pam1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# a former version of this function was called 'genea'
#FUTURE
# It could be more logical (and efficient when the number
# of nodes is important) that the matrices be replaced 
# with couples of nodes...
#AUTHOR J.-B. Denis
#CREATED 07_06_17
#REVISED 09_05_10
#--------------------------------------------
{
# checking
if (rbsb.mck) {check4valid(valid8pam(pam));}
pama <- pam@rlt; nbn <- nrow(pama);
if (!all(iir %in% 1:nbn)) {
    erreur(iir,"is not acceptable for dim(pama) =",dim(pama));
}
if (!all(jjc %in% 1:nbn)) {
    erreur(jjc,"is not acceptable for dim(pama) =",dim(pama));
}
if (length(unique(iir)) < length(iir)) {
    erreur(iir,"repetitions are not accepted!");
}
if (length(unique(jjc)) < length(iir)) {
    erreur(jjc,"repetitions are not accepted!");
}
# defining the objects
d1 <- new("des",orig="",time="",role="",
      name="relation",
      defi=paste("0 when no relation,",
                 "1 when the row is an ascendant of the column,",
                 "-1 when the row is a descendant of the column."));
d2 <- new("des",orig="",time="",role="",
      name="minimum",
      defi=paste("0 when no relation,",
                 "otherwise the minimum number of arcs between",
                 "the two nodes."));
d3 <- new("des",orig="",time="",role="",
      name="maximum",
      defi=paste("0 when no relation,",
                 "otherwise the maximum number of arcs between",
                 "the two nodes."));
d4 <- new("des",orig="",time="",role="",
      name="number",
      defi=paste("0 when no relation,",
                 "otherwise the number of different paths",
                 "following the arcs to go from one node",
                 "to the other."));
# finding the relationship matrices
pama <- pam@rlt;
pamb <- t(pama);
m1 <- pama - pamb;
m2 <- m3 <- m4 <- pama + pamb;
ka <- pama; kb <- pamb;
for (ii in (1+sj(nbn-2))) {
    ka <- ka %*% pama; kb <- kb %*% pamb;
    m1 <- m1 + ka - kb;
    m2[(m2==0)&((ka>0)|(kb>0))] <- ii;
    m3[        ((ka>0)|(kb>0))] <- ii;
    m4 <- m4 + ka + kb;
}
nana <- list(iir,jjc);
m1 <- m1[iir,jjc,drop=FALSE]; dimnames(m1) <- nana;
m2 <- m2[iir,jjc,drop=FALSE]; dimnames(m2) <- nana;
m3 <- m3[iir,jjc,drop=FALSE]; dimnames(m3) <- nana;
m4 <- m4[iir,jjc,drop=FALSE]; dimnames(m4) <- nana;

list(des=list(d1,d2,d3,d4),
     rel=list(m1,m2,m3,m4));
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
mindis8pam <- function(pam)
#TITLE  finds the minimum distance between every pair of nodes
#DESCRIPTION (ba)
# From the parentship matrix, the minimum distance matrix between
# every pairs of nodes is computed. [i,j] means from node ith to node
# jth. In most cases (especially for non empty dags) the matrix is
# not symmetric.\cr
# The distance (not a mathematical distance) is the minimal number of 
# arcs to follow before reaching the node j starting from the node i.
#DETAILS
# -1 are introduced on the diagonal.
#KEYWORDS utilities
#PKEYWORDS pam 
#INPUTS
#{pam} <<The pam matrix to be investigated>>
#[INPUTS]
#VALUE
# The matrix containing the distances
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# mindis8pam(rbsb.pam1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# The transposed matrix is also of value
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 07_11_07
#--------------------------------------------
{
if (rbsb.mck) {check4valid(valid8pam(pam));}
nbno <- nrow(pam@rlt); 
mm <- res <- pam@rlt; 
# going on
if (nbno > 2) { for (jbd in 2:(nbno-1)) {
    mm <- mm %*% pam@rlt;
    res[(res==0)&(mm>0)] <- jbd;
}}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pos2pos <- function(pos)
#TITLE  returns the equivalent pos object with standard view
#DESCRIPTION (ba)
# This function returns the equivalent pos object with standard view
# (i.e. view=c(0,0)). The central point of zoom is modified
# accordingly.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS plot pos
#INPUTS
#{pos} <<the pos object to be modified>>
#[INPUTS]
#VALUE
# The transformed pos object
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(pos2pos(rbsb.pos0));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# At the beginning, pos2pos was called projection since it is
# the way to obtain the coordinates (in new (x,y)) for the
# representation.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_25
#REVISED 07_11_12
#--------------------------------------------
{
if (rbsb.mck) {check4valid(valid8pos(pos));}
xyz <- rbind(pos@posi[,1:3],pos@zoom[1:3]);
for (jbd in sj(nrow(xyz))) {
    uu <- xyz[jbd,];
    vv <- geom3xyz2pol(uu);
    vv[2:3] <- vv[2:3] + pos@view;
    xyz[jbd,] <- geom3pol2xyz(vv);
}
if (nrow(xyz) > 1) { pos@posi[,1:3] <- xyz[-nrow(xyz),];}
pos@zoom[1:3] <- xyz[nrow(xyz),];
# returning
pos;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pam2path <- function(pam,d,a)
#TITLE  returns all paths between two nodes
#DESCRIPTION (ba)
# From the parentship matrix computes recursively
# all paths between two nodes.
#DETAILS
# The basic idea is quite simple. All paths 
# between d (departure) and a (arrival) are the 
# paths between every children of d and a, so
# a recursive procedure can be used.
#KEYWORDS utilities
#PKEYWORDS genealogy 
#INPUTS
#{pam} <<pam object>>
#{d} <<departure node (internal number)>>
#{a} <<arrival node (internal number)>>
#[INPUTS]
#VALUE
# a list of vectors describing all possible paths
# going from d to a. The list is sorted according to
# the length of the path.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# pam2path(rbsb.pam1,1,5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_12
#REVISED 07_10_19
#--------------------------------------------
{
if (rbsb.mck) {check4valid(valid8pam(pam));}
# is the final node reached?
if (d == a) {
   res <- list(d);
}
else {
    # initialization
    res <- vector("list",0);
    # looking for the d children
    ee <- which(1==pam@rlt[d,]);
    # is there no child?
    if (length(ee) == 0) {
        res <- -1;
    }
    else {
        # loop over all children from node d
        for (jbd in ee) {
            # computing the paths from this children
            ff <- Recall(pam,jbd,a);
            # is there a valid path?
            if (is.list(ff)) {
                # preparing a list with the right dimension
                nres <- vector("list",length(res)+length(ff));
                # copying the previous components
                if (length(res)>0) { for (jd in 1:length(res)) {
                    nres[[jd]] <- res[[jd]];
                }}
                # adding the component with this child
                if (length(ff)>0) { for (jd in 1:length(ff)) {
                    nres[[jd+length(res)]] <- c(d,ff[[jd]]);
                }}

                # storing to continue the loop
                res <- nres;
            }
            else {
                res <- -1;
            }
        }
    }
}
# removing component -1 (means no path)
if (length(res) > 0) {
    ko <- 0;
    for (i in length(res)) { 
        if (identical(res[[i]],-1)) { ko <- 1+ko;}
    }
    if (ko > 0) {
        uu <- vector("list",length(res)-ko);
        ik <- 0;
        for (i in length(res)) { 
            if (!identical(res[[i]],-1)) {
                ik <- 1+ik;
                uu[[ik]] <- res[[i]];
            }
        }
    res <- uu;
    }
}
# sorting the list
if (length(res) > 1) {
    long <- rep(NA,length(res));
    for (yd in 1:length(res)) { long[yd] <- length(res[[yd]]);}
    res <- res[order(long)];
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
arc2pam <- function(arc)
#TITLE  constructs the object pam from the object arc
#DESCRIPTION (ba)
# exploring the matrix arc@fle, returns a binary matrix with
# rows associated to each node giving their children.
# 0 means that this column is not a children as a pam object.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS genealogy arc pam
#INPUTS
#{arc} <<The arc object>>
#[INPUTS]
#VALUE
# the pam object
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.arc0);
# print(arc2pam(rbsb.arc0));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 07_10_18
#--------------------------------------------
{
if (rbsb.mck) {check4valid(valid8arc(arc));}
nbno <- arc@nbn;
res <- new("pam",rlt=matrix(0,nbno,nbno));
# doing it
for (jbd in sj(nrow(arc@fle))) {
    res@rlt[arc@fle[jbd,1],arc@fle[jbd,2]] <- 1;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pam2arc <- function(pam)
#TITLE  from the parentship object pam returns the arcs object
#DESCRIPTION (ba)
#   from the parentship object pam returns the arcs object
#DETAILS
#KEYWORDS utilities
#PKEYWORDS genealogy arc pam
#INPUTS
#{pam} <<The object pam.>>
#[INPUTS]
#VALUE
# the arc object.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(pam2arc(rbsb.pam0));
# print(pam2arc(rbsb.pam1));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_18
#REVISED 09_10_20
#--------------------------------------------
{
if (rbsb.mck) {check4valid(valid8pam(pam));}
nbno <- nrow(pam@rlt);
res <- new("arc",nbn=nbno,fle=matrix(0,0,3));
# doing it
for (jbd in sj(nbno)) { for (jd in sj(nbno)) {
    if (pam@rlt[jbd,jd] == 1) {
        res@fle <- rbind(res@fle,c(jbd,jd,211));
    }
}}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geneal4pam <- function(pam,i,asc=TRUE)
#TITLE  constructs the genealoty of a given node
#DESCRIPTION (ba)
#   This function constructs the genealoty of a given node,
# that is returns the graph of all ascendants (descendants)
# starting from this node.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS genealogy
#INPUTS
#{pam} <<The pam matrix describing the parentship.>>
#{i} <<The considered node.>>
#[INPUTS]
#{asc} << must ascendants be looked for, if not
#        the descendants.>>
#VALUE
# an arc object describing the obtained tree
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# geneal4pam(rbsb.pam1,2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_11_07
#REVISED 09_10_13
#--------------------------------------------
{
# starting
if (rbsb.mck) {
    check4valid(valid8pam(pam));
    check4tyle(i,"numeric",1,"Only one node at once");
}
nbn <- nrow(pam@rlt);
if ((i<1) | (i>nbn)) { erreur(NULL,"the node",i,"does not fit with the pam provided.");}
if (asc) { pam@rlt <- t(pam@rlt);}
res <- new("arc",nbn=nbn,fle=matrix(0,0,3));
# the included nodes
nin <- i;
# finding all involved nodes
ajout <- 1;
while (ajout > 0) {
    ajout <- 0;
    #cat("----------",ajout,"\n");
    for (noe in nin) {
        #cat("(((",nin,")))",noe,"\n");
        pot <- which(pam@rlt[noe,] == 1);
        #cat("|||",pot,"|||\n");
        for (po in sjl(pot)) {
            p <- pot[po];
            if (!(p %in% nin)) {
                #cat("uuu\n");
                nin <- c(nin,p);
                ajout <- ajout+1;
            }
        }
    }
}
# deducing the restricted graph
for (i in sjl(nin)) { 
    ii <- nin[i];
    for (j in sjl(nin)) {
        jj <- nin[j];
        if (pam@rlt[ii,jj] == 1) {
           res@fle <- rbind(res@fle,c(ii,jj,211));
        }
    }
}

if (asc) { res@fle[,1:2] <- res@fle[,c(2,1)];}
if (rbsb.mck) {check4valid(valid8arc(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
range8pos <- function(pos,padding=rbsb.pgr0@padding)
#TITLE  returns the ranges (x,y) for a pos
#DESCRIPTION (ba)
#   This function returns the ranges (x,y) for a pos
# taking into account the zoom specification of the pos
# and the extreme values of its positions.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS geometry
#INPUTS
#{pos} <<The pos object.>>
#[INPUTS]
#{padding} <<The padding coefficient to apply.>>
#VALUE
# a list with two components \$x and \$y giving
# the ranges (min,max) of the two first positions
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# uu <- new("pos",posi=matrix(round(10*cos(1:20)),5));
# range8pos(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_11_07
#REVISED 07_11_07
#--------------------------------------------
{
xra <- diff(range(pos@posi[,1]))/2;
yra <- diff(range(pos@posi[,2]))/2;
# computing the middle point of the nodes
xmi <- mean(range(pos@posi[,1]));
ymi <- mean(range(pos@posi[,2]));
# computing the range of the representation
xm <- xmi + xra*pos@zoom[1];
ym <- ymi + yra*pos@zoom[2];
xr <- xm + padding*c(-1,1)*xra/pos@zoom[4];
yr <- ym + padding*c(-1,1)*yra/pos@zoom[4];
list(x=xr,y=yr);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
normalize4pta <- function(pt)
#TITLE  normalize pt
#DESCRIPTION (ba)
# Whatever are the values given in the @pro
# they are normalized and rounded to be printed according to its
# slot @kkk as explained in the details of print8pta
#DETAILS
#KEYWORDS utilities
#PKEYWORDS pt
#INPUTS
#{pt}<<The pt to be normalized.>>
#[INPUTS]
#VALUE
# the pt after normalization
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(normalize4pta(rbsb.pta0));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_23
#REVISED 08_09_24
#--------------------------------------------
{
# first the checking
if (rbsb.mck) {check4valid(valid8pta(pt));}
nd <- length(dim(pt@pro));
if (nd == 0) { return(pt);}
# conditional dimensions
cd <- as.vector(outer(pt@vac,names(dimnames(pt@pro)),"==") %*% (1:nd));
# normalization
if (length(cd)==0) {
    pt@pro <- pt@pro/sum(pt@pro);
} else {
   ssuu <- apply(pt@pro,cd,sum);
   pt@pro <- sweep(pt@pro,cd,ssuu,"/");
}
# rounding as integers
if (pt@kkk > 0) {
    pt@pro <- pt@pro*10^pt@kkk;
    nbdec <- 0;
}
if (pt@kkk == 0) { nbdec <- 3;}
if (pt@kkk < 0) { nbdec <- -pt@kkk;}
pt@pro <- round(pt@pro,nbdec); 
# returning
pt;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
categ3beta <- function(a=1,b=1,n=3,xname=form3names(n))
#TITLE  compute a univariate categ distribution
#DESCRIPTION (ba)
# from a beta(a,b) distribution operates an approximate
# equal spaced discretization with n classes
#DETAILS
#KEYWORDS utilities
#PKEYWORDS categ
#INPUTS
#[INPUTS]
#{a}<< First parameter for the Beta distribution.>>
#{b}<< Second parameter for the Beta distribution.>>
#{n}<< Number of classes to use.>>
#{xname}<< names to provide for the 
#        probability vector.>>
#VALUE
# a probability vector of size n
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# categ3beta(a=1,b=9,n=5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_25
#REVISED 08_09_25
#--------------------------------------------
{
# first the checking
if ((a <= 0) | (b <= 0) | (n <= 0)) {
    erreur(c(a,b,n),"One of the first three arguments is non positive.");
}
if (n != length(xname)) {
    form3affiche(xname);
    erreur(n,"The name vector is not of length n.");
}
# doing it
pc <- dbeta((1:n)/(n+1),a,b);
pc <- pc / sum(pc);
names(pc) <- xname;
# returning
pc;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
categ3beta2 <- function(co=c(1,1),re=c(1,1),n=c(3,4),
                        xnames=list(V.1=form3names(n[1]),
                                    V.2=form3names(n[2])))
#TITLE  compute a bivariate categ distribution
#DESCRIPTION (ba)
# from beta distributions operates an approximate
# equal spaced discretization for two variables
#DETAILS
# The central idea is the product of two betas onto
# the domain of the two variables. The first along the 
# first diagonal (common distribution), the second along
# the second diagonal (relationship distribution) 
# restricted to the corresponding interval. See the code
# for more details.
#KEYWORDS utilities
#PKEYWORDS categ
#INPUTS
#[INPUTS]
#{co}<<) parameters for the common Beta distribution.>>
#{re}<<) parameters for the Beta distribution 
#           associated to the relationship between
#           the two variables.>>
#{n}<<) Number of classes to use for the first and 
#          second variable.>>
#{xnames}<< the dimnames of to apply onto the resulting 
#           probability matrix.>>
#VALUE
# a probability matrix of dimension n
#EXAMPLE categ3beta2
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_25
#REVISED 08_10_19
#--------------------------------------------
{
# first the checking
if ((length(re)!=2) | (length(co)!=2) | 
    (length(n)!=2)  | (length(xnames)!=2)) {
    form3affiche(re);
    form3affiche(co);
    form3affiche(xnames);
    erreur(n,"Bad length in at least one of the arguments");
}
if ((any(c(re,co,n)<=0)) |
    (length(xnames[[1]])!=n[1]) |
    (length(xnames[[2]])!=n[2]) ) {
    form3affiche(re);
    form3affiche(co);
    form3affiche(xnames);
    erreur(n,"At least one of the arguments is not convenient");
}
# doing it
pc <- matrix(NA,n[1],n[2]);
xx <- rep((1:n[1])/(n[1]+1),n[2]);
yy <- rep((1:n[2])/(n[2]+1),each=n[1]);
d1 <- (xx+yy) / 2;
d2 <- (xx-yy) / 2;
# (restriction of the range of d2)
su <- pmin(2*d1,2*(1-d1));
d2 <- d2/su + 0.5;
pc[] <- dbeta(d1,co[1],co[2]) *
        dbeta(d2,re[1],re[2]);
pc <- pc / sum(pc);
dimnames(pc) <- xnames;
# returning
pc;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
categ3betap <- function(co=c(1,1),re=c(2,2),n=c(3,4,5),
                        xnames=rbsb.cha0)
#TITLE  compute a p-variable categ distribution
#DESCRIPTION (ba)
# from beta distributions generates the joint probability
# table for p (=length(n)) categoric variables coming from
# the discretization of the betas (approximately
# equal spaced discretized).
#DETAILS
# The central idea is the product of pairs of beta 
# obtained from categ3beta2. For instance, in case of
# p=3, if P1(i,j) is associated to the first two variables,
# P2(i,k) is associated to the first and last variables &
# P3(j,k) is associated to the last two variables, then
# the joint proba is P(i,j,k) propto P1(i,j)P2(i,k)P3(j,k).
#KEYWORDS utilities
#PKEYWORDS categ
#INPUTS
#[INPUTS]
#{co}<<) First parameter when calling categ3beta2.>>
#{re}<<) Second parameter when calling categ3beta2.>>
#{n}<<) Number of classes to use for the p variables.
#                 Its length provides the number of variables.>>
#{xnames}<< dimnames of the resulting probability array.
#          The default generates some standard names.>>
#VALUE
# a probability array of dim n
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# categ3betap();
#REFERENCE
#SEE ALSO
#CALLING categ3beta categ3beta2
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_19
#REVISED 08_10_20
#--------------------------------------------
{
if (isvide(xnames)) {
    xnames <- vector("list",length(n));
    for (xn in sjl(n)) {
        xnames[[xn]] <- form3names(n[xn]);
    }
    if (length(n) > 0) { 
        names(xnames) <- paste("CV",sjl(n),sep="");
    }
}
# first the checking
if ((length(re)!=2) | (length(co)!=2) | 
    (length(n) == 0) |
    (length(n) != length(xnames))) {
    form3affiche(re);
    form3affiche(co);
    form3affiche(xnames);
    erreur(n,"Bad length in at least one of the arguments");
}
if (any(c(re,co,n)<=0)) {
    form3affiche(re);
    form3affiche(co);
    erreur(n,"At least one of the first three arguments is not acceptable");
}
nva <- length(n);
if (nva == 1) {
    res <- categ3beta(a=co[1],b=co[2],n=n,xname=xnames[[1]]);
    res <- matrix(res,1,dimnames=list(NULL,xnames[[1]]));
}
for (va in sj(nva)) { if (length(xnames[[va]])!=n[va]) {
    erreur(xnames[[va]],"The names for the",va,"th",
           "variable is not consistent with n[va] (=",
           n[va],").");
}}
if (nva == 2) {
    res <- categ3beta2(co,re,n,xnames);
}
if (nva > 2) {
    # initialization
    res <- array(1,dim=n,dimnames=xnames);
    # incorporating the probability tables of
    # pairs of variables
    for (iv in 1:(nva-1)) { for (jv in (iv+1):nva) {
        pp <- unique(c(iv,jv,1:nva));
        if (!all(pp[1:2]==c(iv,jv))) {
            form3affiche(c(iv,jv));
            erreur(pp,"unique does not work as supposed!");
        }
        pp <- order(pp);
        pro <- categ3betap(co,re,n[c(iv,jv)]);
        pro <- array(rep(pro,prod(n[-c(iv,jv)])),c(dim(pro),n[-c(iv,jv)]));
        pro <- aperm(pro,pp);
        res <- res * pro;
    }}
    # normalization
    res <- res / sum(res);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp2dim <- function(eas,nom)
#TITLE  returns the dimension of the variable
#           defined in an easyp character(1)
#DESCRIPTION (ba)
# Decoding the node/variables of ONE easyp, returns the dimension
# of the associated variable according to the associated /nom/.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS name var
#INPUTS
#{eas} << character of the complete variable name>>
#{nom} << referent /nom/.>>
#[INPUTS]
#VALUE
# dimension in terms of number of variables
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# easyp2dim('{{A}}',rbsb.nom2);
# easyp2dim('{{A[c]}}',rbsb.nom3);
# easyp2dim('{{A[a]}}*{{C}}',rbsb.nom3);
## '{{A}}*{{B}}' -> max(dim(B),dim(A))
## but a fatal error is issued when 
## (dim(A)!=dim(B)) & min(dim(A),dim(B))!=1
## which the case with uu  
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_01
#REVISED 09_06_15
#--------------------------------------------
{
# checking 
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(eas,"character",1);
}
#
if (eas=="") { return(1); }
# looking for node parents in 'eas'
dd <- easyp3cut(eas,rbsb.cpt);
ee <- easyp3trouve(dd,nom)$node;
# preparing the result
res <- 1;
# checking the possible parent nodes
for (nn in ee) {
    didi <- nbnv(nom,nn);
    if (res==1) { res <- didi;
    } else {
        if (didi>1) { if (res!=didi) {
            form3affiche(ee);
            erreur(list(nom,eas),
                   "different dimensions are generated by parent nodes"
                  );
        }}
    }
}
# returning
as.numeric(res);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyps2dim <- function(meas,nom)
#TITLE  returns the dimension of the variable
#           defined with an easyp of any length
#DESCRIPTION (ba)
# Similar to the single version (easyp2dim) but
# deals with 'meas' having more than one component.
# When there are several component, it is checked
# that every one is of dimension one: then the
# dimension is the number of components.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS name var
#INPUTS
#{meas} << character of the complete variable name>>
#{nom} << referent /nom/.>>
#[INPUTS]
#VALUE
# dimension in terms of number of variables
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# easyps2dim(c('{{A[c]}}','{{A[a]}}*B'),rbsb.nom3);
#REFERENCE
#SEE ALSO easyp2dim
#CALLING easyp2dim
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_03
#REVISED 09_06_17
#--------------------------------------------
{
# checking 
if (rbsb.mck) {
    check4valid(valid8nom(nom));
    check4tyle(meas,c("null","character","numeric"),-1);
}
#
if (isvide(meas)) { return(1);}
if (is.numeric(meas)) { meas <- as.character(meas);}
if (length(meas) < 2) {
    res <- easyp2dim(meas,nom);
} else {
    for (eas in meas) {
        nn <- easyp2dim(eas,nom);
        if (nn != 1) {
            erreur(list(meas,nom),
               "Not all the components of 'meas' are univariate");
        }
    }
    res <- length(meas);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8pt2 <- function(pt,comment=0,kkk=-3,tra=FALSE)
#TITLE  prints a pt object of dimension two
#DESCRIPTION (ba)
#   This function prints a two-way probability table
#   as a joint distribution, a conditional in both sides.
#   This is quite peculiar function to be generalized
#   later on. BE AWARE that the 'pt' must comprise the
#   joint distribution.
#DETAILS
#KEYWORDS print
#PKEYWORDS pt
#INPUTS
#{pt} <<The pt object.>>
#[INPUTS]
#{comment} << Numeric vector defining the way to print.
#             0: joint probability,
#             1: conditional by rows,
#             2: conditional by columns.>>
#{kkk} << the coefficient for rounding the probabilities.>>
#{tra} << Must the table be transposed when printed?>>
#VALUE
# nothing but a print is performed
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# uu <- rbsb.pta1;
# print8pt2(uu,kkk=3);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_05
#REVISED 09_01_05
#--------------------------------------------
{
# some checking
if (rbsb.mck) {check4valid(valid8pta(pt));}
if (length(dim(pt@pro))!=2) {
    erreur(dim(pt@pro),"ONLY for two-way tables");
}
# extracting the table
pro <- pt@pro;
nos <- names(dimnames(pro));
# normalizing it
if (comment == 2) { pro <- t(pro);}
if (comment > 0) {
    mar <- apply(pro,1,sum);
    pro <- pro /mar;
} else { pro <- pro / sum(pro);}
if (comment == 2) { pro <- t(pro);}
# possibly transposing it
if (tra) { pro <- t(pro);}
# rounding it as integer
kkk <- min(2,kkk);
pro <- round(pro*10^kkk);
add <- paste(" - multiplied by 10^",kkk,sep="");
# finally printing
if (comment == 1) {
    form3titre(paste(" ",nos[2],"|",nos[1],add," "),0);
} else {
    if (comment == 2) {
        form3titre(paste(" ",nos[1],"|",nos[2],add," "),0);
    } else {
        form3titre(paste(" ",nos[1],"x",nos[2],add," "),0);
    }
}
print(pro);
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isvide <- function(x) 
#TITLE  to avoid difficulty with is.null
#DESCRIPTION (ba)
# returns TRUE is the structure is empty
#DETAILS
# extension of \code{isempty}.
#PKEYWORDS 
#KEYWORDS utilities
#PKEYWORDS utilities
#INPUTS
#{x}    <<object to be scrutinazed>>
#[INPUTS]
#VALUE
# TRUE when the object is considered as empty
# FALSE if not
#EXAMPLE isvide(NULL);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_15
#REVISED 09_10_20
#--------------------------------------------
{
  if (isempty(x))                                          { return(TRUE);}
  if (is.function(x))                                      { return(FALSE);}
  if (is.data.frame(x))                                    { return(FALSE);}
  if (identical(x,rbsb.nom0))               { return(TRUE);}
  if (identical(x,rbsb.ion0))               { return(TRUE);}
  if (identical(x,rbsb.arc0))               { return(TRUE);}
  if (identical(x,rbsb.pam0))               { return(TRUE);}
  if (identical(x,rbsb.win0))               { return(TRUE);}
  if (identical(x,rbsb.alk0))               { return(TRUE);}
  if (identical(x,rbsb.pta0))               { return(TRUE);}
  #
  if (is.character(x)) { if (length(x)==1) { if (x=="")    { return(TRUE);}}}
  FALSE;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp3trouve <- function(x,nom)
#TITLE  checks the consistency of the result obtained after 
# easyp3cut and returns the used node/variable numbers of nom
#DESCRIPTION (ba)
# The aim of this function is to precise the analysis made by 
# easyp3cut applying it to a precise nom object. Using the composition
# of easyp3trouve(easyp3cut(chain),nom) gives a result directly
# usable (for well introduced users of rebastaba).
# The check is made on the validity of node names, on 
#     the validity of the numerical constants.
# Completed information about the $typ=2 (nodes and
#     variables) is returned.
#DETAILS
# when a node is involved, all its variables are considered as
# involved. If A is a node repeated three times, then "A" implies 
# "A[1]", "A[2]" and "A[3]" as variables but "A[2]" implies node
# "A" and only variable "A[2]".
#KEYWORDS utilities
#PKEYWORDS expression
#INPUTS
#{x} <<output of easyp3cut>>
#{nom}<<nom object where node/variable names will be investigated>>
#[INPUTS]
#VALUE
# A list comprising
# $node: the internal number (i with respect to nom) of the nodes involved,\cr
# $vari: the internal number (k with respect to nom) of the variables involved,\cr
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# uu <- easyp3cut("sqrt({{A}}) + {{U}}/10",rbsb.cpt);
# easyp3trouve(uu,char2nom(LETTERS));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_04
#REVISED 09_11_21
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    if (length(x$blo) != length(x$typ)) {
        erreur(x,"block and type components have got different lengths!");
    }
}
###
pano <- pava <- numeric(0);
for (jbd in sjl(x$blo)) {
    if (!is.element(x$typ[jbd],c(0:nrow(rbsb.cpt)))) {
    cat("the ",jbd,"th component has got the type ",x$typ[jbd],"\n",sep="");
    erreur(x,"This is an unknown type!");
    }
    if (x$typ[jbd] == 1) {
        # dealing with the expected node or variable
        inconnu <- x$blo[jbd];
        # does it exist? (when not the node itself)
        if (inconnu!=rbsb.cni) {
            qui <- nv3nom(inconnu,nom)
            if (qui == 0) {
                erreur(list(nom,inconnu),"'inconnu' was not found to be a possible node/variable");
            }
            # decoding it
            dede <- nv2ion(inconnu,nom,check=FALSE);
           if (dede@iden == "v") {
                # must be added as a variable
                pava <- c(pava,dede@vk);
            } else {
                if (dede@iden == "nn") {
                    # must be added as a node
                    pano <- c(pano,dede@nk);
                } else {
                    # must not occur
                    rapport("easyp3trouve: not a variable not a node!");
                }
            }
        }
    }
    if (x$typ[jbd] == 2) {
        # indication of a rounding
        uu <- as.numeric(x$blo[jbd]);
        if ( is.na(uu)) {
            cat("For the ",jbd,"th block is ",x$blo[jbd]);
            erreur(uu,"Must be integer for rounding");
        }
    }
}
list(node=unique(pano),
     vari=unique(pava));
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check8alk <- function(object)
#TITLE  checks an /alk/
#DESCRIPTION (ba)
#   This function checks /alk/ more deeply than \code{valid8alk}
#  which is also called.
#DETAILS
# After numerous attempts all the checks made in that function
# where removed from valid8alk because it seemed difficult
# to comply the requirements of a standard package by R checking.
# As far as possible, only complementary tests to \code{valid8alk}
# are performed.\cr
# The main idea is that non completed /alk/ must
# avoid the redundancy while completed /alk/ must
# have all slot filled in the standard way.
#KEYWORDS classes
#INPUTS
#{object} <<The alk object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
# but when something badly strong is discovered
# a fatal error is issued.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# check8alk(rbsb.alk1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_10
#REVISED 10_09_03
#--------------------------------------------
{
#
chcado2 <- function(d1,d2) {
    res <- length(setdiff(d2,d1)) >= 0;
    res;
}
#
chnudo1 <- function(dd,fini) {
    res <- TRUE;
    if (length(dd) != 2) { res <- FALSE;
    } else {
        if (!is.numeric(dd)) { res <- FALSE;
        } else {
            if (fini) { res <- all(is.finite(dd));}
            if (dd[1] > dd[2]) { res <- FALSE;}
        }
    }
    #
    res;
}
#
chnudo2 <- function(d1,d2) {
    res <- (d1[1] > d2[1]) | (d1[2] < d2[2]);
    res;
}
#
############################
# STANDARD CHECKS
############################
#
    res <- valid8alk(object);
    if (identical(res,TRUE)) { res <- character(0);}
#
############################
# PERMANENT CHECKS
############################
#
    # the type must be known
    if (!(object@ltype %in% rbsb.ltn)) {
        erreur(list(rbsb.ltn,object@ltype),"This 'alk@ltype' is not registrated!");
    }
    # inheritance is not allowed from ltransfo
    if (length(object@ltransfo)>0) {
    if (!all(object@ltransfo=="")) {
        # looking for forbidden parentship
        deco <- easyp3cut(object@ltransfo,rbsb.cpt);
        if (sum(deco$typ==1)>0) { if(!all(deco$blo==rbsb.cni)) {
            res <- c(res,paste(object@ltransfo,"No parents can be involved in the transformation"));
        }}
    }}
    # number of variables
    nbv <- length(alkvar(object));
    # consistence for data based nodes
    if (rbsb.tlk[[object@ltype]]$fami == "data_based") {
        # getting the variables names of the "data.frame"
        dfe <- get8daf(object@ldaf,1:5);
        nava <- dimnames(dfe)[[2]];
        navat <- paste(nava,collapse="");
        if ((length(strsplit(navat,rbsb.cpt["variables","opening"],fixed=TRUE)[[1]]) > 1) |
            (length(strsplit(navat,rbsb.cpt["variables","closing"],fixed=TRUE)[[1]]) > 1) ) {
            res <- c(res,paste("|",nava,"|",
                   paste("Variable names of data frames must not comprise '",
                         rbsb.cpt["variables","opening"],"' or '",
                         rbsb.cpt["variables","closing"],"'...",sep="")
                  ));
        }
    }
    # presence of the necessary parameters
    if ("lpara" %in% rbsb.tlk[[object@ltype]]$argYES) {
        # parameters are compulsory
        for (para in sjl(rbsb.tlk[[object@ltype]]$par)) {
            npa <- names(rbsb.tlk[[object@ltype]]$par)[para];
            if (isvide(object@lpara[[npa]])) {
                res <- c(res,paste(object@ldes@name,":",paste("The parameter",npa,"was expected for the",object@ltype,"type of node")));
            }
        }
    }
    ###
    ###
    # consistence of the lengths of parameter vectors and the number of repetitions
    if (isalkrepeatable(object@ltype)) { for (jbd in sjl(object@lpara)) {
        if ((object@lrep != length(object@lpara[[jbd]])) &
            (length(object@lpara[[jbd]]) != 1) &
            (object@lrep > 0)
           ) {
             res <- c(res,paste(paste(object@lrep,length(object@lpara[[jbd]]),sep="!="),
                    "For repeatable /alk/, some consistency is asked between @lpara and @lrep"));
        }
    }}
    ###
#
############################
# CHECKS FOR COMPLETED /alk/
############################
#
    if (identical(object@lcomp,TRUE)) {
        # the node is supposed to be completed
        #
	# checking the domain with respect to the nature of the variables
	for (ii in sj(nbv)) {
	    if (rbsb.snp[object@lnat[ii],"categoric"]) {
		# categoric variable
		if (!chcado2(object@lpod[[ii]],object@lred[[ii]])) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lred[[ii]],"All categories for representation are not possible !"));
		}
		if (!chcado2(object@lpod[[ii]],object@lcod[[ii]])) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lcod[[ii]],"All categories for common view are not possible !"));
		}
	    } else {
		# numeric variable
                #
		if (!chnudo1(object@lpod[[ii]],FALSE)) { 
		    res <- c(res,paste("variable number",ii,": lpod for numeric variables must be of numeric",
                                       "of length 2 defining an interval"));
		}
		if (!chnudo1(object@lred[[ii]],TRUE)) { 
		    res <- c(res,paste("variable number",ii,": lred for numeric variables must be of numeric",
                                       "of length 2 defining a finite interval"));
		}
		if (!chnudo1(object@lcod[[ii]],TRUE)) { 
		    res <- c(res,paste("variable number",ii,": lcod for numeric variables must be of numeric",
                                       "of length 2 defining a finite interval"));
		}
                if (!chnudo2(object@lpod[[ii]],object@lred[[ii]])) {
                    res < c(res,paste("variable number",ii,": lred not included in lpod"));
                }
                if (!chnudo2(object@lpod[[ii]],object@lcod[[ii]])) {
                    res < c(res,paste("variable number",ii,": lcod not included in lpod"));
                }
                #
	    }
	}
	#
	# checking the strong requirement described by rbsb.tlk
        # at the level definition which is applied to non completed /alk/
        #
        ############ DOESN'T WORK TO BE IMPLEMENTED AFTER BETTER THOUGHTS
	speci <- rbsb.tlk[[object@ltype]] ;
        speci <- rbsb.lis0; ### JUST THIS LINE TO ERASE
	for (ii in speci[["argNO"]]) {
	    if (!identical(slot(object,ii),speci[[ii]])) {
		rr <- paste("For alk of ltype =",object@ltype,"the",ii);
		rr <- paste(rr,"supposed to be (",speci[[ii]],") and it is (");
		rr <- paste(rr,slot(object,ii),").");
		res <- c(res,rr);
	    }
	}
    } else {
#
##############################
# CHECKS FOR UNCOMPLETED /alk/
##############################
#
        # the node is supposed not to be completed
	if (length(object@lvar) > 0) {
	    if(length(object@lnat) != length(object@lvar)) {
                res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The lengths of lnat and lvar must be equal for multivariate nodes"));
            }
	} else {
	    if(length(object@lnat) != 1) { if (!(object@ltype %in% c("empidata"))) {
                form3affiche(list(object@ldes@name,object@lcomp));
                res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The length of lnat must be equal to ONE for scalar and repeated nodes"));
            }}
	}
	# checking the dimensions of the declared domains -> already done in valid8alk
        ## establishing the nature
        natu <- object@lnat;
        if ((object@lrep > 1) & (length(natu)==1)) { natu <- rep(object@lnat,object@lrep);}
        if (object@ltype %in% c("empidata")) { natu <- object@lwin@nat;}
	# checking the domain with respect to the nature of the variables if it is known
	for (ii in sjl(natu)) {
	    if (rbsb.snp[natu[ii],"categoric"]) {
		if (!isvide(object@lred)) { if (!chcado2(object@lpod[[ii]],object@lred[[ii]])) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lred[[ii]],"All categories for representation are not possible !"));
		}}
		if (!isvide(object@lcod)) { if (!chcado2(object@lpod[[ii]],object@lcod[[ii]])) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lcod[[ii]],"All categories for common view are not possible !"));
		}}
	    } else {
		# numeric variable
                #
		if (!chnudo1(object@lpod[[ii]],FALSE)) { 
		    res <- c(res,paste("variable number",ii,": lpod for numeric variables must be of numeric",
                                       "of length 2 defining an interval"));
		}
		if (!isvide(object@lred)) { if (!chnudo1(object@lred[[ii]],TRUE)) { 
		    res <- c(res,paste("variable number",ii,": lred for numeric variables must be of numeric",
                                       "of length 2 defining a finite interval"));
		}}
		if (!isvide(object@lcod)) { if (!chnudo1(object@lcod[[ii]],TRUE)) { 
		    res <- c(res,paste("variable number",ii,": lcod for numeric variables must be of numeric",
                                       "of length 2 defining a finite interval"));
		}}
                if (!isvide(object@lred)) { if (!chnudo2(object@lpod[[ii]],object@lred[[ii]])) {
                    res < c(res,paste("variable number",ii,": lred not included in lpod"));
                }}
                if (!isvide(object@lcod)) { if (!chnudo2(object@lpod[[ii]],object@lcod[[ii]])) {
                    res < c(res,paste("variable number",ii,": lcod not included in lpod"));
                }}
	    }
	}
    }
    # checking the natures 
    for (jbd in object@lnat) { if (!(jbd %in% rbsb.sna)) {
        res <- c(res,jbd,"is not an acceptable nature for a link");
    }}
    #
    # At last returning
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=TRUE);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isalkrepeatable <- function(ltype)
#TITLE  indicates if the link is a standard
#           repeatable link
#DESCRIPTION (ba) returns TRUE/FALSE according to
# fact that this type of link is repeatable or not.
#DETAILS
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{ltype} << (character) type of the link.>>
#[INPUTS]
#VALUE
# TRUE/FALSE
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# isalkrepeatable("normal");
# isalkrepeatable("program");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_09
#REVISED 09_11_05
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(ltype,"character",1,"One on only one 'ltype' is expected!");
    if (!(ltype %in% rbsb.ltn)) {
        cat(rbsb.ltn);
        erreur(ltype,"This ltype is not registrated!");
    }
}
# returning
rbsb.l_a[ltype,"rep?"] == "TRUE";
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2nv <- function(xx)
#TITLE  transforms node[variable] characters
# into node and variable characters.
#DESCRIPTION (ba)
# just removing possible square brackets to give
# back the node names ($nod) and the variable 
# names ($var)
#DETAILS
# In fact square brackets are the parentheses
# given by the constant \code{rbsb.cpt["variables",]}.
#KEYWORDS misc
#INPUTS
#{xx} <<The character to transform>>
#[INPUTS]
#VALUE
# a list with two components\cr
#{$nod} <<The node names.>>
#{$var} <<The variable names: '' when absent.>>
#{$vva} <<The variable names but node name when absent.>>
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nv2nv(c("A","A[666]","")); # list(nod=c("A","A"),var=c("","666",""),vva=c("A","666","")
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_23
#REVISED 10_08_12
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(xx,"character",-1);
}
#
res <- list(nod=character(0),var=character(0),vvar=character(0));
cou1 <- rbsb.cpt["variables","opening"];
cou2 <- rbsb.cpt["variables","closing"];
#
for (jbd in sjl(xx)) {
    x <- xx[jbd];
    if (x == "") {
        res$nod <- c(res$nod,"");
        res$var <- c(res$var,"");
        res$vva <- c(res$vva,"");
    } else {
        uu <- strsplit(x,cou1,fixed=TRUE);
        res$nod <- c(res$nod,uu[[1]][1]);
        if (x==uu[[1]][1]) {
            res$var <- c(res$var,"");
        } else {
            vv <- strsplit(uu[[1]][2],cou2,fixed=TRUE);
            if (uu[[1]][2]==vv[[1]][1]) {
                erreur(x,"Missing bracketing");
            } else {
                res$var <- c(res$var,vv[[1]][1]);
            }
        }
        #
        if (res$var[jbd]=="") { res$vva <- c(res$vva,res$nod[jbd]);
        } else { res$vva <- c(res$vva,res$var[jbd]);}
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
new8arc <- function(nbno,nbar)
#TITLE  creates an /arc/ with a specified number of nodes and arcs
#DESCRIPTION (ba)
# After checking that it is possible, returns an /arc/ object
# with the specified number of nodes (\code{nbno}) and arcs (\code{nbar}).
#DETAILS
#KEYWORDS misc
#PKEYWORDS arc
#INPUTS
#{nbno}<<Number of desired nodes.>>
#{nbar}<<Number of desired arcs.>>
#[INPUTS]
#VALUE
# A valid \code{arc} object. If not possible,
# a fatal error is issued  
#EXAMPLE
# rsba3k("RESET"); # For R checking to be forgetten
# new8arc(10,5);
# \dontrun{new8arc(3,4);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_20
#REVISED 09_10_20
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(nbno,"integer",1,"nbno is the number of nodes");
    check4tyle(nbar,"integer",1,"nbar is the number of arcs");
    if (nbno < 0) { erreur(nbno,"nbno is the number of nodes");}
    if (nbar < 0) { erreur(nbar,"nbar is the number of arcs");}
    if (nbar > nbno*(nbno-1)/2) {
        erreur(list(nbno,nbar),"Too much arcs for the number of nodes");
    }
}
#
# filling the @fle matrix
fle <- matrix(211,nbar,3);
nba <- 0;
for (dep in sj(nbno)) { for (arr in sj(nbno)) {
if (dep<arr) { if (nba < nbar) {
    nba <- nba + 1;
    fle[nba,1:2] <- c(dep,arr);
}}}}
# returning
new("arc",nbn=nbno,fle=fle);
              }
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
parents8win <- function(win)
#TITLE  finds the parents at the variable level of a /win/
#DESCRIPTION (ba) finds the variable parents indicated by a
# \code{win}. Notice that, contrary to a previous version, the 
# possible monitoring variable is not considered as a parent;
# it must be comprised in the empidata.
#DETAILS
# The finding of the parents is done by analyzing the different
# fields of the /win/. This is a basic function, no check is performed.
#KEYWORDS misc
#PKEYWORDS parents
#INPUTS
#{win} <<win object>>
#[INPUTS]
#VALUE
# the names of the /win/ parents at the variable level.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# parents8win(rbsb.win1);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_07_21
#REVISED 10_07_28
#--------------------------------------------
{
# no check made in this very basic function
res <- names(win@swg);
if (isvide(res)) { res <- character(0);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
variables8win <- function(win)
#TITLE  returns all variables involved in a /win/
#DESCRIPTION (ba) finds the variable indicated by a
# \code{win}.
#DETAILS
# No check is performed.
#KEYWORDS misc
#PKEYWORDS parents
#INPUTS
#{win} <<win object>>
#[INPUTS]
#VALUE
# the names of the variables.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# variables8win(rbsb.win1);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 10_08_05
#REVISED 10_08_05
#--------------------------------------------
{
# no check made in this very basic function
if (isvide(win)) { res <- character(0);
} else {
    res <- unique(c(names(win@swg),names(win@rwg),win@rmo));
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
parents8alk <- function(alk,nom,of="v")
#TITLE  finds the parents at the variable level of an /alk/
#DESCRIPTION (ba) finds the variable parents of \code{alk} with 
# respect to \code{nom}.
#DETAILS
# The finding of the parents is done by analyzing the different
# fields of the /alk/. This is a basic function, no check is performed.
#KEYWORDS misc
#PKEYWORDS parents
#INPUTS
#{alk} <<alk object>>
#{nom} <</nom/ of the supposed /bn/ where the alk will be inserted.>>
#[INPUTS]
#{of} <<indicates at wich level the parents must be described:
#       'v' at the variable level and 'n' at the node level.>>
#VALUE
# the names of the /alk/ parents alk at the variable or node level.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# parents8alk(rbsb.alk2,rbsb.nom2);
# parents8alk(rbsb.alk2,rbsb.nom2,of="n");
# parents8alk(rbsb.alk3,rbsb.nom3);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_16
#REVISED 10_07_27
#--------------------------------------------
{
# no check made in this very basic function
# gathering every possible source
# those already in esayprogramming style
para <- paste(sapply(alk@lpara,paste,collapse=" "),collapse=" ");
tran <- paste(alk@ltransfo,collapse=" ");
# those which need some padding
padd <- c(alk@lparent,parents8win(alk@lwin));
if (length(padd) > 0) {
    padd <- paste(rbsb.cpt["nodes","opening"],padd,
                  rbsb.cpt["nodes","closing"],collapse="",sep="");
}
#
mama <- paste(para,tran,padd);
# looking for parent in it
dd <- easyp3cut(mama,rbsb.cpt);
ee <- easyp3trouve(dd,nom);
# preparing
res <- c(nv2ion(ee$node,nom,"N")@nn,
         nv2ion(ee$vari,nom,"v")@nvn);
res <- nom2char(char2nom(res),"v");
if (of=="n") {
    res <- nom2char(char2nom(res),"n")
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
help8ltype <- function(quelles="ls")
#TITLE  provides information about type of links
#DESCRIPTION (ba)
# This function displays requirement and default values
# for the different types of links (ltype) implemented
# into rebastaba
#DETAILS
#KEYWORDS
#PKEYWORDS 
#INPUTS
#[INPUTS]
#{quelles} << 
#            "all": all ltype information\cr
#            "ls" : list of available ltype with comment\cr
#            "list": list of availables ltype with comment\cr
#            "argu": possible arguments for each ltype\cr
#            else the ltype ones want to get information
#                 about; e.g. "normal">> 
#VALUE
# nothing but a printing is issued
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE improve the formatting...
#AUTHOR J.-B. Denis
#CREATED 07_10_01
#REVISED 10_06_11
#--------------------------------------------
{
nvp <- c(2,1,0);inp <- c(1,3,5);
quelles <- quelles[1];
possibles <- c("ls","all","list","argu",rbsb.ltn);
if (!(quelles %in% possibles)) {
    form3titre("The possible arguments are:",3);
    cat(possibles,"\n",sep="\n");
    cat(" And you provided '",quelles,"'!\n",sep="");
}
if (quelles == "ls") {
    form3titre("The present different 'ltype' are",nvp[2],inp[2]);
    for (jbd in sjl(rbsb.tlk)) {
        cat("         ",rbsb.ltn[jbd],"\n");
    }
}
if (quelles=="argu") {
    form3titre("Arguments/Properties according to the different 'ltype's",2);
    cat(" <YES> for 'must be provided by the user'\n",
        "<yes> for 'can be provided by the user but there is a default value'\n",
        "< no> for 'necessary but generated by rebastaba'\n",
        "< NO> for 'necessary but fixed by rebastaba'\n",
        "< - > for 'irrelevant'\n");
    kY <- c(4,5);
    kn <- ncol(rbsb.l_a);
    print(as.data.frame(rbsb.l_a[,-c(kY,kn)]));
    cat("     (",dimnames(rbsb.l_a)[[2]][kY],") are always YES\n");
    cat("     (",dimnames(rbsb.l_a)[[2]][kn],") is always  no\n");
    cat("Probability families are:\n");
    for (uu in sjl(rbsb.f_d)) {
        cat("   ",names(rbsb.f_d)[uu],":",rbsb.f_d[uu],"\n");
    }
    cat(" You can also 'print(rbsb.alk_examples)' and 'getClassDef('alk')'\n");
}
if (quelles == "list") {
    form3titre("The present different 'ltype' are",nvp[2],inp[2]);
    for (jbd in sjl(rbsb.tlk)) {
        cat("\n<<<",rbsb.ltn[jbd],">>>\n");
        cat((rbsb.tlk)[[jbd]]$defi,fill=56);
    }
}
if (quelles=="all") { quelles <- rbsb.ltn;}
if (any(quelles %in% rbsb.ltn)) { for (jbd in quelles) {
    if(!(jbd %in% rbsb.ltn)) {
        erreur(jbd,"This is not a valid argument see \"help8ltype()\"");
    }
    uu <- rbsb.tlk[[jbd]];
    form3titre(paste("ltype =",jbd),nvp[1],inp[1]);
    cat((rbsb.tlk)[[jbd]]$defi,fill=56);
    if (isalkrepeatable(jbd)) {
        form3titre("This distribution can be repeated",nvp[3],inp[3]);
    } else {
        form3titre("This distribution cannot be repeated",nvp[3],inp[3]);
    }
    if (rbsb.l_a[jbd,"bugs?"]) {
        form3titre("This distribution can be translated into Bugs",nvp[3],inp[3]);
    } else {
        form3titre("This distribution cannot be  translated into Bugs",nvp[3],inp[3]);
    }
    for (jd in dimnames(rbsb.l_a)[[2]]) {
        pres <- form3justifie(paste("(",jd,"):",sep=""),10,1);
        if (!(expr3present(rbsb.l_a[jbd,jd],c("-","no","NO"),exact=TRUE))) {
            defaut <- (rbsb.l_a[jbd,jd]=="yes");
            if (jd == "lvar") {
               form3titre(paste(pres,"names for the variables of the node"),nvp[3],inp[3]);
               if (defaut) {form3titre(paste("default: ['",paste(uu$lvar,collapse="','"),"']",sep=""),nvp[3],inp[3]+16)};
            }
            if (jd == "lpara") {
                form3titre(paste(pres,"there are",
                                 length(uu$par),
                                 "parameters:"),
                           nvp[3],inp[3]);
                for (hd in sjl(uu$par)) { 
                    form3titre(paste(names(uu$par)[hd],":",uu$par[hd],sep=""),nvp[3],inp[3]+16);
                }
            }
            if (jd == "ltransfo") {
                form3titre(paste(pres,"some transformation can be asked for the generated values"),
                           nvp[3],inp[3]);
                if (is.null(uu$ltransfo)) { vv <- "NULL";} else { vv <- uu$ltransfo;}
                if (defaut) {form3titre(paste("default:",vv),nvp[3],inp[3]+16)};
            }
            if (jd == "lnat") {
                form3titre(paste(pres,"nature(s) of the variable(s) node"),nvp[3],inp[3]);
                if (defaut) {form3titre(paste("default:",uu$lnat),nvp[3],inp[3]+16)};
            }
            if (jd == "lpod") {
                form3titre(paste(pres,"possible domain(s) of the variable(s) node"),nvp[3],inp[3]);
                if (defaut) {form3titre(paste("default:",uu$lpod),nvp[3],inp[3]+16)};
            }
            if (jd == "lred") {
                form3titre(paste(pres,"representation domain(s) of the variable(s) node"),nvp[3],inp[3]);
                if (defaut) {form3titre(paste("default: lpod value"),nvp[3],inp[3]+16)};
            }
            if (jd == "lcod") {
                form3titre(paste(pres,"common domain(s) of the variable(s) node"),nvp[3],inp[3]);
                if (defaut) {form3titre(paste("default: lred value"),nvp[3],inp[3]+16)};
            }
            if (jd == "lparent") {
                form3titre(paste(pres,"names of the node parents"),nvp[3],inp[3]);
                if (defaut) {form3titre(paste("default: no parents"),nvp[3],inp[3]+16)};
            }
            if (jd == "lfunct") {
                form3titre(paste(pres,"piece of code (depends on the ltype)"),nvp[3],inp[3]);
                if (defaut) {form3titre(paste("default:"),nvp[3],inp[3]+16);
                form3repete(" ",32,imp=TRUE); print(uu$lfunct);}
            }
            if (jd == "ldaf") {
                form3titre(paste(pres,"how to get the values of an data based distribution"),nvp[3],inp[3]);
                form3titre(paste("directly provided with a data.frame"),nvp[3],inp[3]+16);
                if (defaut) {form3titre(paste("default:",uu$ldaf),nvp[3],inp[3]+16)};
            }
            if (jd == "lwin") {
                form3titre(paste(pres,"defines variables/natures and other features for",
                                      "empidata (see function /win/ definition)"),
                           nvp[3],inp[3]);
            }
        } else {
            rres <- "not used";
            # ??? 10_08_11 suppressed because not understood at the moment
            #if (jd == "lvar") {
            #    rres <- paste("not used, always",uu$nbv&&&,"variable(s)");
            #    form3titre(paste(pres,rres),nvp[3],inp[3]);
            #    form3titre(paste(uu$nat,"is the default nature"),nvp[3],inp[3]+16);
            #    form3titre(paste(uu$pod,"is the possible domain"),nvp[3],inp[3]+16);
            #}
        }
    }
}}
cat("\nhelp8ltype(\"",rbsb.ltn[1],
    "\") will give you details about this distribution.\n",sep="");
cat("help8ltype(\"argu",
    "\")   will give you the needed arguments.\n",sep="");
cat("help8ltype(\"list",
    "\")   will give you details about each.\n",sep="");
cat("help8ltype(\"ls",
    "\")     will give you the list without comments.\n",sep="");
cat("help8ltype(\"all",
    "\")    will give you details about all available distributions.\n",sep="");
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isndmulti <- function(ltype)
#TITLE  indicates if the node is a standard
#           multidimensional node
#DESCRIPTION (ba) indicates if the node is a standard
#           multidimensional node
#DETAILS
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{ltype} <<type of the link.>>
#[INPUTS]
#VALUE
# TRUE/FALSE
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# ??? to be generalized to answer more than one question ???
#AUTHOR J.-B. Denis
#CREATED 09_01_09
#REVISED 09_01_09
#--------------------------------------------
{
# checking
if (!(ltype %in% rbsb.ltn)) {
    cat(rbsb.ltn);
    erreur(ltype,"This ltype is not registrated!");
    }
# returning
rbsb.tlk[[ltype]]$fami %in% c("conti_vector","discr_vector");
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
new8alk <- function(ldes,ltype,
                    lpara=rbsb.lis0,lrep=0,
                    lnat=rbsb.cha0,lvar=rbsb.cha0,
                    lparent=rbsb.cha0,
                    lpod=rbsb.lis0,lred=rbsb.lis0,lcod=rbsb.lis0,
                    ltransfo="",
                    ldaf=rbsb.daf0,lwin=rbsb.win0,
                    lfunct=rbsb.fun0
                   )
#TITLE  generates a new alk from user specification
#DESCRIPTION (ba)
# Just collecting specifications provided by the user to
# build an alk object up taking into consideration the
# specificity of the ltype. \cr
# Default values are consistent for not to be used 
# arguments.
#DETAILS
# The complete list of standard already programmed distributions is given
# with \code{help8ltype} function.\cr
#KEYWORDS
#PKEYWORDS node
#INPUTS
#{ldes} <<Human description of the potential node through 
#         either a character for the name or a /des/ object.>>
#{ltype} << type of link: a character string which must belong
#          to the \code{names(rbsb.tlk)}. This list is
#          returned by \code{help8ltype("ls")}.>>
#[INPUTS]
#{lpara} << list of parameter values for standard distributions.>>
#{lrep} << the number of repetitions; zero means no repetitions. Not all
#              node types can be repeated.>>
#{lnat} << According to the ltype (see \code{help8ltype}) can be any
#        component of \code{rbsb.sna}. When
#        rbsb.cha0, the standard nature associated to \code{ltype}.
#        When it is a multivariate node, it must be a vector with
#        the corresponding natures. For 'empidata' it is given by the
#        slot \code{@nat} of \code{lwin}.>>
#{lvar} << This argument can be used either for multivariate
#        nodes, or for univariate nodes which are repeated (indeed it
#        is the way to indicate repetitions which are then possible
#        only for univariate distribution.\cr
#        In both case, lvar provides the vector of variable names of the
#        node. Its length gives the number of variables within the
#        added node. Standard probability distributions are
#        associated with a number of variables and it will be checked.
#        For 'empidata' it is given by the names of
#        slot \code{@nat} of \code{lwin}.>>
#{lparent} << parent vector at the node level; in most 
#             cases these are deduced from the other arguments.>>
#{lpod} <<  When rbsb.lis0, possible domain(s) associated to ltype
#         is provided (see rbsb.tlk).\cr
#         This must be a list with as many components as variables
#         in the added node. Each component of the list is associated
#         with a variable of the node (in the same order as given in
#         slot lvar. When there is only one variable, can be a vector 
#         instead of a list with a unique element which is a vector.\cr
#         (*) when lnat== 'cateo' or 'categ',
#           as many character strings as the number of categories.
#           Be aware that the length of lpod provides also the number
#           of categories!
#         (*) when lnat=="integ" the lower and upper bounds of the
#           variation of the integer variable (e.g. c(0,17))
#         (*) when lnat=="conti" the strict lower and upper bounds of the
#           variation of the continuous variable (e.g. c(-Inf,25.5))
#{lred} << Idem of lpod but for representation domain instead of 
#                 possible domaine>>
#{lcod} << Idem of lpod but for common domain instead of 
#                 possible domaine>>
#{ltransfo} << a simple way to define a transformation of the
#             drawn values by some easyprogramming string.
#             Also "[|2|]" means rounding with 2 decimals and
#             remind that the alias for the node under
#             creation is "*Y*".>>
#{ldaf} << in the case of data.frame distributions provides the 
#           associated /daf/.>>
#{lwin} << /win/ object describing the way to use a data frame based
#           distribution. See the description of this object for more details.>>
#{lfunct} << can be a function to be used as simulating
#           function when ltype is \code{program}.>>
#VALUE
# The generated 'alk'
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_04
#REVISED 10_06_11
#--------------------------------------------
{
# very raw checking of the types
if (rbsb.mck) {
    check4tyle(ltype,"character",1);
}
#
if (!isvide(lpara)) { if (!is.list(lpara)) {
    # ??? is still used this possibility???
    check4tyle(ltype,"character",-1);
}} else { lpara <- rbsb.lis0; }
#
if (!isvide(lrep)) {
    check4tyle(lrep,"numeric",-1);
} else { lrep <- 0;}
#
if (!isvide(lnat)) {
    check4tyle(lnat,"character",-1);
    if (!(all(lnat %in% rbsb.sna))) {
        erreur(list(lnat,rbsb.sna),"lnat must belong to rbsb.sna");
    }
} else { lnat <- rbsb.cha0;}
#
if (!isvide(lvar)) {
    check4tyle(lvar,"character",-1);
} else { lvar <- "";} # small exception because isvide("") is TRUE
#
if (!isvide(lparent)) {
    check4tyle(lparent,"character",-1);
} else { lparent <- rbsb.cha0;}
#
if (!isvide(lpod)) { 
    check4tyle(lpod,"list",-1);
} else { lpod <- rbsb.lis0;}
#
if (!isvide(lred)) { 
    check4tyle(lred,"list",-1);
} else { lred <- rbsb.lis0;}
#
if (!isvide(lcod)) {
    check4tyle(lcod,"list",-1);
} else { lcod <- rbsb.lis0;}
#
if (!isvide(ltransfo)) { 
    check4tyle(ltransfo,"character",-1);
} else { ltransfo <- "";}
#
if (!isvide(ldaf)) {
    if (rbsb.mck) {check4valid(valid8daf(ldaf));}
} else { ldaf <- rbsb.daf0;}
#
if (!isvide(lwin)) {
    if (rbsb.mck) {check4valid(valid8win(lwin));}
} else { 
    lwin <- rbsb.win0;
}
#
if (ltype=="program") {
    if (!is.function(lfunct)) {
        erreur(lfunct,"For 'program' ltype, a function must be provided");
    }
    if (identical(lfunct,rbsb.fun0)) {
        erreur(lfunct,"For 'program' ltype, a TRUE function must be provided");
    }
} else { lfunct <- rbsb.fun0;}
##################################################
#
# checking the ltype
if (!(ltype %in% rbsb.ltn)) {
    cat("Acceptable 'ltype's are:",rbsb.ltn,"\n");
    erreur(ltype,"This one does not belong to the list!");
}
##################################################
#
# first checking of lrep
if (lrep > 0) {
    if (isalkrepeatable(ltype)) {
        if (isvide(lvar)) { lvar <- var3standard(lrep);}
        if (length(lnat)==1) { lnat <- rep(lnat,lrep);}
    } else {
        erreur(list(ltype,lrep),
               "This non-repeatable node has got lrep > 0");
    }
} else { lrep <- 0;}
#
# checking and completing the ldes
ldes <- char2des(ldes);
#
# if necessary computing the parents at the node level from the definition
if ("lparent" %in% rbsb.tlk[[ltype]][["argno"]]) {
if (identical(rbsb.tlk[[ltype]]$lparent,rbsb.cac)) {
    if (ltype=="empidata") {
        # from the associated /win/
        lparentnod <- parents8win(lwin);
    } else {
        # from the parameters and the transformation
        tout <- "";
        if (!isvide(lpara)) {
            tout <- paste(tout,
                 paste(sapply(lpara,paste,collapse=" "),collapse=" "));
        }
        if (!isvide(ltransfo)) {
            tout <- paste(tout,
                          paste(ltransfo,collapse=" "))
        }
        dd <- easyp3cut(tout,rbsb.cpt);
        lparentnod <- dd$blo[dd$typ==1];
    }
}}
#
# the other arguments
# doing first the correspondance for an automatic check
# the order is of importance for the filling of the
# default values made just afterwards and more
# this order can depend of ltype...
argu <- list(lpara=lpara,lrep=lrep,lvar=lvar,
             lparent=lparent,
             ldaf=ldaf,lwin=lwin,
             lfunct=lfunct,lnat=lnat,lpod=lpod,
             ltransfo=ltransfo,
             lred=lred,lcod=lcod);
if (ltype %in% c("program","empidata")) {
    argu <- list(lpara=lpara,lvar=lvar,lrep=lrep,
                 lparent=lparent,
		 ldaf=ldaf,lwin=lwin,
		 lfunct=lfunct,lnat=lnat,lpod=lpod,
		 ltransfo=ltransfo,
		 lred=lred,lcod=lcod);
}
# -------------------------------------
# from now only argu values are used !
for (iii in sjl(argu)) {
    arg <- names(argu)[iii];
    argument <- argu[[arg]];
    quoi <- rbsb.l_a[ltype,arg];
    #==================================
    # compulsory arguments
    #==================================
    if (quoi =="YES") {
        if (isvide(argument)) {
            if (arg!="lvar") { 
                cat("With ltype =",ltype,"\n");
                erreur(argument,"The argument",arg,"must be provided (see 'help8ltype')");
            }
        } else {
            # checking some consistencies for repeatable distributions
            if (isalkrepeatable(ltype)) {
              if (arg == "lpara") {
                mle <- 1;
                # all components must have the same length or one
                for (aaa in sjl(argu[["lpara"]])) {
                    mle <- max(mle,length(argu[["lpara"]][[aaa]]));
                    if (!(length(argu[["lpara"]][[aaa]]) %in% c(1,mle))) {
                        erreur(argu[["lpara"]],"The components of 'lpara' does not have the same length or length one!");
                    }
                }
                if (argu[["lrep"]] != 0) {
                    if ( (mle != argu[["lrep"]]) & (mle>1) ) {
                        erreur(list(lrep,lpara),"lrep and lpara are not consistent for a repeatable link");
                    }
                }
                if (mle > 1) { argu[["lrep"]] <- mle; }
              }
            }
        }
    }
    #==================================
    # possible but not compulsory arguments
    #==================================
    if (quoi =="yes") {
      if (isvide(argument)) {
        if (arg=="lred")     { argu[[arg]] <- argu[["lpod"]];}
        if (arg=="lcod")     { argu[[arg]] <- argu[["lred"]];}
        if (arg=="ltransfo") { argu[[arg]] <- "";}
        if (arg=="lnat")     { argu[[arg]] <- "conti";}
        if (arg=="lparent")  { argu[[arg]] <- rbsb.cha0;}
        if (arg=="lrep")     { argu[[arg]] <- 0;}
        if (arg=="lvar")     {
            if (isalkrepeatable(ltype)) {
                # variable are no more than repetitions
                argu[[arg]] <- var3standard(argu[["lrep"]]);
            } else {
                # it must be a multivariate node
                nnv <- -1;
                if (ltype == "multinomial") { nnv <- length(argu[["lpara"]]$p); }
                if (ltype == "Dirichlet")   { nnv <- length(argu[["lpara"]]$a); }
                if (ltype == "numcat")      { nnv <- 1; }
                if (ltype == "parcat")      { nnv <- 1; }
                if (nnv < 0) { rapport(paste("Some code to add here in new8alk for the case of ltype =",ltype)); }
                argu[[arg]] <- var3standard(nnv,"~","~");
            }
        }
        if (arg=="lwin")       {
             if (isvide(argu[["lwin"]])) {
                 lpapa <- length(argu[["lparent"]]);
                 argu[["lwin"]] <- rbsb.win0;
                 argu[["lwin"]]@swg <- rep(1,lpapa);
             }
        }
      } else {
        # nothing for the moment
      }
    }
    #==================================
    # forbidden arguments
    #==================================
    if (quoi %in% c("-","NO","no")) { if (!isvide(argument)) {
        # the exception of the null value for the node dimension???
        protester <- TRUE;
        if (arg=="lrep") { if (argument==0) { protester <- FALSE; }}
        if (protester) {
            cat("With ltype =",ltype,"\n");
            erreur(argument,"The argument",arg,"must not be provided by the user (see 'help8ltype')",w=TRUE);
        }
    }}
    #==================================
    if (quoi %in% c("NO")) {
        vvv <-  rbsb.tlk[[ltype]][[arg]];
        if (!is.null(vvv)) {argu[[arg]] <- vvv; }
        if (arg=="lvar")     {
            if (isalkrepeatable(ltype)) {
                # variable are no more than repetitions
                argu[[arg]] <- var3standard(argu[["lrep"]]);
            }
        }
    }
    #==================================
    if (quoi %in% c("no")) {
        # the exception of the parentship previoulsy done
        if (arg=="lparent") {
            if (ltype != "empidata") {
                argu[[arg]] <- lparentnod;
            }
        } else {
            vvv <-  rbsb.tlk[[ltype]][[arg]];
	    if (is.null(vvv)) {
		meme <- paste("This parameter (",arg,
			  ") must be provided in rbsb.tlk",
			  "for the links {",ltype,"}");
		rapport(meme);
	    } else {
		eval(parse(text=vvv));
	    }
        }
    }
}
#
#==================================
#==================================
# further adjustments to introduce 
# specificities of the different
# ltypes...
#==================================
#==================================
#
# giving a standard name to the non-existing variable
if (length(argu[["lvar"]])==0) { argu[["lvar"]] <- "";}
# repeating if necessary the domains for multidimensional nodes
nbv <- max(1,length(argu[["lvar"]]));
if (ltype == "empidata") { nbv <- length(argu[["lwin"]]@nat);}
for (arg in c("lpod","lred","lcod")) {
     if (length(argu[[arg]]) != nbv) {
         if (length(argu[[arg]]) != 1) {
             erreur(list(argu[["lvar"]],arg,argu[[arg]]),"The number of domains must be ONE or the number of variables");
         } else {
             llpp <- vector("list",nbv);
             for (ss in sjl(llpp)) { llpp[[ss]] <- argu[[arg]][[1]];}
             argu[[arg]] <- llpp;
         }
     }
}
# adjusting the number of variables according
# to the repetitions
nbv <- argu[["lrep"]];
if (nbv>1) {
    if (length(argu[["lnat"]]) == 1) {
        argu[["lnat"]] <- rep(argu[["lnat"]],nbv);
    }
    if (isvide(argu[["lvar"]])) {
        argu[["lvar"]] <- var3standard(nbv);
    }
}
# generating the object
res <- new("alk",
           ldes     = ldes,
           ltype    = ltype,
           lrep     = argu[["lrep"]],
           lpara    = argu[["lpara"]],
           lvar     = argu[["lvar"]],
           ltransfo = argu[["ltransfo"]],
           lnat     = argu[["lnat"]],
           lpod     = argu[["lpod"]],
           lred     = argu[["lred"]],
           lcod     = argu[["lcod"]],
           lparent  = argu[["lparent"]],
           lfunct   = argu[["lfunct"]],
           ldaf      = argu[["ldaf"]],
           lwin     = argu[["lwin"]],
           lcomp    = FALSE
          );
if (rbsb.mck) {check4valid(valid8alk(res));}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2alk <- function(li,name="-?-")
#TITLE  transforms a consistent list into a new alk
#DESCRIPTION (ba)
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of an alk object. The produced alk is not completed
# since it is defined independently of any bn.
#DETAILS
# It is worth noticing that there is some shortcuts for
# defining the \code{@ldes} slots of the resulting /alk/.
# (i) The logical way: a \code{li$ldes} component exists
# which can be transmitted to the function \code{list2des}. (ii.a)
# if not the possible slots (as \code{@defi}, \code{@orig},...) 
# are available at the first level of \code{li}. (ii.b) the
# compulsory slot \code{@name} can be also present as 
# \code{li$name} or given by the argument \code{name}.\cr
# The complete list of standard already programmed distributions is given
# with help8ltype function.\cr
# The main use of this function is to tackle \code{alk} read from text files
# with the function \code{file2list}. It is mainly used
# in \code{read8bn} function.
#KEYWORDS misc
#PKEYWORDS alk nd
#INPUTS
#{li} <<The list to be transformed into an alk object.>>
#[INPUTS]
#{name} << gives the name of the node
#         when li$name does not exist.>>
#VALUE
# The generated 'alk'
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# list2alk(alk2list(rbsb.alk0));
# list2alk(alk2list(rbsb.alk1));
# list2alk(alk2list(rbsb.alk2));
#REFERENCE
#SEE ALSO
#CALLING {new8alk} 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_17
#REVISED 10_09_01
#--------------------------------------------
{
#
# getting the description
if ("ldes" %in% names(li)) {
    # (i) the description is given as such
    des <- list2des(li$ldes);
} else {
    # (ii) the description is at the first level
    if (!("name" %in% names(li))) {
        # (ii.a) the compulsory slot does not exist
        li$name <- name;
    }
    des <- list2des(li);
}
if (rbsb.mck) {check4valid(valid8des(des));}
#
# checking the type
if (isvide(li$ltype)) {
    erreur(li,"ltype is compulsory");
} else {
    if (!(li$ltype %in% rbsb.ltn)) {
        cat("Known ltype's are:",rbsb.ltn,"\n");
        erreur(li$ltype,"This ltype is not (yet) implemented!");
    }
}
# getting the parameters
# (taken as they are)
#
# getting the transformation
if(isvide(li$ltransfo)) { li$transfo <- "";
} else { li$ltransfo <- li$ltransfo;}
#
# getting the repetition indicator
if(!isvide(li$lrep)) { li$lrep <- as.numeric(li$lrep);
} else { li$lrep <- 0;}
#
# getting the variable names
sansvana <- FALSE;
if(isvide(li$lvar)) { 
    if (li$ltype!="empidata") {
        # forseeing the case without variable name
        li$lvar <- "";
        navar <- li$lvar;
        sansvana <- TRUE;
    } else {
        # in case of 'empidata', variable names are in the lwin!
        navar <- names(char2vma(li$lwin$nat,rbsb.vma["V"]));
    }
} else {
    navar <- li$lvar;
}
#
# getting the variable natures
if(isvide(li$lnat)) { li$lnat <- rbsb.cha0;}
#
# getting the different domains
#||||||||||||||||||||||||||||||
domaines <- function(dom,navar,sansvana)
#{dom} list corresponding to the domain
#{navar} variable name
#{sansvana} TRUE when there is no variable name
{ 
    # variable number
    nbvar <- length(navar);
    if(!isvide(dom)) {
        if (sansvana) {
            # when there are not variable nname
            # the domain can be given as a vector
            if (!is.list(dom)) { dom <- list(dom);}
        } else {
            # if not a complete correspondance must exist
            # with the variable names
            if (length(dom) != nbvar) {
	        erreur(list(navar,dom),
                   "The length of 'lpod' is not equal to the variable number!");
	    }
	    if (length(union(names(dom),navar)) != nbvar) {
	        erreur(list(navar,names(dom)),
                   "'lpod' names does not correspond to the variable names");
	    }
            dom <- dom[navar];
            # translation to numeric is made after the creation of the /alk/
            # because the nature must be deduced from the type
        }
    }
    dom;
}
#||||||||||||||||||||||||||||||
li$lpod <- domaines(li$lpod,navar,sansvana);
li$lred <- domaines(li$lred,navar,sansvana);
li$lcod <- domaines(li$lcod,navar,sansvana);
#
# getting the node parent names
if(isvide(li$lparent)) { li$lparent <- rbsb.cha0;}
#
# getting the easyprogramming definition
# (in fact as it is)
#
# getting the already defined function
if(!isvide(li$lfunct)) {
    if (is.character(li$lfunct)) {
        coda <- paste("li$lfunct <-",li$lfunct);
        eval(parse(text=coda));
    } else {
        erreur(li$funct,"Through this way, must be a character indicating the name of a preexisting function");
    }
} else {
    li$lfunct <- rbsb.fun0;
}
#
# getting the win
if (is.list(li$lwin)) {
    li$lwin <- list2win(li$lwin);
}
#
# getting the daf
if (is.list(li$ldaf)) {
    li$ldaf <- list2daf(li$ldaf);
}
#
# At last creating the alk
#
res <- new8alk(des,
               ltype=li$ltype,
               lpara=li$lpara,ltransfo=li$ltransfo,
               lrep=li$lrep,lvar=li$lvar,
               lparent=li$lparent,
               lfunct=li$lfunct,
               ldaf=li$ldaf,lwin=li$lwin,
               lnat=li$lnat,
               lpod=li$lpod,lred=li$lred,lcod=li$lcod
              );
#
# some special cases (I dislike them!)
if (res@ltype == "numcat") {
    if (is(res@lpara$p,"faux")) {
        erreur(res@lpara$p,"The probability vector/table was not properly understood");
    }
    if ((length(res@lpara$p)%%length(res@lpod))!=0) {
        form3affiche(res@lpod);
        erreur(res@lpara,"Lengths of lpod and lpara are not compatible");
    }
    res@lpara$p <- as.numeric(res@lpara$p);
    res@lpara$p <- matrix(res@lpara$p,ncol=length(res@lpod[[1]]));
}
#
# making numeric the domains for 'numeric' variables
if (res@ltype == "empidata") {
    nanat <- res@lwin@nat;
} else {
    nanat <- res@lnat;
}
for (nunu in sjl(nanat)) {
    if (rbsb.snp[nanat[nunu],"numeric"]) {
        # here the NA warned were generated
        res@lpod[[nunu]] <- as.numeric(res@lpod[[nunu]]);
        res@lred[[nunu]] <- as.numeric(res@lred[[nunu]]);
        res@lcod[[nunu]] <- as.numeric(res@lcod[[nunu]]);
    }
}
#
# checking the result
if (rbsb.mck) {check4valid(valid8alk(res));}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
alk2list <- function(alk)
#TITLE  transforms a /alk/ object into a list
#DESCRIPTION (ba)
# More or less the inverse operation of \code{list2alk}.
#DETAILS
#KEYWORDS
#PKEYWORDS alk nd
#INPUTS
#{alk} <<The alk to be transformed into a list.>>
#[INPUTS]
#VALUE
# The resulting list.
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# alk2list(rbsb.alk0);
# alk2list(rbsb.alk1);
# alk2list(rbsb.alk2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_08
#REVISED 10_06_24
#--------------------------------------------
{
# checking
uu <- check8alk(alk);
if (!identical(uu,TRUE)) {
    str(alk);
    erreur(uu,"Not acceptable /alk/!");
}
# initializing
res <- vector("list",0);
# getting the description
res <- as.list(c(res,des2list(alk@ldes)));
# getting the type
lty <- slot(alk,"ltype");
res[["ltype"]] <- lty;
res[["ldaf"]] <- daf2list(alk@ldaf);
res[["lwin"]] <- win2list(alk@lwin);
# the list of components except 'ltype' and 'ldaf'
eaco <- c("lpara","ltransfo",
          "lrep","lvar","lnat",
          "lpod","lred","lcod",
          "lparent",
          "lfunct","lcomp");
for (ec in eaco) {
    uu <- slot(alk,ec);
    vv <- rbsb.l_a[lty,ec];
    if ((vv == "YES") | (vv == "yes")) {
        if (!isvide(uu)) { res[[ec]] <- uu;}
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbrep4alk <- function(alk,nom)
#TITLE  determines the number of repetitions of an alk
#DESCRIPTION (ba)
# determines the number of repetitions of an alk, taking
# care of its possible parents.
#DETAILS
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{alk} <<alk object>>
#{nom} <<nom where the possible parents of the node can be found.>>
#[INPUTS]
#VALUE
# the number of repetitions: 0 for non repeatable
# nodes; the dimension for repeatable nodes
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_05_01
#REVISED 09_06_15
#--------------------------------------------
{
# no check is made at this level
res <- 0;
if (isalkrepeatable(alk@ltype)) {
    if ((isvide(alk@lrep))|alk@lrep==0) {
    res <- 1;
    # looking for the dimension implied by the node parents
    for (pp in alk@lpara) {
        rr <- easyps2dim(as.character(pp),nom);
        if (res==1) { res <- rr;
        } else { if (!(rr %in% c(1,res))) {
            erreur(list(nom,alk@lpara),
                   "These parameters lead to inconsistent number of repetitions"
                  );
        }}
    }
    } else {
        check4tyle(alk@lrep,"integer",1);
        res <- alk@lrep;
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
var4alk <- function(alk,nom,nbrep)
#TITLE  finds the variable names for an alk.
#DESCRIPTION (ba)
# They can be already given, in that case they
# are not modified
#DETAILS
# In case of repetition, the compatibility of their
# number is given.
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{alk} <<alk object>>
#{nom} <<nom of the bn where the alk must inserted.>>
#{nbrep} <<repetition number already found by nbrep4alk.>>
#[INPUTS]
#VALUE
# the names of the alk variables (without the node name)
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_05_02
#REVISED 09_06_17
#--------------------------------------------
{
# no check is made at this level
#
# finding the names according to nbrep
if (nbrep==0) {
    # We are in the case of a non repeated node
    if (!isvide(alk@lvar)) {
        # the variable names are already established
        res <- alk@lvar;
    } else {
        # they are provided with respect to the 
        # dimension generated by the parent node
        # but specific cases must be filtered
        pdim <- easyps2dim(alk@lpara$p,nom);
        if (pdim==1) { res <- "";
        } else { res <- var3standard(pdim,"~","~");}
        if (alk@ltype%in%c("parcat","numcat")) {res <- "";}
    }
} else {
    # We are in the case of a repeated node
    if (isvide(alk@lvar)) {
        # standard names
        res <- var3standard(nbrep);
    } else {
        # names are provided by the user
        if (length(alk@lvar)==nbrep) {
            res <- alk@lvar;
        } else {
            erreur(list(alk@lvar,nbrep),
                   "The number of provided names is not in accordance with the detected number of repetitions"
                  );
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
new8noralk <- function(des,parent=character(0),
                           coef=0:length(parent),sigma=1)
#TITLE  very simplified version of new8alk
#DESCRIPTION (ba)
# In this simplified version of new8alk, the user has no
# more than giving the name and the parents of the node 
# and the regression coefficients.
# The distribution will be automatically put to
# 'normal' and the parameter 'mu' to the linear
# combinations of the parents.
#DETAILS
# length(coef) must equals 1 + length(parent);
# sigma must be numeric.
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{des} <<either the name or a description ('ds' object).>>
#[INPUTS]
#{parent} <<(\code{character(0)}) Character vector with the names of the parents.
#           \code{character(0)} means that there is no parents.>>
#{coef} << coefficient to define mu. If there are two 
#         parents, then mu will be
#         'coef[1]+coef[2]*{\{parent[1]}\}+coef[3]*{\{parent[2]}\}'.>>
#{sigma} << standard deviation.>>
#VALUE
# an object of class "bn" with an additional node
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(new8noralk(char2des("some /alk/"),parent=c("A","AA")));
#REFERENCE
#SEE ALSO new8alk
#CALLING {new8alk}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_03_03
#REVISED 09_03_18
#--------------------------------------------
{
# some checking
if (rbsb.mck) {
    check4tyle(coef, "numeric",-1);
    check4tyle(sigma,"numeric",-1);
    check4tyle(parent,"character",-1);
    if (length(coef)!=1+length(parent)) {
        erreur(list(coef,parent),"Non consistent lengths of coef and parent");
    }
    check4tyle(sigma,"any",1);
}
#
# preparing the description
des <- char2des(des);
# preparing the parameter
if (length(parent) == 0) { mu <- as.character(coef);
} else {
    parent <- paste(rbsb.cpt["nodes","opening"],
                    parent,
                    rbsb.cpt["nodes","closing"],
                    sep="");
    parent <- paste(coef[-1],parent,sep="*",collapse=" + ");
    mu <- paste(coef[1],"+",parent);
}
# preparing the alk
alk <- new8alk(des,ltype="normal",
               lpara=list(mu=mu,sigma=sigma),
               lpod=list(c(-Inf,Inf)),
               lred=list(c(-100,100)));
# checking the result (a report should be advocated...)
if (rbsb.mck) {check4valid(valid8alk(alk));}
# returning
alk;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp2code1 <- function(eas,nom,iin,transfo=FALSE,bugs=FALSE)
#TITLE expands an easyp expression
#DESCRIPTION (bn)
# Expands an easyp expression in the context precised by \code{nom}
# for the dimensions. The aim is to give the function easyp2code2 
# an easier input. Mainly checks are performed about the consistency
# between the proposed \code{eas} and the involved parents. Also
# \code{eas} is vectorized when its parentship asks for it.
#DETAILS
# Notice that rbsb.cni node is escaped from the variable/node names.
# Also that the node dimension and possible variable was already 
# determined before calling the function.
#KEYWORDS utilities
#PKEYWORDS expression code
#INPUTS
#{eas} <<The easyp expression to deal with.>>
#{nom} <<The /nom/ giving the context of the expansion.>>
#{iin} <<node number concerned with \code{eas}.>>
#[INPUTS]
#{transfo} << Are rounding and transformation accepted?>>
#{bugs} << Must the code be bugs or R (not yet implemented).>>
#VALUE
# An interpretable character string to be proposed to easyp2code2
#EXAMPLE
# rsbn3k("RESET"); # (only for R checking)
# easyp2code1("{{A}}",rbsb.nom2,2);
# easyp2code1(1234,rbsb.nom2,2);
# easyp2code1("1234",rbsb.nom2,2);
# easyp2code1("2*pi",rbsb.nom2,2);
# easyp2code1("1+sqrt({{A}})",rbsb.nom2,2);
# easyp2code1("1+sqrt({{A}}*{{B}})",rbsb.nom2,2);
# easyp2code1(c(1234,5678),rbsb.nom2,2);
# \dontrun{easyp2code1(c("{{A}}","{{B}}"),rbsb.nom2,2);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# the bugs case is to be made.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_09
#REVISED 10_07_13
#--------------------------------------------
{
# checking and going to the character style
if (rbsb.mck) {
    check4tyle(eas,c("numeric","character"),-1);
    check4valid(valid8nom(nom));
}
#
if (is.numeric(eas)) { eas <- as.character(eas); }
#
# dealing with empty easyp code: character(0) or only spaces.
neas <- length(eas);
RET <- 0;
for (i in sjl(eas)) {
    if (eas[i] == paste(rep(" ",nchar(eas[i])),collapse="")) { RET <- RET+1;}
}
if (RET == neas) { return("");}
#
# getting the declared dimension of the node
#
cdim <- nbnv(nom,iin);
#
# getting the dimensions, names of the involved parents into
# 'eas' gathered into 'tout'
#
#  determining the dimension of the parents if any
# 'pdim' for parent dimensions and 'pnam' for parent names
# 'nbp' for number of parents and 'pvnam' for parent variable names
pdim <- numeric(0); pnam <- character(0);
nbp <- 0; pvnam <- vector("list",0);
tout <- paste(eas,collapse="");
dd <- easyp3cut(tout,rbsb.cpt);
# loop over each component associated to a node which is not rbsb.cni
for (jj in sjl(dd$typ)) { if (dd$typ[jj]==1) { if (dd$blo[jj]!=rbsb.cni) {
    # Indeed it is a node, the number of parents
    # must be incremented
    nbp <- nbp+1;
    # 'quel' stores the parent name and pnam cumulates them
    quel <- dd$blo[jj];
    pnam <- c(pnam,quel);
    # 'que' is 'quel' limited to the node names
    que <- nv2nod(quel);
    if (que==quel) {
        # It is a complete node (= no variable specified)
        # then its number of variables must be investigated
        # here obtained into 'nono'
        nunu <- which(nanv(nom)==que);
        nono <- nbnv(nom,nunu);
        if ((neas>1)&(nono>1)) {
            # when eas is multivariate, complete parent nodes must be univariate
            erreur(eas,"One of the components of this multivariate easyp has got a multivariate node");
        } else {
            pdim <- c(pdim,nono);
            vava <- nanv(nom,nunu);
            if (length(vava)==1) { vava <- rep(vava,cdim);}
            pvnam[[nbp]] <- vava;
        }
    } else {
        # it is a variable of multivariate node then
        pdim <- c(pdim,1);
        pvnam[[nbp]] <- "";
    }
}}}
#
# naming the parent variable names list
# result of the investigation (they must have 'nbp' of them)
# Also 'pdim' contains their own dimensions
#
names(pvnam) <- pnam;
#
###############################################
if (neas > 1) {
    # when eas is already a vector, it is returned as it is
    # but previously, it is checked that no parents are 
    # multivariate
    if (nbp > 0) { if (max(pdim) > 1) {
        cat("Parent Names:",pnam,"\n");
        cat("Parent Dimensions:",pdim,"\n");
        erreur(eas,"For multiple parameters, the parents must all be univariate");
    }}
} else {
    # when eas is scalar, according to the parents
    # it can be expanded.
    if (length(pdim)>0) {
        # all dimensions must be one or equal
        ppxx <- max(pdim);
        if ((sum(pdim==ppxx)+sum(pdim==1))/(1+(ppxx==1))!=length(pdim)) {
            form3affiche(nom);
            erreur(list(eas,pdim),
             "Not all dimension of pseudo-parents are equal to the max or to one");
        }
        # constructing the expanded eas
        eas <- rep(eas,ppxx);
    } else { ppxx <- 1;}
    # introducing the variables when necessary
    for (ii in sj(ppxx)) {
        dd <- easyp3cut(eas[ii],rbsb.cpt);
        dp <- which(dd$typ==1);
        for (jj in dp) {
          quel <- dd$blo[jj];
          if (quel!=rbsb.cni) {
            que <- nv2nod(quel);
            if (quel==que) {
                # the detailed variable name must be introduced
                # for R being able to recognize the column names of X
                # It was previously stored in the list 'pvnam'
                #
                dd$blo[jj] <- pvnam[[dd$blo[jj]]][ii];
            }
          }
        }
        eas[ii] <- easyp3stickback(dd,rbsb.cpt);
    }
    # In transfo context, no expansion must be made ???
    if (transfo) { eas <- eas[1];}
}
# returning
eas;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp2code2 <- function(eas,transfo=TRUE,bugs=FALSE)
#TITLE  transforms an easyp expression into an R/bugs block
#DESCRIPTION (ba)
# Numeric values are accepted as easyp expressions. Length(eas)
# can be greater than one, in that case a matrix comprising
# as many columns will be returned.
# Rounding and transformation are options.\cr
# The code where Bugs code is constructed is not yet available.
#DETAILS
# In rebastaba perspective, this function must be called to 
# define the parameters of the standard nodes like "normal",
# "Bernoulli", "multinomial" and so on... But the analysis
# of the different types of parents is supposed to be 
# already done, and \code{eas} accordingly prepared. For instance
# repeated parents are already expanded.\cr
# For each component it is determined if the expression is vectorial when not
# a repetition of \code{nrow(X)} is added as well as a \code{cbind}.
# When more than one component, a nesting \code{cbind} is added
# to produce a matrix. Vectorial components are identified with
# the first two rows of \code{rbsb.cpt} matrix, respectively variable name
# (with the special case of \code{rbsb.cni}) and rounding transformation.
# 
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{eas} <<either a numeric or a rebastaba expression (character).
#      Its length can be greater than one for repeated standard
#      scalar distributions or vector parameters of other
#      distributions.>>
#[INPUTS]
#{transfo} <<(\code{logical(1)}) Are transformations and Y itself admitted?>>
#{bugs} <<(\code{logical(1)}) Must the code be bugs or R (default). Not yet available.>>
#VALUE
# An interpretable character string to be included
# when generating code (see the following examples).
#EXAMPLE
# rsba3k("RESET"); # To comply the R checking
# easyp2code2(1234)               # "{cbind(rep(1234,nrow(X)))}" 
# easyp2code2("1234")             # "{cbind(rep(1234,nrow(X)))}" 
# easyp2code2("2*pi")             # "{cbind(rep(2*pi,nrow(X)))}"
# easyp2code2("1+sqrt({{A}})")    # "{1 + sqrt(X[,'A'])}" 
# easyp2code2("{{*Y*}}+1")        # "{Y+1}"
# easyp2code2(11:12)              # "{cbind(rep(11,nrow(X)),rep(12,nrow(X)))}"
# easyp2code2("(|2|)")            # "{round(Y,2)}"
# easyp2code2(c("{{A}}","(|2|)")) # "{cbind(X[,'A'],round(Y,2))}"
# easyp2code2("{{POPU[wgt]}}*10000/{{POPU[hgt]}}^2")
#REFERENCE
#SEE ALSO easyp2code1
#CALLING
#COMMENT
# the bugs case is to be made.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_09
#REVISED 10_07_13
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(eas,c("numeric","character"),-1);
}
if (bugs) {
    rapport("easyp2code2: sorry, the functionality 'bugs' is not yet implemented");
}
### constants for construction
# around constants
opaco  <- "cbind(rep("; cpaco <- ",nrow(X)))";
# around parents
opapa  <- "X[,'"; cpapa <- "']";
# around rounding
oprou  <- "round(Y,"; cprou <- ")";
# going to the character style
if (is.numeric(eas)) { eas <- as.character(eas); }
#
# dealing with empty easyp code: character(0) or only spaces.
RET <- 0;
for (i in sjl(eas)) {
    if (eas[i] == paste(rep(" ",nchar(eas[i])),collapse="")) { RET <- RET+1;}
}
if (RET == length(eas)) { return("");}
#
##########################################
axil <- function(dd,transfo) {
    #
    # listing the admissible types in admi
    admi <- 0:1;
    if (transfo) { admi <- 0:2;}
    #
    # some checkings
    if (length(unique(c(dd$typ,admi))) != length(admi)) {
	form3affiche(dd);
        form3affiche(eas);
	rapport("Check this piece of easyprogramming!");
    } 
    if (length(dd$blo) == 0) {
        form3affiche(eas);
	rapport("An empty easyp expression was not detected by easyp2code2!");
    }
    if ((sum(dd$blo[dd$typ==1]==rbsb.cni) > 0) & (!transfo)) {
	erreur(eas,"At this level, the node itself is not expected!");
    }
    if ((2 %in% dd$typ) & !transfo){
	erreur(eas,"At this level, no rounding is expected!");
    }
    #
    # translating
    nbb <- length(dd$typ);
    res <- rep("",nbb);
    vecteur <- FALSE;
    for ( ip in sj(nbb)) {
        quoi <- dd$typ[ip];
        if (quoi == 0) {
            res[ip] <- dd$blo[ip];
        }
        if (quoi == 1) {
            if (dd$blo[ip] == rbsb.cni) {
                res[ip] <- "Y"; 
            } else {
                res[ip] <- paste(opapa,dd$blo[ip],cpapa,sep="");
            }
            vecteur <- TRUE;
        }
        if (quoi == 2) {
            res[ip] <- paste(oprou,dd$blo[ip],cprou,sep="");
            vecteur <- TRUE;
        }
    }
    res <- paste(res,collapse="");
    if (!vecteur) {
        # the vector structure must be imposed
        res <- paste(opaco,res,cpaco,sep="");
    }
res;
} # end of 'axil'
##########################################
#
# about the eas to be analyzed
neas <- length(eas);
#
#
# when there is only one component
if (neas == 1) {
    # analyzing the expression
    dd <- easyp3cut(eas,rbsb.cpt);
    # checking and translating
    res <- axil(dd,transfo);
    # returning
    res <- paste(form3ind(2,FALSE),"{",
                 form3ind(3),res,";",
                 form3ind(2),"}",
                 sep="");
} else {
    # preparing the coding of the more than one components
    rres <- rep("",neas);
    # translating each eas component
    for (nn in sj(neas)) {
        # analyzing the expression under consideration
        dd <- easyp3cut(eas[nn],rbsb.cpt);
        # checking and translating
        rres[nn] <- axil(dd,transfo);
    } # ending the loop over more than one component
    # assembling
    nive <- 2;
    rr <- paste(form3ind(0+nive,FALSE),"{",
                form3ind(1+nive),"cbind(",
                sep="");
    for (nn in sj(neas)) {
        rr <- paste(rr,form3ind(2+nive),rres[nn],sep="");
        if (nn < neas) { rr <- paste(rr,",",sep=""); }
    }
    res <- paste(rr,
                 form3ind(1+nive),");",
                 form3ind(0+nive),"}",
                 sep="");
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
alkvar <- function(alk)
#TITLE returns the variable names of an /alk/
#DESCRIPTION(ba) 
# Whatever is the state (completed or not) of
# an /alk/ returns the names of its variable
# names.
#DETAILS
# No check, this function is to be used for
# checking functions
#KEYWORDS 
#PKEYWORDS 
#INPUTS
#{alk}<<The alk object>>
#[INPUTS]
#VALUE
# A \code{character} containing the natures of the variables
#EXAMPLE
# rsba3k("RESET"); # for R checking
# alkvar(rbsb.alk2)
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_09_03
#REVISED 10_09_03
#--------------------------------------------
{
if (alk@ltype %in% c("empidata")) {
    # empidata or similar
    res <- names(alk@lwin@nat);
} else {
    # other cases
    if (length(alk@lvar) > 0) {
        # exisiting names
        res <- alk@lvar;
    } else {
        # no name
        res <- "";
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
