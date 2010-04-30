
rs003k("reset");

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rsba3k <- function(whi)
#TITLE (ba) assigns the rsba constants
#DESCRIPTION
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
# >>
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
#REVISED 09_12_28
#--------------------------------------------
{
# checking
if (!expr3present(whi,c("RESET","reset","names","definitions","values"))) {
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
sc["ion0"] <- "null /ion/"; 
sc["arc0"] <- "null /arc/"; 
sc["pam0"] <- "null /pam/"; 
sc["pam1"] <- "Example 1 of /pam/"; 
sc["win0"] <- "null /win/"; 
sc["alk0"] <- "null /alk/"; 
sc["alk1"] <- "Example 1 of /alk/ (without parent)"; 
sc["alk2"] <- "Example 2 of /alk/ (with parents)"; 
sc["pta0"] <- "null /pta/"; 
sc["pta1"] <- "Example 1 of /pta/"; 
sc["pgr"] <- "current /pgr/"; 
sc["pos"] <- "current /pos/"; 
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
	  argNO=c("lnat","lvar"),
	  argno=c("lparent"),
	  argyes=c("ltransfo","lrep","lred","lcod"),
	  argYES=c("lpara","lpod"),
	  par=list(mu="expectation",sigma="standard deviation"),
	  ltransfo="[[3]]",lrep=1,
	  lvar="",
	  lnat="conti",
	  lparent=rbsb.cac
	  ),
	###
	uniform=list(
	  defi="Classical univariate uniform distribution.",
	  fami="conti_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lnat","lvar"),
	  argno=c("lparent"),
	  argyes=c("ltransfo","lrep","lred","lcod"),
	  argYES=c("lpara","lpod"),
	  par=list(a="lower bound",b="upper bound"),
	  ltransfo="[[3]]",lrep=1,
	  lvar="",lnat="conti",lparent=rbsb.cac
	  ),
	###
	beta=list(
	  defi="Classical univariate beta distribution.",
	  fami="conti_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lnat","lvar"),
	  argno=c("lparent"),
	  argyes=c("ltransfo","lrep","lred","lcod"),
	  argYES=c("lpara","lpod"),
	  par=list(a="lower bound",b="upper bound"),
	  ltransfo="[[3]]",lrep=1,
	  lvar="",lnat="conti",lparent=rbsb.cac
	  ),
	###
	Bernoulli=list(
	  defi="Binary variable taking values 1 or 0",
	  fami="discr_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lvar","lnat","lpod","lred","lcod","ltransfo"),
	  argno=c("lparent"),
	  argyes=c("lrep"),
	  argYES=c("lpara"),
	  par=list(p="probability of 1"),
	  ltransfo="",lrep=1,
	  lpod=list(0:1),lred=list(0:1),lcod=list(0:1),
	  lvar="",lnat="integ",lparent=rbsb.cac
	  ),
	###
	binomial=list(
	  defi=c("Classical univariate distribution of number of successes.",
		 "Notice that 'lpod' must be given by the user since /rbsb/",
		 "cannot know in advance which can be the highest value of",
		 "the binomial size which can be inherited from other node",
		 "values."),
	  fami="discr_scalar",rep=TRUE,bugs=TRUE,
	  argNO=c("lvar","lnat"),
	  argno=c("lparent"),
	  argyes=c("lrep","lred","lcod"),
	  argYES=c("lpara","lpod"),
	  par=list(n="size",p="probability of success"),
	  ltransfo="",lrep=1,
	  lvar="",lnat="integ",lparent=rbsb.cac
	  ),
	###
	Dirac=list(
	  defi="Classical Dirac distribution: the variable can take only one value.",
	  fami="miscellaneous",rep=TRUE,bugs=TRUE,
	  argNO=c("lvar"),
	  argno=c("lparent"), 
	  argyes=c("ltransfo","lrep","lred","lcod","lnat"), # lnat must be limited to 'conti' or 'integ'
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
		 "Notice that 'lpod' must be given by the user since /rbsb/",
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
	  # lpod="ax<-c(0,argu[['lpara']]$n);nx<-length(argu[['lpara']]$p);argu[['lpod']]<-vector('list',nx);argu[['lpod']][1:nx]<-list(ax);", This cannot be computed through /rbsb/ since the upper value can depend of the parents
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
	  argno=c("lvar","lparent"),
	  argyes=c("lrep","lvar","ltransfo","lred","lcod"),
	  argYES=c("lnat","lpod","lpara"),
	  lvar="",lrep=1,
	  ltransfo="",lparent=rbsb.cac
	  ),
	###
	empidata=list(
	  defi=c("To allow the distribution to be drawn from available data sets"),
	  fami="data_based",rep=FALSE,bugs=FALSE,
	  argNO=character(0),
	  argno=c("lrep"),
	  argyes=c("lwin","lparent","lred","lcod"),
	  argYES=c("lnat","lpod","lvar","ldaf"),
	  lparent=character(0),
	  lwin=-1,
	  lrep=0
	  ),
	###
	popula=list(
	  defi=c("To allow the distribution to be imposed from some data set.",
		 "They are used systematically, right in the order of the",
		 "data frame, with cycling if necessary. This explains why",
		 "parent(s) is(are) not possible for this kind of link"),
	  fami="data_based",rep=FALSE,bugs=FALSE,
	  argNO=c("lparent"),
	  argno=c("lrep"),
	  argyes=c("lwin","lred","lcod"),
	  argYES=c("lnat","lpod","lvar","ldaf"),
	  lparent=character(0),
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
    rbsb.ltn <- names(rbsb.tlk);
#
rbsb.tlk_argu <- matrix("-",length(rbsb.tlk),3+length(rbsb.lta));
dimnames(rbsb.tlk_argu) <- list(rbsb.ltn,c("family","rep?","bugs?",rbsb.lta));
for (jbd in sjl(rbsb.tlk)) {
    if (!(rbsb.tlk[[jbd]]$fami %in% rbsb.fam_dis)) {
        erreur(list(names(rbsb.tlk)[jbd],rbsb.tlk[[jbd]]$fami),
               "This family is not registrated in rbsb.fam_dis!");
    } else {
        rbsb.tlk_argu[jbd,1] <- names(rbsb.fam_dis)[which(rbsb.tlk[[jbd]]$fami==rbsb.fam_dis)];
    }
    rbsb.tlk_argu[jbd,2] <- rbsb.tlk[[jbd]]$rep;
    rbsb.tlk_argu[jbd,3] <- rbsb.tlk[[jbd]]$bugs;
    for (jd in rbsb.tlk[[jbd]]$argNO) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rbsb.tlk)[jbd],"in argNO"));
        }
        rbsb.tlk_argu[jbd,jd] <- "NO";
    }
    for (jd in rbsb.tlk[[jbd]]$argno) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rbsb.tlk)[jbd],"in argno"));
        }
        rbsb.tlk_argu[jbd,jd] <- "no";
    }
    for (jd in rbsb.tlk[[jbd]]$argyes) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rbsb.tlk)[jbd],"in argyes"));
        }
        rbsb.tlk_argu[jbd,jd] <- "yes";
    }
    for (jd in rbsb.tlk[[jbd]]$argYES) {
        if (!(jd %in% rbsb.lta)) {
            form3affiche(rbsb.lta);
            form3affiche(jd);
            erreur(list(names(rbsb.tlk)[jbd],jd),paste("bad argument for",names(rbsb.tlk)[jbd],"in argYES"));
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
    assign("rbsb.ion0", new(
                      "ion",nn=character(0),vn=character(0),nvn=character(0),
                      nk=numeric(0),ij=numeric(0),vk=numeric(0),
                      iden=character(0)),pos=".GlobalEnv");
    assign("rbsb.arc0",new("arc",nbn=0,fle=matrix(NA,0,3)),pos=".GlobalEnv");
    assign("rbsb.pam0",new("pam",rlt=matrix(NA,0,0)),pos=".GlobalEnv");
    assign("rbsb.pam1",new("pam",rlt=matrix(c(rep(0,10),1,1,0,rep(c(0,0,0,0,1),2),1,0),5)),pos=".GlobalEnv");
    assign("rbsb.win0", new("win",wgt=numeric(0),k=2,di=c(0,1),nb=c(1,Inf),ty="random"),pos=".GlobalEnv");
    assign("rbsb.alk0",new("alk",ldes=new("des",name="AA"),
                                                      ltype="normal",lpara=list(mu=0,sigma=1),
                                                      lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                      lpod=list(c(-3,3)),ltransfo=character(0),
                                                      ldaf=rbsb.daf0,lwin=rbsb.win0,lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.alk1",new("alk",ldes=char2des("my pretty alk"),
                                                 ltype="normal",lpara=list(mu=10,sigma=1),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-14,14)),ltransfo="[[1]]",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.alk2",new("alk",ldes=char2des("simple alk with two parents"),
                                                 ltype="normal",lpara=list(mu="{{A}}",sigma="abs({{B[a]}})"),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-10,50)),ltransfo="[[1]]",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rbsb.pta0",new("pta",name="Null pta",
                                               vam="A",vac=character(0),
                                               vad=character(0),vav=character(0),
                                      kkk=2,pro=array(1:3,dim=3,dimnames=list(A=c("a","b","c")))
                                 ),pos=".GlobalEnv");
    assign("rbsb.pta1",new("pta",name="P.Table",vam=c("A","B"),vac=character(0),vad=character(0),kkk=2,pro=array(rep(0.05,20),dim=c(4,5),dimnames=list(A=letters[1:4],B=LETTERS[1:5]))),pos=".GlobalEnv");
    assign("rbsb.pgr",new("pgr"),pos=".GlobalEnv");
    assign("rbsb.pos",new("pos",
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
#TITLE (ba) checks a /nom/
#DESCRIPTION
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
#REVISED 09_10_11
#--------------------------------------------
{
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
#TITLE (ba) prints the node-variable names
#DESCRIPTION
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{what} <<(='v') what to print ('v'=node-variable names;
#         'n' only node names)
#{type} <<(=0) type of printing (0) a node a line;
#              (1) everything on the same line.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.nom2);
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
che <- valid8nom(x);
if (!identical(che,TRUE)) {
    erreur(che,"This /nom/ is not valid");
}
#
types <- 0:1;
if (!(type %in% types)) {
    erreur(list(types,type),"Bad argument");
}
if (nbnv(x) > 0) {
    if (type==0) {
        for (ii in names(x@x)) {
            cat(" ",ii);
            if (what=="v") {
                cat(form3liste(x@x[[ii]],none="",OPA=" (",CPA=")",opa="",cpa="",sep=","));
            }
        }
    cat("\n");
    }
    if (type==1) {
        for (ii in names(x@x)) {
            cat(form3justifie(ii,15,3),":");
            if (what=="v") {
                cat(form3liste(x@x[[ii]],none="",OPA="  (",CPA=")",opa="",cpa="",sep=", "))
            }
        cat("\n");
        }
    cat("\n");
    }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
length8nom <- function(x)
#TITLE (ba) returns the length of a 'nom' object
#DESCRIPTION
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
#TITLE (ba) checks a /ion/
#DESCRIPTION
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
#REVISED 09_10_12
#--------------------------------------------
{
    res <- character(0);
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
#TITLE (ba) returns the length of a 'ion' object
#DESCRIPTION
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
#TITLE (ba) prints a 'ion' object
#DESCRIPTION
# print associated to a \code{/ion/} object.
#DETAILS
#KEYWORDS print
#PKEYWORDS 
#INPUTS
#{x}<<the 'ion' object to be printed.>>
#[INPUTS]
#{how} <<(='n') the way to make the printing:
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
#TITLE (ba) checks a /arc/
#DESCRIPTION
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
#REVISED 09_10_07
#--------------------------------------------
{
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
#TITLE (ba) prints an /arc/
#DESCRIPTION
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
#TITLE (ba) checks a /pam/
#DESCRIPTION
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
#REVISED 09_10_03
#--------------------------------------------
{
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
#TITLE (ba) prints a /pam/
#DESCRIPTION
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
#TITLE (ba) checks a /win/
#DESCRIPTION
#   This function checks /win/ objects
#DETAILS
# It is the validity method for /win/ objects.
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
#REVISED 10_04_23
#--------------------------------------------
{
    res <- character(0);
    #
    # checking the window definition
    rr <- check4tyle(object@wgt,"numeric",  -1,"win@wgt must be numeric",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    rr <- check4tyle(object@k  ,"numeric",   1,"win@k   must be numeric of size 1",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    rr <- check4tyle(object@di ,"numeric",c(2,Inf),"win@di  must be numeric of size 2 or more",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    rr <- check4tyle(object@nb ,"numeric",c(2,Inf),"win@nb  must be numeric of size 2 or more",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    rr <- check4tyle(object@ty ,"character", c(1,2),"win@ty  must be character of size 1 or 2",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    if(!all(object@wgt>=0)) { res <- c(res,"win@wgt Cannot be negative",as.character(object@wgt));}
    if (length(object@wgt)>0) { if (sum(object@wgt)==0) {
        res <- c(res,"In win@wgt, not all weights can be zero!");
    }}

    if(!all(object@k  >=0)) { res <- c(res,"win@k  cannot be negative");}
    if(!all(object@di >=0)) { res <- c(res,"win@di cannot be negative");}
    if(!all(object@nb >0))  { res <- c(res,"win@nb cannot be null or negative");}
    if(!all(diff(object@di) >=0)) { res <- c(res,"win@di must be non decreasing");}
    if(!all(diff(object@nb) >=0)) { res <- c(res,"win@nb must be non decreasing");}
    if(length(object@nb)!=length(object@di)) { res <- c(res,"win@nb and win@di must have the same length");}

    #
    choix <- c("random","mean*","median*","median1");
    lchoi <- c(1       ,1      ,1        ,2);
    if (!(object@ty[1] %in% choix)) {
         res <- c(res,paste("win@ty is not consistent (=",object@ty[1],
                            ") must be among",paste(choix,collapse="/")));
    }
    if (length(object@ty) != lchoi[which(object@ty[1]==choix)]) {
        res <- c(res,paste("win@ty[2] has got a bad lenght with respect to win@ty[1]"));
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rbsb.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



###########################################
setClass("win",representation(
                         # defines a window for selection with empidata
    wgt="numeric",       #  vector of the weights for the involved variables
      k="numeric",       #  coefficient for the calculation of the distance
     di="numeric",       #  minimum and successive maxima distances accepted
     nb="numeric",       #  minimum and successive maxima numbers accepted
     ty="character"),    #  type of prediction
               prototype(wgt=numeric(0),k=2,di=c(0,1),nb=c(1,Inf),ty="random"),
               validity=valid8win
        );


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8win <- function(x,...,fo=1)
#TITLE (ba) prints the node-variable names
#DESCRIPTION
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{fo} <<(=1) format to use, 1 for developed,
#            0 for one line.>>
#VALUE
# nothing a printing is issued
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.win0);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_08
#REVISED 10_04_23
#--------------------------------------------
{
# some checks
if (rbsb.mck) {
    che <- valid8win(x);
    if (!identical(che,TRUE)) {
        erreur(che,"This /win/ is not valid");
    }
}
# preparing
selection <- matrix(c(x@nb,x@di),ncol=2);
dimnames(selection) <- list(c("mini",paste("max",sj(length(x@nb)-1),sep="")),
                            c("number","distance"));
# printing
if (identical(fo,1)) {
    nsou <- 39;
    #
    form3repete("-",nsou,TRUE);
    cat("/win/ with type",x@ty,"\n");
    cat("Power coefficient of",x@k,"\n");
    if (length(x@wgt)==0) { cat("No weights\n");
    } else {cat("Weights are:",x@wgt,"\n");}
    print(selection);
    form3repete("-",nsou,TRUE);
} else {
    if (length(x@wgt)==0) { cat("No wgt\n");
    } else {cat(x@wgt,"\n");}
    print(selection);
}
# returning
invisible();
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
#TITLE (ba) checks an /alk/
#DESCRIPTION
#   This function checks /alk/ objects in
# a very light way. For a more complete checking
# one must use \code{check8alk}
#DETAILS
# It is the validity method for /alk/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The alk object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
#EXAMPLE
#REFERENCE
#SEE ALSO check8alk
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_04
#REVISED 10_02_12
#--------------------------------------------
{
    res <- character(0);
    #
    # an alk must comprises a valid description
    rr <- valid8des(object@ldes);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    # an alk must have a logical lcomp slot
    rr <- check4tyle(object@lcomp,"logical",1,"for /alk/, logical slot @lcomp must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    # the type must be defined
    rr <- check4tyle(object@ltype,"character",1,"for /alk/, character slot @ltype must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    # the nature must be known
    rr <- check4tyle(object@lnat,"character",-1,"for /alk/, character slot @lnat must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    # no inheritance with ltransfo
    if (length(object@ltransfo)>0) {
    if (!all(object@ltransfo=="")) {
        # looking for forbidden parentship
        deco <- easyp3cut(object@ltransfo,rbsb.cpt);
        if (sum(deco$typ==1)>0) {
            res <- c(res,paste(object@ltransfo,"No parents can be involved in the transformation"));
        }
    }}
    ###
    if (identical(object@lcomp,TRUE)) {
        # the node is supposed to be completed
        #
        # length of lnat
	if(length(object@lnat) != length(object@lvar)) {
            res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The length of lnat must be equal to the number of variables"));
        }
	# checking the dimensions of the declared domains
	nbv <- length(object@lnat);
	if (length(object@lpod) != nbv) {
	    res <- c(res,paste(object@lpod,"Bad @lpod?",nbv,"variable(s) await possible domain(s)"));
	}
	if (length(object@lred) != nbv) {
	    res <- c(res,paste(object@lred,"Bad @lred?",nbv,"variable(s) await representation domain(s)"));
	}
	if (length(object@lcod) != nbv) {
	    res <- c(res,paste(object@lcod,"Bad @lcod?",nbv,"variable(s) await common domain(s)"));
	}
    } else {
        # the node is supposed not to be completed
        if (object@lrep < 0) {
             # consistence of parameter
             for (jbd in sjl(object@lpara)) {
                 if ((object@lrep-length(object@lpara[[jbd]]) != 0) &
                     (length(object@lpara[[jbd]]) != 1)) {
                     res <- c(res,paste(paste(object@lrep,object@lpara[[jbd]],collapse="/"),
                            "For non completed repeated alk, all parameter must be of size lrep or one"));
                 }
             }
        }
	if (length(object@lvar) > 0) {
	    if(length(object@lnat) != length(object@lvar)) {
                res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The lengths of lnat and lvar must be equal for multivariate nodes"));
            }
	} else {
	    if(length(object@lnat) != 1) {
                form3affiche(list(object@ldes,object@lcomp));
                res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The length of lnat must be equal to ONE for scalar and repeated nodes"));
            }
	}
	# checking the dimensions of the declared domains
	nbv <- max(1,length(object@lvar));
	if (length(object@lpod) != nbv) {
            form3affiche(object@ldes);
	    res <- c(res,paste(paste(object@lpod,object@lvar,collapse="/"),"Inconsistent lpod and lvar"));
	}
	if (length(object@lred)>0) { if (length(object@lred) != nbv) {
            form3affiche(object@ldes);
	    res <- c(res,paste(paste(object@lred,object@lvar,collapse="/"),"Inconsistent lred and lvar"));
	}}
	if (length(object@lcod)>0) { if (length(object@lcod) != nbv) {
            form3affiche(object@ldes);
	    res <- c(res,paste(paste(object@lcod,object@lvar,collapse="/"),"Inconsistent lcod and lvar"));
	}}
    }
    #
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
    lparent="character", # at the node level
    lpod="list", # as many as variables for multivariate, the same if not
    lred="list", # as lpod
    lcod="list", # as lpod
    ltransfo="character", # done by the way of easyprogramming
                          # when there are several variables, the same
                          # transformation is applied to each of them
                          # so it must be of length one
    ldaf="daf",           # for ltype='empidata' and 'popula'
                          #           
    lwin="win",           # for ltype='empidata' in case of parents
    lfunct="function",    # R function for ltype='program'
    lcomp="logical"),     # indicates if the alk is completed (in general by /rbsb/)
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
                         lwin=new("win",wgt=numeric(0),k=2,
                                  di=c(0,1),nb=c(1,Inf),ty="random"),
                         lcomp=FALSE),
               validity=valid8alk
);


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8alkcomp <- function(alk,des=0,retrait=4)
#TITLE (ba) prints an alk object supposed to be completed
#           for a bn
#DESCRIPTION
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
#{des} <<(=0) indicates if the description slot must be
#        printed. O: no; 1: in a short way; 2: completely.
#        Other values means that nothing is printed.>>
#{retrait} <<(=4) number of spaces for indentation.>>
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
#TITLE (ba) prints an alk object not supposed to be completed
#           for a bn
#DESCRIPTION
#   This function prints in a interpreted way an alk object
# supposed not to be completed. Must not be called by the user
# who might use directly the \code{print} associated to \code{/alk/}.
#DETAILS The interpretation is hand made for each type of links.
#KEYWORDS print
#PKEYWORDS link bn
#INPUTS
#{alk} <<The alk object.>>
#[INPUTS]
#{des} <<(=0) indicates if the description slot must be
#        printed. O: no; 1: in a short way; 2: completely.
#        Other values means nothing is printed.>>
#{retrait} <<(=4) number of spaces for indentation.>>
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
red <- alk@lred;
cod <- alk@lcod;
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
#TITLE (ba) prints the node-variable names
#DESCRIPTION
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{retrait} <<(=4) number of spaces for indentation.>>
#{proba} <<(=FALSE) Must the probability table be printed?
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
#                         lwin=new("win",wgt=numeric(0),k=2,
#                                  di=c(0,1),nb=c(1,Inf),ty="random"),
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
#TITLE (ba) checks a /pgr/
#DESCRIPTION
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
#REVISED 09_10_04
#--------------------------------------------
{
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
#TITLE (ba) prints the node-variable names
#DESCRIPTION
# prints the node-variable names of x (a 'nom' object)
#DETAILS
#KEYWORDS print
#PKEYWORDS print
#INPUTS
#{x} <<nom object>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the print function.>>
#{what} <<(='v') what to print ('v'=node-variable names;
#         'n' only node names)
#{type} <<(=0) type of printing (0) a node a line;
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
#TITLE (ba) checks a /pos/
#DESCRIPTION
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
#REVISED 09_10_04
#--------------------------------------------
{
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
#TITLE (ba) prints a /pos/
#DESCRIPTION
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
#TITLE (ba) checks a /pta/
#DESCRIPTION
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
#REVISED 09_10_04
#--------------------------------------------
{
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
#TITLE (ba) prints the node-variable names
#DESCRIPTION
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
#{comment} <<(=1) Numeric scalar defining the way to print
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
#TITLE (ba) returns the "nom" slot of an object
#DESCRIPTION
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
#TITLE (ba) number of nodes/variables for an bn/gn/dn/nom object
#DESCRIPTION
# According to 'what', returns the number of 
# nodes/variables for an bn/gn/dn/nom object
#DETAILS
# no check is performed.
#KEYWORDS misc
#PKEYWORDS node nb gn dn nom
#INPUTS
#{x} <<the bn/gn/dn/gn or nom object>>
#[INPUTS]
#{what} <<(="-1") 
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
#TITLE (ba) returns the /ion/ of a series of
# nodes/variables
#DESCRIPTION
# This is a central, basic and very general function for
# programming with /rbsb/, so not 
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
# variables of /nom/ uu given in the example section. Intuitively, we
# coud designate the first node in different ways: (1) as the first node, 
# (2) as the node of name 'A', (3) as the first three variables of uu,
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
# argument (\code{nom}). When they are several subsets, \code{nv2ion} deals with the union of them. The indication can be made (1) by names (\code{character}
# interpreted of nodes or variates according to the third argument \code{kwhat};
# (ii) a numeric matrix of two rows giving its columns the [node number, variable number];
# (iii) a numeric giving the index of nodes or variables according to the third argument \code{kwhat}.\cr
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
#{check} <<((=TRUE) when TRUE checking of the argument consistence
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
#SEE ALSO Before using \code{nv2ion}, it is suggested to run the script \code{rsba.demo.nv2ion.r}.
#CALLING
# uniqueness is attempted but redundancy is not avoided...
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_28
#REVISED 09_10_13
#--------------------------------------------
{
#=====================================================
# STEP 1: investigation of the arguments, checking, preparation
#===================================================== 
# some checking and preparation
if (check) {
    if (rbsb.mck) {valid8nom(nom);}
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
		erreur(list(nom,nn),"Not accepted as node name");
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
# returning
new("ion",nn=res$nn,vn=res$vn,nvn=res$nvn,
          nk=res$nk,ij=res$ij[2,],vk=res$vk,
          iden=iden);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nanv <- function(x,what=-1)
#TITLE (ba) names of nodes/variables for an bn/gn/dn/nom object
#DESCRIPTION
# According to 'what', returns the names of 
# nodes/variables for an bn/gn/dn/nom object
#DETAILS
# No check is made about \code{x}
#KEYWORDS misc
#PKEYWORDS variable node nb gn dn nom
#INPUTS
#{x} <<the bn/gn/dn/gn or nom object>>
#[INPUTS]
#{what} <<(="-1") 
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
char2nom <- function(x)
#TITLE (ba) transforms a character into a 'nom' object
#DESCRIPTION
# returns a \code{/nom/} whose name is \code{x}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS nom
#INPUTS
#{x} <<character.>>
#[INPUTS]
#VALUE
# a 'nom' object with node having the non-named variable
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# print(char2nom(LETTERS));
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
check4tyle(x,"character",-1);
# creating
xx <- as.list(rep("",length(x)));
names(xx) <- x;
res <- new("nom",x=xx);
if (rbsb.mck) {valid8nom(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
and4nom <- function(nom,nod,var="")
#TITLE (ba) adds one node to a /nom/
#DESCRIPTION
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
if (rbsb.mck) {valid8nom(nom);}
check4tyle(nod,"character", 1,"The node name (only one) was expected.");
check4tyle(var,"character",c(1,Inf),"The variable name(s) was expected [At least one].");
if (nod %in% nanv(nom,"n")) {
    erreur(list(nom,nod),"The node you proposed already exists.");
}
if (length(unique(var))!=length(var)) {
    erreur(var,"You proposed duplicated variable names for the same node.");
}
# addition
nom@x[[nod]] <- var;
# returning
nom;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rmnd4nom <- function(nom,nod)
#TITLE (ba) removes one node to a /nom/
#DESCRIPTION
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
if (rbsb.mck) {valid8nom(nom);}
check4tyle(nod,"character", 1,"The node name (only one) was expected.");
if (!(nod %in% nanv(nom,"n"))) {
    erreur(list(nom,nod),"The node you proposed does not exist in that /nom/.");
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
#TITLE (ba) detects the existence of names into nom
#DESCRIPTION
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
if (rbsb.mck) {valid8nom(nom);}
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
#TITLE (ba) computes the dimension of a node
#DESCRIPTION
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
    valid8nom(nom);
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
    if (type %in% c("empidata","popula","program")) {
        res <- length(var);
    }
    # (only non compulsorily univraite nodes are considerd)
    if (type=="multinomial") { res <- length(para$p); }
    if (type=="Dirichlet")   { res <- length(para$a); }
    # this type is to be made!
    if (res == -1) {
        rapport(paste("This type node (",type,") is not yet considered by /rbsb/"));
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
#TITLE (ba) sorts a 'ion' object
#DESCRIPTION sorts a 'ion' object possibly eliminating
# the redundancies
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{ion}<<The 'ion' to be sorted.>>
#{nom}<<associated 'nom' structure to perform the sorting.>>
#[INPUTS]
#{sort} <<(='n') the way to make the sorting
#               'n': according to 'nom'
#               'a'; according to the alphabet.>>
#{rm.redun} <<(=TRUE) Must the redundancies be removed?>>
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
if (rbsb.mck) {valid8nom(nom);}
if (rbsb.mck) {valid8ion(ion);}
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
# eliminating the redundancies
if (rm.redun) { if (length(ion)>1) {
    dd <- length(ion);
    rr <- c(ion@nvn[-1]!=ion@nvn[-dd],TRUE);
    if (sum(rr)<length(rr)) {
        for (ii in slotNames(ion)) {
            slot(ion,ii) <- slot(ion,ii)[rr];
        }
    }
}}
# returning
ion;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
short8nv <- function(nv,nom,sort="n")
#TITLE (ba) condenses in a unique way a set of nodes/variables
#DESCRIPTION
# Redundant nodes are eliminated, variables are restricted to their nodes.
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{nv}<<character giving the nodes/variables as names.>>
#{nom}<<associated 'nom' structure to perform the sorting.>>
#[INPUTS]
#{sort} <<(='n') the way to make the sorting
#               'n': according to 'nom'
#               'a'; according to the alphabet.>>
#VALUE
# a shortened character comprising the same series of nodes/variables
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# uu <- rbsb.nom3;
# short8nv(c("B","C[2]","C[1]","B"),uu); # -> c("B","C")
# short8nv(c("A[b]","A","C[2]"),uu);     # -> c("A","C[3]")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_29
#REVISED 09_05_05
#--------------------------------------------
{
# checking
if (rbsb.mck) {valid8nom(nom);}
check4tyle(nv,"character",-1);
# returning the empty cases first
if (isvide(nv)) { return(nv);}
if (all(nv=="")) { return("");}
# going through the 'ion' coding
ion <- nv2ion(nv,nom);
# sorting and eliminating redundancies
ion <- sort8ion(ion,nom,sort=sort,rm.redun=TRUE);
# reducing for complete nodes
nv <- character(0);
nod <- unique(ion@nk);
for (nn in nod) {
    if (sum(ion@nk==nn)==nbnv(nom,nn)) {
        # we have got a complete node
        nv <- c(nv,ion@nn[ion@nk==nn][1]);
    } else {
        # no reduction is possible
        nv <- c(nv,ion@nvn[ion@nk==nn]);
    }
}
# removing the non-human '[-]'
suf <- rbsb.who;
for (ii in sjl(nv)) {
    nv[ii] <- strsplit(nv[ii],suf,fixed=TRUE)[[1]];
}
# returning
if (identical(nv,"")) { nv <- character(0);}
nv;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nv2nod <- function(vvn)
#TITLE (ba) transforms complete variable names into node names
#DESCRIPTION
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
#TITLE (ba) returns the variable name from a node[variable] name
#DESCRIPTION
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
#TITLE (ba) detects if a cycle exists from a pam matrix
#DESCRIPTION
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
if (rbsb.mck) {valid8pam(pam);}
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
#TITLE (ba) finds different characteristics of relations
#           between nodes
#DESCRIPTION
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
#{iir} <<(=nrow(pam@rlt) the subset of nodes to be considered
#        as starting nodes. They will be en rows of the
#        resulting matrices.>>
#{jjc} <<(=iir) the subset of nodes to be considered as ending
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
if (rbsb.mck) {valid8pam(pam);}
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
#TITLE (ba) finds the minimum distance between every pair of nodes
#DESCRIPTION
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
if (rbsb.mck) {valid8pam(pam);}
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
#TITLE (ba) returns the equivalent pos object with standard view
#DESCRIPTION
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
# print(pos2pos(rbsb.pos));
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
if (rbsb.mck) {valid8pos(pos);}
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
#TITLE (ba) returns all paths between two nodes
#DESCRIPTION
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
if (rbsb.mck) {valid8pam(pam);}
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
#TITLE (ba) constructs the object pam from the object arc
#DESCRIPTION
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
if (rbsb.mck) {valid8arc(arc);}
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
#TITLE (ba) from the parentship object pam returns the arcs object
#DESCRIPTION
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
if (rbsb.mck) {valid8pam(pam);}
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
#TITLE (ba) constructs the genealoty of a given node
#DESCRIPTION
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
#{asc} <<(=TRUE) must ascendants be looked for, if not
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
if (rbsb.mck) {valid8pam(pam);}
check4tyle(i,"numeric",1,"Only one node at once");
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
if (rbsb.mck) {valid8arc(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
range8pos <- function(pos,padding=rbsb.pgr@padding)
#TITLE (ba) returns the ranges (x,y) for a pos
#DESCRIPTION
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
#TITLE (ba) normalize pt
#DESCRIPTION
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
if (rbsb.mck) {valid8pta(pt);}
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
#TITLE (ba) compute a univariate categ distribution
#DESCRIPTION
# from a beta(a,b) distribution operates an approximate
# equal spaced discretization with n classes
#DETAILS
#KEYWORDS utilities
#PKEYWORDS categ
#INPUTS
#[INPUTS]
#{a}<<(=1) First parameter for the Beta distribution.>>
#{b}<<(=1) Second parameter for the Beta distribution.>>
#{n}<<(=3) Number of classes to use.>>
#{xname}<<(=form3names(n)) names to provide for the 
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
#TITLE (ba) compute a bivariate categ distribution
#DESCRIPTION
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
#{co}<<(=c(1,1)) parameters for the common Beta distribution.>>
#{re}<<(=c(1,1)) parameters for the Beta distribution 
#           associated to the relationship between
#           the two variables.>>
#{n}<<(=c(3,4)) Number of classes to use for the first and 
#          second variable.>>
#{xnames}<<(=list(V.1=form3names(n[1]),V.2=form3names(n[2]))) 
#           the dimnames of to apply onto the resulting 
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
#TITLE (ba) compute a p-variable categ distribution
#DESCRIPTION
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
#{co}<<(=c(1,1)) First parameter when calling categ3beta2.>>
#{re}<<(=c(2,2)) Second parameter when calling categ3beta2.>>
#{n}<<(=c(3,4,5)) Number of classes to use for the p variables.
#                 Its length provides the number of variables.>>
#{xnames}<<(=rbsb.cha0) dimnames of the resulting probability array.
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
#TITLE (ba) returns the dimension of the variable
#           defined in an easyp character(1)
#DESCRIPTION
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
if (rbsb.mck) {valid8nom(nom);}
check4tyle(eas,"character",1);
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
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyps2dim <- function(meas,nom)
#TITLE (ba) returns the dimension of the variable
#           defined with an easyp of any length
#DESCRIPTION
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
if (rbsb.mck) {valid8nom(nom);}
check4tyle(meas,c("null","character","numeric"),-1);
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
#TITLE (ba) prints a pt object of dimension two
#DESCRIPTION
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
#{comment} <<(=0) Numeric vector defining the way to print.
#             0: joint probability,
#             1: conditional by rows,
#             2: conditional by columns.>>
#{kkk} <<(=-3) the coefficient for rounding the probabilities.>>
#{tra} <<(=FALSE) Must the table be transposed when printed?>>
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
if (rbsb.mck) {valid8pta(pt);}
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
#TITLE (ba) to avoid difficulty with is.null
#DESCRIPTION
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
#TITLE (ba) checks the consistency of the result obtained after 
# easyp3cut and returns the used node/variable numbers of nom
#DESCRIPTION
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
#TITLE (ba) checks an /alk/
#DESCRIPTION
#   This function checks /alk/ objects much more deeply than
# does valid8alk
#DETAILS
# After numerous attempts all the checks made in that function
# where removed from valid8alk because it seemed difficult
# to comply the requirements of a standard package by R checking.
#KEYWORDS classes
#INPUTS
#{object} <<The alk object to be validated.>>
#[INPUTS]
#VALUE
# TRUE when the object seems acceptable
# else a character describing the error(s)
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
#REVISED 09_10_10
#--------------------------------------------
{
    res <- character(0);
    #
    # an alk must comprises a valid description
    rr <- valid8des(object@ldes);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    # an alk must have a logical lcomp slot
    rr <- check4tyle(object@lcomp,"logical",1,"for /alk/, logical slot @lcomp must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    #
    # the type must be defined and known
    rr <- check4tyle(object@ltype,"character",1,"for /alk/, character slot @ltype must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    if (!(object@ltype %in% rbsb.ltn)) {
        res <- c(res,paste(object@ltype,"This @ltype is not registrated!"));
    }
    # the nature must be known
    rr <- check4tyle(object@lnat,"character",-1,"for /alk/, character slot @lnat must exist",FALSE);
    if (is.character(rr)) { res <- c(res,rr);}
    # no inheritance with ltransfo
    if (!isvide(object@ltransfo)) {
        # looking for forbidden parentship
        deco <- easyp3cut(object@ltransfo,rbsb.cpt);
        if (sum(deco$typ==2)>0) {
            res <- c(res,paste(object@ltransfo,"No parents can be involved in the transformation"));
        }
    }
    # consistence for data based nodes
    if (rbsb.tlk[[object@ltype]]$fami == "data_based") {
        # getting the variables names of the "data.frame"
        dfe <- get8daf(object@ldaf,1:5);
        nava <- dimnames(dfe)[[2]];
        navat <- paste(nava,collapse="");
        if (!all(object@lvar %in% nava)) {
            res <- c(res,paste(paste(object@lvar,nava,dfe,collapse="/"),
                               "The defined variables are not in the data base!"));
        }
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
                res <- c(res,paste(object@ldes,":",paste("The parameter",npa,"was expected for the",object@ltype,"type of node")));
            }
        }
    }
    ###
    if (identical(object@lcomp,TRUE)) {
        # the node is supposed to be completed
        #
        # length of lnat
	if(length(object@lnat) != length(object@lvar)) {
            res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The length of lnat must be equal to the number of variables"));
        }
	# checking the dimensions of the declared domains
	nbv <- length(object@lnat);
	if (length(object@lpod) != nbv) {
	    res <- c(res,paste(object@lpod,"Bad @lpod?",nbv,"variable(s) await possible domain(s)"));
	}
	if (length(object@lred) != nbv) {
	    res <- c(res,paste(object@lred,"Bad @lred?",nbv,"variable(s) await representation domain(s)"));
	}
	if (length(object@lcod) != nbv) {
	    res <- c(res,paste(object@lcod,"Bad @lcod?",nbv,"variable(s) await common domain(s)"));
	}
	# checking the domain with respect to the nature of the variables
	for (ii in sj(nbv)) {
	    if (rbsb.snp[object@lnat[ii],"categoric"]) {
		# categoric variable
		if (length(setdiff(object@lred[[ii]],object@lpod[[ii]])) > 0) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lred,"All categories for representation are not possible !"));
		}
		if (length(setdiff(object@lcod[[ii]],object@lpod[[ii]])) > 0) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lcod,"All categories for common view are not possible !"));
		}
	    } else {
		# numeric variable
		if (length(object@lpod[[ii]]) != 2) { 
		    res <- c(res,paste(object@lpod,"lpod vectors for continuous variables must be of length 2"));
		}
                if (!is.numeric(object@lpod[[ii]])) {
                    res <- c(res,paste(object@lpod,"lpod vector must be numeric for numeric variables"));
                }
		if (object@lpod[[ii]][1] >= object@lpod[[ii]][2]) { 
		    res <- c(res,paste(object@lpod,"lpod vectors must define an interval"));
		}
		if (length(object@lred[[ii]]) != 2) { 
		    res <- c(res,paste(object@lred,"lred vectors for continuous variables must be of length 2"));
		}
                if (!is.numeric(object@lred[[ii]])) {
                    res <- c(res,paste(object@lred,"lred vector must be numeric for numeric variables"));
                }
		if (object@lred[[ii]][1] >= object@lred[[ii]][2]) { 
		    res <- c(res,paste(object@lred,"lred vectors must define an interval"));
		}
		if (!all(is.finite(as.numeric(object@lred[[ii]])))) {
		    res <- c(res,paste(object@lred,"lred vectors must define a finite interval"));
		}
		if (length(object@lcod[[ii]]) != 2) { 
		    res <- c(res,paste(object@lcod,"lcod vectors for continuous variables must be of length 2"));
		}
                if (!is.numeric(object@lcod[[ii]])) {
                    res <- c(res,paste(object@lcod,"lcod vector must be numeric for numeric variables"));
                }
		if (object@lcod[[ii]][1] >= object@lcod[[ii]][2]) { 
		    res <- c(res,paste(object@lcod,"lcod vectors must define an interval"));
		}
		if (!all(is.finite(as.numeric(object@lcod[[ii]])))) {
		    res <- c(res,paste(object@lcod,"lcod vectors must define a finite interval"));
		}
	    }
	}
    } else {
        # the node is supposed not to be completed
        if (object@lrep < 0) {
             # consistence of parameter
             for (jbd in sjl(object@lpara)) {
                 if ((object@lrep-length(object@lpara[[jbd]]) != 0) &
                     (length(object@lpara[[jbd]]) != 1)) {
                     res <- c(res,paste(paste(object@lrep,object@lpara[[jbd]],collapse="/"),
                            "For non completed repeated alk, all parameter must be of size lrep or one"));
                 }
             }
        }
	if (length(object@lvar) > 0) {
	    if(length(object@lnat) != length(object@lvar)) {
                res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The lengths of lnat and lvar must be equal for multivariate nodes"));
            }
	} else {
	    if(length(object@lnat) != 1) {
                form3affiche(list(object@ldes,object@lcomp));
                res <- c(res,paste(paste(object@lvar,object@lnat,collapse="/"),"The length of lnat must be equal to ONE for scalar and repeated nodes"));
            }
	}
	# checking the dimensions of the declared domains
	nbv <- max(1,length(object@lvar));
	if (length(object@lpod) != nbv) {
            form3affiche(object@ldes);
	    res <- c(res,paste(paste(object@lpod,object@lvar,collapse="/"),"Inconsistent lpod and lvar"));
	}
	if (!isvide(object@lred)) { if (length(object@lred) != nbv) {
            form3affiche(object@ldes);
	    res <- c(res,paste(paste(object@lred,object@lvar,collapse="/"),"Inconsistent lred and lvar"));
	}}
	if (!isvide(object@lcod)) { if (length(object@lcod) != nbv) {
            form3affiche(object@ldes);
	    res <- c(res,paste(paste(object@lcod,object@lvar,collapse="/"),"Inconsistent lcod and lvar"));
	}}
	# checking the domain with respect to the nature of the variables
        if (object@lrep > 1) { natu <- rep(object@lnat,object@lrep);
        } else { natu <- object@lnat;}
	for (ii in sj(nbv)) {
	    if (rbsb.snp[natu[ii],"categoric"]) {
		# categoric variable
		if (!isvide(object@lred)) { if (length(setdiff(object@lred[[ii]],object@lpod[[ii]])) > 0) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lred,"All categories for representation are not possible !"));
		}}
		if (!isvide(object@lcod)) { if (length(setdiff(object@lcod[[ii]],object@lpod[[ii]])) > 0) {
		    form3affiche(object@lpod);
		    res <- c(res,paste(object@lcod,"All categories for common view are not possible !"));
		}}
	    } else {
		# numeric variable
		if (length(object@lpod[[ii]]) != 2) { 
		    res <- c(res,paste(object@lpod,"lpod vectors for continuous variables must of length 2"));
		}
                if (!isvide(object@lred)) {
    		  if (length(object@lred[[ii]]) != 2) { 
		      res <- c(res,paste(object@lred,"lred vectors for continuous variables must of length 2"));
		  }
                }
                if (!isvide(object@lcod)) {
		  if (length(object@lcod[[ii]]) != 2) { 
		      res <- c(res,paste(object@lcod,"lcod vectors for continuous variables must of length 2"));
		  }
                }
	    }
	}
    }
    # checking the natures 
    for (jbd in object@lnat) { if (!(jbd %in% rbsb.sna)) {
        res <- c(res,jbd,"is not an acceptable nature for a link");
    }}
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=TRUE);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isalkrepeatable <- function(ltype)
#TITLE (ba) indicates if the link is a standard
#           repeatable link
#DESCRIPTION returns TRUE/FALSE according to
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
#TITLE (ba) transforms node[variable] characters
# into node and variable characters.
#DESCRIPTION
# just removing possible square brackets to give
# back the node names ($nod) and the variable 
# names ($var)
#DETAILS
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
#REVISED 09_06_28
#--------------------------------------------
{
# checking
check4tyle(xx,"character",-1);
#
res <- list(nod=character(0),var=character(0));
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
#TITLE (ba) creates an /arc/ with a specified number of nodes and arcs
#DESCRIPTION
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
check4tyle(nbno,"integer",1,"nbno is the number of nodes");
check4tyle(nbar,"integer",1,"nbar is the number of arcs");
if (nbno < 0) { erreur(nbno,"nbno is the number of nodes");}
if (nbar < 0) { erreur(nbar,"nbar is the number of arcs");}
if (nbar > nbno*(nbno-1)/2) {
    erreur(list(nbno,nbar),"Too much arcs for the number of nodes");
}
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
nparent8alk <- function(alk,nom)
#TITLE (ba) finds the parents at the node level
#DESCRIPTION finds the node parents of an alk with 
# respect to \code{nom}.
#DETAILS
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{alk} <<alk object>>
#{nom} <<nom of the bn where the alk must inserted.>>
#[INPUTS]
#VALUE
# the names of the alk parents composed of nodes and/or variables...
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# nparent8alk(rbsb.alk2,rbsb.nom2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_16
#REVISED 09_05_27
#--------------------------------------------
{
# no check is made at this level
rech <- rbsb.l_a[alk@ltype,"lparent"];
if (rech %in% c("no","NO")) {
    # gathering every possible source
    para <- paste(sapply(alk@lpara,paste,collapse=" "),collapse=" ");
    pare <- paste(alk@lparent,collapse=" ");
    tran <- paste(alk@ltransfo,collapse=" ");
    mama <- paste(para,pare,tran);
    # looking for parent in it
    dd <- easyp3cut(mama,rbsb.cpt);
    ee <- easyp3trouve(dd,nom);
    # returning
    res <- c(nv2ion(rbind(ee$node,rep(0,length(ee$node))),nom,"n")@nn,
	     nv2ion(ee$vari,nom,"v")@nvn);
} else {
    res <- alk@lparent;
}
res <- short8nv(res,nom);
# return
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
vparent8alk <- function(alk,nom)
#TITLE (ba) finds the parents at the variable level
#DESCRIPTION finds the variable parents of an alk with 
# respect to \code{nom}.
#DETAILS
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{alk} <<alk object>>
#{nom} <<nom of the bn where the alk must inserted.>>
#[INPUTS]
#VALUE
# the names of the alk composed of nodes and/or variables...
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# vparent8alk(rbsb.alk2,rbsb.nom2);
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_16
#REVISED 09_05_02
#--------------------------------------------
{
# no check is made at this level
# gathering every possible source
para <- paste(sapply(alk@lpara,paste,collapse=" "),collapse=" ");
pare <- paste(alk@lparent,collapse=" ");
tran <- paste(alk@ltransfo,collapse=" ");
mama <- paste(para,pare,tran);
# looking for parent in it
dd <- easyp3cut(mama,rbsb.cpt);
ee <- easyp3trouve(dd,nom);
# returning
res <- c(nv2ion(ee$node,nom,"n")@nn,
         nv2ion(ee$vari,nom,"v")@nvn);
res <- short8nv(res,nom);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
help8ltype <- function(quelles="ls")
#TITLE (ba) provides information about type of links
#DESCRIPTION
# This function displays requirement and default values
# for the different types of links (ltype) implemented
# into rebastaba
#DETAILS
#KEYWORDS
#PKEYWORDS 
#INPUTS
#[INPUTS]
#{quelles} <<(="ls") 
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
#REVISED 09_10_23
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
if (quelles=="para") {
    form3titre("The needed parameters for the different 'ltype' are",nvp[2],inp[2]);
    for (jbd in sjl(rbsb.tlk)) {
        cat("\n");
        cat(form3justifie(paste(rbsb.ltn[jbd]," : "),22));
        if (length(names(rbsb.tlk[[jbd]]$lpara))==0) { cat("-");
        } else { cat(names(rbsb.tlk[[jbd]]$lpara));}
    }
    cat("\n");
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
        if (rbsb.l_a[jbd,jd]!="-") {
            if (jd == "lvar") {
               form3titre(paste(pres,"names for the variables of the node"),nvp[3],inp[3]);
               form3titre(paste("default: ['",paste(uu$lvar,collapse="','"),"']",sep=""),nvp[3],inp[3]+16);
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
                form3titre(paste("default:",vv),nvp[3],inp[3]+16);
            }
            if (jd == "lnat") {
                form3titre(paste(pres,"nature(s) of the variable(s) node"),nvp[3],inp[3]);
                form3titre(paste("default:",uu$lnat),nvp[3],inp[3]+16);
            }
            if (jd == "lpod") {
                form3titre(paste(pres,"possible domain(s) of the variable(s) node"),nvp[3],inp[3]);
                form3titre(paste("default:",uu$lpod),nvp[3],inp[3]+16);
            }
            if (jd == "lred") {
                form3titre(paste(pres,"representation domain(s) of the variable(s) node"),nvp[3],inp[3]);
                form3titre(paste("default: lpod value"),nvp[3],inp[3]+16);
            }
            if (jd == "lcod") {
                form3titre(paste(pres,"common domain(s) of the variable(s) node"),nvp[3],inp[3]);
                form3titre(paste("default: lred value"),nvp[3],inp[3]+16);
            }
            if (jd == "lparent") {
                form3titre(paste(pres,"names of the node parents"),nvp[3],inp[3]);
                form3titre(paste("default: no parents"),nvp[3],inp[3]+16);
            }
            if (jd == "lfunct") {
                form3titre(paste(pres,"piece of code (depends on the ltype)"),nvp[3],inp[3]);
                form3titre(paste("default:"),nvp[3],inp[3]+16);
                form3repete(" ",32,imp=TRUE); print(uu$lfunct);
            }
            if (jd == "ldaf") {
                form3titre(paste(pres,"how to get the values of an data based distribution"),nvp[3],inp[3]);
                form3titre(paste("directly provided with a data.frame"),nvp[3],inp[3]+16);
                form3titre(paste("default:",uu$ldaf),nvp[3],inp[3]+16);
            }
            if (jd == "lwin") {
                form3titre(paste(pres,"how to define the conditional subset for",
                                      "empidata distributions (see function 'draw8empidata')"),
                           nvp[3],inp[3]);
            }
        } else {
            rres <- "not used";
            if (jd == "lvar") {
                rres <- paste("not used, always",uu$nbv,"variable(s)");
                form3titre(paste(pres,rres),nvp[3],inp[3]);
                form3titre(paste(uu$nat,"is the default nature"),nvp[3],inp[3]+16);
                form3titre(paste(uu$pod,"is the possible domain"),nvp[3],inp[3]+16);
            }
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
cat("help8ltype(\"para",
    "\")   will give you the needed parameters of available distributions.\n",sep="");
cat("help8ltype(\"all",
    "\")    will give you details about all available distributions.\n",sep="");
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
isndmulti <- function(ltype)
#TITLE (ba) indicates if the node is a standard
#           multidimensional node
#DESCRIPTION indicates if the node is a standard
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
#TITLE (ba) defines a new alk from user specification
#DESCRIPTION
# Just collecting specifications provided by the user to
# build an alk object up taking into consideration the
# specificity of the ltype. \cr
# The potential links can be defined according to different
# possible way.
#DETAILS
#KEYWORDS
#PKEYWORDS nd
#INPUTS
#{ldes} <<Human description of the potential node through 
#         either a character for the name or a 'ds' object.>>
#{ltype} << type of link: a character string which must belong
#          to the "rbsb.tlk" names. Type help8ltype("ls") to get the list.>>
#[INPUTS]
#{lpara} <<(=rbsb.lis0) list of parameter values for standard distributions.>>
#{lrep} <<(=0) the number of repetitions; zero means no repetitions. Not all
#              node types can be repeated.>>
#{lnat} <<(=rbsb.cha0) According to the ltype (see help8ltype()) can be any
#        component of "rbsb.fun0". When
#        rbsb.cha0, the standard nature associated to ltype is provided
#        (see help8ltype).\cr
#        When it is a multivariate node, it must be a vector with
#        the corresponding natures>>
#{lvar} <<(=rbsb.cha0) This argument can be used either for multivariate
#        nodes, or for univariate nodes which are repeated (indeed it
#        is the way to indicate repetitions which are then possible
#        only for univariate distribution.\cr
#        In both case, lvar provides the vector of variable names of the
#        node. Its length gives the number of variables within the
#        added node. Standard probability distributions are
#        associated with a number of variables and it will be checked. \cr
#        For multivariate nodes, rbsb.cha0 will imply lvar=1,2,...
#        Due to R flexibility the vector can be numerical.>>
#{lparent} <<(=rbsb.cha0) parent vector at the node level; in most 
#             cases these are deduced from the other arguments.>>
#{lpod} <<(=rbsb.lis0)  When rbsb.lis0, possible domain(s) associated to ltype
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
#{lred} <<(=rbsb.lis0) Idem of lpod but for representation domain instead of 
#                 possible domaine>>
#{lcod} <<(=rbsb.lis0) Idem of lpod but for common domain instead of 
#                 possible domaine>>
#{ltransfo} <<(="") a simple way to define a transformation of the
#             drawn values. E.g. "[[2]]" means rounding with 2 decimals.
#             Remind also that the pseudo-nom for the node under
#             creation is "*Y*".>>
#{ldaf} <<(=rbsb.daf0) in the case of data.frame distributions provides the 
#           associated /daf/.>>
#{lwin} <<(=rbsb.win0) values for selecting a subset when drawing in case
#           of an empidata distribution. There is one value for each
#           parent, and its use depends on the nature of the
#           parents. See draw8empidata function for more details.>>
#{lfunct} <<(=rbsb.fun0) can be a function to be used as simulating
#           function when ltype is program.>>
#DETAILS
# The complete list of standard already programmed distributions is given
# with help8ltype function.\cr
#KEYWORDS
#PKEYWORDS nd
#VALUE
# The generated 'alk'
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_04
#REVISED 10_02_12
#--------------------------------------------
{
# very raw checking of the types
if (rbsb.mck) {
    check4tyle(ltype,"character",-1);
}
#
if (!isvide(lpara)) { if (!is.list(lpara)) {
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
if (!isvide(ldaf)) { if (!is(ldaf,"daf")) {
    erreur(ldaf,"ldaf must be a /daf/!");
}} else { ldaf <- rbsb.daf0;}
#
if (!isvide(lwin)) {
    if (rbsb.mck) {valid8win(lwin);}
} else { 
    lwin <- rbsb.win0;
    #lwin@wgt <- rep(1,length(lparent));
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
#
# checking and completing the ldes
ldes <- char2des(ldes);
#
# if necessary computing the parents at the node level from the definition
if ("lparent" %in% rbsb.tlk[[ltype]][["argno"]]) {
if (identical(rbsb.tlk[[ltype]]$lparent,rbsb.cac)) {
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
### modified on 10_02_12 replacing 2 with 1!
    lparentnod <- dd$blo[dd$typ==1];
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
if (ltype %in% c("program","empidata","popula")) {
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
                 argu[["lwin"]]@wgt <- rep(1,lpapa);
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
            argu[[arg]] <- lparentnod;
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
# further adjustments
# giving a standard name to the non-existing variable
if (length(argu[["lvar"]])==0) { argu[["lvar"]] <- "";}
# repeating if necessary the domains for multidimensional nodes
nbv <- max(1,length(argu[["lvar"]]));
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
if (rbsb.mck) {valid8alk(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2alk <- function(li,name=rbsb.cha0)
#TITLE (ba) transforms a consistent list into a new alk
#DESCRIPTION
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of an alk object. The produced alk is not completed
# since it is defined independently of any bn.\cr
# It is worth noticing that the slots for the description
# are at the same level.
#DETAILS
# The complete list of standard already programmed distributions is given
# with help8ltype function.\cr
# The main use of this function is to tackle alk read from text files
# with the function \code{file2list}.
#KEYWORDS
#PKEYWORDS alk nd
#INPUTS
#{li} <<The list to be transformed into an alk object.>>
#[INPUTS]
#{name} <<(=rbsb.cha0) gives the name of the node
#         when li$name does not exist. If both are absent
#         then an error is issued.>>
#VALUE
# The generated 'alk'
#EXAMPLE
# rsba3k("RESET"); # needed only for R checking, to be forgotten
# #list2alk(alk2list(rbsb.alk0));
# #list2alk(alk2list(rbsb.alk1));
# #list2alk(alk2list(rbsb.alk2));
#REFERENCE
#SEE ALSO
#CALLING {new8alk} 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_17
#REVISED 10_04_21
#--------------------------------------------
{
# checking
# getting the description
des <- list2des(li,name=name);
if (rbsb.mck) {valid8des(des);}
# getting the type
if (isvide(li$ltype)) {
    erreur(li,"ltype is compulsory");
} else {
    if (!(li$ltype %in% rbsb.ltn)) {
        cat("Known ltype's are:",rbsb.ltn,"\n");
        erreur(li$ltype,"This ltype is not (yet) implemented!");
    }
}
# getting the parameters
#if(!isvide(li$lpara)) {
#    for (iii in sjl(li$lpara)) {
#        li$lpara[[iii]] <- char2chars(li$lpara[[iii]]);
#        if (is(li$lpara[[iii]],"faux")) {
#            erreur(list(iii,li$lpara[[iii]]),"In list2alk");
#        }
#    }
#}
# getting the transformation
if(!isvide(li$ltransfo)) { li$ltransfo <- li$ltransfo;
} else { li$transfo <- "";}
# getting the repetition indicator
if(!isvide(li$lrep)) { li$lrep <- as.numeric(li$lrep);
} else { li$lrep <- 0;}
# getting the variable names
sans <- FALSE;
if(isvide(li$lvar)) { 
    li$lvar <- "";
    sans <- TRUE;
}
nbvar <- length(li$lvar);
# getting the variable natures
if(isvide(li$lnat)) { li$lnat <- rbsb.cha0;}
#
# getting the different domains
if(!isvide(li$lpod)) {
    if (sans) {
       li$lpod <- list(li$lpod);
    } else {
	if (length(li$lpod) != nbvar) {
	    erreur(list(li$lvar,li$lpod),"The length of 'lpod' is not equal to the variable number!");
	}
	if (length(union(names(li$lpod),li$lvar)) != nbvar) {
	    erreur(list(li$lvar,names(li$lpod)),"'lpod' names does not correspond to the variable names");
	}
        li$lpod <- li$lpod[li$lvar];
    }
}
#
if(!isvide(li$lred)) {
    if (sans) {
       li$lred <- list(li$lred);
    } else {
	if (length(li$lred) != nbvar) {
	    erreur(list(li$lvar,li$lred),"The length of 'lred' is not equal to the variable number!");
	}
	if (length(union(names(li$lred),li$lvar)) != nbvar) {
	    erreur(list(li$lvar,names(li$lred)),"'lred' names does not correspond to the variable names");
	}
        li$lred <- li$lred[li$lvar];
    }
}
#
if(!isvide(li$lcod)) {
    if (sans) {
       li$lcod <- list(li$lcod);
    } else {
	if (length(li$lcod) != nbvar) {
	    erreur(list(li$lvar,li$lcod),"The length of 'lcod' is not equal to the variable number!");
	}
	if (length(union(names(li$lcod),li$lvar)) != nbvar) {
	    erreur(list(li$lvar,names(li$lcod)),"'lcod' names does not correspond to the variable names");
	}
        li$lcod <- li$lcod[li$lvar];
    }
}
#
# getting the node parent names
if(isvide(li$lparent)) { li$lparent <- rbsb.cha0;}
# getting the easyprogramming definition
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
if (li$ltype %in% dimnames(rbsb.l_a)[[1]][rbsb.l_a[,"family"]=="dab"]) {
    if (isvide(li$ldaf)) {
        erreur(li,paste("For data_based",li$ltype,"$ldaf field is compulsory"));
    }
    # going to the list structure
    li$ldaf <- as.list(li$ldaf);
    if (isvide(li$ldaf$name)) {
        erreur(li$ldaf,"For data_based ltype, $ldaf$name is compulsory");
    }
    if (isvide(li$ldaf$what)) {
        erreur(li$ldaf,"For data_based ltype, $ldaf$what is compulsory");
    }
    if (isvide(li$ldaf$valu)) {
        erreur(list(names(li$ldaf),li$ldaf),paste("For data_based ltype",li$ltype,"$ldaf$valu is compulsory"));
    }
    # getting the data.frame by names
    # ldaf(name): name
    # ldaf(orig): origin
    # ldaf(time): time
    # ldaf(defi): definition
    # ldaf(role): role
    # ldaf(comm): comment (only one!)
    # ldaf(what): way with which the data.frame is provided
    # ldaf(valu): the name of the text file / data.frame / function
    #
    #
    if (isvide(li$ldaf$orig)) {li$ldaf$orig <-   rbsb.cha0;}  
    if (isvide(li$ldaf$time)) {li$ldaf$time <-   rbsb.cha0;}  
    if (isvide(li$ldaf$defi)) {li$ldaf$defi <-   rbsb.cha0;}  
    if (isvide(li$ldaf$role)) {li$ldaf$role <-   rbsb.cha0;}  
    if (isvide(li$ldaf$comm)) {li$ldaf$comm <-   rbsb.cha0;}  
    if (isvide(li$ldaf$name)) {
        erreur(li$ldaf,"The field 'name' is compulsory");
    }
    lldafds <- new("des",
                  name=li$ldaf$name,
                  orig=li$ldaf$orig,
                  time=li$ldaf$time,
                  defi=li$ldaf$defi,
                  role=li$ldaf$role,
                  comm=li$ldaf$comm);
    lldaf <- new("daf",des=lldafds,what=li$ldaf$what,
                         valu=li$ldaf$valu);
    if (rbsb.mck) {valid8daf(lldaf);}
    #
    #
    if (li$ltype=="empidata") {
        # some /win/ must be constituted
        #
        # getting the window definition
        if(!isvide(li$lwin)) {
            lolo <- as.list(li$lwin);
            li$lwin <- rbsb.win0;
            #
            if(!isvide(lolo$wgt)) {
                tradu <- char2vma(lolo$wgt,nat="N");
                if(is(tradu,"faux")) {
                    erreur(lolo$wgt,"This parameter must be numeric!");
                }
                li$lwin@wgt <- tradu;
            }
            #
            if(!isvide(lolo$k)) {
                tradu <- char2vma(lolo$k,nat="N");
                if(is(tradu,"faux")) {
                    erreur(lolo$k,"This parameter must be numeric!");
                }
                li$lwin@k <- tradu;
            }
            #
            if(!isvide(lolo$di)) {
                tradu <- char2vma(lolo$di,nat="N");
                if(is(tradu,"faux")) {
                    erreur(lolo$di,"This parameter must be numeric!");
                }
                li$lwin@di <- tradu;
            }
            #
            if(!isvide(lolo$nb)) {
                tradu <- char2vma(lolo$nb,nat="N");
                if(is(tradu,"faux")) {
                    erreur(lolo$nb,"This parameter must be numeric!");
                }
                li$lwin@nb <- tradu;
            }
            #
            if(!isvide(lolo$ty)) { li$lwin@ty <- lolo$ty;}
        } else {
# ??? here rbsb.win0 seems to be modified! ??? (09_07_15)
#form3affiche(rbsb.win0);
            li$lwin <- rbsb.win0;
            li$lwin@wgt <- rep(1,length(li$lparent));
#form3affiche(rbsb.win0,T);
        }
    } else {
        li$lwin <- rbsb.win0;
    }
} else {
    # not a data base distribution
    lldaf <- rbsb.daf0;
    li$lwin <- rbsb.win0;
}
#
# creating the alk
res <- new8alk(des,
               ltype=li$ltype,
               lpara=li$lpara,ltransfo=li$ltransfo,
               lrep=li$lrep,lvar=li$lvar,
               lparent=li$lparent,
               lfunct=li$lfunct,
               ldaf=lldaf,lwin=li$lwin,
               lnat=li$lnat,
               lpod=li$lpod,lred=li$lred,lcod=li$lcod
              );
# some special cases
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
# making numeric the domains for 'numeric' variables
for (nunu in sjl(res@lnat)) {
    if (rbsb.snp[res@lnat[nunu],"numeric"]) {
        # here the NA warned were generated
        res@lpod[[nunu]] <- as.numeric(res@lpod[[nunu]]);
        res@lred[[nunu]] <- as.numeric(res@lred[[nunu]]);
        res@lcod[[nunu]] <- as.numeric(res@lcod[[nunu]]);
    }
}
# checking the result
if (rbsb.mck) {valid8alk(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
alk2list <- function(alk)
#TITLE (ba) transforms a /alk/ object into a list
#DESCRIPTION
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
#REVISED 10_03_08
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
# the list of components except 'ltype'
eaco <- c("lpara","ltransfo",
          "lrep","lvar","lnat",
          "lpod","lred","lcod",
          "lparent","ldaf","lwin",
          "lfunct","lcomp");
for (ec in eaco) {
    uu <- slot(alk,ec);
    vv <- rbsb.l_a[lty,ec];
    if ((vv == "YES") | (vv == "yes")) {
        if (!isvide(uu)) { res[[ec]] <- uu;}
    }
}
# returning
return(res);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbrep4alk <- function(alk,nom)
#TITLE (ba) determines the number of repetitions of an alk
#DESCRIPTION
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
#TITLE (ba) finds the variable names for an alk.
#DESCRIPTION
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
#TITLE (ba) very simplified version of new8alk
#DESCRIPTION
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
#{coef} <<(=0:length(parent)) coefficient to define mu. If there are two 
#         parents, then mu will be
#         'coef[1]+coef[2]*{\{parent[1]}\}+coef[3]*{\{parent[2]}\}'.>>
#{sigma} <<(=1) standard deviation.>>
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
check4tyle(coef, "numeric",-1);
check4tyle(sigma,"numeric",-1);
check4tyle(parent,"character",-1);
if (length(coef)!=1+length(parent)) {
    erreur(list(coef,parent),"Non consistent lengths of coef and parent");
}
check4tyle(sigma,"any",1);
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
if (rbsb.mck) {valid8alk(alk);}
# returning
alk;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp2code2 <- function(eas,transfo=TRUE,bugs=FALSE)
#TITLE (ba) transforms an easyp expression into an R/bugs block
#DESCRIPTION
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
# already done, and eas accordingly prepared. For instance
# repeated parents are already extended.\cr
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
# easyp2code2("[[2]]")            # "{round(Y,2)}"
# easyp2code2(c("{{A}}","[[2]]")) # "{cbind(X[,'A'],round(Y,2))}"
#REFERENCE
#SEE ALSO easyp2code1
#CALLING
#COMMENT
# the bugs case is to be made.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_09
#REVISED 10_02_11
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(eas,c("numeric","character"),-1);
}
if (bugs) {
    erreur('easyp2code2',"sorry, this functionality is not yet implemented");
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
if (length(eas) == 0) { return("");}
RET <- 0;
for (i in sjl(eas)) {
    if (eas[i] == paste(rep(" ",nchar(eas[i])),collapse="")) { RET <- RET+1;}
}
if (RET == length(eas)) { return("");}
#
##########################################
axil <- function(dd,transfo) {
    # listing the admissible types in admi
    if (transfo) { admi <- 0:nrow(rbsb.cpt); # every type
    } else { admi <- c(0:1,3:nrow(rbsb.cpt));} # no rounding
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
    # translating
    nbb <- length(dd$typ);
    res <- rep("",nbb);
    vecteur <- FALSE;
    for ( ip in sj(nbb)) {
        quoi <- dd$typ[ip];
        if (quoi == 0) {
            res[ip] <- dd$blo[ip];
            vecteur <- TRUE;
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
}
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
