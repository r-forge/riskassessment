
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rebastaba3k <- function(whi)
#TITLE  assigns the rsba constants
#DESCRIPTION (ba)
# defines or returns the constants used within /rbsbba/. 
# The performed action depends on the argument.
#DETAILS
# see the details in \code{rbsb3k} NEVERTHELESS
# be aware that changing some constants like \code{rbsb.nom?}
# will possibly imply inconsistency with the constants of 
# other layers, like \code{g4n.gn?},  \code{rebastaba.bn?}, 
# \code{rebastaba.dn?}...
#PKEYWORDS helpful
#KEYWORDS misc
#INPUTS
#{whi}    <<a character(1) indicating either to reset or
#           to return the names or the current values. The three possible
#           values are \code{RESET}, \code{reset}, \code{names}, \code{definitions} or \code{values}.>>
#[INPUTS]
#VALUE
# When \code{whi=="RESET"} nothing (but the assignments are
# performed for all layers \code{rbsb} and \code{rsba}).
# When \code{whi=="reset"} nothing (but the assignments of 
# the layer \code{rsba} are performed).
# When \code{whi=="names"} the names as a character vector.
# When \code{whi=="definitions"} the definitions as a named character vector.
# When \code{whi=="values"} the values through a named list.
#EXAMPLE
## First assign the standard values
# rebastaba3k("RESET");
# print(g4n.gn1);
## to get the short labels
# rebastaba3k("names");
## to obtain the current values
# rebastaba3k("values");
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
    stop("rebastaba3k does not accept this argument");
}
#
if (whi=="RESET") { g4n3k("RESET");}
#
# definition of the different constants
sc <- md <- character(0);
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
sc["tlk"] <- "different types of links"; 
sc["ltn"] <- "names of the types of links"; 
sc["lta"] <- "slot names of alk"; 
sc["l_a"] <- "defines the needed arguments for the different ltypes"; 
sc["f_d"] <- "The different families of distributions"; 
#
sc["mck"] <- "Must systematic checks be done?";
sc["mwa"] <- "Must warning be simple warning?";
sc["msi"] <- "rebastaba signature";
sc["bn0"] <- "null /bn/"; 
sc["bn1"] <- "Example 1 of /bn/"; 
sc["bn2"] <- "Example 2 of /bn/"; 
sc["bn3"] <- "Example 3 of /bn/"; 
sc["bn4"] <- "Example 4 of /bn/"; 
sc["bn5"] <- "Example 5 of /bn/"; 
sc["bn6"] <- "Example 6 of /bn/ (with empidata node)"; 
sc["emnd"] <- "*em*phasized level for the @des of nodes"; 
sc["cond"] <- "*co*ntents to be print for the @des of nodes"; 
sc["ena"] <- "Specific names of variable for empidata node to ask information about the candidates";
#
sc["dn0"] <- "null /dn/"; 
sc["dn1"] <- "Example 1 of /dn/"; 
sc["dn2"] <- "Example 2 of /dn/"; 
sc["dn3"] <- "Example 3 of /dn/"; 
sc["dn4"] <- "Example 4 of /dn/"; 
sc["emdn"] <- "standard 'quoi' for 'print8dn'"; 
sc["scn"] <- "name for the scoring variable"; 
sc["scv"] <- "4 values for the scoring (cod,red,pod,NA)"; 
sc["mnu"] <- "minimum number of individuals for the computation of univariate statistics";
sc["mnb"] <- "minimum number of individuals for the computation of bibariate statistics";
sc["mnd"] <- "minimum number of observations for a drawing in 'draw8empidata'";
sc["cdi"] <- "'sig' for categorical distance (used in draw8empidata)";
sc["efi"] <- "file name for storing characteristics of empirical drawings";
sc["eti"] <- "every this value a title is introduced in empirical storage";
sc["ede"] <- "decimal precision for empirical storage";
sc["nar"] <- "minimal ratio of missing values (warnig issued otherwise)";
sc["esu"] <- "must the selected candidates be written in rebastaba.efi?";
sc["csv"] <- "coding for the complete set of variables";
sc["sii"] <- "intermediate printing within simulate8bn [0|1|2|3|4]";
#
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
        eee <- paste("res[[\"",noco,"\"]] <- rebastaba.",noco,";",sep="");
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
    rebastaba.lta <- c("ldes","ltype","lpara","lrep","lnat","lvar",
                  "lparent","lpod","lred","lcod","ltransfo","ldaf",
                  "lwin","lfunct","lcomp");  
    #================================================================
    ### BE AWARE THAT rebastaba.lta MUST BE IDENTICAL TO slotNames("alk")
    ###    but a this time, it is not accessible (in R check).
    #================================================================
    rebastaba.fam_dis <- c("conti_scalar","discr_scalar","conti_vector","discr_vector",
                  "categoric","data_based","miscellaneous");
    names(rebastaba.fam_dis) <- c("c_s","d_s","c_v","d_v",
                  "cat","dab","mis");
    rebastaba.f_d <- rebastaba.fam_dis;
    #
    rebastaba.tlk <- list(
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
    rebastaba.ltn <- names(rebastaba.tlk);
#
# preparing the matrix for 'help8ltype'
#
rebastaba.tlk_argu <- matrix("-",length(rebastaba.tlk),3+length(rebastaba.lta));
dimnames(rebastaba.tlk_argu) <- list(rebastaba.ltn,c("family","rep?","bugs?",rebastaba.lta));
#
for (jbd in bf(rebastaba.tlk)) {
    if (!(rebastaba.tlk[[jbd]]$fami %in% rebastaba.fam_dis)) {
        erreur(list(names(rebastaba.tlk)[jbd],rebastaba.tlk[[jbd]]$fami),
               "This family is not registrated in rebastaba.fam_dis!",w=TRUE);
        rapport("Defining (1) the 'rebastaba.tlk' constant in 'rsba.code.r'");
    } else {
        rebastaba.tlk_argu[jbd,1] <- 
           names(rebastaba.fam_dis)[which(rebastaba.tlk[[jbd]]$fami==rebastaba.fam_dis)];
    }
    rebastaba.tlk_argu[jbd,2] <- rebastaba.tlk[[jbd]]$rep;
    rebastaba.tlk_argu[jbd,3] <- rebastaba.tlk[[jbd]]$bugs;
    for (jd in rebastaba.tlk[[jbd]]$argNO) {
        if (!(jd %in% rebastaba.lta)) {
            form3affiche(rebastaba.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rebastaba.tlk)[jbd],"in argNO"),w=TRUE);
            rapport("Defining (2) the 'rebastaba.tlk' constant in 'rsba.code.r'");
        }
        rebastaba.tlk_argu[jbd,jd] <- "NO";
    }
    for (jd in rebastaba.tlk[[jbd]]$argno) {
        if (!(jd %in% rebastaba.lta)) {
            form3affiche(rebastaba.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rebastaba.tlk)[jbd],"in argno"),w=TRUE);
            rapport("Defining (3) the 'rebastaba.tlk' constant in 'rsba.code.r'");
        }
        rebastaba.tlk_argu[jbd,jd] <- "no";
    }
    for (jd in rebastaba.tlk[[jbd]]$argyes) {
        if (!(jd %in% rebastaba.lta)) {
            form3affiche(rebastaba.lta);
            form3affiche(jd);
            erreur(jd,paste("bad argument for",names(rebastaba.tlk)[jbd],"in argyes"),w=TRUE);
            rapport("Defining (4) the 'rebastaba.tlk' constant in 'rsba.code.r'");
        }
        rebastaba.tlk_argu[jbd,jd] <- "yes";
    }
    for (jd in rebastaba.tlk[[jbd]]$argYES) {
        if (!(jd %in% rebastaba.lta)) {
            form3affiche(rebastaba.lta);
            form3affiche(jd);
            erreur(list(names(rebastaba.tlk)[jbd],jd),paste("bad argument for",names(rebastaba.tlk)[jbd],"in argYES"),w=TRUE);
            rapport("Defining (5) the 'rebastaba.tlk' constant in 'rsba.code.r'");
        }
        rebastaba.tlk_argu[jbd,jd] <- "YES";
    }
    rebastaba.tlk_argu[jbd,c("ldes","ltype")] <- "YES";
    rebastaba.tlk_argu[jbd,c("lcomp")] <- "no";
}
#
    ##############################################################
    # ENDING THE DEFINITION OF ltypes
    ##############################################################
    rebastaba.l_a <- rebastaba.tlk_argu;
#
#
#
# loading the standard values
#
if (tolower(whi)=="reset") {
    assign("rebastaba.win0", new("win",nat=rbsb.nch0,swg=rbsb.nnu0,skk=rbsb.num0,
                                  sdi=rbsb.num0,snb=rbsb.num0,
                                  rwg=rbsb.num0,rkk=rbsb.num0,
                                  rty=c("NULL","NULL"),rmo=rbsb.cha0,
                                  rk2=rbsb.num0),pos=".GlobalEnv");
    assign("rebastaba.win1", new("win",nat=structure(rep("conti",3), .Names = letters[1:3]),
                                  rty=c("1","systematic")),pos=".GlobalEnv");
    assign("rebastaba.win2", new("win",
                    nat=structure(c("categ",rep("conti",2)),.Names=c("SEX","AGE","HGT")),
                    swg=rbsb.nnu0,skk=rbsb.num0,sdi=rbsb.num0,snb=rbsb.num0,
                                  rty=c("1","systematic"),rmo="AGE",
                                  rwg=rbsb.num0,rkk=rbsb.num0,
                                  rk2=numeric(0)),pos=".GlobalEnv");
    assign("rebastaba.win3", 
           new("win",nat=structure(c("categ",rep("conti",3)), .Names = c("SEX","AGE","HGT","WGT")),
                     swg=structure(c(100,1,1),
                                   .Names = c("A[SEX]","A[AGE]","A[HGT]")),
                                  skk=2,sdi=c(0,10),snb=c(1,Inf),
                                  rty=c("0","random"),rmo=character(0),
                                  rwg=rep(1,4),rkk=1,
                                  rk2=numeric(0)),pos=".GlobalEnv");
    assign("rebastaba.alk0",new("alk",ldes=new("des",name="AA"),
                                                      ltype="normal",lpara=list(mu=0,sigma=1),
                                                      lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                      lpod=list(c(-3,3)),ltransfo=character(0),
                                                      ldaf=rbsb.daf0,lwin=rebastaba.win0,lcomp=FALSE),pos=".GlobalEnv");
    assign("rebastaba.alk1",new("alk",ldes=char2des("my pretty alk"),
                                                 ltype="normal",lpara=list(mu=10,sigma=1),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-14,14)),ltransfo="(|1|)",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rebastaba.alk2",new("alk",ldes=char2des("simple alk with two parents"),
                                                 ltype="normal",lpara=list(mu="{{A}}",sigma="abs({{B[a]}})"),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-10,50)),ltransfo="(|1|)",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rebastaba.alk3",new("alk",ldes=char2des("simple alk with all the variable of one parent"),
                                                 ltype="normal",lpara=list(mu="{{C[1]}}+{{C[2]}}",sigma=2),
                                                 lrep=0,lnat="conti",lvar="",lparent=character(0),
                                                 lpod=list(c(-10,50)),ltransfo="(|1|)",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rebastaba.alk4",new("alk",ldes=char2des("Cat"),
                                                 ltype="numcat",lpara=list(p=c(0.4,0.2,0.4)),
                                                 lrep=0,lvar="",lparent=character(0),
                                                 lpod=list(letters[1:3]),lnat="categ",
                                                 lcomp=FALSE),pos=".GlobalEnv");
    assign("rebastaba.pta0",new("pta",name="Null pta",
                                               vam="A",vac=character(0),
                                               vad=character(0),vav=character(0),
                                      kkk=2,pro=array(1:3,dim=3,dimnames=list(A=c("a","b","c")))
                                 ),pos=".GlobalEnv");
    assign("rebastaba.pta1",new("pta",name="P.Table",vam=c("A","B"),vac=character(0),vad=character(0),kkk=2,pro=array(rep(0.05,20),dim=c(4,5),dimnames=list(A=letters[1:4],B=LETTERS[1:5]))),pos=".GlobalEnv");
    assign("rebastaba.tlk",rebastaba.tlk,pos=".GlobalEnv");
    assign("rebastaba.ltn",rebastaba.ltn,pos=".GlobalEnv");
    assign("rebastaba.lta",rebastaba.lta,pos=".GlobalEnv");
    assign("rebastaba.l_a",rebastaba.l_a,pos=".GlobalEnv");
    assign("rebastaba.f_d",rebastaba.f_d,pos=".GlobalEnv");
    #
    ######################
    #
    assign("rebastaba.mck", TRUE,pos=".GlobalEnv");
    assign("rebastaba.mwa", TRUE,pos=".GlobalEnv");
    assign("rebastaba.msi","/rebastaba/",pos=".GlobalEnv");
    #
    assign("rebastaba.bn0",new("bn",
                          description=rbsb.des0,
                          nom=rbsb.nom0, 
                          ntype=rbsb.cha0,   
                          ndes=rbsb.lis0,         
                          npara=rbsb.lis0,         
                          nrep=rbsb.num0,      
                          ntransfo=rbsb.cha0,    
                          ndaf=rbsb.lis0,         
                          nwin=rbsb.lis0,         
                          nfun=rbsb.lis0,         
                          nfug=rbsb.lis0,         
                          vnat=rbsb.cha0,    
                          vpod=rbsb.lis0,         
                          vred=rbsb.lis0,         
                          vcod=rbsb.lis0,         
                          vparent=rbsb.lis0       
                        ),pos=.GlobalEnv);
    assign("rebastaba.bn1",new("bn",
                          description=char2des("Example 1 bn"),
                          nom=rbsb.nom1, 
                          ntype=rep("normal",3),   
                          ndes=list(char2des("First Node"),
                                    char2des("Second Node"),
                                    char2des("Third Node")),         
                          npara=list(list(mu=0,sigma=1),
                                     list(mu="{{A}}",sigma=1),
                                     list(mu="{{B}}",sigma=1)),
                          nrep=rep(1,3),
                          ntransfo=rep("(|1|)",3),    
                          ndaf=rep(list(rbsb.daf0),3),
                          nwin=rep(list(rebastaba.win0),3),         
                          nfun=rep(list(rbsb.cha0),3),
                          nfug=list(function(X){rnorm(nrow(X),mean=0,sd=1);},
                                    function(X){rnorm(nrow(X),mean=X[,"A"],sd=1);},
                                    function(X){rnorm(nrow(X),mean=X[,"B"],sd=1);}),         
                          vnat=rep("conti",3),    
                          vpod=rep(list(c(-10,10)),3),         
                          vred=rep(list(c(-10,10)),3),         
                          vcod=rep(list(c(-10,10)),3),         
                          vparent=list(character(0),"A","B")       
                        ),pos=.GlobalEnv);
    assign("rebastaba.bn2",new("bn",
                          description=char2des("Example 2 bn"),
                          nom=rbsb.nom2, 
                          ntype=rep("normal",2),   
                          ndes=list(char2des("Node 1"),
                                    char2des("Node 2")),         
                          npara=list(list(mu=0,sigma=1),
                                     list(mu="{{A}}",sigma=1)),
                          nrep=c(1,3),
                          ntransfo=rep("(|1|)",2),    
                          ndaf=rep(list(rbsb.daf0),2),
                          nwin=rep(list(rebastaba.win0),2),         
                          nfun=rep(list(rbsb.cha0),2),         
                          nfug=list(function(X){rnorm(nrow(X),mean=0,sd=1);},
                                    function(X){cbind(rnorm(nrow(X),mean=X[,"A"],sd=1),
                                                      rnorm(nrow(X),mean=X[,"A"],sd=1),
                                                      rnorm(nrow(X),mean=X[,"A"],sd=1));}
                                   ),         
                          vnat=rep("conti",4),    
                          vpod=rep(list(c(-10,10)),4),         
                          vred=rep(list(c(-10,10)),4),         
                          vcod=rep(list(c(-10,10)),4),         
                          vparent=list(character(0),"A","A","A")       
                        ),pos=.GlobalEnv);
    assign("rebastaba.bn3",new("bn",
                          description=char2des("Example 3 bn"),
                          nom=rbsb.nom3, 
                          ntype=c("normal","Dirac","normal"),   
                          ndes=list(char2des("First Node"),
                                    char2des("Second Node"),
                                    char2des("Third Node")),         
                          npara=list(list(mu=100,sigma=10),
                                     list(k="{{A[a]}}+10"),
                                     list(mu="{{A[c]}}",sigma=1)),
                          nrep=c(3,1,2),
                          ntransfo=rep("(|1|)",3),    
                          ndaf=rep(list(rbsb.daf0),3),
                          nwin=rep(list(rebastaba.win0),3),         
                          nfun=rep(list(rbsb.cha0),3),         
                          nfug=list(function(X){cbind(rnorm(nrow(X),mean=100,sd=10),
                                                      rnorm(nrow(X),mean=100,sd=10),
                                                      rnorm(nrow(X),mean=100,sd=10));},
                                    function(X){X[,"A[a]"]+10;},
                                    function(X){cbind(rnorm(nrow(X),mean=X[,"A[c]"],sd=1),
                                                      rnorm(nrow(X),mean=X[,"A[c]"],sd=1));}
                                   ), 
                          vnat=rep("conti",6),    
                          vpod=rep(list(c(-100,100)),6),         
                          vred=rep(list(c(-100,100)),6),         
                          vcod=rep(list(c(-100,100)),6),         
                          vparent=list(character(0),character(0),character(0),"A[a]","A[c]","A[c]")       
                        ),pos=.GlobalEnv);
    assign("rebastaba.bn4",new("bn",
                          description=char2des("Example 4 bn"),
                          nom=rbsb.nom4, 
                          ntype=c("numcat","numcat"),   
                          ndes=list(char2des("First Node"),
                                    char2des("Second Node")),         
                          npara=list(list(p=c(50,50)),
                                     list(p=matrix(c(20,70,10,70,20,10),2))),
                          nrep=c(1,1),
                          ntransfo=rep("",2),    
                          ndaf=rep(list(rbsb.daf0),2),
                          nwin=rep(list(rebastaba.win0),2),         
                          nfun=rep(list(rbsb.fun0),2),         
                          nfug=rep(list(rbsb.fun0),2),         
                          vnat=rep("categ",2),    
                          vpod=list(c("no","yes"),c("black","grey","white")),
                          vred=list(c("no","yes"),c("black","grey","white")),
                          vcod=list(c("no","yes"),c("black","grey","white")),
                          vparent=list(character(0),"a")       
                        ),pos=.GlobalEnv);
    assign("rebastaba.bn5",new("bn",
                          description=char2des("Example 5 bn"),
                          nom=rbsb.nom5, 
                          ntype=c("numcat","normal"),   
                          ndes=list(char2des("First Node"),
                                    char2des("Second Node")),         
                          npara=list(list(p=c(50,50)),
                                     list(mu="1+({{a}}=='yes')",sigma=1)),
                          nrep=c(1,1),
                          ntransfo=rep("",2),    
                          ndaf=rep(list(rbsb.daf0),2),
                          nwin=rep(list(rebastaba.win0),2),         
                          nfun=rep(list(rbsb.fun0),2),         
                          nfug=list(rbsb.fun0,function(X){rnorm(nrow(X),mean=1+(X[,"a"]=="yes"),sd=1)}),         
                          vnat=c("categ","conti"),    
                          vpod=list(c("no","yes"),c(-3,3)),
                          vred=list(c("no","yes"),c(-3,3)),
                          vcod=list(c("no","yes"),c(-3,3)),
                          vparent=list(character(0),"a")       
                        ),pos=.GlobalEnv);
    assign("rebastaba.bn6",new("bn",
                          description=char2des("Example 6 of bn"),
                          nom=new("nom",x=list(A=c("SEX","AGE","HGT"),
                                               B=c("SEX","AGE","HGT","WGT"))),
                          ntype=c("empidata","empidata"),   
                          ndes=list(char2des("Target"),
                                    char2des("Predictor")),         
                          npara=list(rbsb.lis0,rbsb.lis0),
                          nrep=c(0,0),
                          ntransfo=rep("",2),    
                          ndaf=list(rbsb.daf2,rbsb.daf3),
                          nwin=list(rebastaba.win2,rebastaba.win3),
                          nfun=rep(list(rbsb.fun0),2),         
                          nfug=list(expr3func("{'Reported within simulate8bn';}"),expr3func("{'Reported within simulate8bn';}")),
                          vnat=c("categ",rep("conti",2),"categ",rep("conti",3)),    
                          vpod=list(c("M","F"),c(20,79),c(120,195),c("M","F"),c(20,79),c(120,195),c(40,125)),
                          vred=list(c("M","F"),c(20,79),c(120,195),c("M","F"),c(20,79),c(120,195),c(40,125)),
                          vcod=list(c("M","F"),c(20,79),c(120,195),c("M","F"),c(20,79),c(120,195),c(40,125)),
                          vparent=list(rbsb.cha0,rbsb.cha0,rbsb.cha0,
                                       c("A[SEX]","A[AGE]","A[HGT]"),c("A[SEX]","A[AGE]","A[HGT]"),
                                       c("A[SEX]","A[AGE]","A[HGT]"),c("A[SEX]","A[AGE]","A[HGT]"))
                        ),pos=.GlobalEnv);
    assign("rebastaba.emnd",-1,pos=.GlobalEnv);
    assign("rebastaba.cond","dor",pos=.GlobalEnv);
    xxx <- c("number of candidates","maximum distance for candidates");
    names(xxx) <- c("nb_c","di_x");
    assign("rebastaba.ena",xxx,pos=.GlobalEnv);
    #
    #######################################
    #
    assign("rebastaba.sii",0,pos=.GlobalEnv);
    assign("rebastaba.csv",paste(rbsb.cpt["variables","opening"],
                            rbsb.all,
                            rbsb.cpt["variables","closing"],sep=""),
           pos=.GlobalEnv);
    assign("rebastaba.esu",FALSE,pos=.GlobalEnv);
    assign("rebastaba.nar",0.3,pos=.GlobalEnv);
    assign("rebastaba.ede", 1,pos=.GlobalEnv);
    assign("rebastaba.eti",50,pos=.GlobalEnv);
    assign("rebastaba.efi","rebastaba.empidat.txt",pos=.GlobalEnv);
    assign("rebastaba.cdi", 1,pos=.GlobalEnv);
    assign("rebastaba.mnu",15,pos=.GlobalEnv);
    assign("rebastaba.mnb",45,pos=.GlobalEnv);
    assign("rebastaba.mnd",10,pos=.GlobalEnv);
    assign("rebastaba.scn",">?<",pos=.GlobalEnv);
    assign("rebastaba.scv",c(0,1,3,10),pos=.GlobalEnv);
    assign("rebastaba.dn0",new("dn",
                          description=rbsb.des0,
                          nat=rbsb.cha0,    
                          pod=rbsb.lis0,         
                          red=rbsb.lis0,         
                          cod=rbsb.lis0,         
                          df=as.data.frame(matrix(NA,0,0))
                        ),pos=.GlobalEnv);
    assign("rebastaba.dn1",new("dn",
                          description=char2des("1rst Example of /dn/"),
                          nat=rep("conti",4),    
                          pod=c(rep(list(c(-10,100)),3),list(c( 0,Inf))),         
                          red=c(rep(list(c(- 5, 80)),3),list(c( 0,90))),         
                          cod=c(rep(list(c(  0, 50)),3),list(c(20,60))),         
                          df=as.data.frame(matrix(round(abs(100*sin(1:40))),10,
                                           dimnames=list(NULL,c(nanv(rbsb.nom1,0),rebastaba.scn))))
                        ),pos=.GlobalEnv);
    assign("rebastaba.dn2",new("dn",
                          description=char2des("2d Example of /dn/"),
                          nat=rep("conti",5),    
                          pod=c(rep(list(c(-10,10)),4),list(c(0,Inf))),         
                          red=c(rep(list(c(-10,10)),4),list(c(0,50))),         
                          cod=c(rep(list(c(-10,10)),4),list(c(0,30))),         
                          df=as.data.frame(matrix(round(abs(100*sin(1:50))),10,
                                           dimnames=list(NULL,c(nanv(rbsb.nom2,0),rebastaba.scn))))
                        ),pos=.GlobalEnv);
    assign("rebastaba.dn3",new("dn",
                          description=char2des("3rd Example of /dn/"),
                          nat=rep("conti",7),    
                          pod=c(rep(list(c(-10,10)),6),list(c(0,Inf))),         
                          red=c(rep(list(c(-10,10)),6),list(c(0,50))),         
                          cod=c(rep(list(c(-10,10)),6),list(c(0,30))),         
                          df=as.data.frame(matrix(round(abs(100*sin(1:70))),10,
                                           dimnames=list(NULL,c(nanv(rbsb.nom3,0),rebastaba.scn))))
                        ),pos=.GlobalEnv);
    assign("rebastaba.dn4",new("dn",
                          description=char2des("4th Example of /dn/"),
                          nat=c(rep("conti",3),rep("categ",3)),    
                          pod=c(list(c(0,50)),
                                list(c(0,100)),
                                list(c(0,100)),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:4]),
                                list(LETTERS[1:4])
                               ),         
                          red=c(list(c(0,20)),
                                list(c(0,50)),
                                list(c(0,100)),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:3]),
                                list(LETTERS[1:4])
                               ),         
                          cod=c(list(c(0,10)),
                                list(c(0,30)),
                                list(c(0,100)),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:2]),
                                list(LETTERS[1:4])
                               ),         
                          df=data.frame(matrix(round(abs(100*sin(1:300))),100,
                                               dimnames=list(NULL,LETTERS[1:3])),
                                        matrix(sample(rep(LETTERS[1:4],75),300),
                                               100,3,
                                               dimnames=list(NULL,LETTERS[4:6]))
                                       )
                        ),pos=.GlobalEnv);
    assign("rebastaba.emdn","dr",pos=.GlobalEnv);
}
#
# Completing the series of null objects
#
rbsb.null <- c(rbsb.null,c("rebastaba.win0","rebastaba.alk0","rebastaba.pta0",
                           "rebastaba.bn0","rebastaba.dn0"));    
assign("rbsb.null",rbsb.null,pos=.GlobalEnv);
#
#
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
    res <- c(res,check4tyle(object@nat,"ncharacter",c(1,Inf)));
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
    res <- c(res,check4tyle(object@swg,"nnumeric",  -1,message="valid8win",fatal=FALSE));
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
    res <- c(res,check4tyle(object@skk  ,"numeric",1*cova,message=paste("win@skk must be numeric of length",1*cova),fatal=FALSE));
    res <- c(res,check4tyle(object@sdi ,"numeric",2*cova,message=paste("win@sdi  must be numeric of length",2*cova),fatal=FALSE));
    res <- c(res,check4tyle(object@snb ,"integer",2*cova,message=paste("win@snb  must be integer of length",2*cova),fatal=FALSE));
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
    res <- c(res,check4tyle(object@rty ,"character", 2,message="win@rty  must be character of size 2",fatal=FALSE));
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
    res <- c(res,check4tyle(object@rmo  ,"character",c(0,1),message=paste("win@rmo must be numeric(0:1)"),fatal=FALSE));
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
        res <- c(res,check4tyle(object@rwg,"numeric",nbva,message="valid8win",fatal=FALSE));
        res <- c(res,check4tyle(object@rkk  ,"numeric",1,message=paste("win@rkk must be numeric of length",1),fatal=FALSE));
    }
    #
    if ((object@rty[2] %in% choix2[1:4]) & 
        (object@rty[1] %in% choix1[1:2]) & 
        (length(object@rmo) > 0)) {
        res <- c(res,paste("No monitoring variable for @rty =",object@rty))
    }
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rebastaba.mwa);}
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# print(rebastaba.win0);
# print(rebastaba.win1);
# print(rebastaba.win2);
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
if (rebastaba.mck) {
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
    for (ii in bf(x@nat)) {
        cat(names(x@nat)[ii],": ",x@nat[ii],";  ",sep="");
    }
    cat("\n");
    cat("@swg  =  ");
    for (ii in bf(x@swg)) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (rebastaba.mck) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (isvide(li)) {return(rebastaba.win0);}
# some checks
if (rebastaba.mck) {
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
    res <- c(res,check4tyle(object@lcomp,"logical",1,message="for /alk/, logical slot @lcomp must exist",fatal=FALSE));
    if (is.character(rr)) { res <- c(res,rr);}
    #
    # the type must be defined
    check4tyle(object@ltype,"character",1,message="for /alk/, character slot @ltype must exist",fatal=TRUE);
    # the nature must be character even if of length zero
    res <- c(res,check4tyle(object@lnat,"character",-1,message="for /alk/, character slot @lnat must exist",fatal=FALSE));
    ###
    nbv <- length(alkvar(object));
    if (object@ltype == "empidata") {
        rr <- valid8win(object@lwin);
        if (is.character(rr)) { res <- c(res,rr);}
        if (identical(object@lwin,rebastaba.win0)) {
            res <- c(res,"NULL /win/ not allowed for an data based /alk/");
        }
    } else {
        if(!identical(object@lwin,rebastaba.win0)) {
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
	    if (!(vv %in% c(names(ddd),names(rebastaba.ena)))) {
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
    } else { erreur(res,w=rebastaba.mwa);}
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
ax <- function(x) { form3titre(x,1,retrait);}
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
quoi <- names(rebastaba.l_a[lt,rebastaba.l_a[lt,]!="-"]);
for (qui in quoi) {
    if (qui == "lpara") {
        nbp <- length(alk@lpara);
        ax(paste("distribution defined with",nbp,
                         "parameter(s)"));
        if (lt %in% c("numcat")) {
            ax("See the probability table below");
        } else {
            for (ip in bc(nbp)) {
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
            form3titre(form3liste(nav,OPA="",CPA="",opa="",cpa="",sep="; "),1,9);
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
ax <- function(x) { form3titre(x,1,retrait);}
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
quoi <- names(rebastaba.l_a[lt,rebastaba.l_a[lt,]!="-"]);
for (qui in quoi) {
    if (!isvide(alk@lpara)) {
    if (qui == "lpara") {
        nbp <- length(alk@lpara);
        ax(paste("distribution defined with",nbp,
                         "parameter(s)"));
        if (lt %in% c("numcat")) {
            ax("See the probability table below");
        } else {
            for (ip in bc(nbp)) {
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
            form3titre(paste(nav,collapse=" ; "),1,9);
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# print(new("alk",ldes=new("des",name="AA"),
#                         ltype="normal",
#                         lpara=list(mu=0,sigma=1),
#                         lrep=0,
#                         lnat="conti",
#                         lvar="",lparent=character(0),
#                         lpod=list(c(-3,3)),
#                         ltransfo=character(0),
#                         ldaf=rbsb.daf0,
#                         lwin=rebastaba.win0,
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
    form3titre("Probability table without interpretation nor normalization",1);
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
    } else { erreur(res,w=rebastaba.mwa);}
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
        for (vv in bf(pt@vam)) {
            cat(pt@vam[vv],"(",didi[dima[vv]]," categories) ",sep="");
        }
        cat("\n");
    } else { cat("NO Marginal dimension\n");}
    if (length(pt@vac)>0) {
        cat("Conditional dimensions are: ");
        for (vv in bf(pt@vac)) {
            cat(pt@vac[vv],"(",didi[dico[vv]]," categories) ",sep="");
        }
        cat("\n");
    } else { cat("NO Conditional dimension\n");}
    if (length(pt@vad)>0) {
        cat("Conditioned dimensions are:\n");
        for (vv in bf(pt@vad)) {
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
        for (uu in bf(dico)) {
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
        for (uu in bf(dima)) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (rebastaba.mck) {
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
    for (dd in bf(para)) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# print(normalize4pta(rebastaba.pta0));
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
if (rebastaba.mck) {check4valid(valid8pta(pt));}
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
    for (xn in bf(n)) {
        xnames[[xn]] <- form3names(n[xn]);
    }
    if (length(n) > 0) { 
        names(xnames) <- paste("CV",bf(n),sep="");
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
for (va in bc(nva)) { if (length(xnames[[va]])!=n[va]) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (rebastaba.mck) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (rebastaba.mck) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# uu <- rebastaba.pta1;
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
if (rebastaba.mck) {check4valid(valid8pta(pt));}
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
    form3titre(paste(" ",nos[2],"|",nos[1],add," "),1);
} else {
    if (comment == 2) {
        form3titre(paste(" ",nos[1],"|",nos[2],add," "),1);
    } else {
        form3titre(paste(" ",nos[1],"x",nos[2],add," "),1);
    }
}
print(pro);
# returning
invisible();
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (rebastaba.mck) {
    if (length(x$blo) != length(x$typ)) {
        erreur(x,"block and type components have got different lengths!");
    }
}
###
pano <- pava <- numeric(0);
for (jbd in bf(x$blo)) {
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
            if (qui <= 0) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# check8alk(rebastaba.alk1);
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
    if (!(object@ltype %in% rebastaba.ltn)) {
        erreur(list(rebastaba.ltn,object@ltype),"This 'alk@ltype' is not registrated!");
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
    if (rebastaba.tlk[[object@ltype]]$fami == "data_based") {
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
    if ("lpara" %in% rebastaba.tlk[[object@ltype]]$argYES) {
        # parameters are compulsory
        for (para in bf(rebastaba.tlk[[object@ltype]]$par)) {
            npa <- names(rebastaba.tlk[[object@ltype]]$par)[para];
            if (isvide(object@lpara[[npa]])) {
                res <- c(res,paste(object@ldes@name,":",paste("The parameter",npa,"was expected for the",object@ltype,"type of node")));
            }
        }
    }
    ###
    ###
    # consistence of the lengths of parameter vectors and the number of repetitions
    if (isalkrepeatable(object@ltype)) { for (jbd in bf(object@lpara)) {
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
	for (ii in bc(nbv)) {
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
	# checking the strong requirement described by rebastaba.tlk
        # at the level definition which is applied to non completed /alk/
        #
        ############ DOESN'T WORK TO BE IMPLEMENTED AFTER BETTER THOUGHTS
	speci <- rebastaba.tlk[[object@ltype]] ;
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
	for (ii in bf(natu)) {
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (rebastaba.mck) {
    check4tyle(ltype,"character",1,message="One on only one 'ltype' is expected!");
    if (!(ltype %in% rebastaba.ltn)) {
        cat(rebastaba.ltn);
        erreur(ltype,"This ltype is not registrated!");
    }
}
# returning
rebastaba.l_a[ltype,"rep?"] == "TRUE";
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# parents8win(rebastaba.win1);
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# variables8win(rebastaba.win1);
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# parents8alk(rebastaba.alk2,rbsb.nom2);
# parents8alk(rebastaba.alk2,rbsb.nom2,of="n");
# parents8alk(rebastaba.alk3,rbsb.nom3);
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
possibles <- c("ls","all","list","argu",rebastaba.ltn);
if (!(quelles %in% possibles)) {
    form3titre("The possible arguments are:",4);
    cat(possibles,"\n",sep="\n");
    cat(" And you provided '",quelles,"'!\n",sep="");
}
if (quelles == "ls") {
    form3titre("The present different 'ltype' are",nvp[2]+1,inp[2]);
    for (jbd in bf(rebastaba.tlk)) {
        cat("         ",rebastaba.ltn[jbd],"\n");
    }
}
if (quelles=="argu") {
    form3titre("Arguments/Properties according to the different 'ltype's",3);
    cat(" <YES> for 'must be provided by the user'\n",
        "<yes> for 'can be provided by the user but there is a default value'\n",
        "< no> for 'necessary but generated by rebastaba'\n",
        "< NO> for 'necessary but fixed by rebastaba'\n",
        "< - > for 'irrelevant'\n");
    kY <- c(4,5);
    kn <- ncol(rebastaba.l_a);
    print(as.data.frame(rebastaba.l_a[,-c(kY,kn)]));
    cat("     (",dimnames(rebastaba.l_a)[[2]][kY],") are always YES\n");
    cat("     (",dimnames(rebastaba.l_a)[[2]][kn],") is always  no\n");
    cat("Probability families are:\n");
    for (uu in bf(rebastaba.f_d)) {
        cat("   ",names(rebastaba.f_d)[uu],":",rebastaba.f_d[uu],"\n");
    }
    cat(" You can also 'print(rebastaba.alk_examples)' and 'getClassDef('alk')'\n");
}
if (quelles == "list") {
    form3titre("The present different 'ltype' are",nvp[2]+1,inp[2]);
    for (jbd in bf(rebastaba.tlk)) {
        cat("\n<<<",rebastaba.ltn[jbd],">>>\n");
        cat((rebastaba.tlk)[[jbd]]$defi,fill=56);
    }
}
if (quelles=="all") { quelles <- rebastaba.ltn;}
if (any(quelles %in% rebastaba.ltn)) { for (jbd in quelles) {
    if(!(jbd %in% rebastaba.ltn)) {
        erreur(jbd,"This is not a valid argument see \"help8ltype()\"");
    }
    uu <- rebastaba.tlk[[jbd]];
    form3titre(paste("ltype =",jbd),nvp[1]+1,inp[1]);
    cat((rebastaba.tlk)[[jbd]]$defi,fill=56);
    if (isalkrepeatable(jbd)) {
        form3titre("This distribution can be repeated",nvp[3]+1,inp[3]);
    } else {
        form3titre("This distribution cannot be repeated",nvp[3]+1,inp[3]);
    }
    if (rebastaba.l_a[jbd,"bugs?"]) {
        form3titre("This distribution can be translated into Bugs",nvp[3]+1,inp[3]);
    } else {
        form3titre("This distribution cannot be  translated into Bugs",nvp[3]+1,inp[3]);
    }
    for (jd in dimnames(rebastaba.l_a)[[2]]) {
        pres <- form3justifie(paste("(",jd,"):",sep=""),10,1);
        if (!(expr3present(rebastaba.l_a[jbd,jd],c("-","no","NO"),exact=TRUE))) {
            defaut <- (rebastaba.l_a[jbd,jd]=="yes");
            if (jd == "lvar") {
               form3titre(paste(pres,"names for the variables of the node"),nvp[3]+1,inp[3]);
               if (defaut) {form3titre(paste("default: ['",paste(uu$lvar,collapse="','"),"']",sep=""),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lpara") {
                form3titre(paste(pres,"there are",
                                 length(uu$par),
                                 "parameters:"),
                           nvp[3]+1,inp[3]);
                for (hd in bf(uu$par)) { 
                    form3titre(paste(names(uu$par)[hd],":",uu$par[hd],sep=""),nvp[3]+1,inp[3]+16);
                }
            }
            if (jd == "ltransfo") {
                form3titre(paste(pres,"some transformation can be asked for the generated values"),
                           nvp[3]+1,inp[3]);
                if (is.null(uu$ltransfo)) { vv <- "NULL";} else { vv <- uu$ltransfo;}
                if (defaut) {form3titre(paste("default:",vv),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lnat") {
                form3titre(paste(pres,"nature(s) of the variable(s) node"),nvp[3]+1,inp[3]);
                if (defaut) {form3titre(paste("default:",uu$lnat),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lpod") {
                form3titre(paste(pres,"possible domain(s) of the variable(s) node"),nvp[3]+1,inp[3]);
                if (defaut) {form3titre(paste("default:",uu$lpod),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lred") {
                form3titre(paste(pres,"representation domain(s) of the variable(s) node"),nvp[3]+1,inp[3]);
                if (defaut) {form3titre(paste("default: lpod value"),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lcod") {
                form3titre(paste(pres,"common domain(s) of the variable(s) node"),nvp[3]+1,inp[3]);
                if (defaut) {form3titre(paste("default: lred value"),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lparent") {
                form3titre(paste(pres,"names of the node parents"),nvp[3]+1,inp[3]);
                if (defaut) {form3titre(paste("default: no parents"),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lfunct") {
                form3titre(paste(pres,"piece of code (depends on the ltype)"),nvp[3]+1,inp[3]);
                if (defaut) {form3titre(paste("default:"),nvp[3]+1,inp[3]+16);
                form3repete(" ",32,imp=TRUE); print(uu$lfunct);}
            }
            if (jd == "ldaf") {
                form3titre(paste(pres,"how to get the values of an data based distribution"),nvp[3]+1,inp[3]);
                form3titre(paste("directly provided with a data.frame"),nvp[3]+1,inp[3]+16);
                if (defaut) {form3titre(paste("default:",uu$ldaf),nvp[3]+1,inp[3]+16)};
            }
            if (jd == "lwin") {
                form3titre(paste(pres,"defines variables/natures and other features for",
                                      "empidata (see function /win/ definition)"),
                           nvp[3]+1,inp[3]);
            }
        } else {
            rres <- "not used";
            # ??? 10_08_11 suppressed because not understood at the moment
            #if (jd == "lvar") {
            #    rres <- paste("not used, always",uu$nbv&&&,"variable(s)");
            #    form3titre(paste(pres,rres),nvp[3]+1,inp[3]);
            #    form3titre(paste(uu$nat,"is the default nature"),nvp[3]+1,inp[3]+16);
            #    form3titre(paste(uu$pod,"is the possible domain"),nvp[3]+1,inp[3]+16);
            #}
        }
    }
}}
cat("\nhelp8ltype(\"",rebastaba.ltn[1],
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
if (!(ltype %in% rebastaba.ltn)) {
    cat(rebastaba.ltn);
    erreur(ltype,"This ltype is not registrated!");
    }
# returning
rebastaba.tlk[[ltype]]$fami %in% c("conti_vector","discr_vector");
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
new8alk <- function(ldes,ltype,
                    lpara=rbsb.lis0,lrep=0,
                    lnat=rbsb.cha0,lvar=rbsb.cha0,
                    lparent=rbsb.cha0,
                    lpod=rbsb.lis0,lred=rbsb.lis0,lcod=rbsb.lis0,
                    ltransfo="",
                    ldaf=rbsb.daf0,lwin=rebastaba.win0,
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
#          to the \code{names(rebastaba.tlk)}. This list is
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
#         is provided (see rebastaba.tlk).\cr
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
if (rebastaba.mck) {
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
    if (rebastaba.mck) {check4valid(valid8daf(ldaf));}
} else { ldaf <- rbsb.daf0;}
#
if (!isvide(lwin)) {
    if (rebastaba.mck) {check4valid(valid8win(lwin));}
} else { 
    lwin <- rebastaba.win0;
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
if (!(ltype %in% rebastaba.ltn)) {
    cat("Acceptable 'ltype's are:",rebastaba.ltn,"\n");
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
if ("lparent" %in% rebastaba.tlk[[ltype]][["argno"]]) {
if (identical(rebastaba.tlk[[ltype]]$lparent,rbsb.cac)) {
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
for (iii in bf(argu)) {
    arg <- names(argu)[iii];
    argument <- argu[[arg]];
    quoi <- rebastaba.l_a[ltype,arg];
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
                for (aaa in bf(argu[["lpara"]])) {
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
                 argu[["lwin"]] <- rebastaba.win0;
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
        if (protester) { if (rebastaba.mwa) {
            cat("With ltype =",ltype,"\n");
            erreur(argument,"The argument",arg,"must not be provided by the user (see 'help8ltype')",w=TRUE);
        }}
    }}
    #==================================
    if (quoi %in% c("NO")) {
        vvv <-  rebastaba.tlk[[ltype]][[arg]];
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
            vvv <-  rebastaba.tlk[[ltype]][[arg]];
	    if (is.null(vvv)) {
		meme <- paste("This parameter (",arg,
			  ") must be provided in rebastaba.tlk",
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
             for (ss in bf(llpp)) { llpp[[ss]] <- argu[[arg]][[1]];}
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
if (rebastaba.mck) {check4valid(valid8alk(res));}
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# list2alk(alk2list(rebastaba.alk0));
# list2alk(alk2list(rebastaba.alk1));
# list2alk(alk2list(rebastaba.alk2));
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
if (rebastaba.mck) {check4valid(valid8des(des));}
#
# checking the type
if (isvide(li$ltype)) {
    erreur(li,"ltype is compulsory");
} else {
    if (!(li$ltype %in% rebastaba.ltn)) {
        cat("Known ltype's are:",rebastaba.ltn,"\n");
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
for (nunu in bf(nanat)) {
    if (rbsb.snp[nanat[nunu],"numeric"]) {
        # here the NA warned were generated
        res@lpod[[nunu]] <- as.numeric(res@lpod[[nunu]]);
        res@lred[[nunu]] <- as.numeric(res@lred[[nunu]]);
        res@lcod[[nunu]] <- as.numeric(res@lcod[[nunu]]);
    }
}
#
# checking the result
if (rebastaba.mck) {check4valid(valid8alk(res));}
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
# alk2list(rebastaba.alk0);
# alk2list(rebastaba.alk1);
# alk2list(rebastaba.alk2);
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
    vv <- rebastaba.l_a[lty,ec];
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
# rebastaba3k("RESET"); # needed only for R checking, to be forgotten
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
if (rebastaba.mck) {
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
if (rebastaba.mck) {check4valid(valid8alk(alk));}
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
# rebastaba3k("RESET"); # (only for R checking)
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
if (rebastaba.mck) {
    check4tyle(eas,c("numeric","character"),-1);
    check4valid(valid8nom(nom));
}
#
if (is.numeric(eas)) { eas <- as.character(eas); }
#
# dealing with empty easyp code: character(0) or only spaces.
neas <- length(eas);
RET <- 0;
for (i in bf(eas)) {
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
for (jj in bf(dd$typ)) { if (dd$typ[jj]==1) { if (dd$blo[jj]!=rbsb.cni) {
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
    for (ii in bc(ppxx)) {
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
# rebastaba3k("RESET"); # To comply the R checking
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
if (rebastaba.mck) {
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
for (i in bf(eas)) {
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
    for ( ip in bc(nbb)) {
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
    for (nn in bc(neas)) {
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
    for (nn in bc(neas)) {
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
# rebastaba3k("RESET"); # for R checking
# alkvar(rebastaba.alk2)
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
