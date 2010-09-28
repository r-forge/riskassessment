

###########################################
###########################################
########
#((((((( NEW S4 CLASS bn
########
###########################################
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
valid8bn <- function(object)
#TITLE checks a /bn/
#DESCRIPTION (bn)
#   This function checks /bn/ objects
#DETAILS
# It is the validity method for /bn/ objects.
#KEYWORDS classes
#INPUTS
#{object} <<The bn object to be validated.>>
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
#CREATED 09_10_23
#REVISED 10_08_13
#--------------------------------------------
{
    if (class(object)!="bn") {
        erreur(NULL,paste("This object is not of class 'bn' but '",class(object),"'",sep=""));
    }
    res <- character(0);
    if (length(setdiff(slotNames("bn"),slotNames(object)))) {
        erreur(slotNames(object),paste("Not all slots (",slotNames("bn"),") are present",sep=""));
    }
    #
    rr <- valid8des(object@description);
    if (!identical(TRUE,rr)) { res <- c(res,rr);}
    #
    rr <- valid8nom(object@nom);
    if (!identical(TRUE,rr)) {
        erreur(object@nom,"Slot @nom is not valid");
    }
    #
    # checking the consistency of slot lengths
    nbn <- nbnv(object,-1); nbv <- nbnv(object,0);
    #
    if (length(object@ntype) != nbn) {
        res <- c(res,"length of @ntype is different from the node number");
    }
    if (length(object@ndes) != nbn) {
        res <- c(res,"length of @ndes is different from the node number");
    }
    if (length(object@npara) != nbn) {
        res <- c(res,"length of @npara is different from the node number");
    }
    if (length(object@nrep) != nbn) {
        res <- c(res,"length of @nrep is different from the node number");
    }
    if (length(object@ntransfo) != nbn) {
        res <- c(res,"length of @ntransfo is different from the node number");
    }
    if (length(object@ndaf) != nbn) {
        res <- c(res,"length of @ndaf is different from the node number");
    }
    if (length(object@nwin) != nbn) {
        res <- c(res,"length of @nwin is different from the node number");
    }
    if (length(object@nfun) != nbn) {
        res <- c(res,"length of @nfun is different from the node number");
    }
    if (length(object@nfug) != nbn) {
        res <- c(res,"length of @nfug is different from the node number");
    }
    #
    if (length(object@vnat) != nbv) {
        res <- c(res,"length of @vnat is different from the variable number");
    }
    if (length(object@vpod) != nbv) {
        res <- c(res,"length of @vpod is different from the variable number");
    }
    if (nbv > 0) { if (sum(sapply(sapply(object@vpod,is.na),sum)) > 0) {
        erreur(object@vpod,"'NA' not allowed in @pod");
    }}
    if (length(object@vred) != nbv) {
        res <- c(res,"length of @vred is different from the variable number");
    }
    if (nbv > 0) { if (sum(sapply(sapply(object@vred,is.na),sum)) > 0) {
        erreur(object@vred,"'NA' not allowed in @red");
    }}
    if (length(object@vcod) != nbv) {
        res <- c(res,"length of @vcod is different from the variable number");
    }
    if (nbv > 0) { if (sum(sapply(sapply(object@vcod,is.na),sum)) > 0) {
        erreur(object@vcod,"'NA' not allowed in @cod");
    }}
    if (length(object@vparent) != nbv) {
        res <- c(res,"length of @vparent is different from the variable number");
    }
    # checking the consistency of the repetition indicator
    if (nbn > 0) {
        repe <- object@nrep>1;
        if (length(repe)>0) {
            long <- sapply(object@nom@x,length);
            if (!all(object@nrep[repe]==long[repe])) {
                 res <- c(res,"Slot nrep (repetition numbers) is not consistent with slot nom");
            }
        }
    }
    #
    if (isvide(res)) {
        for (jjb in bc(nbn)) {
            rr <- valid8win(object@nwin[[jjb]]);
            if (is.character(rr)) {
                res <- c(res,rr);
              }
        }
    }
    #
    #
    if (length(res)== 0) { res <- TRUE;
    } else { erreur(res,w=rebastaba.mwa);}
    res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

###########################################
###########################################
setClass("bn",representation(
    description="des",      # description of the bn
    nom="nom",              # node and variable names
    #
    ntype="character",      # vector of the node types
    ndes="list",            # list of the node descriptions
    npara="list",           # list of the node lists of parameters
    nrep="numeric",         # repetition numbers of the  nodes
    ntransfo="character",   # list of transformation definitions
    ndaf="list",            # list of the node /daf/ objects
    nwin="list",            # list of the node /win/ objects
    nfun="list",            # list of programed defined functions
    nfug="list",            # list of the node generating functions
    #
    vnat="character",       # variable natures 
    vpod="list",            # variable possible domains
    vred="list",            # variable representation domains
    vcod="list",            # variable common domains
    vparent="list"          # list of the name parent variables 
                            #  (as character for each variable;
                            #   when there is no parent: character(0) NOT "")

        ));
###########################################

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8bn <- function(x,...,quoi="nv",qui=rbsb.cha0,comment="n",
                     quoi.des="nd",quoi.alk="d")
#TITLE prints a bn object
#DESCRIPTION (bn)
# prints different aspects of a bn object.
#DETAILS
# When two incompatible specifications are ordered into
# "comment" argument, the used one is the first in the description
# list.\cr
# \code{rbsb.mwi} could be increased to see the parentship without
# limitation due to the line length.
#PKEYWORDS bn
#KEYWORDS print
#INPUTS
#{x} <<The bn to be printed>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the
# standard print function.>>
#{quoi} << This character string indicates what must be printed.
#                 It can comprise:\cr
#         "a" for all\cr
#         "s" for standard\cr
#         "D" for description of the bn\cr
#         "t" for title\cr
#         "n" for nodes\cr
#         "P" for ascendants and descendants at the node level\cr
#         "p" for ascendants and descendants at the variable level\cr
#         "v" for variables\cr
#         "d" for description of the nodes\cr
#         "l" for definition of the links\cr
#         "g" for generating fonction of each node>>
#{qui}<< The nodes which must be considered for the printing.
#   rbsb.cha0 implies all nodes, if not either a character vector 
#   providing the nodes or a numeric vector with the iin (internal numbers 
#   of them). Using this argument is a way to modify the order of displaying
#   the information about the nodes when printing. E.g. qui=length(x@nom):1
#   will present the node in decreasing order. No selection is (for the moment)
#   possible at the variable level.>>
#{comment} << This character string indicates how it must
#                   be printed. It can comprise:\cr
#       "n": natural order based on @nom,\cr
#       "a": alphabetical order on the node/variable names,\cr
#{quoi.des} << what to print when printing the bn description>>
#{quoi.alk} << what to print when printing a node. Not used for the moment.>>
#VALUE
# nothing but some printing is issued
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(rebastaba.bn2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# (*) implement the printing of nodes and variables in a 
# nested list, without the parents and children
#AUTHOR J.-B. Denis
#CREATED 07_06_13
#REVISED 09_11_24
#--------------------------------------------
{
# old constant to be integrated
rbsb.lar_col <- 50;
# safety checks
if (rebastaba.mck) {
    check4tyle(   quoi,"character",1,message="from print8bn");
    check4tyle(comment,"character",1,message="from print8bn");
    # checking that something was indeed asked
    qquoi <- strsplit(quoi,"")[[1]];
    if (length(intersect(qquoi,
                         c("a","D","s","t","n","P","p","v","d","l","g")))==0) {
        erreur(quoi,"Nothing is asked with this character for 'print8bn'",w=rebastaba.mwa);
        quoi <- "s";
    }
}
#
# useful constants
nbno <- nbnv(x,-1); nbva <- nbnv(x,0);
nsou <- 50; ligne <- form3repete("-",nsou,FALSE,TRUE);
# nodes to be considered
if (isvide(qui)) { qui <- bc(nbno);
} else {
    if (is.numeric(qui)) { qui <- nv2ion(qui,x@nom,"n",FALSE)@nn;}
    qui <- match(qui,nv2ion(0,x@nom,"n",FALSE)@nn);
    qui <- qui[!is.na(qui)];
}
# which order to use for node printing?
if(expr3present("a",comment)) {
    nord <- order(nanv(x@nom));
} else {
    nord <- bc(nbno);
}
# complete or standard printing?
if (expr3present("s",quoi)) { quoi <- "tDn";}
if (expr3present("a",quoi)) { quoi <- "tdDnvPplg";}
#
# starting the printing
#
# general title
if (expr3present("t",quoi)) {
    form3titre(paste("Object ",class(x)," '",x@description@name,"' with ",nbno," nodes",sep=""),6);
    if (nbva > nbno) {
        cat("(and",nbva,"variables)\n\n");
    }
}
#
# description of the bn
if (expr3present("D",quoi)) {
    print(x@description,quoi=quoi.des);
    for (no in bc(nbnv(x))) { if (!isvide(x@ndes[[no]]@defi)) {
        nno <- paste("(",no,") ",nanv(x@nom,no)," : ",sep="");
        lnno <- nchar(nno);
        cat(nno);
        form3paragraphe(x@ndes[[no]]@defi,titre=-2,
                        wid=rbsb.mwi-5,fli=c(0,rbsb.mwi-lnno,lnno-3),
                            ed="    ",ef="",
                            imp=TRUE);
    }}
}
#
# node list
if (expr3present("n",quoi)) {
  if (length(qui) > 0) {
    # finding the necessary genealogy
    dadd <- parents8bn(x,"n");
    fifi <- children8bn(x,"n");
    form3titre("Node List",2);
    resume <- matrix(NA,nbno,6);
    dimnames(resume)[[2]] <- c("Name","nb.Var","ltype",
                               "Node  ",
                               "Parent(s)","Child(ren)");
    nn <- 0;
    lar <- round(rbsb.mwi/4);
    for (jbd in qui) {
        nn <- nn + 1;
	resume[nn,1] <- nv2ion(jbd,x@nom,"n")@nn;
        nbv <- nbnv(x,jbd);
        if (nbv==1) {
            nbv <- paste(nbv,"   ",sep="");
        } else {
            if (x@nrep[jbd]) {
                nbv <- paste(nbv,"(R)",sep="");
            } else {
                nbv <- paste(nbv,"(M)",sep="");
            }
        }
        resume[nn,2] <- nbv;
        resume[nn,3] <- x@ntype[jbd];
        resume[nn,4] <- form3nd(x,jbd);
        papa <- form3liste(dadd[[resume[nn,1]]],none="",opa="",cpa="",sep=";");
	resume[nn,5] <- form3justifie(papa,lar,0);
        fiston <- form3liste(fifi[[resume[nn,1]]],none="",opa="",cpa="",sep=";");
	resume[nn,6] <- form3justifie(fiston,lar,0);
    }
    print(as.data.frame(resume[1:nn,c(2,3,5,4,6),drop=FALSE]));
  } else { form3titre("No node to print",2);}
}
#
# variable list
if (expr3present("v",quoi)) {
  if (length(qui) > 0) {
    lar <- round(rbsb.mwi/4);
    dadd <- parents8bn(x,"v");
    fifi <- children8bn(x,"v");
    form3titre("Variable List",2);
    resume <- matrix(NA,nbva,5);
    dimnames(resume)[[2]] <- c("Name","Nature","Domain","Parents","Children");
    nv <- 0;
    for (jbd in qui) {
        vava <- nv2ion(matrix(c(jbd,0),2),x@nom,check=FALSE)@nvn;
        for (jd in bf(vava)) {
            vkk <- nv2ion(matrix(c(jbd,jd),2),x@nom,check=FALSE)@vk;
            nv <- nv + 1;
            resume[nv,1] <- vava[jd];
            resume[nv,2] <- x@vnat[vkk];
            if (rbsb.snp[resume[nv,2],"numeric"]) { sepa <- " -> "
	    } else { sepa <- " | ";}
            resume[nv,3] <- paste(x@vpod[[vkk]],collapse=sepa);
            papa <- form3liste(dadd[[vkk]],none="",opa="",cpa="",sep=";");
	    resume[nv,4] <- form3justifie(papa,lar,0);
            fiston <- form3liste(fifi[[vkk]],none="",opa="",cpa="",sep=";");
	    resume[nv,5] <- form3justifie(fiston,lar,0);
            if (nchar(resume[nv,3]) > rbsb.lar_col) {
                resume[nv,3] <- form3justifie(resume[nv,3],rbsb.lar_col,1);
            }
        }
    }
    print(as.data.frame(resume[1:nv,c(2,3,4,1,5),drop=FALSE]));
  } else { form3titre("No variable to print",2);}
}
#
# complete parentship at the node level
if (expr3present("P",quoi)) { if(length(qui) > 0) {
    # getting the ascendants and descendants at the node level
    papam <- arc2pam(bn2gn(x,"n")@arc);
    aadd <- explore8pam(papam)$rel[[1]];
    form3titre("Ascendants and Descendants at the node level",2);
    resume <- matrix(NA,length(qui),2);
    items <- nanv(x@nom,"n");
    dimnames(resume) <- list(items,c("Ascendants","Descendants"));
    nn <- 0;
    for (jbd in qui) { 
        nn <- nn+1;
	resume[nn,1] <- form3liste(items[which(aadd[jbd,]==-1)],none="");
	resume[nn,2] <- form3liste(items[which(aadd[jbd,]== 1)],none="");
    }
    print(resume);
  } else { form3titre("No nodes to give the genealogy",2);}
}
#
# complete parentship at the variable level
if (expr3present("p",quoi)) { if(length(qui) > 0) {
    # getting the ascendants and descendants at the variable level
    papam <- arc2pam(bn2gn(x,"v")@arc);
    aadd <- explore8pam(papam)$rel[[1]];
    form3titre("Ascendants and Descendants at the variable level",2);
    items <- nv2ion(rbind(qui,rep(0,length(qui))),x@nom,check=FALSE)@nvn;
    resume <- matrix(NA,length(items),2);
    dimnames(resume) <- list(items,c("Ascendants","Descendants"));
    nn <- 0;
    for (jbd in qui) { 
        nn <- nn+1;
	resume[nn,1] <- form3liste(items[which(aadd[jbd,]==-1)],none="");
	resume[nn,2] <- form3liste(items[which(aadd[jbd,]== 1)],none="");
    }
    print(resume);
  } else { form3titre("No variables to give the genealogy",2);}
}
#
# link definition and description
if (length(qui) > 0) {
if ((expr3present("l",quoi)) |
    (expr3present("d",quoi))) { 
    form3titre("Link Definition of nodes",3);
    for (jbd in qui) { 
	form3titre(paste(" Node name:",nanv(x@nom,jbd)),2);
        xalk <- bn2alk(x,jbd);
        print8alk(xalk,proba=TRUE);
        cat(ligne);
    }
    cat(ligne);
}}
# simulation function
if (expr3present("g",quoi)) { if(length(qui) > 0) {
    form3titre("Simulation Function of nodes",3);
    for (jbd in qui) { 
	form3titre(paste(" Node name:",nanv(x@nom,jbd)),2);
        print(x@nfug[[jbd]]);
        cat(ligne);
    }
}}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8bn <- function(x,y="useless",...,frame=FALSE)
#TITLE plots an object bn
#DESCRIPTION (bn)
# produces the plot of the graph of the BN x
#DETAILS
# The constant g4n.pgr0 is used to build the graph
#PKEYWORDS bn
#KEYWORDS
#INPUTS
#{x} <<the /bn/ object to plot>>
#{y} << Useless argument introduced for compatibility with
# generic plot functions of R.>>
#[INPUTS]
#{\dots} <<Further arguments to be passed to the
# standard plot function.>>
#{frame} << must a frame be added?>>
#VALUE
#  returns nothing but a plot is drawn
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# \dontrun{plot(rebastaba.bn2);}
#REFERENCE
#SEE ALSO modify4x
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_16
#REVISED 09_11_05
#--------------------------------------------
{
# constructing the "grbsb" object
gf <- bn2gn(x);
gf@pgr <- g4n.pgr0;
# performing the plot
plot(gf,frame=frame,
     main=x@description@name);
invisible();    
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

setMethod("plot", signature(x = "bn"),  plot8bn);
setMethod("print",signature(x = "bn"), print8bn);

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
subn <- function(bn)
#TITLE finds independent sub-bn in a BN
#DESCRIPTION (bn)
#  /// FOR THE MOMENT DOES NOT WORK PROPERLY ///
# exploring the parentship, returns an order on the nodes,
# such that generating the nodes
# following this order produces a draw from the joint
# distribution.
#\cr By the way describes the independent subsets of nodes and
# stops with a message when a cycle is suspected.
#DETAILS
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{bn} <<The bn object>>
#[INPUTS]
#VALUE
# a list with
#{subsets} <<independent subsets indicated by successive
#            numbers with respect to iin order. Its max
#            is then the number of independent subsets.>>
#{order} <<a compatible order to the calculation
#          of the joint probability. It is given
#          with respect to the iin order. It follows
#          the independent subsets.>>
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# cat("To be added\n");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_16
#REVISED 07_06_30
#--------------------------------------------
{
if (rebastaba.mck) {check4valid(valid8bn(bn));}
rapport("FOR THE MOMENT DOES NOT WORK PROPERLY");
nbno <- nbnv(bn);
# 
# local constants
#
rbsb.di_num <- 0.5;
rbsb.di_cat <- 1;
#
# looking for independent subsets
#
nu <- 0;
# initialization
rer <- rep(0,nbno);
# looking for the ancestors and giving each a
# different group number
for (jbd in 1:nbno) {
    if (length(bn@par[[jbd]]) == 0) {
        nu <- nu + 1;
        rer[jbd] <- nu;
    }
}
cat("(1)<<",rer,">>\n");
# giving each other node a group number
verif <- 0;
while (sum(rer == 0) > 0) {
    verif <- verif + 1; 
    if ((verif - 3) > nbno) {erreur("subn","[1]: parentship comprises a CYCLE?");}
    for (jbd in 1:nbno) {
        if (rer[jbd] == 0) {
        for (jd in 1:length(bn@par[[jbd]])) {
            iic <- bn@par[[jbd]][jd];
            if ((rer[iic] >  0) & (rer[jbd] == 0)) { rer[jbd] <- rer[iic];}
            if ((rer[iic] == 0) & (rer[jbd] >  0)) { rer[iic] <- rer[jbd];}
            if ((rer[iic] > 0) & (rer[jbd]  >  0)) {
                rer[iic] <- min(rer[jbd],rer[iic]);
                rer[jbd] <- rer[iic];
            }
        }}
    }
}
# adjusting the group number
quoi <- sort(unique(rer));
nbg <- length(quoi);
for (jbd in 1:nbg) { rer[rer == quoi[jbd]] <- jbd;}
cat("(2)<<",rer,">>\n");
# checking the consistency of the child+parents of every nodes
  totosum <- function() {
      tts <- 0;
      for (its in 1:nbno) { if (length(bn@par[[its]]) > 0) {
          qui <- c(its,bn@par[[its]]);cat("(2b) ",its,"-",rer[qui],"\n");
          if (length(unique(rer[qui])) > 1) { tts <- tts + 1;}
      }}
  tts;
  }
#
while (totosum() > 0) {
verif <- 0;
cat("(3)##",rer,"##\n");
    verif <- verif + 1;
    if ((verif - 5) > nbno) {rapport("subn: [2]: INTERNAL ERROR!!!");}
    for (jbd in 1:nbno) {
        if (length(bn@par[[jbd]]) > 0) { for (jd in 1:length(bn@par[[jbd]])) {
            iic <- bn@par[[jbd]][jd];
            if (rer[jbd] == 0) { rer[jbd] <- rer[iic];}
            else {
                jjc <- rer[jbd];
                iret <- min(iic,jjc);
                ifon <- max(iic,jjc);
                rer[rer == ifon] <- iret;
            }
        }}
    }
}
cat("(4)<<",rer,">>\n");
# looking for the order and checking for cycles
res <- numeric(0);
nb_subset <- max(rer);
cat("(5)<<",nb_subset,">>\n");
verif <- 0;
yd <- 1;
for (hd in 1:nbno) {
    # can we go a subset further?
    if(sum(rer == yd) == (sum(rer[res] == yd))) { yd <- yd+1;}
    # dealing with subset yd which is not empty
    #cat("[[",hd,"--",yd,"--",res,"]]\n");
    verif <- verif + 1;
    if ((verif - 3) > nbno) {erreur(NULL,"[3]: parentship comprises a CYCLE?");}
    for (jbd in 1:nbno) { if (rer[jbd] == yd) {
        # can node number jbd be introduced?
        # i.e. all its parents are already introduced
        a <- sum(bn@par[[jbd]] %in% res);
        b <- length(bn@par[[jbd]]);
        #cat("/",bn@par[[jbd]],"/\n");
        #cat(" {",jbd,"(",bn@nom[jbd],")","} {",yd,"} {",a,"} & {",b,"}\n");
        if (a == b) { if (sum(res == jbd) == 0) { res <- c(res,jbd);}
        }
    }}
}
if(yd != nb_subset) {rapport("subn: [4]: Internal Error!");}
cat("(6)<<",rer,">>\n");
cat("(7)<<",bn@nom[res],">>\n");
list(subsets=rer,order=res);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
complete8alk <- function(bn,alk)
#TITLE completes an alk with respect to a given bn
#DESCRIPTION (bn)
# This function explores the alk slot to check them and
# complete them in such a way that it could be incorporated to 
# the referent bn. Most often, the /bn/ is under construction
# and the necessary parent nodes must have been already 
# integrated.
#DETAILS
# The principle is not that simple. Some hints are given
# in the rebastaba manual. A key point of complet8alk
# is to finish inheritance according the properties
# of parent nodes.
#KEYWORDS
#PKEYWORDS bn nd
#INPUTS
#{bn}       <<The referent bn object.>>
#{alk}      <<The alk object to use.>>
#[INPUTS]
#VALUE
# an object of class alk completed (but parentship is provided in code4bn)
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(complete8alk(rebastaba.bn2,rebastaba.alk2));
#REFERENCE
#SEE ALSO and4bn code4bn
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_13
#REVISED 10_08_13
#--------------------------------------------
{
# checking the non yet completed alk
if (rebastaba.mck) {check4valid(valid8alk(alk));}
if (alk@lcomp) {
    erreur(alk,"For complete8alk, the input must be a non completed alk");
}
#
alk@lparent <- parents8alk(alk,bn@nom);
lpapa <- length(alk@lparent);
#
# looking for the number of repetitions
alk@lrep <- nbrep4alk(alk,bn@nom);
#
#
# special cases
if (alk@ltype != "empidata") {
    # no value for this slot
    alk@lwin <- rebastaba.win0;
} else {
    # must be provided with values
    # getting some information from the /win/
    alk@lnat <- as.vector(alk@lwin@nat);
    alk@lvar <- names(alk@lwin@nat);
    #
    if (isvide(alk@lwin@swg)) {
        # standard values
        alk@lwin@swg <- structure(rep(1,lpapa),.Names=as.character(alk@lparent));
    } else {
        # standard values
        if (isvide(alk@lwin@skk)) { alk@lwin@skk <- 2;}
        if (isvide(alk@lwin@sdi)) { alk@lwin@sdi <- c(0,1);}
        if (isvide(alk@lwin@snb)) { alk@lwin@snb <- c(1,Inf);}
    }
    if (isvide(alk@lwin@rty)) { alk@lwin@rty <- c("*","systematic");}
}
#
# looking for the variable names
alk@lvar <- var4alk(alk,bn@nom,alk@lrep);
#
# completing some slots at the variable level
nbv <- length(alk@lvar);
#
if (length(alk@lnat) != nbv) {
    if (length(alk@lnat)==1) {
        alk@lnat <- rep(alk@lnat,nbv);
    } else {
        erreur(list(alk@lvar,alk@lnat),
               "The numbers of variables and proposed natures differ"
              );
    }
}
#
if (length(alk@lpod) != nbv) {
    if (length(alk@lpod)==1) {
        alk@lpod <- rep(alk@lpod,nbv);
    } else {
        erreur(list(alk@lvar,alk@lpod),
               "The numbers of variables and proposed possible domains differ"
              );
    }
}
#
if (isvide(alk@lred)) {
    alk@lred <- alk@lpod;
} else {
    if (length(alk@lred) != nbv) {
        if (length(alk@lred)==1) {
            alk@lred <- rep(alk@lred,nbv);
        } else {
            erreur(list(alk@lvar,alk@lred),
	           "The numbers of variables and proposed representation domains differ"
                  );
        }
    }
}
#
if (isvide(alk@lcod)) {
    alk@lcod <- alk@lred;
} else {
    if (length(alk@lcod) != nbv) {
        if (length(alk@lcod)==1) {
            alk@lcod <- rep(alk@lcod,nbv);
        } else {
	    erreur(list(alk@lvar,alk@lcod),
	           "The numbers of variables and proposed common domains differ"
	           );
        }
    }
}
# 
alk@lcomp <- TRUE;
# checking the result
if (rebastaba.mck) {
    check4valid(check8alk(alk));
}
# returning
alk;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
code4bn <- function(bn,nk,bugs=FALSE)
#TITLE builds the code for the nk-th node of a /bn/.
#DESCRIPTION (bn)
# When bugs=FALSE this function completes the @nfug component 
# for the nk-th given node.
# When bugs=TRUE, the result is the bugs code which has a similar
# structure.
#DETAILS
#KEYWORDS
#PKEYWORDS bn nd
#INPUTS
#{bn}<<The bn>>
#{nk} <<node ('nk') for which the code must be calculated.>>
#[INPUTS]
#{bugs} << when TRUE computes bugs code.>>
#VALUE
# The resulting bn completed or when bugs the bugs code
# as a character vector.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# uu <- code4bn(rebastaba.bn2,1);
# print(uu,quoi="g",qui=1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# This function was formerly called 'get8code'
#FUTURE
# For the moment, the bugs translation is left as it was before
# the crucial introduction of objects /nom/. Possibly, another
# function could be derived to keep the consistency of the
# denomination 'code4bn' returns a /bn/ and 'get8code' returns
# a piece of code.
#AUTHOR J.-B. Denis
#CREATED 07_07_11
#REVISED 09_06_11
#--------------------------------------------
{
# no checking ...
iid <- nk;
jjd <- nv2ion(iid,bn@nom,"N",check=FALSE)@vk;
name <- nanv(bn@nom)[nk];
ltype <- bn@ntype[iid];
#
if (bugs) {
    if (!rebastaba.l_a[ltype,"bugs"]) {
        rapport("Translation into bugs must be replaced");
        erreur(bn@alks[[iid]],"This node is not bugs compatible.");
    }
    cbu <- paste("#",form3repete("-",50),name);
    # is the type bugs compatible?
    if (!(rebastaba.l_a[ltype,"bugs?"])) {
        cbu <- c(cbu,paste("The link of type \"",ltype,
                           "\" cannot be translated into bugs!",sep=""));
    }
}
#
fait <- FALSE; # not very smart but efficient!
#
########## case of programmed link
if (ltype == "program") {
    # nothing to do!
     bn@nfug[[iid]] <- bn@nfun[[iid]];
    fait <- TRUE;
}
#
########## case of easy programming
if (ltype == "easyp") {
    # building the link up
    gege <- easyp2code(bn@npara[[nk]]$pro,bn,iid);
    trtr <- easyp2code(bn@ntransfo[nk],bn,iid,TRUE);
    bn@nfug[[iid]] <- expr3func(gege,trtr);
    fait <- TRUE;
}
#
########## case of data based distribution
if (rebastaba.tlk[[ltype]]$fami == "data_based") {
    # inserting a pseudo-function
    bn@nfug[[iid]] <-  expr3func("{'Reported within simulate8bn';}");
    fait <- TRUE;
}
#
########## case of a score link
if (ltype == "score") {
    # checking the consistence of the proposed link
    if (bn@nrep[iid] > 1) { erreur(bn,"Score nodes must have no repetitions");}
    check4tyle(jjd,"integer",1,message="Score nodes must have only one variable");
    papa <- bn@vparent[[jjd]];
    pp <- nv2ion(papa,bn@nom,check=FALSE)@nk;
    if (length(papa) != 1) {
        form3affiche(bn@vparent);
        form3affiche(bn@nom);
        form3affiche(bn@ntype);
        erreur(papa,"Every  'score' node must have ONE and ONLY ONE parent!>");
    }
    if (rebastaba.tlk[[bn@ntype[pp]]]$fami != "categoric") {
        cat("The ltype of the parent is",rebastaba.tlk[[bn@ntype[pp]]]$fami,"\n");
        erreur(bn,"A 'score' node must have a 'categoric' parent!");
    }
    if (length(bn@npara[[pp]]$p) != length(bn@npara[[iid]]$scores)) {
        form3affiche(papa);
        form3affiche(name);
        erreur(list(papa,name),
               "The 'scores' parameter has got a wrong length with respect to its unique parent");
    }
    bn@nfug[[iid]] <- expr3func("{'Reported within simulate8bn';}");
    fait <- TRUE;
}
#
########## standard distributions
if (!fait) {
    lpara <- bn@npara[[iid]];
    ### general things
    if (!bugs) {
        didi <- nbnv(bn@nom,iid);
    } else { # bugs
        # changing the name in case of transformation
        if (isvide(bn@ntransfo[[iid]])) { zname <- name;
        } else { zname <- paste("z4",name,sep="");}
    }
    # level of indentation
    nive <- 1;
    ### normal distribution
    if (ltype == "normal") {
        if (!bugs) {
	    # getting the parameters under code mode
	    dm    <- easyp2code(lpara$mu,bn,iid);
	    ds    <- easyp2code(lpara$sigma,bn,iid);
	    # building the drawing code up
            fong <- paste(form3ind(0+nive,FALSE),"{ rnorm(",
                          form3ind(1+nive),"nrow(X)*",didi,",",
                          form3ind(1+nive),"mu <-\n",dm,",",
                          form3ind(1+nive),"sigma <-\n",ds,
                          form3ind(0+nive),");}",
                          sep="");
        } else { # bugs
	    # getting the parameters under code mode
	    dm    <- expr3cobu(lpara$mu);
	    ds    <- expr3cobu(lpara$sigma);
	    # building the bugs code up
            cbu <- c(cbu,expr3bugs(ltype,bn,iid,zname,dm,ds));
        }
        fait <- TRUE;
    }
    ### uniform distribution
    if (ltype == "uniform") {
        if (!bugs) {
	    # getting the parameters under code mode
	    da    <- easyp2code(lpara$a,bn,iid);
	    db    <- easyp2code(lpara$b,bn,iid);
	    # building the drawing code up
            fong <- paste(form3ind(0+nive,FALSE),"{ runif(",
                          form3ind(1+nive),"nrow(X)*",didi,",",
                          form3ind(1+nive),"a <-\n",da,",",
                          form3ind(1+nive),"b <-\n",db,
                          form3ind(0+nive),");}",
                          sep="");
        } else { # bugs
	    # getting the parameters under code mode
	    da    <- expr3cobu(lpara$a);
	    db    <- expr3cobu(lpara$b);
	    # building the bugs code up
            cbu <- c(cbu,expr3bugs(ltype,bn,iid,zname,dm,ds));
        }
        fait <- TRUE;
    }
    ### beta distribution
    if (ltype == "beta") {
        if (!bugs) {
	    # getting the parameters under code mode
	    da    <- easyp2code(lpara$a,bn,iid);
	    db    <- easyp2code(lpara$b,bn,iid);
	    # building the drawing code up
            fong <- paste(form3ind(0+nive,FALSE),"{ rbeta(",
                          form3ind(1+nive),"nrow(X)*",didi,",",
                          form3ind(1+nive),"a <-\n",da,",",
                          form3ind(1+nive),"b <-\n",db,
                          form3ind(0+nive),");}",
                          sep="");
        } else { # bugs
	    # getting the parameters under code mode
	    da    <- expr3cobu(lpara$a);
	    db    <- expr3cobu(lpara$b);
	    # building the bugs code up
            cbu <- c(cbu,expr3bugs(ltype,bn,iid,zname,dm,ds));
        }
        fait <- TRUE;
    }
    ### Dirac distribution
    if (ltype == "Dirac") {
        if (!bugs) {
            # getting the parameters under code mode
            dk    <- easyp2code(lpara$k,bn,iid);
            # building the drawing code up
            fong <- paste(form3ind(0+nive,FALSE),dk,
                          sep="");
        } else { # bugs
            # getting the parameters under code mode
            dk    <- expr3cobu(lpara$k);
            # building the bugs code up
            cbu <- c(cbu,paste(name," <- ",dk,";",sep=""));
        }
        fait <- TRUE;
    }
    ###  Bernoulli distribution
    if (ltype == "Bernoulli") {
        if (!bugs) {
            # getting the parameters under code mode
            dp    <- easyp2code(lpara$p,bn,iid);
            # building the drawing code up
            fong <- paste(form3ind(0+nive,FALSE),"{ rbinom(",
                          form3ind(1+nive),"nrow(X)*",didi,",",
                          form3ind(1+nive),"size =1,",
                          form3ind(1+nive),"prob <-\n",dp,
                          form3ind(0+nive),");}",
                          sep="");
        } else { # bugs
            # getting the parameters under code mode
            dp    <- expr3cobu(lpara$p);
            # building the drawing code up
            cbu <- c(cbu,paste(name," ~ dbern(",
                               dp,
                               ");",sep=""));
        }
        fait <- TRUE;
    }
    ###  binomial distribution
    if (ltype == "binomial") {
        if (!bugs) {
            # getting the parameters under code mode
            dp    <- easyp2code(lpara$p,bn,iid);
            dn    <- easyp2code(lpara$n,bn,iid);
            # building the drawing code up
            fong <- paste(form3ind(0+nive,FALSE),"{ rbinom(",
                          form3ind(1+nive),"nrow(X)*",didi,",",
                          form3ind(1+nive),"size <-\n",dn,",",
                          form3ind(1+nive),"prob <-\n",dp,
                          form3ind(0+nive),");}",
                          sep="");
        } else { # bugs
            # getting the parameters under code mode
            dp    <- expr3cobu(lpara$p);
            dn    <- expr3cobu(lpara$n);
            # building the drawing code up
            lignes <- paste(name," ~ dbin(",
                            dp,", ",
                            dn,
                            ");",sep="");
        }
        fait <- TRUE;
    }
    ###  multinomial distribution
    if (ltype == "multinomial") {
        # getting the parameters under code mode
        dp    <- easyp2code(lpara$p,bn,iid);
        dn    <- easyp2code(lpara$n,bn,iid);
        # building the drawing code up
        fong <- paste(form3ind(0+nive,FALSE),"{ draw8multinom(",
                      form3ind(1+nive),"nrow(X),",
                      form3ind(1+nive),"size <- ",dn,",",
                      form3ind(1+nive),"prob <- ",dp,
                      form3ind(0+nive),");}",
                      sep="");
        fait <- TRUE;
    }
    ###  Dirichlet distribution
    if (ltype == "Dirichlet") {
        # getting the parameters under code mode
        da    <- easyp2code(lpara$a,bn,iid);
        dA    <- easyp2code(lpara$A,bn,iid);
        # building the drawing code up
        fong <- paste(form3ind(0+nive,FALSE),"{",
                      form3ind(0+nive),"  draw8Dirichlet(",
                      form3ind(1+nive),"n=nrow(X),",
                      form3ind(1+nive),"A <- ",dA,",",
                      form3ind(1+nive),"a <- ",da,
                      form3ind(0+nive),"    );",
                      form3ind(0+nive),"}",
                      sep="");
        fait <- TRUE;
    }
    ###  numcat distribution
    if (ltype == "numcat") {
        # getting the parameters under code mode
        fong <- "{'Reported into simulate8bn';}";
        fait <- TRUE;
    }
    ###  parcat distribution
    if (ltype == "parcat") {
        # getting the parameters under code mode
        dp    <- easyp2code(lpara$p,bn,iid);
        check4tyle(jjd,"integer",1,message="For 'parcat' node only one variable is expected");
        # building the drawing code up
        fong <- paste(form3ind(0+nive,FALSE),"{ draw8multinom(",
                      form3ind(1+nive),"nrow(X),",
                      form3ind(1+nive),"size <- rep(1,nrow(X)),",
                      form3ind(1+nive),"prob <- ",dp,",",
                      form3ind(1+nive),"levels <- ",
                      form3liste(bn@vpod[[jjd]],OPA="c('",CPA="')",opa="",cpa="",sep="','"),
                      form3ind(0+nive),");}",
                      sep="");
        fait <- TRUE;
    }
    ### the distribution was not found
    if (!fait) {
        erreur(NULL,"So sorry: the link type (",ltype,") is not yet",
                     "introduced into 'code4bn'!");
    }
    if (!bugs) {
        ### introducing the generation function
        # finding the transformation if any
        if (rebastaba.l_a[ltype,"ltransfo"] %in% c("yes","YES")) {
            font <- easyp2code(bn@ntransfo[iid],bn,iid,TRUE);
        } else {
            font <- "      {Y;}\n      ### (there is no transformation)";
        }
        # assembling
        bn@nfug[[iid]] <- expr3func(fong,font);
    } else { # bugs
        if (zname != name) {
            rapport("To be done!!!");
            #ax <- easyp2code(l@ltransfo,bn,iid,TRUE,TRUE);
            #???ax <- gsub(rbsb.coding,name,ax);
            #cbu <- c(cbu,ax);
        }
    }
}
# returning
if (bugs) { return(cbu);
} else { return(bn);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp3expanded <- function(ldim,eas,bn)
#TITLE returns an expanded easyp from an easyp
#DESCRIPTION (bn)
# Changing the nodes into the node variables.
# This is done when necessary, e.g. eas can
# be returned as it is. Detected inconsistencies
# provoke fatal errors.
#DETAILS
#KEYWORDS
#PKEYWORDS easyp
#INPUTS
#{ldim} <<When greater than one, must be 
#         the length of the resulting easyp.>>
#{eas} <<The scalar easyp expression.>>
#{bn} <<The associated /bn/.>>
#[INPUTS]
#VALUE
# A character containing the expanded easyp
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(easyp3expanded(1,"{{A}}+{{B[a]}}",rebastaba.bn2));
# print(easyp3expanded(2,"{{A}}+{{B[a]}}",rebastaba.bn2));
# print(easyp3expanded(3,"{{B}}",rebastaba.bn2));
# print(easyp3expanded(3,"{{A}}+{{B}}",rebastaba.bn2));
# \dontrun{easyp3expanded(5,"{{B}}",rebastaba.bn2)};
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_01
#REVISED 09_11_23
#--------------------------------------------
{
# checking
if (is.numeric(eas)) { eas <- as.character(eas);}
if ((!is.character(eas)) |
    (length(eas)!=1)) {
    erreur(eas,"Must be 'character(1)'!");
}
# looking for the dimension of the provided easyp
didi <- easyp2dim(eas,bn@nom);
if ((ldim > 1) &
    (didi > 1) &
    (didi != ldim)) {
    erreur(list(ldim,eas),
           "This easyp is not consistent for the proposed 'ldim' dimension"
          );
}
didi <- max(ldim,didi);
if (didi == 1) {
    # no modification: eas remains of length one
    res <- eas;
} else {
    # eas must be expanded
    res <- rep("",didi);
    dd <- easyp3cut(eas,rbsb.cpt);
    # bl2 contains the case where there is a node or a node[variable]
    bl2 <- which(dd$typ==1);
    # each component of the result will be filled
    for (ii in bc(didi)) {
        ddd <- dd;
        # exploration of each node or node[variable]
        for (jj in bl2) {
            nv <- dd$blo[jj];
            nn <- nv2nod(nv);
            ddd$blo[jj] <- nv
            if (nv == nn) {
                # case where it is a node not a node[variable]
                udidi <- easyp2dim(paste(rbsb.cpt[1,1],nn,rbsb.cpt[1,2],sep=""),bn@nom);
                if (udidi == didi) {
                   vvv <- nv2ion(nv,bn@nom,"N")@nvn;
                   ddd$blo[jj] <- vvv[ii];
                } else { if(udidi != 1) {
                    rapport("This case was supposed to be protected by the first call to easyp2dim");
                }}
            }
            res[ii] <- easyp3stickback(ddd,rbsb.cpt);
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
zero2bn <- function(description)
#TITLE starting an empty bn
#DESCRIPTION (bn)
# Creation of a new bn object with zero nodes, that is just
#  giving the structure to be able to add nodes further.
#DETAILS
# The argument can be no more than a single character, in that
# case a 'des' object will be created with it as name.
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{description} <<Description field ('des' object) for the new bn.
#                Alternatively can be the name (single character).>>
#[INPUTS]
#VALUE
# an object of class "bn" with zero node
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(zero2bn(char2des("BN without any node")));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_12
#REVISED 09_04_30
#--------------------------------------------
{
bn <- new("bn",
          description=char2des(description),
          nom=rbsb.nom0,
          ntype=character(0),
          ndes=rbsb.lis0,
          npara=rbsb.lis0,
          nrep=numeric(0),
          ntransfo=character(0),
          ndaf=rbsb.lis0,
          nwin=rbsb.lis0,
          nfun=rbsb.lis0,
          nfug=rbsb.lis0,
          vnat=character(0),
          vpod=rbsb.lis0,
          vred=rbsb.lis0,
          vcod=rbsb.lis0,
          vparent=rbsb.lis0
         );
if (rebastaba.mck) {check4valid(valid8bn(bn));}
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2gn <- function(bn,nv="n")
#TITLE creates a gn object from a bn object
#DESCRIPTION (bn)
# from a bn object extracts the necessary information
# to create a gn object (the directed graph of the BN).
# According to 'nv', the graph can be built at the 
# node or variable level.
#DETAILS
#KEYWORDS
#PKEYWORDS bn gn
#INPUTS
#{bn}<<the bn object>>
#[INPUTS]
#{nv} << 'n' for node level, 'v' for variable level.>>
#VALUE
# the resulting gn object
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(bn2gn(rebastaba.bn1));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_14
#REVISED 09_05_26
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(nv,"character",1,message="'nv' must be 'character'");
    if (!(nv %in% c("n","v"))) {
        erreur(nv,"'nv' must be 'n' or 'v'");
    }
}
dd <- bn@description;
dd@comm[[length(dd@comm)+1]] <- paste("extracted by 'bn2gn' on",today());
#
papa <- bn2pam(bn,nv);
#
nounom <- bn@nom;
if (nv=="n") {
    # removing every variable from all nodes
    for (ii in bf(nounom)) {
        nounom@x[[ii]] <- "";
    }
}
#
res <- new("gn",
           description=dd,
           nom=nounom,
           item=nv,
           arc=pam2arc(papa),
           pos=new("pos",posi=matrix(0,nbnv(bn,nv),4),
                         view=c(0,0),zoom=c(0,0,0,1)),
           pgr=g4n.pgr0
          );
res <- position4gn(res);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
and4bn <- function(bn,alk)
#TITLE adds a new node to a bn object
#DESCRIPTION (bn)
# A new node is added to a bn, it is potentially defined
# by the alk argument. \cr
# The proposed name for the new node must be different of
# those already registrated. Also, the parent nodes must
# already exist in the bn. This prevents the occurrence of
# cycles.\cr
# The function returns the BN with the added node.\cr
# For details about the way to define the node, see the 
# 'new8alk' function.
#DETAILS
# The complete list of standard already programmed distributions is given
# with help8ltype function.\cr
# From the definition of the link given by ("l*" arguments),
# a function which generates values for the link is constructed
# and stored into the slot "lien" of the BN.
#KEYWORDS
#PKEYWORDS bn nd
#INPUTS
#{bn} <<The bn object to which a node has to be added.>>
#{alk} <<An alk object not completed as given by the user.>>
#[INPUTS]
#VALUE
# an object of class "bn" with an additional node
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(and4bn(rebastaba.bn1,new8alk(char2des("new"),ltype="normal",
#              lpara=list(mu="{{A}}+{{B}}",sigma=2),lpod=list(c(-1,6)))),
#       quoi="n");
#REFERENCE
#SEE ALSO
# see code4bn
#CALLING {code4bn}
#COMMENT
#FUTURE
# the vparent genealogy is not optimal for 
# repeated nodes.
#AUTHOR J.-B. Denis
#CREATED 07_06_13
#REVISED 09_11_23
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
#
# completing the proposed alk
cat(alk@lparent);
alk <- complete8alk(bn,alk);
cat(alk@lparent);
#
# number of the new node to be added
nono <- nbnv(bn@nom,-1) +1; 
#
# adding the name of the new node but not its possible variables
nom <- alk@ldes@name;
if (nv3nom(nom,bn@nom)>0) {
    erreur(list(bn@nom,nom),"The new node has an ALREADY USED name !");
} else {
    bn@nom <- and4nom(bn@nom,nom,"");
}
#
# completing some slots at the node level
bn@ntype[nono] <- alk@ltype;
bn@ndes[[nono]] <- alk@ldes;
bn@npara[[nono]] <- alk@lpara;
bn@ntransfo[[nono]] <- alk@ltransfo;
bn@ndaf[[nono]] <- alk@ldaf;
bn@nfun[[nono]] <- alk@lfunct;
#
# looking for the parents of the node
papa <- alk@lparent;
#
# looking for the number of repetitions
bn@nrep[nono] <- alk@lrep;
#
# establishing the variable places
nbv <- bn@nrep[nono];
if (nbv==0) {
    nbv <- length(alk@lnat);
}
ouv <- 1:nbv + length(bn@vparent);
#
# looking for the variable parents
if (bn@nrep[nono]<2) {
    # all node parents are parents of every variable
    for (ii in ouv) {
        bn@vparent[[ii]] <- papa;
    }
} else {
    # in case of multiple repetitions things
    # can be more intricated but for the moment
    # a global dependence is supposed. This is
    # necessary for multivariate nodes due to
    # the correlations...
    # ??? to be improved ???
    for (ii in ouv) {
        bn@vparent[[ii]] <- papa;
    }
}
#
bn@nwin[[nono]] <- alk@lwin;
#
# looking for the variable names
bn@nom@x[[nono]] <- alk@lvar;
#
# completing some slots at the variable level
nbv <- nbnv(bn@nom,nono);
ouv <- 1:nbv + (nbnv(bn@nom,0)-nbv);
#
bn@vnat[ouv] <- alk@lnat;
#
bn@vpod[ouv] <- alk@lpod;
bn@vred[ouv] <- alk@lred;
bn@vcod[ouv] <- alk@lcod;
#
# finally the generating function
bn <- code4bn(bn,nono);
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2pta <- function(bn,ion,pos=2)
#TITLE computes the probability table
#           associated to a numcat/parcat node
#DESCRIPTION (bn)
# In case of a numcat node, provides its 
# conditional probability table as defined in
# the bn. The number of dimensions of the array
# is one plus the number of the node parents.
# The order of the node parents is not modified
# and the position number of the child node 
# is given by pos.
#DETAILS
# Remenber that the order in @npara$p corresponds
# to pos with the greatest value.
#KEYWORDS
#PKEYWORDS bn numcat
#INPUTS
#{bn}<<the bn.>>
#{ion} << identification of the node. This node must
#         be a numcat node.>>
#[INPUTS]
#{pos} << index position for the child node. When
#        there are no parents, this argument is 
#        forced to one.>>
#VALUE
# the resulting pta object
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# uu <- and4bn(rebastaba.bn1,new8alk(char2des("NC1"),ltype="numcat",
#              lpara=list(p=1:4),lpod=list(LETTERS[1:4])))
# uu <- and4bn(uu,new8alk(char2des("NC2"),ltype="numcat",lparent="NC1",
#              lpara=list(p=matrix(1:12,4)),lpod=list(letters[1:3])))
# print(bn2pta(uu,"NC1"));
# print(bn2pta(uu,"NC2"));
#REFERENCE
#SEE ALSO bn2dn
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_06_30
#REVISED 09_11_23
#--------------------------------------------
{
# checking, investigating
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# finding the node number
ii <- nv2ion(ion,bn@nom,"n",check=FALSE)@nk;
# checking that the node is numcat
if ( !(bn@ntype[ii] %in% c("numcat","parcat"))) {
    erreur(bn@ntype[ii],paste("Non relevant type of node to ask a cond. prob. table!",
                              "[a 'numcat'/'parcat' parent was expected!]"));
}
# finding the variable number
jj <- nv2ion(matrix(c(ii,0),2),bn@nom,check=FALSE)@vk;
# finding the structure of the proba table
nbp <- 0; # number of parents
nup <- dip <- vector("numeric",0); # their variable numbers and dimensions
nop <- vector("character",0); # their names
for (jd in bf(bn@vparent[[jj]])) {
    nbp <- nbp+1;
    pap <- bn@vparent[[jj]][jd];
    nup[nbp] <- nv2ion(pap,bn@nom,"N",check=FALSE)@vk;
    xxx <- nv2ion(pap,bn@nom,"N",check=FALSE)@nk;
    dip[nbp] <- length(bn@vpod[[nup[nbp]]]);
    nop[nbp] <- nv2ion(nup[nbp],bn@nom,"v",check=FALSE)@nn;
    if (!(bn@ntype[xxx] %in% c("numcat","parcat"))) {
        erreur(bn@ntype[xxx],paste("Non relevant parent for asking a cond. prob. table!",
                          "{a 'numcat'/'parcat' parent was expected!}"));
    }
}
di0 <- length(bn@vpod[[jj]]); # dimension of the child
# constraining pos
pos <- max(0,min(pos,1+nbp));
# preparing the table
res <- array(NA,c(dip,di0));
if (bn@ntype[ii] %in% c("numcat")) {
    dimnames(res)[[nbp+1]] <- bn@vpod[[jj]];
} else {
    rapport("???never used branch to be suppressed some day (09_06_02)");
    if (is.null(bn@alks[[ii]]@lparent)) { kk <- length(bn@alks[[ii]]@lpara$p);
    } else { kk <- ncol(bn@alks[[ii]]@lpara$p);}
    dimnames(res)[[nbp+1]] <- 1:kk;
}
for (jd in bc(nbp)) {
    jbd <- nbp - jd + 1;
    dimnames(res)[[jbd]] <- bn@vpod[[nup[jbd]]];
}
names(dimnames(res)) <- c(nop,nanv(bn@nom,-1)[ii]);
# loading the table
res[1:prod(dim(res))] <- bn@npara[[ii]]$p;
# permuting the table according to pos
if (!(pos == (1+nbp))) {
    oper <- c(bc(pos-1),nbp+1,bc(1+nbp-pos)+pos-1);
    res <- aperm(res,oper);
}
# returning
new("pta",
    name="compute4pta_made",
    kkk=2,
    vam=nanv(bn@nom,-1)[ii],
    vac=nop,
    vad=character(0),
    vav=character(0),
    pro=res);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
grappa4check <- function(bn)
#TITLE checks if the provided bn is Grappa-compatible
#DESCRIPTION (bn)
#   More precisely, all nodes must be of numcat type.
#DETAILS
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{bn} <<The bn object.>>
#[INPUTS]
#VALUE
# TRUE or FALSE according to
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# grappa4check(rebastaba.bn2);
# uu <- and4bn(zero2bn(char2des("oui")),new8alk(char2des("NC1"),ltype="numcat",
#              lpara=list(p=1:4),lpod=list(LETTERS[1:4])));
# grappa4check(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT 
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_07_31
#REVISED 09_06_10
#--------------------------------------------
{
# some checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
res <- TRUE;
for (i in bc(nbnv(bn@nom))) { if (bn@ntype[i] != "numcat") {
     res <- FALSE;
}}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
mnd4bn <- function(bn,ion,alk)
#TITLE modifies an existing node into a bn object
#DESCRIPTION (bn)
# An already existing node is replaced into a bn. Some of its 
# characteristics cannot be modified: name, lvar and 
# lnat. This is a condition for not to modify the other nodes.\cr
# The user must take great care that the effect of default is NOT
# AT ALL as in the similar function and4bn ! When adding a new node,
# the default is nothing, when modifying an existing node the default
# is NO MODIFICATION except for lfunct, ldaf... Some checks are
# performed.\cr
# For the moment repeated node cannot be replaced.
#DETAILS
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn} <<The bn object to which a node has to be modified.>>
#{ion} <<The node to replace, by internal number or name.>>
#{alk} <<Definition of the replacing node by the user.>>
#[INPUTS]
#DETAILS
# More insights are given in the description of and4bn.
#KEYWORDS
#PKEYWORDS bn node
#VALUE
# The modified bn
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(mnd4bn(rebastaba.bn1,2,new8alk(char2des("B"),ltype="uniform",
#                                 lpara=list(a="{{A}}",b="2*{{A}}"),
#                                 lpod=list(c(-10,10))
#                                )));
#REFERENCE
#SEE ALSO
#CALLING {complete8alk} {code4bn}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_21
#REVISED 09_05_27
#--------------------------------------------
{
# standard checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(ion,c("numeric","character"),1,message="'mnd4bn' deals with one node at once");
    check4valid(valid8alk(alk));
}
#
# finding the node to replace
nono <- nv2ion(ion,bn@nom,"n",FALSE);
ind <- nono@nk;
nnd <- nono@nn;
iva <- nv2ion(matrix(c(ind,0),2),bn@nom,"n",FALSE)@vk;
### checking the consistence of the replacement
# non repeated?
if ((bn@nrep[ind]>1) |
    (alk@lrep>1)) {
    erreur(list(bn@nrep[ind],alk@lrep),
           "node modification is restricted to non repeated nodes");
}
# the name(s)
if (nnd != alk@ldes@name) {
    erreur(list(nnd,alk@ldes@name),
           "The node to replace has got a different name");
}
if (length(alk@lvar)!=length(nanv(bn@nom,ind))) {
    erreur(list(nnd,alk@lvar,nanv(bn@nom,ind)),
           "The node to replace has got different variable NUMBER");
}
if (!all(alk@lvar==nv2var(nanv(bn@nom,ind)))) {
    erreur(list(nnd,alk@lvar,nanv(bn@nom,ind)),
           "The node to replace has got different variable NAMES");
}
# the parents
# There is no more control on the parent ???
#papa <- nparent8alk(alk,bn@nom);
#
# the variable natures
for (jj in bf(iva)) {
    if (bn@vnat[iva[jj]]!=alk@lnat[jj]) {
        erreur(list(nnd,jj,bn@vnat[iva[jj]],alk@lnat[jj]),
               "The node to replace has got different nature(s)");
    }
}
# modifying the alk object from the parameters
alk@lcomp <- FALSE;
alk <- complete8alk(bn,alk);
# modifying the other slots of the bn
bn@ntype[ind] <- alk@ltype;
bn@ndes[[ind]] <- alk@ldes;
bn@npara[[ind]] <- alk@lpara;
bn@ntransfo[ind] <- alk@ltransfo;
bn@ndaf[[ind]] <- alk@ldaf;
bn@nwin[[ind]] <- alk@lwin;
bn@nfun[[ind]] <- alk@lfunct
for (jj in bf(iva)) {
    bn@vparent[[iva[jj]]] <- alk@lparent;
    bn@vpod[[iva[jj]]] <- alk@lpod[[jj]];
    bn@vred[[iva[jj]]] <- alk@lred[[jj]];
    bn@vcod[[iva[jj]]] <- alk@lcod[[jj]];
}
# introducing the simulating function from the alk and other things
bn <- code4bn(bn,ind);
# precaution checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rnd4bn <- function(bn,ion)
#TITLE removes a series of nodes into a bn object
#DESCRIPTION (bn)
# Some of the nodes of a bn are removed. A condition is that
# no orphans must be generated.
#DETAILS
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn} <<The bn object to which some nodes will be removed.>>
#{ion} <<The nodes to remove, designated by 
#        their internal numbers or names.>>
#[INPUTS]
#DETAILS
#KEYWORDS
#PKEYWORDS bn node
#VALUE
# The modified bn
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(rnd4bn(rebastaba.bn1,3));
# print(rnd4bn(rebastaba.bn1,2:3));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_22
#REVISED 09_06_02
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# finding the node to remove
# names are used because when removing the nodes
# the iin are modified
nnd <- nv2ion(ion,bn@nom,"n",check=FALSE)@nn;
# the removing loop
while (length(nnd)>0) {
    # looking a node without children to remove
    ntr <- rbsb.cha0;
    for (nr in bf(nnd)) {
        if (isvide(children8bn(bn,"n")[[nnd[nr]]])) { ntr <- nnd[nr];}
    }
    #
    if (isvide(ntr)) {
        # orphans would be left!
        for (nr in bf(nnd)) {
            cat("The node",nnd[nr],
                "remains with the child(ren)",
                children8bn(bn,"n")[[nnd[nr]]],"\n");
        }
        erreur(nnd,"Set of nodes with children.");
    } else {
        ## removing the removed node in the list
        nnd <- nnd[nnd!=ntr];
        ## removing the node ntr
        ii <- nv2ion(ntr,bn@nom,"n",check=FALSE)@nk;
        jj <- nv2ion(ntr,bn@nom,"N",check=FALSE)@vk;
        # modifying the description
        message <- paste("Node",ntr,"was removed.");
        bn@description@comm <- c(bn@description@comm,message);
        # at the variable level
        bn@vnat    <- bn@vnat[-jj];
        bn@vpod    <- bn@vpod[-jj];
        bn@vred    <- bn@vred[-jj];
        bn@vcod    <- bn@vcod[-jj];
        bn@vparent <- bn@vparent[-jj];
        # at the node level
        bn@ntype    <- bn@ntype[-ii];
        bn@ndes   <- bn@ndes[-ii];
        bn@npara    <- bn@npara[-ii];
        bn@nrep     <- bn@nrep[-ii];
        bn@ntransfo <- bn@ntransfo[-ii];
        bn@ndaf     <- bn@ndaf[-ii];
        bn@nwin     <- bn@nwin[-ii];
        bn@nfun   <- bn@nfun[-ii];
        bn@nfug   <- bn@nfug[-ii];
        # and finally modification of slot 'nom'
        bn@nom@x <- bn@nom@x[-ii];
    }
}
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
part8bgn <- function(bgn,ion,asc=TRUE)
#TITLE classifies a set of nodes according to a subset of them
#DESCRIPTION (bn)
# From a bn/gn structure and a subset of nodes, the set of nodes
# is partitioned into four exclusive classes:\cr
# (i) the targetted subset ["nod"],\cr
# (ii) their direct parents (excluding all of them) ["par"/"chi"],\cr
# (iii) their other ascendance ["asc"/"des"],\cr
# (iv) the remaining nodes ["nas"/"nde"].\cr
# Of course, some of the classes can be empty. The partition
# can be established for the descendance.
#DETAILS No check is done about \code{bgn} validity
#KEYWORDS
#PKEYWORDS bn gn
#INPUTS
#{bgn}<<The original bn or gn.>>
#{ion}<<The definition of targetted subset of nodes (iin or 
#       name vector). It is checked that it is a subset.>>
#[INPUTS]
#{asc} << Must the partition be done for ascendance (TRUE)
#        or descendance (FALSE).>>
#VALUE
# a list of named numeric vectors with the names of the corresponding
# nodes. The list names are given in the description field between 
# squared brackets.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(part8bgn(rebastaba.bn2,"A"));
# print(part8bgn(rebastaba.bn2,"B"));
# print(part8bgn(rebastaba.bn1,"C"));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_22
#REVISED 09_06_04
#--------------------------------------------
{
#
iin <- nv2ion(ion,bgn@nom,"n",check=FALSE)@nk;
# going through /bn/
if (is(bgn,"gn")) { bgn <- gn2bn(bgn);}
# preparing the resulting named list
res <- vector("list",4);
if (asc) {
    names(res) <- c("nod","par","asc","nas");
} else {
    names(res) <- c("nod","chi","des","nde");
}
## finding the parentship at the node level
if (asc) { didi <- parents8bn(bgn,"n");
} else { didi <- children8bn(bgn,"n"); }
pame <- bn2pam(bgn,"n");
## filling the list
# the nodes
res$nod <- nv2ion(iin,bgn@nom,"n",check=FALSE)@nn;
# the direct nodes
if (asc) {
    res$par <- character(0);
    for (i in bf(res$nod)) {
	xx <- didi[[res$nod[i]]];
	res$par <- union(res$par,xx);
    }
    res$par <- setdiff(res$par,res$nod);
} else {
    res$chi <- character(0);
    for (i in bf(res$nod)) {
	xx <- didi[[res$nod[i]]];
	res$chi <- union(res$chi,xx);
    }
    res$chi <- setdiff(res$chi,res$nod);
}
# the non direct nodes
nunu <- numeric(0);
if (asc) {
    res$asc <- character(0);
    for (i in res$nod) {
        ii <- nv2ion(i,bgn@nom,check=FALSE)@nk;
        nunu <- union(nunu,geneal4pam(pame,ii,TRUE)@fle[,1:2]);
    }
    res$asc <- nv2ion(nunu,bgn@nom,"n",check=FALSE)@nn;
    res$asc <- setdiff(res$asc,c(res$nod,res$par));
} else {
    res$des <- character(0);
    for (i in res$nod) {
        ii <- nv2ion(i,bgn@nom,check=FALSE)@nk;
        nunu <- union(nunu,geneal4pam(pame,ii,FALSE)@fle[,1:2]);
    }
    res$des <- nv2ion(nunu,bgn@nom,"n",check=FALSE)@nn;
    res$des <- setdiff(res$des,c(res$nod,res$chi));
}
# the remaining nodes
res[[4]] <- setdiff(nanv(bgn@nom,-1),c(res[[1]],res[[2]],res[[3]]));
# going back to named numeric vectors
rres <- res;
for (nnn in 1:4) {
    rres[[nnn]] <- nv2ion(res[[nnn]],bgn@nom,"n",check=FALSE)@nk;
    names(rres[[nnn]]) <- res[[nnn]];
}
# returning
rres;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ind8bgn <- function(bgn,ion)
#TITLE returns the nodes between two nodes
#DESCRIPTION (bn)
# From a bn/gn structure, and two related nodes
# (first being an ancestor of second) of it,
# the set of all nodes having them as ascendant
# and descendant (intermediate nodes) is returned.
# Notice that the starting and ending nodes are 
# not returned.
#DETAILS No checks about \code{bgn}
#KEYWORDS
#PKEYWORDS bn gn
#INPUTS
#{bgn}<<The original bn or gn.>>
#{ion}<<A pair of nodes (not variables) of bgn.>>
#[INPUTS]
#VALUE
#  The set of concerned nodes without the ion
#    nodes as a named numeric. So a returned numeric(0)
#    means that ion[1] is a direct parent of ion[2].
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# ind8bgn(rebastaba.bn1,c(1,3));
# ind8bgn(rebastaba.bn1,c(2,3));
# \dontrun{ind8bgn(rebastaba.bn2,c(2,1))}
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_23
#REVISED 09_11_23
#--------------------------------------------
{
#
if (length(ion) != 2) {
    erreur(ion,"TWO (and only two) nodes were expected.");
}
pa <- nv2ion(ion[1],bgn@nom,"n",check=FALSE)@nk;
ch <- nv2ion(ion[2],bgn@nom,"n",check=FALSE)@nk;
# gettiong ascendant and descendant
ax  <- part8bgn(bgn,ch);
asc <- c(ax$par,ax$asc);
ax  <- part8bgn(bgn,pa,FALSE);
des <- c(ax$chi,ax$des);
# is pa a parent of ch?
if (pa %in% asc) {
    # YES
    res <- intersect(asc,des);
    names(res) <- nv2ion(res,bgn@nom,"n",check=FALSE)@nn;
} else {
    erreur(ion,"ion[1] is not ancestor of ion[2]!");
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
uniform4bn <- function(bn,ion,val=rbsb.num0)
#TITLE introduces a uniform distribution on 
#      one node
#DESCRIPTION (bn)
# A new bn is generated from the initial bn 
# where the ion-th node is modified with a
# uniform distribution without parents. Then
# the structure of the bn can be different.
#DETAILS
# If numeric, the limits for the uniform
# are taken from lpod (or lred if infinite).
# If categoric, the limits are numcat
# uniform onto lpod.\cr
# This modification of node is possible only
# for univariate nodes.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn}<<The original bn.>>
#{ion}<<The definition of the node (iin or name)>>
#[INPUTS]
#{val}<<(\code{numeric}) Defines the range of the uniform
#       distribution. For a numeric node it is a numeric(2);
#       for a categoric node, it is a character(p). Be aware
#       that the lpod are not modified from the initial bn.
#       The default uses the lpod (see the DETAILS section).
# In case of multivariate or repeated node, when the default
# is not used, val must be a matrix with as many rows as
# variables, the first column for the lower limit of the 
# uniform and the second column for the upper limit of the
# uniform.>>
#VALUE
# The resulting bn
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(uniform4bn(rebastaba.bn2,1));
# print(uniform4bn(rebastaba.bn1,"B"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE ??? multivariate nodes have to be considered
#AUTHOR J.-B. Denis
#CREATED 08_10_23
#REVISED 09_11_25
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(ion,c("integer","character"),1,message="One (and only one) ion was expected");
}
# modifying the proposed node
iin <- nv2ion(ion,bn@nom,"n",check=FALSE)@nk;
jjn <- nv2ion(ion,bn@nom,"N",check=FALSE)@vk;
nnn <- nv2ion(ion,bn@nom,"n",check=FALSE)@nn;
if (length(jjn)>1) {
    # it will be a repeated node
    erreur(nv2ion(iin,bn@nom,"N",check=FALSE)@vn,"Replacement with 'uniform4bn' is restricted to univariate nodes");
}
des <- bn@ndes[[iin]];
des@name <- nnn;
ajout <- paste("This node was modified on",today(),
               "to loose its possible parents and",
               "getting a uniform distribution onto",
               "its domain.");
if (isvide(des@comm)) { des@comm <- ajout;
} else { des@comm <- c(des@comm,ajout);}
#
popo <- bn@vpod[[jjn]]; rere <- bn@vred[[jjn]];
#
if (rbsb.snp[bn@vnat[jjn],"categoric"]) {
    # a categoric variable
    tyty <- "numcat";
    if (isvide(val)) {
        nbca <- length(popo);
        papa <- list(p=rep(1/nbca,nbca));
    } else {
        if (!is.character(val)) {
            erreur(val,"For a categoric, val must be character");
        }
        if (length(setdiff(val,popo))!=0) {
            erreur(list(val,popo),"val must be included in lpod");
        }
        nbca <- length(val);
        p <- rep(0,length(popo));
        p[match(val,popo)] <- 1/nbca;
        papa <- list(p=p);
    }
} else {
    tyty <- "uniform";
    if (isvide(val)) {
        val <- matrix(NA,1,2);
        if (is.infinite(popo[1])) { popo[1] <- rere[1];}
        if (is.infinite(popo[2])) { popo[2] <- rere[2];}
        val[1,]=c(popo[1],popo[2]);
    } else {
        check4tyle(val,"numeric",2,message=paste(val,"Two numeric values were expected for val"));
        val <- matrix(range(val),ncol=2);
        if ((val[1,1]<popo[1])|(popo[2]<val[1,2])) {
            erreur(val,"val must belong to lpod this is not the case");
        }
    }
    papa <- list(a=val[,1],b=val[,2]);
}
popo <- list(popo);
alk <- new8alk(ldes=des,ltype=tyty,
	       lpara=papa,lparent=character(0),
	       lpod=popo,lred=popo,lcod=popo);
bn <- mnd4bn(bn,iin,alk);
# returning
if (rebastaba.mck) {check4valid(valid8bn(bn));}
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
Dirac4bn <- function(bn,ion,val=rbsb.num0)
#TITLE introduce a Dirac distribution on 
#      one univariate node
#DESCRIPTION (bn)
# A new bn is generated from the initial bn 
# where the ion-th node is modified with a
# Dirac distribution without parents. Then
# the structure of the bn can be different.
# Parameter 'val' gives the value to provide.
#DETAILS
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn}<<The original bn.>>
#{ion}<<The definition of the node (iin or name vector)>>
#[INPUTS]
#{val} <<The value to give to the node. Must be a numeric 
# if the node is numeric and a character if it is
# categoric (belonging to the lpod).\cr
# Default will imply
# the mean value of the lcod for numeric and a median 
# category for categoric (using the order of the categories).>>
#VALUE
# The resulting bn
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(Dirac4bn(rebastaba.bn2,1));
# uu <- and4bn(rebastaba.bn1,new8alk(char2des("NC1"),ltype="numcat",
#              lpara=list(p=1:4),lpod=list(LETTERS[1:4])))
# print(uu,quoi="l",qui=4);
# print(Dirac4bn(uu,4),quoi="l",qui=4);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE extend it to multivariate/repeated nodes
#AUTHOR J.-B. Denis
#CREATED 08_10_23
#REVISED 09_11_24
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(ion,c("character","integer"),1,message="One (and only one) ion was expected");
}
iin <- nv2ion(ion,bn@nom,"n",check=FALSE)@nk;
jjn <- nv2ion(ion,bn@nom,"N",check=FALSE)@vk;
if (length(jjn)>1) {
    # it will be a repeated node
    erreur(nv2ion(iin,bn@nom,"N",check=FALSE)@vn,"Replacement with 'Dirac4bn' is restricted to univariate nodes");
}
# modifying the proposed node
des <- bn@ndes[[iin]];
des@name <- nanv(bn@nom,iin);
ajout <- paste("This node was modified on",today(),
               "to loose its possible parents and",
               "getting a Dirac distribution.");
if (isvide(des@comm)) { des@comm <- ajout;
} else { des@comm <- c(des@comm,ajout);}
popo <- bn@vpod[[jjn]]; rere <- bn@vred[[jjn]];
if (rbsb.snp[bn@vnat[[jjn]],"categoric"]) {
    tyty <- "numcat";
    nbca <- length(popo);
    p <- rep(0,nbca);
    if (isvide(val)) {
        medi <- (nbca+((nbca%%2)==1))/2;
        p[medi] <- 1;
    } else {
        if (!is.character(val)) {
            erreur(val,"For a categoric, val must be character");
        }
        if (length(setdiff(val,popo))!=0) {
            erreur(list(val,popo),"val must be included in lpod");
        }
        if (length(val) != 1) {
            erreur(val,"val must be of length 1 for a categoric Dirac");
        }
        p[match(val,popo)] <- 1/nbca;
    }
    papa <- list(p=p);
} else {
    # a numeric variable
    tyty <- "Dirac";
    if (isvide(val)) {
        if (is.infinite(popo[1])) { popo[1] <- rere[1];}
        if (is.infinite(popo[2])) { popo[2] <- rere[2];}
        val=(popo[1]+popo[2])/2;
    } else {
        if (length(val) != 1) {erreur(val,"A numeric of size '1' is expected for val");}
        for (da in bc(1)) {
            if ((val[da]<popo[1])|(popo[2]<val[da])) {
                erreur(list(popo,val),"val must belong to vpod this is not the case for this node");
            }
        }
    }
    papa <- list(k=val);
    }
#
popo <- list(popo);
alk <- new8alk(ldes=des,ltype=tyty,
               lpara=papa,lparent=character(0),
               lpod=popo,lred=popo,lcod=popo);
bn <- mnd4bn(bn,iin,alk);
# returning
if (rebastaba.mck) {check4valid(valid8bn(bn));}
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3nd <- function(bn,kn=bc(nbnv(bn)))
#TITLE symbolizes the kind of nodes
#DESCRIPTION (bn)
# A character vector is returned giving
# for each node designated in 'kn', the number
# of parents and number of children.
#DETAILS
# Description of the symbols:\cr
# "   (#)   " : isolated node\cr
# " ->(#)   " : leaf node with one parent\cr
# "3=>(#)   " : leaf node with three parents\cr
# " ->(#)-> " : transmitter node\cr
# "2=>(#)=>2" : two-parents two-children node\cr
# where '#' stands for the node name.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn}<<The original bn (to get the genealogy).>>
#[INPUTS]
#{kn}<< indicates the nodes to specify (numeric vector)>>
#VALUE
# The resulting named vector of symbols
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# form3nd(rebastaba.bn1,1);
# form3nd(rebastaba.bn1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_29
#REVISED 09_06_05
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# preparing the structure
nbn <- length(kn);
res <- vector("character",nbn);
names(res) <- nv2ion(kn,bn@nom,"n",check=FALSE)@nn;
# finding the necessary genealogy
papa <- parents8bn(bn,"n");
fifi <- children8bn(bn,"n");
# constructing the symbols
for (i in bf(res)) {
    no <- names(res)[i];
    if (identical(papa[[no]],"")) { nbp <- 0;
    } else { nbp <- length(papa[[no]]);} 
    if (identical(fifi[[no]],"")) { nbe <- 0;
    } else { nbe <- length(fifi[[no]]);}
    rr <- paste("(",no,")",sep="");
    if ( nbp == 0 ) { cap <- "   ";
    } else {
        if (nbp==1) { cap <- " ->";
        } else { cap <- paste(nbp,"=>",sep="");}
    }
    if ( nbe == 0 ) { cae <- "   ";
    } else {
        if (nbe==1) { cae <- "-> ";
        } else { cae <- paste("=>",nbe,sep="");}
    }
    res[i] <- paste(cap,rr,cae,sep="");
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
range8node <- function(bn,ion,range="standard")
#TITLE returns the variable ranges of a given node
#DESCRIPTION (bn)
# The ranges can have different acception, see the
# 'range' argument.
#DETAILS
# When the variable is not numeric, 'standard' is
# synonymous with 'lpod'.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn}<<The bn in which the node is defined>>
#{ion}<<indicates the unique node to be considered.>>
#[INPUTS]
#{range}<< the lpod interval if finite else
#          the lred interval. Other possibilities are
#          "lpod","lred","lcod".>>
#VALUE
# a list (as many components as variables) with the 
# corresponding domain. When the variable is numeric,
# intervals are given as numeric(2), the lower bound 
# being the first.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# range8node(rebastaba.bn2,1);
# range8node(rebastaba.bn1,3);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_11_06
#REVISED 09_06_11
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(ion,"integer",1,message="One and only one node must be indicated");
    if (!(range %in% c("standard","lpod","lred","lcod"))) {
        erreur(range,"This type of range does not exist.");
    }
}
#
#
jjd <- nv2ion(ion,bn@nom,"N",check=FALSE)@vk;
res <- vector("list",length(jjd));
for (jd in bf(jjd)) {
    if (range == "lpod") {
	res[[jd]] <- bn@vpod[[jjd[jd]]];
    }
    if (range == "lred") {
	res[[jd]] <- bn@vred[[jjd[jd]]];
    }
    if (range == "lcod") {
	res[[jd]] <- bn@vcod[[jjd[jd]]];
    }
    if (range == "standard") {
	res[[jd]] <- bn@vpod[[jjd[jd]]];
	if (rbsb.snp[bn@vnat[jjd[jd]],"numeric"]) {
	for (i in bf(res[[jd]])) {
	    ou <- is.infinite(res[[jd]][i]);
	    if (ou) {res[[jd]][i] <-  bn@vred[[jjd[jd]]][i];}
	}}
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plan0 <- function(bn,ion,N=5,type="U")
#TITLE generates values from one node of a bn
#DESCRIPTION (bn)
# A vector of N values is extracted from a (most often
# marginal) node according to different ways to serve as
# value(s) when further generating a simulation design.\cr
# The possibilities depends on the node: root, distribution...\cr
# The result depends also on the nature of the variable (
# numeric or character).
#DETAILS
# Notice that the result can be random, so it is advisable to
# monitor the pseudo-random seed value.
# Multivariate nodes are accepted and their variables are considered
# as independent nodes.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn}<<The original bn.>>
#{ion}<<indicates the unique node from which values must
#       be returned (iin or name).>>
#[INPUTS]
#{N}<< number of values to extract.>>
#{type}<< The way to extract the values: 
# "U" for equally spaced along the plausible range (numeric)
# "R" for randomly drawn from the distribution (this
#            applies only for root nodes),
# "P" for proportional to the distribution (this
#            applies only for numeric root nodes and when the 
#            distribution is a standard case where R
#            provides the cumulative distribution.\cr
# For the moment, only the "U" option is implemented.>>
#VALUE
# The resulting vector of values (in fact a matrix with as many
# columns as variables in the node, and N rows).
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# plan0(rebastaba.bn2,1);
# plan0(rebastaba.bn2,2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT The case of multivariate nodes seems dubious...
#FUTURE Most options are to be implemented! 
#AUTHOR J.-B. Denis
#CREATED 08_11_05
#REVISED 09_11_24
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(ion,"integer",1,message="One and only one node must be indicated");
    if (type != "U") {
        erreur(type,"Sorry, for the moment only the 'U' option is implemented");
    }
}
#
#
jjd <- nv2ion(ion,bn@nom,"N",check=FALSE)@vk;
# determining the plausible range
rra <- range8node(bn,ion,"standard");
nbv <- length(rra);
if (nbv != length(jjd)) {
    erreur(list(nbv,jjd),"inconsistency in the variable number");
}
if (rbsb.snp[bn@vnat[jjd[1]],"categoric"]) {
    # it is supposed that categoric nodes are univariate
    if (nbv!=1) {erreur(nbv,"Categoric nodes were supposed to be univariate");}
    N <- min(N,length(rra[[1]]));
}
# preparing the resulting structure
res <- as.data.frame(matrix(NA,N,nbv));
names(res) <- nv2ion(ion,bn@nom,"N",check=FALSE)@nvn;
# extracting the values
for (vv in bf(rra)) {
    if (rbsb.snp[bn@vnat[jjd[vv]],"numeric"]) {
	if (type=="U") {
	    res[,vv] <- seq(rra[[vv]][1],rra[[vv]][2],length=N);
	}
    } else { 
        if (rbsb.snp[bn@vnat[jjd[vv]],"categoric"]) {
	    if (type=="U") {
		rrr <- sample(length(rra[[vv]]),N);
		res[,vv] <- rra[[vv]][sort(rrr)];
	    }
	} else {
		 erreur(bn@vnat[jjd[vv]],"This nature is not accepted");
	}
    }
} 
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plan1 <- function(bn,ion,Nx=7,Nn=3,repl=0)
#TITLE generates imposed values to start a simulation plan on a bn
#DESCRIPTION (bn)
# A matrix of phi(N) values is extracted from (most often
# marginal) nodes according to the so-called uniform way to serve
# starting value(s) when simulating from a bn. It is what we
# called a simulation plan.\cr
# The constraints about the nodes to plan are those of the
# function plan0 which is called (with Nx or Nn) for each node.
# It is supposed that Nx >= Nn since x is for maXimum and n for
# miNimum. For the convenience of the explanation, let us denote
# Vx[i] the set of Nx values associated to the node number ith and
# Vn[i] the set of Nn values associated to the same node. If
# Nx==Nn, the two sets are identical (resulting from the same call
# of plan0) but when Nn < Nx, there is no obligation that Vn be 
# a subset of Vx (see the algorithm in plan0 to get the answer).\cr
# Now the plan is constructed as the following union of combinations
# of these level sets:\cr
# Union(for i varying over the nodes) of
#  (Vx[i] times (product(for j!=i) of the Vn[j])).\cr
# So a duplicate complete plan is produced with Nx=Nn and a central
# composite plan with Nn=1. Intermediate plans for 1<Nn<Nx.\cr
# When all involved nodes can return to plan0 the Nx number of levels,
# the number of observation of the plan is given by p.(Nx).(Nn)^p where 
# p is the number of nodes. 
#DETAILS
# Notice that the result can be random, so it is advisable to
# monitor the pseudo-random seed value.\cr
# Multivariate nodes are accepted and their variables are considered
# as independent nodes.
#KEYWORDS
#PKEYWORDS bn simulation
#INPUTS
#{bn}<<The original bn.>>
#{ion}<<indicates the node(s) from which values must
#       be fixed (iin or name).>>
#[INPUTS]
#{Nx}<< number of values to extract for expanded nodes.>>
#{Nn}<< number of values to extract for node in the central part.>>
#{repl}<< abs(repl) is the number of replications to provide once
#            the first replication has been done. When the number
#            is negative, TRUE repetition of the first replication
#            are not numerically removed. 0 is equivalent to -1.>>
#VALUE
# The resulting data.frame of the values. Columns have got the
# node names; categoric are factors.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# plan1(rebastaba.bn2,1);
# plan1(rebastaba.bn2,2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE more type of plans might be implemented
#AUTHOR J.-B. Denis
#CREATED 08_11_06
#REVISED 09_11_24
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# preparation of standard constants
iid <- nv2ion(ion,bn@nom,"n",check=FALSE)@nk;
jjd <- vector("list",length(iid));
for (jbd in bf(iid)) {
    jjd[[jbd]] <- nv2ion(iid[jbd],bn@nom,"N",check=FALSE)@vk;
}
if ( Nx < 1 ) { Nx <- 7;}
if ( Nn < 1 ) { Nn <- 3;}
repl <- round(repl);
if (repl == 0) { repl <- -1;}
without <- (repl < 0);
if (repl < 0) { repl <- -repl;}
Nx <- round(Nx); Nn <- round(Nn);
# preparing the necessary structures
# number of variables of each concerned node
nv <- sapply(jjd,length);
nbv <- sum(nv);
nam <- character(nbv); # for the variable names
nbx <- numeric(nbv);   # for the maximum number of values for each variable
nbn <- numeric(nbv);   # for the minimum number of values for each variable
exx <- as.data.frame(matrix(NA,Nx,nbv));
enn <- as.data.frame(matrix(NA,Nn,nbv));
# extracting the nodes
for (i in bf(iid)) {
    oui <- sum(nv[bc(i-1)]) + bc(nv[i]);
    ii <- iid[i];
    nam[oui] <- nanv(bn@nom,ii);
    xxx <- plan0(bn,ii,N=Nx);
    nnn <- plan0(bn,ii,N=Nn);
    if (ncol(xxx) != length(oui)) {rapport("Dans plan1 [1]");}
    if (ncol(nnn) != length(oui)) {rapport("Dans plan1 [2]");}
    ### here, it is supposed the same number of values for...
    nbx[oui] <- nrow(xxx); nbn[oui] <- nrow(nnn);
    ### ...the variables coming from the same node
    exx[bc(nrow(xxx)),oui] <- xxx;
    enn[bc(nrow(nnn)),oui] <- nnn;
}
# constituting the simulation plan
ax <- function(m,v) {
 r <- matrix(NA,0,ncol(m)+1);
 for (i in bf(v)) {
  r <- rbind(r,cbind(m,rep(v[i],nrow(m))));
 }
 r;
}
for (i in bc(nbv)) {
    for (j in bc(nbv)) {
        if (j==i) { vv <- 1:nbx[j];
        } else {    vv <- 1:nbn[j];}
        if (j==1) { mm <- matrix(vv,ncol=1);
        } else {    mm <- ax(mm,vv);}
    }
    for (j in bc(nbv)) {
        if (j==i) { vv <- exx[1:nbx[j],j];
        } else {    vv <- enn[1:nbn[j],j];}
        if (j==1) { dd <- data.frame(   vv[mm[,j]]);
        } else {    dd <- data.frame(dd,vv[mm[,j]]);}
    }
    if (i==1) { res <- as.matrix(dd);
    } else {    res <- rbind(res,dd);}
}
if (is.matrix(res)) { 
    dimnames(res) <- list(NULL,nam);
} else {
    names(res) <- nam;
}
# removing repetition within the replication
if (without) {
    nbs <- nrow(res);
    # virtually sorting out the simulations
    vaso <- character(nbs);
    for (jbd in bc(nbs)) {
        vaso[jbd] <- paste(res[jbd,],collapse="=");
    }
    vaor <- order(vaso);
    vadu <- logical(nbs);
    if (nbs > 0) { vadu[1] <- TRUE;}
    # looking for duplicated simulations
    if (nbs > 1) {
        vadu[2:nbs] <- (vaso[vaor[2:nbs]]!=vaso[vaor[1:(nbs-1)]]);
    }
    # keeping only non duplicated values
    res <- res[vaor[vadu],,drop=FALSE];
}

# repeating the plan
if (repl > 1) {
    resu <- res;
    for (re in bc(repl-1)) {
        res <- rbind(res,resu);
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
gn2bn <- function(gn)
#TITLE creates a bn object from a gn object
#DESCRIPTION (bn)
# From a gn object, this function creates a bn object. 
# The transformation is made at the node level.\cr
# As gn objects are poorer than bn object, this
# implies some arbitraryness. Dirac distributions 
# are imposed everywhere with parameter equals to
# its nk when no parents, if not the parent sum.
#DETAILS
#KEYWORDS
#PKEYWORDS gn bn
#INPUTS
#{gn} <<the gn object>>
#[INPUTS]
#VALUE
# the resulting bn object
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(gn2bn(g4n.gn7));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_18
#REVISED 09_05_07
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8gn(gn));}
if (cycle8bgn(gn)) {
    erreur(gn,"This graph has got cycle(s) and cannot be transformed into a bn!");
}
# preparation
nbno <- nbnv(gn,"n");
items <- nanv(gn,"n");
ds <- gn@description;
ds@orig <- paste("Created from the gn",gn@description@name,"[",gn@description@orig,"]");
bn <- zero2bn(ds);
# progressive construction
ppo <- porder(gn);
papas <-neighbours8gn(gn,what="parents");
for (jbd in bc(nbnv(gn))) {
    jno <- ppo[jbd];
    papa <- papas[[jno]];
    mode <- form3liste(papa,"","","","{{","}}");
    if (mode=="") { mode <- jbd; }
    nalk <- new8alk(new("des",name=jno),
                    ltype="Dirac",lnat="conti",
                    lpara=list(k=mode),
                    lpod=list(c(0,nbno^2)));
    bn <- and4bn(bn,nalk);
}
# returning
if (rebastaba.mck) {check4valid(valid8bn(bn));}
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2bn <- function(li,bnd=1,nod=bf(li)[-1])
#TITLE transforms a consistent list into a new bn object
#DESCRIPTION (bn)
# Just analyzing the components of the list
# (consistent names have to be used) which are supposed
# to be character and tackle them to produce consistent
# slots of a des object and alk objects.
# The main use of this function is to generate bn read from text files
# with the function \code{file2list}.
#DETAILS
# Be aware that the node must be given in a compatible order
# with the sequential construction of the bn
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{li} <<The list to be transformed into a bn object.
# The bnd-th component must be the description of the bn;
# the nod components are supposed to be the nodes
# when the remaining arguments are left to default values.>>
#[INPUTS]
#{bnd} << the number of the li component to be
#         interpreted as the description of the bn.>>
#{nod} << the numbers of the li to be
#         considered as defining a node.>>
#VALUE
# The generated 'bn' object
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(list2bn(bn2list(rebastaba.bn4)));
# bn2list(rebastaba.bn0);
#REFERENCE
#SEE ALSO
#CALLING  {list2des} {zero2bn} {bf} {and4bn}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_09
#REVISED 10_06_29
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4tyle(li,"list",-1,message="The first argument must be a list");
}
# getting the bn description
na <- li[[bnd]]$name;
if (isvide(na)) { li$name <- na;}
des <- list2des(li[[bnd]]);
# starting the bn
bn <- zero2bn(des);
# building the bn up
for (nn in bf(nod)) {
    no <- li[[nod[nn]]];
    na <- no$name;
    if (isvide(na)) { na <- names(li)[nod[nn]];}
    lili <- list2alk(no,na);
    bn <- and4bn(bn,lili);
}
# checking the result
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2list <- function(bn)
#TITLE transforms a /bn/ into a list
#DESCRIPTION (bn)
# Just translating all components of a /bn/ into
# a list compatible with \code{list2bn}.
#DETAILS
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{bn} <<The /bn/ to be transformed.>>
#[INPUTS]
#VALUE
# The generated list
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# bn2list(rebastaba.bn4);
# bn2list(rebastaba.bn6);
#REFERENCE
#SEE ALSO
#CALLING  {list2bn} {alk2list}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_04_16
#REVISED 10_08_12
#--------------------------------------------
{
# checking
if (rebastaba.mck) { check4valid(valid8bn(bn));}
# initialization
res <- vector("list",0);
# getting the bn description
res[[1]] <- des2list(bn@description);
# getting all the nodes
for (node in bf(bn@nom)) {
    # extracting the node
    nono <- bn2alk(bn,node);
    # transforming it into list
    lili <- alk2list(nono);
    # imposing the name
    lili$name <- names(bn@nom@x)[node];
    # integrating it into the result
    res[[1+node]] <- lili;
}
# giving the names of the list component
names(res) <- c(bn@description@name,names(bn@nom@x));
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2pam <- function(bn,nv="n")
#TITLE gets the pam from a /bn/.
#DESCRIPTION (bn) produces the pam object associated to the 
#      parentship of "bn"
#DETAILS
#KEYWORDS
#PKEYWORDS bn pam
#INPUTS
#{bn} <<bn object.>>
#[INPUTS]
#{nv} << gives the level at which to proceed, 'n'
#       for node level; 'v' for variable level.>>
#VALUE
# the dimnamed matrix of parentships
#EXAMPLE
# rebastaba3k("RESET"); # needed when R is checking the examples
# bn2pam(rebastaba.bn3,"v");
# bn2pam(rebastaba.bn3,"n");
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_17
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(nv,"character",1,message="'nv' must be 'character(1)'");
    if (!(nv %in% c("n","v"))) {
        erreur(nv,"'nv' must belong to c('n','v')");
    }
}
# preparing the output matrix
if (nv=="v") {
    # identification
    nai <- nv2ion(0,bn@nom,"v",check=FALSE)@nvn;
    nbi <- length(nai);
    # direct parentship at the variable level
    gg <- parents8bn(bn,"v");
    for (jj in bf(gg)) { if (!isvide(gg[[jj]])) {
        # This double call to nv2ion is quite strange ???
        gg[[jj]] <- nv2ion(gg[[jj]],bn@nom,check=FALSE)@nvn;
        gg[[jj]] <- nv2ion(gg[[jj]],bn@nom,check=FALSE)@nvn;
    }}
}
if (nv=="n") {
    # identification
    nai <- nv2ion(0,bn@nom,"n",check=FALSE)@nn;
    nbi <- length(nai);
    # direct parentship at the node level
    gg <- parents8bn(bn,"n");
    for (jj in bf(gg)) { if (!isvide(gg[[jj]])) {
        gg[[jj]] <- nv2ion(gg[[jj]],bn@nom,check=FALSE)@nn;
    }}
}
res <- matrix(0,nbi,nbi);
dimnames(res) <- list(parents=nai,children=nai);
# filling the parentship matrix
for (jbd in bf(gg)) { if (!isvide(gg[[jbd]])) {
form3affiche(gg);
form3affiche(dimnames(res)[[1]]);
form3affiche(gg[[jbd]]);
form3affiche(jbd);
    res[gg[[jbd]],jbd] <- 1;
}}
# returning
new("pam",rlt=res);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2alk <- function(bn,nk)
#TITLE gets an alk from a given bn
#DESCRIPTION (bn)  extracts the alk object associated to the 
#      node number \code{nk}
#DETAILS
# To allow the produced /alk/ to be used with other /bn/
# it is of type non completed.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn} <<bn object.>>
#{nk} <<number of the node.>>
#[INPUTS]
#VALUE
# a non completed /alk/
#EXAMPLE
#REFERENCE
#FUTURE 
#SEE ALSO
#CALLING
#COMMENT
#AUTHOR J.-B. Denis
#CREATED 09_04_20
#REVISED 10_09_03
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(nk,"numeric",-1);
    if ((nk < 1) | (nk > nbnv(bn))) {
        erreur(list(nk,nbnv(bn)),"'nk' cannot designate a node of this 'bn'");
    }
}
#
# gathering the necessary information
vk <- nv2ion(matrix(c(nk,0),2),bn@nom,check=FALSE)@vk;
ldes <- bn@ndes[[nk]];
ldes@comm <- c(ldes@comm,
               paste("Extracted on",today(),"from the bn",bn@description@name)
              );
ltype <- bn@ntype[nk];
lpara <- bn@npara[[nk]];
lrep <-  bn@nrep[nk];
lwin <-  bn@nwin[[nk]];
lnat <-  bn@vnat[vk];
lvar <-  nv2ion(vk,bn@nom,"v",check=FALSE)@vn;
lpod <- lred <- lcod <- vector("list",length(vk));
names(lpod) <- names(lred) <- names(lcod) <- lvar;
for (ii in bf(vk)) {
    lpod[[ii]] <- bn@vpod[[vk[ii]]];
    lred[[ii]] <- bn@vred[[vk[ii]]];
    lcod[[ii]] <- bn@vcod[[vk[ii]]];
}
ltransfo <- bn@ntransfo[nk];
ldaf <-        bn@ndaf[[nk]];
lfunct <-    bn@nfug[[nk]];
lcomp <- FALSE;
lparent <- rbsb.cha0;
if (tolower(rebastaba.l_a[ltype,"lparent"]) == "yes") {
    for (ipp in vk) {
        lparent <- c(bn@vparent[[ipp]]);
    }
    lparent <- nom2char(char2nom(lparent));
}
# dealing with the specificity of "empidata"
# to provide a non completed /alk/
if (ltype == "empidata") {
    lnat <- rbsb.cha0;
    lvar <- rbsb.cha0;
    lparent <- rbsb.cha0;
}
# preparing the structure
res <- new("alk",ldes=ldes,
                 ltype=ltype,
                 lpara=lpara,
                 lrep=lrep, 
                 lnat=lnat, 
                 lvar=lvar,
                 lparent=lparent,
                 lpod=lpod,
                 lred=lred,
                 lcod=lcod,
                 ltransfo=ltransfo,
                 ldaf=ldaf, 
                 lwin=lwin, 
                 lfunct=lfunct, 
                 lcomp=lcomp
          ); 
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
parents8bn <- function(bn,of="n")
#TITLE returns the direct parents of a bn
#DESCRIPTION (bn)
# Returns the parents of all nodes or all variables
# of 'bn'.
#DETAILS
#KEYWORDS
#PKEYWORDS genealogy
#INPUTS
#{bn}<<the bn object.>>
#[INPUTS]
#{of} << 'n' of the nodes; 'v' of the variables.>>
#VALUE
# A list of character vectors containing the names of the parents,
# each component of the list is associated to an item (node or
# variable). When there is no parent, character(0) is returned.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# parents8bn(rebastaba.bn2,"n");
# parents8bn(rebastaba.bn2,"v");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_18
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(of,"character",1,c("n","v"),
               message="'of' must be 'character(1)'");
}
# preparing the list
if (of=="v") {
    # the variable case
    res <- bn@vparent;
    names(res) <- nom2char(bn@nom,"v");
} else {
    # the node case
    res <- lapply(bc(length(bn@nom)),function(x){character(0);});
    names(res) <- nom2char(bn@nom,"n");
    vv <-  nom2char(bn@nom,"v");
    for (j in bf(vv)) {
        moi <- vv[j]; moin <- nv2nod(moi);
        mom <- bn@vparent[[j]];
        res[[moin]] <- c(res[[moin]],mom);
    }
    for (i in bf(res)) {
        res[[i]] <- nom2char(char2nom(res[[i]]),"n");
    } 
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
children8bn <- function(bn,of="n")
#TITLE returns the direct children of a bn
#DESCRIPTION (bn)
# Returns the children of all nodes or all variables
# of 'bn'.
#DETAILS
#KEYWORDS
#PKEYWORDS genealogy
#INPUTS
#{bn}<<the bn object.>>
#[INPUTS]
#{of} << 'n' of the nodes; 'v' of the variables.>>
#VALUE
# A list of character vectors containing the names of the children,
# each component of the list is associate to an item (node or variable).
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# children8bn(rebastaba.bn3,"n");
# children8bn(rebastaba.bn3,"v");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# bn2pam is used because it is built on parents8bn.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_18
#REVISED 10_07_08
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(of,"character",1,c("n","v"),
               message="'of' must be 'character'");
}
# analyzing the genealogy
papa <- bn2pam(bn,of);
# preparing the list
res <- lapply(bc(nrow(papa@rlt)),function(x){character(0);});
names(res) <- dimnames(papa@rlt)[[1]];
for (ii in bc(nrow(papa@rlt))) {
    res[[ii]] <- c(res[[ii]],dimnames(papa@rlt)[[1]][papa@rlt[ii,]==1]);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2nd <- function(bn,nod,what,check=TRUE)
#TITLE extracts and returns some traits of a node
#           defined into a bn
#DESCRIPTION (bn)
# The extraction is made for only one node but several
# different extractions can be asked when 'what' is a
# vector.\cr
# The hope is that function be used very extensively to
# ease further modifications within the /bn/ object structure.
#DETAILS
# The characteristics which are at the bn@nom level
# (like the variable names, indices of the variable node) are
# not included here because they might exist for this
# object.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn} <<The /bn/ from which the extraction will be made.>>
#{nod} <<The node (name or nk specification) to be extracted.>>
#{what} <<A character defining by each of its composents what
#         must be extracted. The different possibilities are:\cr
#  'par': the direct parents of the node (in name specification)\cr
#  'asc': all ascendants of the node (in name specification)\cr
#  'chi': the direct parents of the node (in name specification)\cr
#  'des': all ascendants of the node (in name specification)\cr
#  'nat': variable natures of the node
#[INPUTS]
#{check} <<Must checking be made?>>
#VALUE
# A list with as many components as asked by 'what's with the same
# names. Each component being the computed response built up on the
# /bn/ exploration.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# bn2nd(rebastaba.bn2,"B","par");
# bn2nd(rebastaba.bn2,"B","asc");
# bn2nd(rebastaba.bn2,"B","chi");
# bn2nd(rebastaba.bn2,"B","des");
# bn2nd(rebastaba.bn2,"B","nat");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# this function was undertaken to minimize the direct
# access to the slots of a bn; allowing more flexibility
# for the future evolutions of the /bn/ object.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_05_12
#REVISED 09_11_24
#--------------------------------------------
{
possible <- c("par","chi","asc","des","nat");
# checking
if (check) { 
    if (rebastaba.mck) {
        check4valid(valid8bn(bn));
        check4tyle(nod,c("integer","character"),1,
                   message="'nod' must single and character or integer");
        if (is.numeric(nod)) {
            if ((nod<1)|(nod>nbnv(bn@nom,-1))) {
                erreur(nod,"'nod' cannot be a nk for this bn");
            }
        }
        if (length(union(what,possible))!=length(possible)) {
            erreur(list(possible,what),"Asked what is/are not implemented");
        }
    }
}
#
# turning the node into 'kn' style
if (is.character(nod)) {
    nod <- nv2ion(nod,bn@nom,check=check)@nk;
}
# preparing the returned list
res <- vector("list",length(what));
names(res) <- what;
#
if ("par" %in% what) {
    # dealing with direct parents
    res[["par"]] <- parents8bn(bn,"n")[[nod]];
}
#
if ("chi" %in% what) {
    # dealing with direct parents
    res[["chi"]] <- children8bn(bn,"n")[[nod]];
}
#
if (("asc" %in% what) | ("des" %in% what)) {
    # dealing with ascendants and/or  descendants 
    papam <- arc2pam(bn2gn(bn,"n")@arc);
    aadd <- explore8pam(papam)$rel[[1]];
    if ("asc" %in% what) {
        # ascendants are required
        res[["asc"]] <- names(bn@nom@x)[which(aadd[nod,]==-1)];
    }
    if ("des" %in% what) {
        # descendants are required
        res[["des"]] <- names(bn@nom@x)[which(aadd[nod,]== 1)];
    }
}
# 
if ("nat" %in% what) {
    vk <- nv2ion(nod,bn@nom,"N",check=check)@vk;
    res[["nat"]] <- bn@vnat[vk];
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
modify4bn <- function(bn,pg=g4n.pgr0)
#TITLE plot an object rbsb to interactively modify its graph
#DESCRIPTION (bn)
# Issues a graphical display of the network and using the mouse,
# the user can change the position of the nodes, add and remove
# arcs, rotate the graph to modify the projection plane.\cr
# See modify4gn for further details.
#DETAILS
#  Everything is done through bn2gn, modify4gn and gn2bn
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{bn} <<the rbsb object to plot>>
#[INPUTS]
#{pg} <<graphics parameters (see rebastaba.la)>>
#VALUE
#  returns the modified bn
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# \dontrun{modify4bn(rebastaba.bn2);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_28
#REVISED 07_08_02
#--------------------------------------------
{
#????
stop("This function (modify4bn) has to be rethought and remade [next version]");
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# constructing the "grbsb" object
gf <- bn2gn(bn);
# performing the modification
gf <- modify4gn(gf);
# storing the modification
bn <- gn2bn(gf);
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2model <- function(bn,comment=1)
#TITLE expresses the model formulae
#DESCRIPTION (bn)
# just returning a character string with the model
#DETAILS
# separators can be easily changed if necessary
# since they are coded as constants
#KEYWORDS
#PKEYWORDS bn print
#INPUTS
#{bn} <<The rbsb object>>
#[INPUTS]
#{comment} << 0: iin, 1:nom
#              (for the identification of the variables)>>
#VALUE
# character string
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# bn2model(rebastaba.bn2);
# bn2model(rebastaba.bn1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# separate independent subsets of nodes by "."
#AUTHOR J.-B. Denis
#CREATED 07_06_14
#REVISED 09_06_05
#--------------------------------------------
{
if (rebastaba.mck) {check4valid(valid8bn(bn));}
cg <- "[";cd <- "]"; sp1 <- "."; st <- "|"; sp2 <- ",";
nbno <- nbnv(bn);
if (cycle8bgn(bn)) {
    erreur(NULL,"Be aware that your model is wrong",
                 "due to cycle(s) between the relationships!",
           w=rebastaba.mwa);
}
res <- "";
pare <- parents8bn(bn);
for (jbd in bc(nbno)) {
    if (jbd > 1) { res <- paste(res,sp1,sep="");}
    res <- paste(res,cg,sep="");
    # the child
    if (comment == 0) { enf <- jbd;}
    if (comment == 1) { enf <- nanv(bn@nom)[jbd];}
    res <- paste(res,enf,sep="");
    # the parents
    pp <- pare[[jbd]]; 
    if (length(pp) > 0) { if (!is.na(pp[1])) {
        res <- paste(res,st,sep="");
        for (jd in 1:length(pp)) {
            if (jd > 1) { res <- paste(res,sp2,sep="");}
            pu <- pp[jd];
            if (comment == 0) { ppp <- nv2ion(pu,bn@nom,"n",check=FALSE)@nk;}
            if (comment == 1) { ppp <- pu;}
            res <- paste(res,ppp,sep="");
        }
    }}
    res <- paste(res,cd,sep="");
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df2bn <- function(df,description=new("des",name="created.by.df2bn"))
#TITLE initializes a bn object from a data frame
#DESCRIPTION (bn)
# just creating a bn without any link and constant
# distribution numbering the nodes from the variables
# of the data frame.\cr When a variable is a factor, then
# the distribution is taken as categoric with uniform
# distribution.
#DETAILS
#KEYWORDS
#PKEYWORDS dn bn
#INPUTS
#{df}<<the data frame>>
#[INPUTS]
#{description} << the descripion to give to the bn.>>
#VALUE
# a bn object 
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(df2bn(data.frame(A=1:3,B=LETTERS[1:3],C=11:13)));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_14
#REVISED 09_01_22
#--------------------------------------------
{
if (!is.data.frame(df)) { erreur(NULL,"First argument must be a data frame");}
res <- zero2bn(description);
for (jbd in bc(ncol(df))) {
    ddd <- rbsb.des0;
    ddd@name <- dimnames(df)[[2]][jbd];
    if(is.numeric(df[,jbd])) {
        alk <- new8alk(ddd,"Dirac",list(k=jbd),lpod=list(c(0,ncol(df))));
        res <- and4bn(res,alk);
    }
    if(is.factor(df[,jbd])) {
        alk <- new8alk(ddd,"numcat",
                       lpod=list(levels(df[,jbd])),
                       lpara=list(p=rep(1,length(levels(df[,jbd]))))
                      );
        res <- and4bn(res,alk);
    }
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8gene <- function(bgn,nodes=NULL,what=-1,nbrow=2,nbcol=2)
#TITLE plots ancestry [descendants] of node(s) of a bn/gn
#DESCRIPTION (bn)
# provides a multi-plot (one plot for each node)
# showing all ascendants [or descendants] of a bn or a gn
#DETAILS
# if bn@bef [bn@aft] is empty then 'lignee.bn' is called. It
# is supposed that they are uptodate. Also if it
# is of use afterwards, better to fill it before...\cr
# No check about \code{bgn}
#KEYWORDS
#PKEYWORDS bn plot genealogy
#INPUTS
#{bgn}<<the rbsb object: either a bn or a gn>>
#[INPUTS]
#{nodes} << vector of ion indicating which nodes to 
#          represent, NULL implies all nodes>>
#{what} << -1 for ancestors and everythingelse for
#         descendants>>
#{nbrow} << number of plot rows in a plot>>
#{nbcol} << number of plot columns in a plot>>
#VALUE
# nothing, a plot is produced
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# \dontrun{plot8gene(g4n.gn7,nbrow=4,nbcol=3);}
#REFERENCE
#SEE ALSO
#CALLING {bn2gn} {plot8gn}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_27
#REVISED 09_06_17
#--------------------------------------------
{
# checking and converting as a gn
if (is(bgn,"bn")) { bgn <- bn2gn(bgn);}
#
# determining the nodes to deal with
if (is.null(nodes)) { nono <- bc(nbnv(bgn));}
else {
    if (bgn@item=="n") {
        nono <- nv2ion(nodes,bgn@nom,"n",check=FALSE)@nk;
    } else {
        nono <- nv2ion(nodes,bgn@nom,"N",check=FALSE)@vk;
    }
}
# getting the pam matrix
papa <- arc2pam(bgn@arc);
# preparing the multi-plot
par(mfrow=c(nbrow,nbcol));
# performing a plot a node
for (jbd in nono) {
    # getting the tree
    hah <- geneal4pam(papa,jbd,what==-1);
    gg <- bgn; gg@arc <- hah;
    gg@pos@posi[,4] <- 1*(jbd == bc(nbnv(bgn)));
    # plotting
    gg@pgr <- g4n.pgr0;
    plot8gn(gg,main=nanv(bgn@nom,-1)[jbd]);
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
plot8path <- function(bgn,d,a,nbrow=2,nbcol=2,quels=NULL)
#TITLE plots separately all paths between two nodes
#DESCRIPTION (bn)
# provides a multi-plot (one plot for each path)
# of all paths joining the node d to the node a.
# If there is no path one graph is performed with no arcs.
#DETAILS
# The sub-plots are sorted according to their lengths which are
# given in the title as well as their number.\cr
# No check about  \code{bgn}
#KEYWORDS
#PKEYWORDS bn plot genealogy gn
#INPUTS
#{bgn}<<the rbsb object, either a bn or a gn>>
#{d} <<departure node>>
#{a} <<arrival node>>
#[INPUTS]
#{nbrow} << number of plot rows in a plot>>
#{nbcol} << number of plot columns in a plot>>
#{quels} << which plots to do? NULL means all. If not the number
#          of the plot to draw, numbering being given after sorting 
#         and provided in the title>>
#VALUE
# nothing
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# \dontrun{plot8path(g4n.gn7,"A","F",nbrow=1,nbcol=1);}
# \dontrun{plot8path(g4n.gn7,"A","B",nbrow=1,nbcol=1);}
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_17
#REVISED 09_11_25
#--------------------------------------------
{
# preparation
if (is(bgn,"bn")) { bgn <- bn2gn(bgn); }
if (bgn@item=="n") {
    d <- nv2ion(d,bgn@nom,"n",check=FALSE)@nk;
    a <- nv2ion(a,bgn@nom,"n",check=FALSE)@nk;
} else {
    d <- nv2ion(d,bgn@nom,"N",check=FALSE)@vk;
    a <- nv2ion(a,bgn@nom,"N",check=FALSE)@vk;
}
# looking for the paths
ppp <- gn2path(bgn,d,a);
nbar <- nrow(bgn@arc@fle);
# preparing the multi-plot
par(mfrow=c(nbrow,nbcol));
# decorating the two nodes
bgn@pos@posi[,4] <- 0;
bgn@pos@posi[c(d,a),4] <- 2;
# performing the plot
if ((is.null(quels)) & (length(ppp) == 0)) {
    # no path
    bgn@arc@fle[,3] <- 0;
    plot8gn(bgn,main="No Path Found");
}
else {
    # which plots
    if (is.null(quels)) { quels <- 1:length(ppp);}
    else { quels <- intersect(quels,1:length(ppp));}
    # a path a plot
    for (jbd in quels) {
        chen <- ppp[[jbd]];
        che <- chen;
	# coding the arcs
	bgn@arc@fle[,3] <- 0;
        na <- length(che) - 1;
        if ( na < 1) { erreur(na,"At least one arc is expected!"); }
        for (hd in 1:na) {
            ir <- which(che[hd]   == bgn@arc@fle[,1]);
            jr <- which(che[hd+1] == bgn@arc@fle[,2]);
            ij <- intersect(ir,jr);
            if (length(ij) != 1) {
                print(bgn@arc);
                cat("che=",che,"\n");
                cat("ir =",ir,"\n");
                cat("jr =",jr,"\n");
                cat("ij =",ij,"\n");
                erreur(NULL,"No ONE (0 or more than 1) arc found!");
            }
            bgn@arc@fle[ij,3] <- 211;
        }
	# plotting
	pg <- g4n.pgr0;
	#pg@arrowlength <- 0.10;
	#pg@cexscale <- 5;
        bgn@pgr <- pg;
        titre <- paste(chen,collapse="->");
        titre <- paste("path length =",na);
        titre <- paste("[",jbd,"] ",titre,sep="");
	plot8gn(bgn,main=titre);
    }
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rnorbn <- function(gn,description=new("des",name=gn@description@name),
                      creg=c(0,5),erre=c(0,1),ndec=1)
#TITLE creates a pseudo-random normal bn object from a given bn
#DESCRIPTION (bn)
# From a gn object adds on all nodes normal probability 
# distributions. The parameters for these are pseudo-randomly
# choosen according to arguments creg and erre.\cr
# Using this function with the same set.seed starting point
# (and the same request) must  produce identical bn.\cr
# The gn can be any graph (without cycle) for instance designed
# by hand or resulting from rgn.
#DETAILS
# Have a look to the code, not too difficult to follow.
#??? Add some details with explained examples
#KEYWORDS
#PKEYWORDS
#INPUTS
#{gn} <<The starting gn object.>>
#[INPUTS]
#{description} << The description for
#                the created object (des object). It can be a simple name.>>
#{creg} << The mu and sigma parameters to draw the 
#         coefficient of regression (associated to the mean 
#         and the possible parents) with rnorm.>>
#{erre} << The two limits to draw the standard
#         deviation of the error with runif.>>
#{ndec} << rounding argument for the parameter values.>>
#VALUE
# a bn object comprising only normal variables
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(rnorbn(g4n.gn7));
#REFERENCE
#SEE ALSO rcatbn
#CALLING
#COMMENT  Following R use, sigmas are standard deviations, not
#         variances.\cr
# The first version of this function was called rbn
#FUTURE
# constructing the bn by hand is not very wise. In a future
# version, much better of built a new 'alk' and then 'and4bn'
# successively for each node. This could be more easily done
# once the 'numnormal' ltype would had been implemented.
#AUTHOR J.-B. Denis
#CREATED 07_10_11
#REVISED 09_05_28
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8gn(gn));}
# introducing the gn
bn <- gn2bn(gn);
bn@description <- char2des(description);
# modifying the resulting bn
items <- nanv(bn,"n");
bn@ntype[] <- "normal";
papas <- parents8bn(bn,"n");
for (jbd in bf(items)) {
    # getting the node and its parents
    mam <- papas[[jbd]];
    if (identical(mam,"")) { mam <- character(0);}
    # getting the mean plus regression coefficients as characters
    freg <- rnorm(1+length(mam),creg[1],creg[2]);
    freg <- round(freg,ndec);
    mu <- freg[1];
    for (jd in bf(mam)) {
        if (!(freg[1+jd]<0)) { mu <- paste(mu,"+",sep="");}
        mu <- paste(mu,freg[1+jd],"*",
                       rbsb.cpt["nodes","opening"],
                       mam[jd],
                       rbsb.cpt["nodes","closing"],sep="");
    }
    # getting the standard deviation
    sig <- abs(runif(1,min(erre),max(erre)));
    sig <- round(sig,ndec);
    # reconstructing the alk
    bn@npara[[jbd]] <- list(mu=mu,sigma=sig);
    bn@nrep[[jbd]] <- 0;
    bn@vnat[[jbd]] <- rebastaba.tlk$normal$lnat;
    bn@vpod[[jbd]] <- c(-Inf,Inf);
    bn@vred[[jbd]] <- c(-100,100);
    bn@vcod[[jbd]] <- c(-100,100);
    bn <- code4bn(bn,jbd);
}
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rcatbn <- function(gn,description=new("des",name=gn@description@name),
                   nbcg=c(3,5),
                   co=c(1,1),re=c(3,3))
#TITLE creates a pseudo-random numcat bn object
#DESCRIPTION (bn)
# From a gn object adds on all nodes numcat probability 
# distributions. The parameters for these are pseudo-randomly
# choosen according to arguments 'nbcg', 'co' and 're'. For each
# beta distribution, the two parameters will be taken equal.
# This explains why there is only one interval for each of the
# two parameters.\cr
# Using this function with the same set.seed starting point
# (and the same request) must  produce identical bn.\cr
# The gn can be any graph (without cycle) for instance designed
# by hand or resulting from rgn.
#DETAILS
#KEYWORDS
#PKEYWORDS
#INPUTS
#{gn} <<The starting gn object.>>
#[INPUTS]
#{description} << The name for
#                  the created object (description object).>>
#{nbcg} << numbers of categories for each node 
#            are chosen with equal probabilities within the
#            numbers nbcg[1]:nbcg[2].>>
#{co} << The first  parameter when calling categ3betap
#                 is drawn with runif with these two values.>>
#{re} << The second parameter when calling categ3betap
#                 is drawn with runif with these two values.>>
#VALUE
# a bn object comprising only categoric variables
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(rcatbn(g4n.gn7),quoi="l",qui=9,proba=TRUE);
#REFERENCE
#SEE ALSO rnorbn
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_17
#REVISED 10_02_15
#--------------------------------------------
{
# introducing the gn
bn <- gn2bn(gn);
bn@description <- char2des(description);
# getting the parentship
papa <- parents8bn(bn,"n");
# modifying each node in turn
for (jbd in bc(nbnv(bn))) {
    # getting the size of the new node
    if (length(nbcg[1]:nbcg[2])==1) { nbca <- nbcg[1]; # boudiou!
    } else { nbca <- sample(nbcg[1]:nbcg[2],1);}
    # getting its parents and their dimension
    nmam <- papa[[jbd]];
    vmam <- nv2ion(nmam,bn@nom,"N",check=FALSE)@vk;
    dime <- sapply(bn@vpod[vmam],function(x){length(x);});
    # getting the necessary parameters for the 
    # conditional probability table to generate
    # with categ3betap
    comm <- runif(2,max(0.0001,co[1]),max(co[1],co[2]));
    rela <- runif(2,max(0.0001,re[1]),max(re[1],re[2]));
    if (length(dime) == 0) { dime <- nbca;
    } else { dime <- c(nbca,dime);}
    proba <- matrix(categ3betap(comm,rela,dime),ncol=nbca);
    #
    # reconstructing the alk
    bn@ntype[jbd] <- "numcat";
    bn@npara[[jbd]] <- list(p=proba);
    bn@vnat[jbd]   <- rebastaba.tlk$numcat$lnat;
    bn@ntransfo[jbd] <- "";
    bn@vpod[jbd] <- list(form3names(nbca,
                         prefi=paste(nanv(bn@nom,-1)[jbd],"_",sep=""),
                         upca=FALSE));
    bn@vred[jbd] <- bn@vpod[jbd];
    bn@vcod[jbd] <- bn@vred[jbd];
    bn@vparent[jbd] <- papa[jbd];
    bn <- code4bn(bn,jbd);
}
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8link <- function(bn,nk,quoi="dl")
#TITLE prints an interpretation of one link of a bn
#DESCRIPTION (bn)
#   This function prints how rebastaba will interpret the link
# as defined for a precised node (nk) of a /bn/.
#DETAILS The interpretation is hand made for each type of links.
# The node name is not given because it could be more
# convenient to print it outside this function with some
# adequate format (as done by print8links).
# Global constant rebastaba.cond and rebastaba.emnd are used for 
# the printing of the description of the nodes.
#KEYWORDS
#PKEYWORDS link bn
#INPUTS
#{bn} <<The bn object.>>
#{nk} <<Identification number of the node.>>
#[INPUTS]
#{quoi} << d for description, l for link.>>
#VALUE
# nothing but a print is performed
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print8link(rebastaba.bn1,2);
#REFERENCE
#SEE ALSO print8links
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_30
#REVISED 09_05_28
#--------------------------------------------
{
# some checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(nk,"integer",1,message="Only ONE node at a time for print8link");
}
#
# preparation
retrait <- 4; deblig <- form3repete(" ",retrait);
ax <- function(x) { form3titre(x,1,retrait);}
# node name
nona <- nv2ion(nk,bn@nom,"n",check=FALSE)@nn;
# variable numbers
vanu <- nv2ion(matrix(c(nk,0),2),bn@nom,"n",check=FALSE)@vk;
# getting the link
lk <- bn2alk(bn,nk);
# printing the description
if (expr3present("d",quoi)) { print(lk@ldes,quoi=rebastaba.cond,empha=rebastaba.emnd);}
# printing the link features
if (expr3present("l",quoi)) {
    pp <- parents8bn(bn,"n")[[nk]];
    if (length(pp) == 0) { ax("This node has got no parents");
    } else {
        ax(paste("This node is based on",length(pp),"parent(s):"));
        form3titre(paste(pp,collapse=" ; "),1,retrait+2);
    }
    cc <- children8bn(bn,"n")[[nk]];
    if (length(cc) == 0) { ax("This node has got no child");
    } else {
        ax(paste(length(cc),"child(ren) depend(s) on this node:"));
        form3titre(paste(cc,collapse=" ; "),1,retrait+2);
    }
    if (lk@ltype %in% c("numcat")) {
print.default(lk);

        if (length(lk@lparent) == 0) {
            ax("marginal probability for the node:");
        } else {
            ax("conditional probability for the node:");
        }
        print(bn2pta(bn,nk));
    }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print8links <- function(bn,quoi="dl",qui=rbsb.cha0)
#TITLE prints the interpretation of some links of a bn
#DESCRIPTION (bn)
#   This function prints how rebastaba will interpret the links
# of a bn object.
#DETAILS Just a loop over the designated links and printing each
#KEYWORDS
#PKEYWORDS link bn
#INPUTS
#{bn} <<The bn object.>>
#[INPUTS]
#{quoi} << d for description, l for link.>>
#{qui}<< The nodes which must be considered for the printing.
#   The default implies all nodes, if not either a character vector 
#   providing the nodes or a numeric vector with the iin (internal numbers 
#   of them). The order of printing the nodes is always the natural order
#   because nv2ion sorts the nodes
#VALUE
# nothing but a print is performed
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print8links(rebastaba.bn1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_31
#REVISED 09_11_24
#--------------------------------------------
{
# some checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# nodes to be considered (iin coding)
if (isvide(qui)) { qui <- bc(nbnv(bn@nom));
} else {
    qui <- unique(nv2ion(qui,bn@nom,"n",check=FALSE)@nk);
}
for (jbd in qui) {
    cat("\n");
    cat("(",jbd,")",sep="");
    form3repete("<",20,TRUE,FALSE);
    cat("<<<(",nv2ion(jbd,bn@nom,"n",check=FALSE)@nn,")>>>\n\n");
    print8link(bn,jbd,quoi);
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
anode4bn <- function(bn,des,parent=character(0))
#TITLE very simplified version of and4bn
#DESCRIPTION (bn)
# In this simplified version of and4bn, the user has no
# more than giving the name and the parents of the node.
# The distribution will be automatically put to
# 'Dirac' and the parameter value as the sum of
# the parents which cannot comprise more than a variable.
#DETAILS
# Behind, a standard call to and4bn is made
#KEYWORDS
#PKEYWORDS bn nd
#INPUTS
#{bn} <<bn object to which a node has to be added>>
#{des} <<either the name (\code{character(1)} or
#        a description (/des/).>>
#[INPUTS]
#{parent} << Character vector with the names of the parents.
#           \code{character(0)} means there is no parents.>>
#VALUE
# an object of class "bn" with one additional node.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(anode4bn(rebastaba.bn2,"sup"));
# print(anode4bn(rebastaba.bn2,"sup",c("A","B[b]")));
#REFERENCE
#SEE ALSO and4bn
#CALLING {and4bn}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_26
#REVISED 10_08_13
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(parent,"character",-1);
    # that all parents are univariate
    for (ii in bf(parent)) {
        papa <- parent[ii];
        fiston <- nv2ion(papa,bn@nom,"N")@nvn;
        if (length(fiston)!=1) {
            erreur(list(papa,fiston),"No more than ONE variable with 'papa'");
        }
    }
}
# preparing the description
des <- char2des(des);
# preparing the parameter
if (length(parent) == 0) { kv <- 0;
} else {
    kv <- form3liste(parent,OPA="",CPA="",
                     opa=rbsb.cpt["nodes","opening"],
                     cpa=rbsb.cpt["nodes","closing"],
                     sep="+");
}
# preparing the alk
alk <- new8alk(des,ltype="Dirac",lpara=list(k=kv),
               lpod=list(c(-Inf,Inf)),
               lred=list(c(-100,100)));
# checking the result (a report should be advocated...)
if (rebastaba.mck) {check4valid(valid8alk(alk));}
# returning
and4bn(bn,alk);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2bugs <- function(bn)
#TITLE translates a bn into a bugs model
#DESCRIPTION (bn)
# /not available for the moment/
# exploring all nodes of a bn, produces a
# set of lines of comments and commands
# corresponding to the definition of
# the model block into bugs language.
#DETAILS
# For the moment, only jags dialect is implemented
#KEYWORDS
#PKEYWORDS 
#INPUTS
#{bn}<<bn to translate>>
#[INPUTS]
#VALUE
# A character vector, a component a line.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# cat("To be added\n");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_02_13
#REVISED 08_02_13
#--------------------------------------------
{
if (rebastaba.mck) {check4valid(valid8bn(bn));}
stop("Doesn't work for the moment");
# some constants
width <- 50;
inden <- 4;
# looking for the variable order
ooo <- porder(bn);
# initializing
res <- character(0);
# beginning the model
res <- c(res,paste("#",form3repete("+",width),sep=""));
res <- c(res,paste("### Beginning of Model from bn \"",bn@description@name,"\"",sep=""));
res <- c(res,paste("model {"));
# looping onto the nodes
for (node in ooo) {
    res <- c(res,
             paste(form3repete(" ",inden),
                   code4bn(bn,node,TRUE),
                   sep=""));
}
# ending the model
res <- c(res,"}");
res <- c(res,paste("### End of Model from bn \"",bn@description@name,"\"",sep=""));
res <- c(res,paste("#",form3repete("+",width),sep=""));
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2jagscript <- function(bn,modna,codna,
                         quoi=bn@nom,
                         iter=c(1000,10000,10))
#TITLE constructs a Jags script from a bn
#DESCRIPTION (bn)
# / not available for the moment /
# exploring all nodes of a bn, produces a
# set of lines which can serve as starting script
# for jags. No data is considered; selected nodes are
# monitored; iteration numbers are indicated
#DETAILS
# Completely specific to Jags dialect
#KEYWORDS
#PKEYWORDS 
#INPUTS
#{bn}<<bn to translate>>
#{modna} <<model name: i.e. the file containing the model
#          definition.>>
#{codna} <<coda root name: i.e. the stem for the file 
#          where Jags will store the simulation results.>>
#[INPUTS]
#{quoi} << set of nodes to monitor.>>
#{iter} << respectively the number
#     of iterations for the burnin, the number of 
#     iterations for the monitoring and their
#     thinning value.>>
#VALUE
# A character vector, a component a line.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# cat("To be added\n");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_02_13
#REVISED 08_02_14
#--------------------------------------------
{
stop("Doesn't work for the moment");
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# checking the inputs
if (length(iter) > 0) {
    nburnin <- round(max(100,iter[1]));
} else { nburnin <- 1000;}
if (length(iter) > 1) {
    nmonito <- round(max(100,iter[2]));
} else { nmonito <- 10000; }
if (length(iter) > 2) {
    nthin   <- round(min(100,iter[3]));
    nthin   <- min(round(nmonito/2),nthin);
} else { nthin <- 10; }
# checking the nodes
faux <- FALSE;
for (node in quoi) {
    if (!(node %in% bn@nom)) {
        faux <- TRUE;
        cat("<<<",node,">>> is not a node of bn!\n");
    }
}
if (faux) { erreur(NULL,"Asked node(s) are not present!"); }
# initializing
res <- character(0);
# beginning the script
res <- c(res,paste("model in",modna));
res <- c(res,"compile");
res <- c(res,"initialize");
res <- c(res,paste("update",nburnin));
if (nthin > 1) { thi <- paste(", thin(",nthin,")",sep="");
} else { thi <- "";}
for (jbd in bf(quoi)) {
    res <- c(res,paste("monitor set ",quoi[jbd],thi,sep=""));
}
res <- c(res,paste("update",nmonito));
# ending the script
res <- c(res,paste("coda *, stem(\"",codna,"\")",sep=""));
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bn2bn <- function(bn,ion)
#TITLE extracts a bn from a bn (way number 1)
#DESCRIPTION (bn)
# A new bn is generated from the initial bn 
# reduced to the targetted nodes indicated in 'ion'
# and the parents of these nodes. Independent uniform
# distributions are given to the parents AND AT THE SAME
# TIME THEIR OWN RELATIONSHIPS ARE DELETED. For some argument
# about this practice, see the DETAILS section of 
# function study8nd.\cr
# To give an example, the graph (A->B; A->C; B->C) will 
# result into (A->C; B->C) if (C) is the targetted node.
#DETAILS
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{bn}<<The original bn.>>
#{ion}<<The definition of the nodes (iin or name vector)>>
#[INPUTS]
#VALUE
# The resulting bn
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(bn2bn(rebastaba.bn2,"A"));
#REFERENCE
#SEE ALSO categ3pairs
#CALLING
#COMMENT
# Extracting sub-bn this way can be useful for the
# study of the operation of one node
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_17
#REVISED 08_10_22
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# Looking for the parent and child nodes
parti <- part8bgn(bn,ion);
# deconnecting the parent nodes from their ascendants
for (nn in parti$par) {
    bn <- uniform4bn(bn,nn);
}
# TAKE CARE the removing of nodes must be done 
# as a last step because, it implies modifications
# of the iin...
# removing all other nodes
bn <- rnd4bn(bn,c(parti$asc,parti$nas));
# returning
if (rebastaba.mck) {check4valid(valid8bn(bn));}
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
study8nd <- function(bn,ion,unfixed=character(0))
#TITLE extracts sub-bn(s) with one parent one child from a bn
#DESCRIPTION (bn)
# From a given bn and for one specified node (ion),
# the list of all 'one relevant root' versus 'this child' bn(s)
# is constructed. Non considered parents are put at a fixed value.
#DETAILS
# For the sake of simplicity, let us call here as "parent" a root
# of the bn which has the studied node as descendant.
# The non varying parents are fixed with Dirac distributions 
# for the continuous variables and single domain for categoric 
# variables. The unique varying parent is inherited from a bn2bn
# call.\cr
# It must be underlined that the desired construction is not as
# straightforward as one can think at first. The difficulty comes with
# the fact that the parents are not necessarily independant. Let us
# consider the simple case: (A->C; B->C). C has got two parents which
# are independent and we probably be happy in studying the two
# probabilistic relations: [C|A,B=b] and [C|A=a,B] for respectively
# assess the influence of A over C; and of B over C.  To do so we use
# these conditional probability adding a uniform distribution onto A
# and B\cr
# Now if we add a third arc: (A->B), this is not so clear because
# A and B are no longer independent! In that case, a choice 
# could be to study sum_b([C|A,B=b]) and [C|B,A=a] loosing the symetry
# between the two parents to follow the structure of the graph.\cr
# But things are indeed much more complicated since the joint 
# probability of (A,B) can depend on non direct parents. Let us add
# a new node (D) and the arc (D->B)... Then natural proposals
# are not so spontaneous.\cr
# This is why the retained choice here, was to study the variation of
# one node for root ancestors. If somebody wants to use directly
# the direct parents, s/he must breaks the relationships
# between the parents down (assuming their independence); this can be
# done with bn2bn function.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn}<<The original bn.>>
#{ion}<<The definition of the target node (so-called child)>>
#[INPUTS]
#{unfixed} << The list of parents not to be fixed. This
#            list can comprise other thing that parent names, they
#            will be neglected.>>
#VALUE
# A list of length the number of generated bn, each component comprising
# a bn. For each, description@comm[1] comprises the model associated
# to the bn, description@comm[2] comprises the list of fixed nodes.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# study8nd(rnorbn(g4n.gn7),"F");
#REFERENCE
#SEE ALSO bn2bn
#CALLING ends8gn part8bgn
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_21
#REVISED 09_06_04
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    check4valid(valid8bn(bn));
    check4tyle(ion,c("integer","character"),1,message="One (and only one) node is required!");
}
#
iin <- nv2ion(ion,bn@nom,"n",check=FALSE)@nk;
# Looking for the root parents
lgen <- part8bgn(bn,ion);
lend <- ends8gn(bn2gn(bn));
needno <- c(iin,lgen$par,lgen$asc); # needed nodes
ropano<- intersect(c(lgen$par,lgen$asc),lend$RwL); # root parent nodes
# getting the name because the iin will be changed
# when removing the useless nodes
npa <- nanv(bn@nom)[ropano];
nod <- nanv(bn@nom)[iin];
# constructing the result
if (length(ropano)==0) {
    # The case without parent : root node
    res <- vector("list",1);
    res[[1]] <- rnd4bn(bn,nanv(bn@nom)[-iin]);
} else {
    ## when there are at least one parent
    # removing from the bn useless nodes
    bn <- rnd4bn(bn,setdiff(bc(nbnv(bn)),needno));
    # initializing the result to return
    res <- vector("list",length(npa));
    names(res) <- paste("Varying",npa,"for",nod);
    for (ip in bf(npa)) {
        # dealing with ip-th parent of ropano
        pa <- npa[ip];
        # preparing the appropriate bn
        bb <- bn;
        # putting uniform on the leading node
        bb <- uniform4bn(bb,pa);
        # putting fixed values on the other nodes
        fixed <- character(0);
        for (jp in bf(npa)) { if (jp!=ip) {
            if (!(npa[jp] %in% unfixed)) {
                bb <- Dirac4bn(bb,npa[jp]);
                fixed <- c(fixed,npa[jp]);
            }
        }}
        model <- bn2model(bb);
        fixed <- form3liste(fixed,opa="",cpa="",sep=",");
        bb@description@comm <- character(2);
        bb@description@comm[1] <- model;
        bb@description@comm[2] <- fixed;
        # loading the finded bn
        res[[ip]] <- bb;
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nd2bn <- function(bn,ion)
#TITLE extracts the sub-bn comprising a node and
# its direct parents
#DESCRIPTION (bn)
# From a given bn and for one its node (defined by ion),
# the bn comprising this node and its direct parent is
# extracted. Every parent is given a uniform distribution.
#DETAILS
# The two limits used for the uniform distribution of the parents
# are taken from their @lpod or @lred attributes. If these
# limits are not convenient, they can easily being modified
# with mnd4bn function afterwards. When there are more than
# one parent, the uniform are independent. See also uniform4bn
# for the description of uniform in case of multivariate
# and repeated nodes.
#KEYWORDS
#PKEYWORDS bn node
#INPUTS
#{bn}<<The original bn.>>
#{ion}<<The definition of the target node.>>
#[INPUTS]
#VALUE
# The resulting bn.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# print(nd2bn(rebastaba.bn1,3));
# print(nd2bn(rebastaba.bn2,"B"));
# print(nd2bn(rebastaba.bn2,1));
#REFERENCE
#SEE ALSO
#CALLING  uniform4bn
#COMMENT
# It is usual to call this function before using study8nd.\cr
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_27
#REVISED 08_10_279_11_25
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
if (length(ion) != 1) {
    erreur(ion,"One (and only one) node is required!");
}
iin <- nv2ion(ion,bn@nom)@nk;
# Looking for the parents
npa <- parents8bn(bn,of="n")[[ion]];
# getting the name because the iin will be changed
# when removing the useless nodes
nnn <- nv2ion(0,bn@nom,"n",check=FALSE)@nn;
pare <- nv2ion(npa,bn@nom,"n",check=FALSE)@nk;
nod <- nnn[iin];
# constructing the resultin bn
if (length(npa)==0) {
    # The case without parent : root node
    res <- rnd4bn(bn,nnn[-iin]);
} else {
    ## when there are at least one parent
    res <- bn;
    for (ip in bf(npa)) {
        # dealing with ip-th parent
        pa <- npa[ip];
        # putting uniform on this node
        res <- uniform4bn(res,pa);
    }
    # removing from the bn useless nodes
    needno <- c(iin,pare);
    res <- rnd4bn(res,setdiff(bc(nbnv(bn)),needno));
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8bn <- function(fi)
#TITLE produced a /bn/ from a text file
#DESCRIPTION (bn)
# The text file must follow the structure compatible
# with \code{file2list}. The first component of the file
# must be the description of the bn; the remaining 
# components are supposed to be the nodes.\cr
# As much as possible, the read list is interpreted to
# stick with the requirements of creating a /bn/ from it.
#DETAILS
# See \code{rsbn.demo.creating2.txt} and 
# \code{rsbn.demo.creating3.txt} files to get 
# a simple example. You can also use \code{write8bn}
# to see the result with an existing /bn/.
#KEYWORDS
#PKEYWORDS bn file
#INPUTS
#{fi} <<The file name to be considered.>>
#[INPUTS]
#VALUE
# The generated /bn/ object
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING  {file2list} {list2bn}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_01_09
#REVISED 10_07_13
#--------------------------------------------
{
# reading the list
li <- file2list(fi,path="");
## interpreting the list
# exploring it
ee <- explore8list(li);
# looking for all the components
ve <- bc(nrow(ee));
for (ii in bc(nrow(ee))) {
# eliminating the list ones
ve[ee[,"classes"] == "list"] <- 0;
    nam <- ee[ii,"name"];
    # eliminating the list ones
    if (nam == "list") {ve[ii] <- 0;
    } else { vava <- get8listcomp(li,ee[ii,])[[1]];}
    # dealing with compulsory character(1)a
    if (expr3present(nam,c("name","orig","time","defi","role"),exact=TRUE)) {
        if (length(vava) > 1) {
            li <- set8listcomp(paste(vava,collapse=" "),li,ee[ii,]);
        }
        ve[ii] <- 0;
    }
    # dealing with compulsory character(1)b
    if (expr3present(nam,c("ltype"),exact=TRUE)) {
        if (length(vava) > 1) {
            li <- set8listcomp(vava[1],li,ee[ii,]);
        }
        ve[ii] <- 0;
    }
    # dealing with standard characters
    if (expr3present(nam,c("comm","nvar","lparent"))) {
        li <- set8listcomp(as.character(vava),li,ee[ii,]);
        ve[ii] <- 0;
    }
}
#
if (any(ve>0)) {
    nochecked <- ee[ve>0,,drop=FALSE];
    # form3affiche(nochecked);
}
# introducing the default name if necessary
if (isvide(li[[1]])) {
    li[[1]] <- vector("list",0);
}
if (!("name" %in% names(li[[1]]))) {
    li[[1]]$name <- names(li)[1];
}
# getting the bn
bn <- list2bn(li);
# precautionary checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
new8audit <- function(gn,proba=rbsb.lis0,
                         scores=rbsb.lis0,def.len=5)
#TITLE produced a bn of kind 'audit' from a /gn/
#DESCRIPTION (bn)
# From a given gn, this function generates a /bn/ adapted
# for the specific modeling associated to 'audit'. More 
# precisely:\cr
# (*) all nodes are integer.\cr
# (*) non root nodes are given as value the sum
#     of the values of their own parents by means
#     of Dirac links.\cr
# (*) root nodes are either numcat or the child of
#     specific Dirichlet new nodes according to 
#     the arguments 'proba' and 'scores'. 
#DETAILS
# More precisely, root nodes are transformed, and also their names
# are modified. Let 'A' and 'B' such root node in /gn/. It is possible
# that 'A' be transformed into {'A.multi','A'} and 'B' into 
# {'B.Diric','B.multi','B'}. 'Diri' means Dirichlet, 'multi' means 
# multinomial, in fact it is a 'numcat' node and original root node
# bears the score.
#KEYWORDS
#PKEYWORDS bn gn
#INPUTS
#{gn} <<The gn to be transformed into bn.>>
#[INPUTS]
#{proba} << Named list. When a root name is found, the associated
#          component must be a numeric vector.
#          When they are positive, a numcat with these
#          values for probas is generated. When at least one of them is negative,
#          a Dirichlet with these absolute values is generated.
#          It serves as the parent of the corresponding multinomial node.
#          The lengths must be consistent with possible scores.\cr
#          When no name can be found for a root node a uniform
#          numcat is issued.>>
#{scores} << Named list. When a root name is found, the associated
#          component must be a vector, the values of wich will be
#          used as scores. When no name can be found for a root node,
#          standard scores 1,2,... are used. When a root node have
#          no proba neither scores, a length of def.len is
#          attributed.>>
#{def.len} << See 'scores'.>>
#VALUE
# The generated /bn/ object
#EXAMPLE
#REFERENCE
#SEE ALSO
# See the work in progress with Pierre Pardon.
#CALLING 
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_03_22
#REVISED 09_06_09
#--------------------------------------------
{
# checking
if (rebastaba.mck) {check4valid(valid8gn(gn));}
if (cycle8bgn(gn)) {
    erreur(gn,"This graph has got cycle(s) and cannot be transformed into a bn");
}
# building the bn up
bn <- zero2bn(new("des",name=gn@description@name,
                       orig="created by new8audit"));
# lax is the current maximum for the score
# used to calculate a convenient limit for the @lred
# of each node
#
papas <-neighbours8gn(gn,what="parents");
oo <- porder(gn);
for (jd in bc(nbnv(gn))) {
    jbd <- oo[jd];
    njbd <- nv2ion(jbd,gn@nom,"n",check=FALSE)@nn;
    # getting the parents by their names
    mam <- papas[[jbd]];
    if (length(mam) > 0) {
        ### there are parents
        ep <- form3liste(mam,"","","","{{","}}");
        lax <- 0;
        for (ma in mam) {
            m <- nv2ion(ma,bn@nom,"n",check=FALSE)@nk;
            jm <- nv2ion(ma,bn@nom,"N",check=FALSE)@vk;
            lax <- lax + bn@vpod[[jm]][2];
        }
        nalk <- new8alk(njbd,ltype="Dirac",
                        lpara=list(k=ep),
                        lpod=list(c(0,lax)));
        bn <- and4bn(bn,nalk);
    } else {
        ### it is a root node
        # looking for the proba/propor
        if (njbd %in% names(proba)) {
            diri <- FALSE;
            pp <- proba[[njbd]];
            if (any(pp < 0)) {
                pp <- abs(pp);
                diri <- TRUE;
            }
        } else {
            pp <- rbsb.num0;
            diri <- FALSE;
            }
        # looking for the scores
        if (njbd %in% names(scores)) {
            ss <- scores[[njbd]];
        } else { ss <- rbsb.num0; }
        # dealing with the default values
        if (isvide(ss)) {
            if (isvide(pp)) { ss <- 1:def.len;
            } else { ss <- 1:length(pp);}
        }
        nbc <- length(ss);
        if (isvide(pp)) {
            pp <- rep(1/nbc,nbc);
        } else {
            if (length(pp)!=nbc) {
                erreur(list(ss,pp),"Proba and scores have got different lenghts!");
            }
        }
        lax <- max(ss);
        if (diri) {
            ### Dirichlet option
            njbd.Diric <- paste(njbd,"Diric",sep=".");
            # adding the new numcat node
            njbd.Diric <- paste(njbd,"Diric",sep=".");
            A <- sum(pp); a <- pp/A;
            nalk <- new8alk(njbd.Diric,
                            "Dirichlet",lpara=list(A=A,a=a),
                            lpod=list(c(0,1))
                           );
            bn <- and4bn(bn,nalk);
        }
        # adding the new numcat node
        njbd.multi <- paste(njbd,"multi",sep=".");
        if (diri) {
            # I don't like too much the following because, I had
	    # consistency pb with the names of Dirichlet variables once
	    # upon a time...
            parapara <- var3standard(nbc,"~","~");
            parapara <- paste(njbd.Diric,
                              rbsb.cpt["variables","opening"],
                              parapara,
                              rbsb.cpt["variables","closing"],
                              sep="");
            parapara <- paste(rbsb.cpt["nodes","opening"],
                              parapara,
                              rbsb.cpt["nodes","closing"],
                              sep="");
            nalk <- new8alk(njbd.multi,"parcat",
                            lpara=list(p=parapara),
                            lpod=list(form3names(nbc))
                           );
            bn <- and4bn(bn,nalk);
        } else {
            bn <- and4bn(bn,new8alk(njbd.multi,
                                    "parcat",lpara=list(p=pp),
                                    lpod=list(form3names(nbc))
                                   ));
        }
        # and now the score node
        bn <- and4bn(bn,new8alk(njbd,"score",
                                lparent=njbd.multi,
                                lpara=list(scores=ss),
                                lpod=list(c(0,lax))));
    }
}
#
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8audit <- function(fi)
#TITLE produced an audit /bn/ from a text file
#DESCRIPTION (bn)
# The text file must follow the structure compatible
# with \code{file2list}. More precisely, the result will be
# dealt with list2gn and additionnal fields $proba
# and $scrores will be integrated with new8audit
#DETAILS
# See rebastaba.etb?.txt file to get 
# straightforward examples.
#KEYWORDS
#PKEYWORDS bn file
#INPUTS
#{fi} <<The name of the file to be considered.>>
#[INPUTS]
#VALUE
# The generated /bn/ object
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING  {file2list} {list2gn} {new8audit}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_04_06
#REVISED 10_02_15
#--------------------------------------------
{
# reading the list
li <- file2list(fi,path="");
# getting the gn
gn <- list2gn(li);
# extracting the proba and scores
nopla <- ends8gn(gn);
roots  <- c(nopla$RwL,nopla$RaL);
proba <- scores <- vector("list",0);
nomi <- union(names(nopla$RaL),names(nopla$RwL));
nomi <- union(nomi,            names(nopla$LwR));
nomi <- union(nomi,            names(nopla$rema));
#
for (rr in names(roots)) {
    quel <- which(rr==nomi);
    if (!isvide(li[[quel]]$proba )) {
        proba[[rr]]  <- char2vma(char2chars(li[[quel]]$proba),nat="N");
    }
    if (!isvide(li[[quel]]$scores)) {
        scores[[rr]] <- char2vma(char2chars(li[[quel]]$scores),nat="N");
    }
}
# building the /bn/
bn <- new8audit(gn,proba=proba,scores=scores);
# precautionary checking
if (rebastaba.mck) {check4valid(valid8bn(bn));}
# returning
bn;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp2code <- function(eas,bn,iin,transfo=FALSE,bugs=FALSE)
#TITLE transforms an easyp expression into an R/bugs block
#DESCRIPTION (bn)
# Numeric values are accepted as easyp expressions. eas can 
# be a vector to encompass repeated variables. Access to the bn
# component allow to know if parent nodes are repeated or not,
# but the number of repetitions of the dealt node is supposed
# already established. Rounding and transformation are options.
#DETAILS
# This function must be called to define the parameters of the 
# standard links like "normal", "Bernoulli", "multinomial" and so on...
# The bn is not finished, so it is not checked but the dertermination
# of repetition is supposed to be done [repetition <=> (convenient
# distribution & (length(lvar)>1))].
# Notice that the slots @par & @lvar of the iin-th node are supposed
# already completed. 
#KEYWORDS utilities
#PKEYWORDS expression code
#INPUTS
#{eas} <<either a numeric or a rebastaba expression (character).
#      Its length can be greater than one for repeated standard
#      scalar distributions or vector parameters of other
#      distributions.>>
#{bn} <<The bn giving the context of the translation.>>
#{iin} <<internal numbering of the bn node concerned with the easyp object.>>
#[INPUTS]
#{transfo} << Are rounding and transformation accepted?>>
#{bugs} << Must the code be bugs or R (default).>>
#VALUE
# An interpretable character string to be included
# when generating code.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# cat(easyp2code("{{A}}",rebastaba.bn2,2),"\n");
# cat(easyp2code(1234,rebastaba.bn2,2),"\n");
# cat(easyp2code("2*pi",rebastaba.bn2,2),"\n");
# cat(easyp2code("1+sqrt({{A}})",rebastaba.bn2,2),"\n");
# cat(easyp2code("1+sqrt({{A}}*{{B}})",rebastaba.bn2,2),"\n");
#
#REFERENCE
#SEE ALSO easyp2code1 easyp2code2
#CALLING
#COMMENT
# the bugs case is to be made.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_09
#REVISED 09_04_01
#--------------------------------------------
{
# expanding the main argument accordingly to the bn
eas <- easyp2code1(eas,bn@nom,iin,transfo=transfo,bugs=bugs);
# getting the associated code
res <- easyp2code2(eas,transfo=transfo,bugs=bugs);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
connect8bgn <- function(bgn)
#TITLE determines if a bn/gn is connected
#DESCRIPTION (bn)
# exploring the links between nodes returns the sub-sets
# of connected nodes.
#DETAILS
#KEYWORDS utilities
#PKEYWORDS 
#INPUTS
#{bgn}<<the /bn/ or /gn/ object to investigate>>
#[INPUTS]
#VALUE
# A list with two components \code{$nodes} and \code{$group}.
# Each component is a named vectors either of the groups to which 
# belongs each node, or the nodes belonging to each group.
# For instance, \code{$nodes} is a vector associated to the nodes
# indicating the subsets (or groups).
# If \code{$nodes[3]} equals 2 this means that the third \code{iin}
# node belongs to the second subset. The number of subsets is then 
# \code{max($nodes)} = \code{length($group)}. The numbering of the 
# groups is done in decreasing order of their size.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# connect8bgn(rebastaba.bn0);
# connect8bgn(rebastaba.bn3);
# connect8bgn(g4n.gn1);
# connect8bgn(g4n.gn6);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_27
#REVISED 10_06_14
#--------------------------------------------
{
# checking
if (rebastaba.mck) {
    if (class(bgn)=="gn") { check4valid(valid8gn(bgn));
    } else { check4valid(valid8bn(bgn)); }
}
# preparing
if (class(bgn)=="bn") { bgn <- bn2gn(bgn); }
nbn <- nbnv(bgn);
nba <- nrow(bgn@arc@fle);
# degenerate cases
if (nbn == 0) {
    # no node
    return(list(nodes=structure(numeric(0),.Names=character(0)),
                group=vector("list",0)));
}
if (nba==0) {
    # no arcs
    return(list(nodes=structure(bc(nbn),.Names=names(bgn@nom@x)),
                group=as.list(names(bgn@nom@x))));
}
# arranging the set of arcs in a convenient order
aaa <- cbind(pmin(bgn@arc@fle[,1],bgn@arc@fle[,2]),
             pmax(bgn@arc@fle[,1],bgn@arc@fle[,2]))
ooo <- order(nbn*aaa[,1] + aaa[,2]);
aaa <- aaa[ooo,,drop=FALSE];
# preparing a column for the group numbering
aaa <- cbind(aaa,rep(NA,nba));            
# looping until all arcs be attributed to a group
nug <- 0; nono <- vector("list",0); nfait <- numeric(0);
while (sum(is.na(aaa[,3]))>0) {
    # increasing the group number
    nug <- nug+1;
    # looking for the first non attributed arc
    fa <- min(which(is.na(aaa[,3])));
    # imputing the involved new arcs
    aaa[fa,3] <- nug;
    nono[[nug]] <- unique(aaa[fa,1:2]);
    for (aa in bc(nba-fa)) {
        if (any(aaa[fa+aa,1:2] %in% nono[[nug]])) {
            nono[[nug]] <- unique(c(nono[[nug]],aaa[fa+aa,1:2]));
            if (!is.na(aaa[fa+aa,3])) {
                form3affiche(aaa);
                rapport("The algorithm of 'connect8bgn' failed!");
            }
            aaa[fa+aa,3] <- nug;
        }
    }
    nono[[nug]] <- sort(nono[[nug]]);
    nfait <- unique(c(nfait,nono[[nug]]));
}
# getting the non already incorporated nodes
ng1 <- setdiff(1:nbn,nfait);
# adding the groups with only one node
for (ng in ng1) {
    nug <- nug + 1;
    nono[[nug]] <- ng;
}
# re-numbering the groups and 
# transcripting to the nodes levels
ogr <- order(sapply(nono,length),decreasing=TRUE);
group <- vector("list",0);
nodes <- rep(NA,nbn);
for (iii in bf(ogr)) {
    quels <- nono[[ogr[iii]]];
    group[[iii]] <- names(bgn@nom@x)[quels];
    nodes[quels] <- iii;
}
names(nodes) <- names(bgn@nom@x);
# returning
list(nodes=nodes,group=group);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
porder <- function(bgn)
#TITLE finds a parentship order onto the nodes and more
#DESCRIPTION (bn)
# Proposes an order exploring the parentship of the nodes,
# such that parents are always before their children. Such an
# order can be used for drawing from the joint distribution.
#DETAILS
# This assumes no cycle within the graph. If so, the fact is
# discovered, a message is issued and the function stops the
# program.\cr
# No check is made about the argument.
#KEYWORDS utilities
#PKEYWORDS bn gn
#INPUTS
#{bgn} <<The bn or gn object>>
#[INPUTS]
#VALUE
#order: a compatible order for the calculation
#          of the joint probability. It is given
#          with respect to the "nk/vk" numbering.
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# porder(rebastaba.bn2);
#REFERENCE
#SEE ALSO cycle8bgn
#CALLING
#COMMENT
#FUTURE Distinguish the independent sub networks.
#AUTHOR J.-B. Denis
#CREATED 07_06_16
#REVISED 09_05_06
#--------------------------------------------
{
# checking
if (nbnv(bgn) == 0) { return(numeric(0));}
if (cycle8bgn(bgn)) {
    erreur(bgn,"There is at least one cycle, so no order can be found!");
}
# going through a gn object at node level
if (is(bgn,"bn")) { bgn <- bn2gn(bgn,"n");}
# initialization
items <- gn2item(bgn);
res <- character(0);
nbfait <- -1; 
papas <- neighbours8gn(bgn,what="parents");
# starting the stupid loop
while (nbfait < length(res)) {
    nbfait <- length(res);
    for (jbd in items) {
        papa <- papas[[jbd]];
        papa <- nv2nv(papa)$nod;
        if (length(unique(c(jbd,papa,res))) == (1+length(res))) {
            res <- c(res,jbd);
        }
    }
}
if (length(res) != length(items)) {
    print(bgn);plot(bgn);save(list="bgn",file="bgn.txt");
    rapport("'porder' failed to find an order but cycle8bgn detected no cycle!!!\n (or parents are missing?)");
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cycle8bgn <- function(bgn)
#TITLE detects if a cycle exists in a bn/gn object
#DESCRIPTION (bn)
# This function returns TRUE or FALSE if at least one cycle
# is detected within the graph of the bgn object. It is intended
# to protect other function which arises fatal errors in such a 
# situation (for instance porder).\cr
#DETAILS No check is performed on the class of \code{bgn}
#KEYWORDS utilities
#PKEYWORDS bn gn cycle
#INPUTS
#{bgn} <<The bn or gn object>>
#[INPUTS]
#VALUE
# logical variable (TRUE: at least a cycle, FALSE : no cycle).
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# cycle8bgn(rebastaba.bn2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# no information is provided about the cycle(s). Let use cycles4gn
# for such a purpose.
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_01
#REVISED 07_10_10
#--------------------------------------------
{
nbn <- nbnv(bgn);
if (nbn < 1) { return(FALSE);}
# looking for the pam matrix
if (is(bgn,"bn")) { 
    mm <- bn2pam(bgn);
} else { mm <- arc2pam(bgn@arc); }
# returning
cycle8pam(mm);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expr3bugs <- function(distri,bn,iid,zname,p1="",p2="",p3="",p4="")
#TITLE returns the piece of Bugs code for standard distributions
#DESCRIPTION (bn)
# Here according to the distribution, the parameters and the parents
# the bugs piece of code is established
#DETAILS
#KEYWORDS 
#PKEYWORDS expression bugs
#INPUTS
#{distri} <<ltype of the distribution.>>
#{bn} <<The Bayesian network under consideration>>
#{iid} <<The node of it under consideration>>
#{zname} <<The name under which it must be coded (possibly
#          not its name when a transformation has to be
#          added afterwards).>>
#[INPUTS]
#{p1} << First parameter of the distribution. This
#             expression must already filtered with expr3cobu. >>
#{p2} << Second parameter (if any) of the distribution.>>
#{p3} << Third parameter (if any) of the distribution.>>
#{p4} << Fourth parameter (if any) of the distribution.>>
#VALUE
# Character expression for the code
#EXAMPLE
# rebastaba3k("RESET"); # (only for R checking)
# cat("To be added\n");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_03_07
#REVISED 09_10_23
#--------------------------------------------
{
rapport("To be updated when expr3cobu will be replaced!");
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
write8bn <- function(bn,fi=rbsb.fou,ap=FALSE)
#TITLE writes a /bn/ to a file
#DESCRIPTION (bn)
# writes a /n/ objects to a file to be
# readable with \code{read8bn}
#DETAILS
#KEYWORDS
#PKEYWORDS bn
#INPUTS
#{bn} <<The /bn/ to be written.>>
#[INPUTS]
#{fi} <<name of the file to be written.
#       When \code{rbsb.cha0} no file is
#       written but a charecter is returned.>>
#{ap} <<Must an existing file be appended?>>
#VALUE
# nothing but a file is created or modified
#EXAMPLE
# rebastaba3k("RESET"); # For R checking compliance
# write8bn(rebastaba.bn3);
# readLines(rbsb.fou);
# unlink(rbsb.fou);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_06_23
#REVISED 10_06_30
#--------------------------------------------
{
# some checks
if (rebastaba.mck) {
    che <- valid8bn(bn);
    if (!identical(che,TRUE)) {
        erreur(che,"/bn/ is not valid");
    }
}
#
# creating the list
lili <- bn2list(bn);
#
# adapting the list for a standard use of list2file
#
# writing the file and returning
list2file(lili,fi,
               tags=rbsb.tag1,
               ap=ap);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
