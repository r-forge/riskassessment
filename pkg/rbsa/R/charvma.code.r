
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text2vma <- function(cha,what=rbsa0$vma$v["v"],
                     xsep=rbsa0$sep1$v,nat="C",
                       monitor=rbsa0$monitor$v)
#TITLE  transforms a character into a vector (or matrix, or array), and conversely
#DESCRIPTION
# from a \samp{character} vector, returns a vector, or a matrix, or
# an array of _characters_ with possibly names, or dimames. The information
# can be supplied in different ways for each of the three possibilities.
# It is advised to try the proposed examples.
#DETAILS
# The processing is done in character mode but the result can be
# transformed into numerical or logical values with the help of argument \samp{nat}.
# \cr
# In fact \samp{rbsa0$vma$v} coding is used for the argument \samp{what}. 
# This allows to easily modify the coding.
#KEYWORDS IO
#INPUTS
#{cha} << The character to transform.>>
#[INPUTS]
#{what} << Indicates which structure to return: either
# a vector, a matrix or an array.
#
# \cr ~~ \cr
#
#   For vectors, the possibilities are c/C/u/v/U/V (in fact the content of
#     \samp{rbsa0$vma$v["c"]}, \samp{rbsa0$vma$v["C"]},... but for the sake of the
#     simplicity, the names will be used because they are by default identical to 
#     the value; the same will be done for the other types):
#    \cr\samp{c} for a no named character(1); collapsing
#             is done with \samp{rbsa0$sep0$v}.
#    \cr\samp{C} for a no named character() of any length
#             (components are separated with \samp{xsep} which are
#              removed from the result); collapsing
#             is done with \samp{rbsa0$sep0$v}.
#    \cr\samp{v} or \samp{u} for a no named vector;
#    \cr\samp{V} for a named vector with
#          all names before the values; then an even number
#          of components must be provided.
#    \cr\samp{U} for a named vector with
#          names interlaced with the value (name_i, value_i); then an even number
#          of components must be provided.
#
# \cr ~~ \cr
#
#   For matrices, the possibilities are m/n/o/p/M/N/O/P:
#    \cr\samp{m} for a no named matrix given by rows, two adjacent rows
#          being separated with \samp{xsep} sequence, introduced as one of the
#          component of \samp{cha}, then for a 2x3 matrix, the length of \samp{cha}
#          will be 6+2 = 8.
#    \cr\samp{n} for a matrix with only the columns names. The expected sequence is
#          the names of columns, then the values as for \samp{m}; then for a 2x3
#          matrix, the length of \samp{cha} will be 3+1+8=12.
#    \cr\samp{o} for a matrix with only rows named. The expected sequence is
#          name of row, values of rows... Then 2x3 will imply a length of 8+2=10.
#    \cr\samp{p} when names for columns and rows, in a mixed way... Then 2x3 will imply
#          a length of 14.
#    \cr
#     When \samp{M}, \samp{N},
#          \samp{O} or \samp{P},
#          the same but the matrix will be transposed after
#          being read; said in another way, the values are given by columns.
#
# \cr ~~ \cr
#
#   For arrays, the possibilities are a/A/b/B:
#    \cr\samp{a} for a no named array, the dimensions, \samp{xsep}, the values in
#    the classical order (varying the first index the fastest). 2x3 will give
#    a length of 2+1+6=9.
#    \cr\samp{A} for a dimnamed array, the dimensions, \samp{xsep}, the dimnames of each
#    dimension in the right order also separated and finished with \samp{xsep}. 
#    2x3 will gives a length of 2+1+2+1+3+1+6=16.
#    \cr\samp{b} for a named dimensions array, the dimensions, \samp{xsep}, the names for the
#    dimension in the right order not separated and finished with \samp{xsep}. 
#    2x3 will gives a length of 2+1+2+1+6=12.
#    \cr\samp{B} for a named and dimnamed array, the dimensions, \samp{xsep}, the names for the
#    dimension in the right order not separated and finished with \samp{xsep}, then the dimnames separated
#    before the values. 
#    2x3 will gives a length of\cr (2+1)+(2+1)+(2+1+3+1)+(6)=19. >>
#{xsep} << Character sequence used to separate the character vector into blocks
#    giving information about the structure (see the examples).>>
#{nat} << Nature of the returned structure. Can be \samp{C} for character, \samp{N}
#         for numeric or \samp{L} for logical.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE a vector or a matrix or an array according to the arguments.
#EXAMPLE
#####
## vectors
# text2vma(letters,"c");
# text2vma(letters,"C",xsep="e");
# text2vma(letters);
# text2vma(letters,"V");
# text2vma(letters,"u");
# text2vma(c(LETTERS,letters),rbsa0$vma$v["V"]);
# text2vma(c("A","a","B","b","C","c"),rbsa0$vma$v["U"]);
#####
## matrices
# text2vma(c(1:3,"//",4:6),rbsa0$vma$v["m"]);
# text2vma(c(1:3,"//",4:6),rbsa0$vma$v["M"]);
# text2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),rbsa0$vma$v["n"]);
# text2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),"N");
# text2vma(c("a",1:3,"//","b",4:6),"o");
# text2vma(c(c(LETTERS[1:3],"//","a",1:3,"//","b",4:6)),rbsa0$vma$v["p"]);
#####
## arrays
# text2vma(c(2:4,"//",1:24),"a");
# text2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",
#          letters[1:4],"//",1:24),"A");
# text2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//",
#          letters[1:4],"//",1:24),"A",nat="N");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_28
#REVISED 10_08_13
#--------------------------------------------
{
# flexibility
if (void9(cha)) { cha <- character(0);}
# of use
ssep <- which(cha==xsep);
# checking
if (monitor$chk$v) {
    object9(cha,"character",-1,mensaje="cha must be a character");
    #
    object9(what,"character",1,
               mensaje="what (character(1)) must indicate the type of desired output");
    if (!(what %in% rbsa0$vma$v)) {
        erreur(what,"'what' not in the list of possible values...");
    }
    #
    object9(xsep,"character",1,mensaje="must indicate the character string of separation");
    #
    if (what %in% rbsa0$vma$v[c("V","U")]) {
        if ((length(cha) %% 2) != 0) {
            erreur(list(cha,what),"Here the length of 'cha' must be even");
        }
    }
    #
    if (what %in% rbsa0$vma$v[c("n","N","m","M","o","O")]) {
        if (length(ssep) >= 1) {
            nbc <- ssep[1] - 1;
            nbr <- (length(cha)+1) / (nbc+1);
            if ((nbr!=round(nbr)) | (length(cha) != (nbc*nbr+nbr-1))) {
                erreur(list(what,length(cha),nbc,nbr),"Dimensions not consistent");
            }
        }
    }
    #
    if (what %in% rbsa0$vma$v[c("p","P")]) {
        if (length(ssep) >= 1) {
            nbc <- ssep[1] + 1;
            X <- 2 + length(cha);
            if ((X %% nbc) != 0) {
                erreur(list(what,length(cha),nbc),"Dimensions not consistent");
            }
        }
    }
    #
    if (what %in% rbsa0$vma$v[c("a","A","b","B")]) { 
	# an array must be returned
        if (length(ssep)==0) {
            erreur(list(cha,xsep),"For arrays, dims must be provided");
        }
        didi <- cha[1:(ssep[1]-1)];
        didi <- as.numeric(didi);
        if (sum(is.na(didi))>0) {
            erreur(list(cha,xsep),"Dimensions are not numeric !");
        }
        if (any(didi<0)) {
            erreur(didi,"Negative dimensions were provided");
        }
        if (any(didi!=round(didi))) {
            erreur(didi,"Non integer dimensions were provided");
        }
        X <- length(didi) + 1 + prod(didi);
        if (what %in% rbsa0$vma$v[c("A","B")]) { X <- X + sum(didi) + length(didi); }
        if (what %in% rbsa0$vma$v[c("b","B")]) { X <- X + length(didi) + 1; }
	if (length(cha) != X) {
            erreur(list(cha,what,xsep),"Inconsistency for an array");
	}
    }
    #
    object9(nat,"character",1,mensaje="'nat' must be a character(1)");
}
#
res <- character(0);
#
if (what %in% rbsa0$vma$v["c"]) {
    # a single character must be returned
    # rbsa0$sep0$v is used as collapsor
    res <- paste(cha,collapse=rbsa0$sep0$v);
}
#
if (what %in% rbsa0$vma$v["C"]) {
    # a character must be returned
    # rbsa0$sep0$v is used as separator
    res <- paste(cha,collapse=rbsa0$sep0$v);
    res <- strsplit(res,xsep,fixed=TRUE)[[1]];
    if (length(res)>1) { for (ii in 2:length(res)) {
        res[ii  ] <- form3crop(res[ii  ],rbsa0$sep0$v,"",1);
        res[ii-1] <- form3crop(res[ii-1],"",rbsa0$sep0$v,1);
    }}
}
#
if (what %in% rbsa0$vma$v[c("v","V","u","U")]) {
    # a vector must be returned
    if (what %in% c(rbsa0$vma$v[c("v","u")])) {
        res <- cha;
    } else {
        nb <- floor(length(cha)/2);
        if (what == rbsa0$vma$v["V"]) { nam <- rep(c(TRUE,FALSE),each=nb);
        } else { nam <- rep(c(TRUE,FALSE),nb); }
    res <- cha[!nam];
    names(res) <- cha[nam];
    }
}
#
if (what %in% rbsa0$vma$v[c("m","M","n","N","o","O","p","P")]) {
    # a matrix must be returned
    nbc <- ssep[1] - 1;
    cha <- c(cha,xsep);
    if (what %in% c(rbsa0$vma$v[c("p","P")])) {
        cha <- c(" ",cha);
    }
    if (what %in% rbsa0$vma$v[c("p","P")]) { add <- 1;
    } else { add <- 0; }
    cha <- matrix(cha,ncol=nbc+1+add,byrow=TRUE);
    cha <- cha[,-ncol(cha),drop=FALSE];
    if (what %in% rbsa0$vma$v[c("m","M")]) {
        res <- cha;
    }
    if (what %in% rbsa0$vma$v[c("n","N")]) {
        res <- cha[-1,,drop=FALSE];
        dimnames(res) <- list(NULL,cha[1,,drop=FALSE]);
    }
    if (what %in% rbsa0$vma$v[c("o","O")]) {
        res <- cha[,-1,drop=FALSE];
        dimnames(res) <- list(cha[,1,drop=FALSE],NULL);
    }
    if (what %in% rbsa0$vma$v[c("p","P")]) {
        res <- cha[-1,-1,drop=FALSE];
        dimnames(res) <- list(cha[-1,1,drop=FALSE],cha[1,-1,drop=FALSE]);
    }
    if (what %in% rbsa0$vma$v[c("M","N","O","P")]) {
        res <- t(res);
    }
}
#
if (what %in% rbsa0$vma$v[c("a","A","b","B")]) {
    if (length(ssep) == 0) { erreur(cha,"For array, dimensions must be provided");}
    # an array must be returned
    didi <- cha[1:(ssep[1]-1)];
    didi <- as.numeric(didi);
    nbdi <- length(didi);
    #
    if (what == rbsa0$vma$v["a"]) { vvv <- cha[-(1:ssep[1])];}
    if (what == rbsa0$vma$v["A"]) { vvv <- cha[-(1:ssep[1+nbdi])];}
    if (what == rbsa0$vma$v["b"]) { vvv <- cha[-(1:ssep[2])];}
    if (what == rbsa0$vma$v["B"]) { vvv <- cha[-(1:ssep[2+nbdi])];}
    #
    res <- array(vvv,dim=didi);
    #
    if (what %in% rbsa0$vma$v[c("A","B")]) {
        ndi <- vector("list",0);
        for (jj in bf(didi)) {
            jjj <- jj + (what == rbsa0$vma$v["B"]);
            if (ssep[jjj+1]-ssep[jjj]>1) {
                ndi[[jj]] <- cha[(ssep[jjj]+1):(ssep[jjj+1]-1)];
            }
        }
        dimnames(res) <- ndi;
    }
    if (what %in% rbsa0$vma$v["b"]) {
        ndi <- vector("list",0);
        for (jj in bf(didi)) {
            ndi[[jj]] <- 1:didi[jj];
        }
        dimnames(res) <- ndi;
    }
    if (what %in% rbsa0$vma$v[c("b","B")]) { 
        names(dimnames(res)) <- cha[(ssep[1]+1):(ssep[2]-1)];
    }
}
# transtyping
if (nat %in% c("N","L")) {
    rrr <- as.numeric(res);
    if (nat == "L") { rrr <- as.logical(rrr);}
    attributes(rrr) <- attributes(res);
    res <- rrr;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
vma2text <- function(x,xsep=rbsa0$sep1$v,
                       monitor=rbsa0$monitor$v)
#TITLE  transforms a vector (or matrix, or array) into a character
#DESCRIPTION
# from a vector, or a matrix, or
# an array, builds a \samp{character} vector. More or less the 
# inverse function of \samp{text2vma}. This vector is the first
# component of a returned list, the second component of the list 
# gives the type (\samp{vector}, \samp{matrix} or \samp{array}) of \samp{x},
# the converted object.
#DETAILS
# When some dimnames exist, the possible missing
# ones will be added.
#KEYWORDS IO
#INPUTS
#{x} << The object to transform.>>
#[INPUTS]
#{xsep} << \samp{character(1)} to be use for the separations.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE a list with two components: \samp{[[1]]} the coded character vector and
# \samp{[[2]]} the type according to \samp{text2vma}.
#EXAMPLE
#####
## vectors
# vma2text(letters);
# x <- letters; names(x) <- LETTERS;
# xx <- vma2text(x);
# text2vma(xx[[1]],xx[[2]]);
# vma2text(character(0));
#####
## matrices
# x <- matrix(1:20,4);
# vma2text(x);
# dimnames(x) <- list(letters[1:4],LETTERS[1:5]);
# vma2text(x);
# x1 <- matrix(NA,3,0);
# xx1 <- vma2text(x1);
# text2vma(xx1[[1]],xx1[[2]]);
# dimnames(x1) <- list(c("i","ii","iii"),NULL);
# xx1 <- vma2text(x1);
# text2vma(xx1[[1]],xx1[[2]]);
#####
## arrays
# x <- array(1:24,2:4);
# vma2text(x);
# dimnames(x) <- list(1:2,c("i","ii","iii"),c("I","II","III","IV"));
# vma2text(x,xsep="|||");
# x0 <- array(NA,c(3,0,2));
# dimnames(x0) <- list(1:3,NULL,c("i","ii"));
# xx0 <- vma2text(x0);
# text2vma(xx0[[1]],xx0[[2]]);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_29
#REVISED 10_06_29
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    #
    object9(xsep,"character",1,mensaje="must indicate the character string of separation");
    #
    if (!is.vector(x) &
        !is.matrix(x) &
        !is.array(x)) {
        erreur(class(x),"'x' must be a vector or a matrix or an array!");
    }
}
#
res <- vector("list",2);
names(res) <- c("character","type");
#
if (is.array(x)&(!is.matrix(x))) {
    # dealing with an array
    res[[1]] <- c(as.character(dim(x)),xsep);
    if (is.null(dimnames(x))) {
        res[[2]] <- rbsa0$vma$v["a"];
    } else {
        nna <- dimnames(x);
        for (hh in bf(nna)) {
            if (is.null(nna[[hh]])) {
                nna[[hh]] <- bc(dim(x)[hh]);
            }
        }
        if (is.null(names(nna))) {
            for (ii in bf(nna)) {
                res[[1]] <- c(res[[1]],nna[[ii]],xsep);
            }
        res[[2]] <- rbsa0$vma$v["A"];
        } else {
            res[[1]] <- c(res[[1]],names(nna),xsep);
            for (ii in bf(nna)) {
                res[[1]] <- c(res[[1]],nna[[ii]],xsep);
            }
        res[[2]] <- rbsa0$vma$v["B"];
        }
    }
    res[[1]] <- c(res[[1]],as.character(x));
} else {
    if (is.matrix(x)) {
        # dealing with a matrix
        if (is.null(dimnames(x))) {
            res[[1]] <- character(0);
            for (ii in bc(nrow(x))) {
                res[[1]] <- c(res[[1]],x[ii,],xsep);
            }
            res[[1]] <- res[[1]][-length(res[[1]])];
            res[[2]] <- rbsa0$vma$v["m"];
        } else {
            nna <- dimnames(x);
            for (hh in bf(nna)) {
                if (is.null(nna[[hh]])) {
                    nna[[hh]] <- bc(dim(x)[hh]);
                }
            }
            res[[1]] <- c(as.character(nna[[2]]),xsep);
            for (ii in bc(nrow(x))) {
                res[[1]] <- c(res[[1]],nna[[1]][ii],as.character(x[ii,]),xsep);
            }
            res[[1]] <- res[[1]][-length(res[[1]])];
            res[[2]] <- rbsa0$vma$v["p"];
        }
    } else {
        # dealing with a simple vector
        if (is.null(names(x))) {
            res[[1]] <- as.character(x);
            res[[2]] <- rbsa0$vma$v["v"];
        } else {
            res[[1]] <- c(names(x),as.character(x));
            res[[2]] <- rbsa0$vma$v["V"];
        }
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
