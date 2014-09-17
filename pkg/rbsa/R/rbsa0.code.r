
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rbsa0 <- 
#TITLE list of the /rbsa/ constants
#DESCRIPTION
# Just a list of constants. Each constant is defined with a name, a definition
# and a value. The initial values can be modified by the user. Some
# specifies the way the \pkg{rbsa} calculations and displays are made. Values
# definitions and names can be obtained with the function \samp{display8k}.\cr
# An important role of the constant is to drive the behavior of many functions
# through \samp{rbsa0$monitor$v} which is itself a \samp{rbsa} constant.
#DETAILS
# It is a named list, one component for each constant.
# A named sublist is associated to each constant with two components: \samp{$d}
# for the definition and \samp{$v} for the value. Be aware that the value
# can be any object (vector, list, matrix, function,...)
#KEYWORDS misc helpful
#INPUTS
#[INPUTS]
#VALUE
# A list see the details for the description.
#EXAMPLE
# print(display8k(rbsa0,"names"));
# print(display8k(rbsa0));
# print(display8k(rbsa0$monitor$v,"names"));
# print(display8k(rbsa0$monitor$v));
# str(rbsa0$monitor$v);
#REFERENCE
#SEE ALSO rbsa7list9 display8k
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_09_16
#REVISED 14_09_01
#--------------------------------------------
{
  # additional variables
  axfpx <- "graph";
  vma <- c("c","C","v","V","u","U","m","n","o","p","M","N","O","P","a","A","b","B");
  names(vma) <- vma;
  # definition of the different constants
  list(
       mwi=list(v=70,
                d="The width (nbr of characters) for printing paragraphs"),
       mfi=list(v=TRUE,
                d="Must the results be directed to files (if not to the screen)?"),
       mgr=list(v="pdf",
                d="Type of graphics files"),
       mnd=list(v=3,
                d="Number of decimals when printing"),
       mep=list(v=1,
                d="When printing an object: *e*m*p*hasize level"),
       fpx=list(v=axfpx,
                d="Prefix for the resulting files"),
       fou=list(v=paste(axfpx,"txt",sep="."),
                d="Standard file for text outputs"),
       fin=list(v=paste(axfpx,"int.txt",sep="."),
                d="Standard intermediary file"),
       tyl=list(v=c("A","a","n"),
                d="Types of bullets for standard lists"),
       l00=list(v="~",
                d="Symbol for the null bullets (see read8list)"),
                # due to the use of easyp3cut, no nesting parenthesis are allowed!
                # also opening and closing must be different
       cpt=list(v= matrix(c("{{","}}",
                            "(|","|)",
                            "[" ,"]" ,
                            "<<",">>"),ncol=2,
                          byrow=TRUE,dimnames=list(c("nodes","rounding","variables","vectors"),
                                                   c("opening","closing"))),
                d="Different closing parentheses as a dimnamed matrix"),
       nch1=list(v=structure("1", .Names = "A"),
                 d="Example 1 of a named character objects"),
       nnu1=list(v=structure(1, .Names = "A"),
                d="Example 1 of a named numeric objects"),
       lis1=list(v=list(A=1:3,
                        B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
                        C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2))))),
                 d="Example1 of a list objects"),
       dfr1=list(v=data.frame(F1=factor(rep(1:3,each=4)),F2=factor(rep(1:4,3))),
                 d="Example1 of data.frame object"),
       dfr2=list(v=data.frame(SEX=factor(c("M","M","F","F","F","M")),
                              AGE=20+5*(1:6),
                              HGT=185-1:6),
                 d="Example2 of data.frame object"),
       dfr3=list(v=data.frame(SEX=factor(c(rep("M",50),rep("F",50))),
                              AGE=round(40+sin(1:100)*20),
                              HGT=c(round(170+cos(1:50)*17),round(155+cos(1:50)*16)),
                              WGT=c(round(75+cos(50:1)*15),round(60+cos(50:1)*20))),
                d="Example3 of data.frame object"),
       #
       vma=list(v=vma,
                d="Different types of vma (see text2vma for details)"),
       sep0=list(v=" ",
                 d="Basic string separator"),
       sep1=list(v="//",
                 d="Second string separator"),
       sep2=list(v=";",
                 d="Third string separator"),
       tag1=list(v=matrix(c("<<",">>",
                            "[[","]]",
                            "((","))"),
                          ncol=2,byrow=TRUE),
                 d="Standard tagging for text file associated to list"),
       tag2=list(v=matrix(c("[[","]]",
                            "((","))"),
                          ncol=2,byrow=TRUE),
                d="Alternative tagging for text file associated to list"),
       # 
       monitor=list(v=list(chk=list(v=TRUE, d="must arguments be checked?"),
                           pau=list(v=FALSE, d="must call to pause be effective in some functions?"),
                           war=list(v=FALSE,d="must warnings be considered as fatal errors"),
                           fat=list(v=TRUE, d="must fatal errors be considered as fatal errors"),
                           ind=list(v=2,    d="size for indentation")),
                    d="global variable to monitor the behavior of some /rbsa/ functions"),
       tgs=list(v=list(
                  deb=list(v="<<<",t="Start",d="Starting point"),
                  mil=list(v="---",t="Middle",d="Intermediary point"),
                  fin=list(v=">>>",t="End",d="Ending point")
                 ),
                d="Example of tagging for text"),
       text1=list(v=c(paste0(rep(c(1:9,0),1),collapse=""),
                      paste0(rep(c(1:9,0),2),collapse=""),
                      paste0(rep(c(1:9,0),3),collapse=""),
                      paste0(rep(c(1:9,0),4),collapse=""),
                      paste0(rep(c(1:9,0),3),collapse=""),
                      paste0(rep(c(1:9,0),2),collapse=""),
                      paste0(rep(c(1:9,0),1),collapse="")),
                  d="text example number 1"),
       text2=list(v=c("Once upon a time there was a sweet little girl.",
                      "Everyone who saw her liked her, but most of all her grandmother",
                      "Once she gave her a little cap made of red velvet.",
                      "Because it suited her so well, and she wanted to wear it all the time,",
                      "she came to be known as Little Red Riding Hood.",
                      "One day her mother said to her: ",
                      "Come Little Red Riding Hood. Here is a piece of cake and a bottle of wine.",
                      "Take them to your grandmother.",
                      "She is sick and weak, and they will do her well.",
                      "Mind your manners and give her my greetings.",
                      "Behave yourself on the way, and do not leave the path,",
                      "or you might fall down and break the glass,",
                      "and then there will be nothing for your sick grandmother."),
                  d="text example number 2"),
       text3=list(v=c("<<< A gentle introduction!",
                      "---",
                      "A quite interesting first part",
                      "---",
                      "The core of the story (1)",
                      "The core of the story (2)",
                      "The core of the story (3)",
                      "---",
                      "Some concluding remarks.",
                      ">>>"),
                  d="text example number 3 (tagged)"),
       text4=list(v=c("Every paragraph must be followed ",
                      "with an empty line",
                      "",
                      "A quite interesting first part",
                      "---",
                      "---",
                      "",
                      "{#} << First item>>",
                      "{#} <<Second item>>",
                      "{#} << Third item",
                      "       can comprise several",
                      "       lines!>>",
                      "{#} <<Fourth item>>",
                      "",
                      "Some concluding remarks."),
                  d="text example number 4 (tagged)"),
       text5=list(v=c("Every paragraph must be followed ",
                      ")BULLETS( * + -",
                      "* FIRST LEVEL 1",
                      " + Second Level 1",
                      "   - third level 1",
                      "   - third level 2",
                      " + Second Level 2",
                      " + Second Level 3",
                      "* FIRST LEVEL 2",
                      " + Second Level 1",
                      " + Second Level 2",
                      "   - third level 1",
                      "   - third level 2",
                      "* FIRST LEVEL 3"),
                  d="text example number 5 (tagged)")
    );
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
display8k <- function(constants,what="exhibit")
#TITLE  returns information about a /rbsa/ constant
#DESCRIPTION
# Exhibits a \samp{rbsa} constant 
# or returns a \samp{character} vector for the names and
# the definitions of \samp{constants} or a \samp{list} for
# the values.
#DETAILS
# A \samp{rbsa} type constant is a named list, each component being
# a list of two components: \samp{$d} for the definition which must be
# a character (concatenated if not a scalar) and \samp{$v} which
# can take any value.
#KEYWORDS misc helpful
#INPUTS
#{constants} << The \samp{rbsa} constant list (see the details for the description).>>
#[INPUTS]
#{what}    <<a character(1) indicating what must be returned. The acceptable
#           values are \samp{exhibit}, \samp{names}, \samp{definitions} and \samp{values}.
#           For user convenience, the argument is lowercased before checked,
#           and only the first character is taken into consideration.>>
#VALUE
# Information is displayed and nothing is returned for exhibition
# else a named \samp{character} or a named \samp{list}. 
#EXAMPLE
# display8k(rbsa0)
# display8k(rbsa0$monitor$v,"v")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_05_21
#REVISED 14_06_03
#--------------------------------------------
{
  # checking
  object9(what,"character",1,mensaje="Awaiting for a character(1)");
  what <- tolower(what);
  what <- substr(what,1,1);
  object9(what,"character",1,c("d","v","n","e"),
          "Must be 'names', 'descriptions', 'values' or 'exhibit'");
  # looking if it is a constant or a list of constants
  nana <- names(constants);
  if (length(unique(c(nana,"v","d")))==2) {
    # must be a constant
    lcon <- list(k=constants);
  } else {
    # must be a list of constant
    lcon <- constants;
  }
  # filling values
  if (what == "v") {
    res <- vector("list",length(lcon));
    names(res) <- names(lcon);
    for (vv in bf(lcon)) {
      res[[vv]] <- lcon[[vv]]$v;
    }
  }
  # filling descriptions
  if (what == "d") {
    res <- vector("character",length(lcon));
    names(res) <- names(lcon);
    for (vv in bf(lcon)) {
      res[vv] <- paste(lcon[[vv]]$d,collapse=" / ");
    }
  }
  # filling names
  if (what == "n") {
    res <- names(lcon);
  }
  # displaying
  if (what == "e") {
    for (cc in bf(lcon)) {
      form3title(names(lcon)[cc],4);
      print(lcon[[cc]]$d);
      print(lcon[[cc]]$v);
    }
    res <- invisible();
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
void9 <- function(x,vector=FALSE) 
#TITLE  tests the nullness of objects
#DESCRIPTION
# Returns TRUE is the structure is empty.\cr
# This function was proposed because the \samp{NULL} cannot replace
#  any kind of objects.\cr
#  Notice that \samp{void9("")} is \samp{TRUE}.
#DETAILS
# Non-existing objects are not detected as void!
# Functions are considered as not void.
# Mainly \samp{NULL}, \samp{NA} and length-zero object are void
# then a \samp{data.frame} with at least one variable is declared as not void.
# Character \samp{""} is considered as void.
#KEYWORDS programming
#INPUTS
#{x}    <<object to be scrutinazed>>
#[INPUTS]
#{vector} <<This argument is considered only when \samp{x} is
#           a \samp{character} or a (\samp{numeric} or \samp{logical}). When \samp{FALSE} it is considered void
#           when all components are \samp{""} or \samp{NA}; when \samp{TRUE}, a vector
#           with same length as \samp{x} is returned indicating which
#           components are \samp{""} or \samp{NA}.>>
#VALUE
# TRUE when the object is considered as void
# FALSE if not. Can also be a vector for characters when \samp{vector}.
# See the proposed examples.
#EXAMPLE
# \dontrun{void9(toto);}
# void9(numeric(0));
# void9(NULL);
# void9("");
# void9(c("","A"));
# void9(c("","A"),vector=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Find a trick for non-existing objects: difficulty of the kind is that \samp{TRUE} 
#       is detected as void and more
#AUTHOR J.-B. Denis
#CREATED 07_10_15
#REVISED 14_08_28
#--------------------------------------------
{
  # non existing object
  #if (!exists(deparse(substitute(x)))) { return(TRUE);}
  # covering some special non null cases
  if (is.function(x))   { return(FALSE);}
  # standard cases
  if (is.null(x))                      { return(TRUE);}
  if (length(x)==0)                    { return(TRUE);}
  # looking when character
  if (is.character(x)) {
    if (vector) {
      return(x=="");
    } else {
      return(all(x==""));
    }
  }
  # looking when numeric
  if (is.numeric(x)|is.logical(x)) {
    if (vector) {
      return(is.na(x));
    } else {
      return(all(is.na(x)));
    }
  }
  # returning
  FALSE;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
erreur <- function(x,...,w=FALSE,monitor=rbsa0$monitor$v)
#TITLE  issues an error message and concludes accordingly
#DESCRIPTION
# when called this function prints x, then displays a message before stopping 
# the process except if it is a warning or if the general constant
# \samp{monitor$fat$v} is true.
#DETAILS
#KEYWORDS error
#INPUTS
#{x} <<object to be printed before the message. When \samp{void9(x)}
#      nothing is printed. When it is a list, all components
#      of the list are successively printed.>>
#{\dots}<<pieces of message to display after pasting>>
#[INPUTS]
#{w} << Indicates if it is a simple warning>> 
#{monitor} << List of monitoring constants, see \samp{rbsa0$monitor} to
#             know its structure.>>
#VALUE
# nothing is returned
#EXAMPLE
# erreur(matrix(1:4,2),"This matrix is not symmetric",w=TRUE)
# erreur("Are you sure of it?",w=TRUE);
#
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_03
#REVISED 13_10_01
#--------------------------------------------
{
 cat(paste(rep("~",60),collapse=""),"\n");
if (!void9(x)) {
    if (is.list(x)) {
        for (i in 1:length(x)) {
             cat(paste(rep("~",40),collapse=""),"\n");
            cat("<< Displaying ",deparse(substitute(x[[i]]))," >>\n")
            print(x[[i]]);
        }
    } else {
        cat("<< Displaying ",deparse(substitute(x))," >>\n")
        print(x);
    }
}
mensaje <- paste(...);
cat("<<<<< MESSAGE >>>>>\n");
print(mensaje);
if (w) {
    cat("/rbsa/","SIMPLE WARNING:\n");
} else {
    on.exit(traceback());
    cat("/rbsa/","ERREUR FATALE\n");
    cat(paste0(rep("~",60),collapse=""),"\n");
    if (monitor$fat$v) { stop("stopped by rbsa");}
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
object9 <- function(x,typ,len=-1,con=numeric(0),
                       mensaje=NULL,fatal=TRUE,na.rm=FALSE,
                       cla=NULL,speci=NULL)
#TITLE  checks the type, the length and the content of some standard object
#DESCRIPTION
#  checks the type, the length and the content
# of some standard object.
# If not correct, a message and a fatal error are issued.
# \samp{NA} are detected and considered as wrong or not.\cr
# When \samp{typ} is 'directory' or 'file', \samp{x} must be
# a character containing path(s) of directories or files.
#DETAILS
# 'integer' has not got the meaning in \samp{is.integer} R
# standard function. 'null' must be understood as
# resulting TRUE with function \samp{void9}.
#KEYWORDS error
#INPUTS
#{x} <<object to be checked.>>
#{typ} << Character vector indicating the accepted types
# for the object to be checked.
# See the code for the list of accepted types, among
# them 'null', 'integer', 'numeric', 'character',
# 'logical', 'list', 'any', 'function', 'data.frame',
# 'matrix','file','directory',...
# As indicated, 'any' implies that no
# check of the type is performed.>>
#[INPUTS]
#{len} <<If \samp{length(len)==1}, the exact length
# to be checked, if not must be of length two for
# the possible range of \samp{length(x)}. When -1,
# no check on the length is performed.\cr
# For data.frame, it is the number of columns.
# When \samp{na.rm} the check is made after
# removing the NA values.>>
#{con} << If \samp{length(con)>0}, some check about
# the content of \samp{x} is done for some of the 
# types. More precisely for (integer, numeric): all
# values must belong to the interval \samp{[con[1],con[2]]}
# and for (character), the set of possible \samp{character(1)}
# is described by \samp{con}.>>
#{mensaje} << Some additional message to be
#            issued before stopping in case of error.>>
#{fatal} << what to do when discovering
#           an inconsistency ? TRUE: this function prints the message
#           and stops; FALSE: this function returns
#           the message as a character.>>
#{na.rm} << Must the \samp{NA} values be removed before checking?
#           This is implemented only for the types integer, numeric,
#           character and logical.>>
#{cla} << The class to which the object is supposed to belong. When \samp{NULL}
# no check is made with respect to the class.>>
#{speci} << Some additional specific check for some object types.
#           The following possibilities are implemented:\cr
#           - \samp{NULL} when not used,\cr
#           - \samp{"named"} to indicate that a vector must be named,\cr
#           - a \samp{matrix} with two rows and two column for matrix objects
#             to indicate intervals for the row number (first
#             row) and the column number (second row); \samp{NA} values means
#             no check,\cr
#           - a \samp{matrix} with \samp{ndim+1} rows and two columns for
#             array objects to indicate intervals for the number of
#             dimensions and the size of each of them; \samp{NA} values means
#             no check.
#         >>
#VALUE
# When everything is OK returns a \samp{character(0)}.
# If not according to the argument \samp{fatal} print an error message
# and stops, or just returns the message as a warning.
#EXAMPLE
# object9(letters,c("numeric","character"),c(20,30),mensaje="!OK");
# object9(structure("A",.Names="a"),"ncharacter",-1,LETTERS);
# object9("A","ncharacter",-1,mensaje="BAD",fatal=FALSE);
# object9(structure("A",.Names="a"),"character",-1,letters,"bad",fatal=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE (i) improves the treatment of 'NA's; (ii) allows the check of forbidden values
#AUTHOR J.-B. Denis
#CREATED 09_05_02
#REVISED 14_08_16
#--------------------------------------------
{
# checking the arguments and preparation
# accepted types by object9
typena <- c("integer","numeric","character",
           "logical","ninteger","nnumeric",
           "ncharacter","nlogical",
            "file","directory");
# In typena are those types from which NA values 
# can be removed
types <- c(typena,"list","function","any",
           "data.frame","matrix","array","null");
type <- match(typ,types);
if (all(is.na(type))) {
    erreur(list(types,typ),
           "OBJECT9: the proposed type was not in the list!"
          );
}
res <- character(0);
# dealing with possible NAs
narm <- 0;
if (na.rm) { if (all(!is.na(match(typ,typena)))) { 
    nu <- !is.na(x);
    narm <- length(x) - length(nu);
    x <- x[nu];
}}
#
# the null case as 'x'
if (is.null(x)) {
    if ("null" %in% typ) { return(character(0));
    } else { 
        res <- paste("The proposed object is NULL not in '",
                     paste(typ,collapse="/"),"'");
	if (!is.null(mensaje)) { res <- c(res,mensaje);}
        cat(paste0(rep("~",60),collapse=""),"\n");
	cat("From object9: in fact 'x' is '",
	    deparse(substitute(x)),"'\n");
	print.default(x);
	if (fatal) {erreur("Message:",paste(res));}
    }
}
if (all(!is.na(match(typ,typena)))) { if (any(is.na(x))) { 
    erreur(typ,"OBJECT9: 'x' is or comprises 'NA' which were not removed!");
}}
# possible type for the check
type <- types[type[!is.na(type)]];
if (!is.numeric(len)) {
    erreur(len,"OBJECT9: 'len' must be NUMERIC of size one or two");
}
if ((length(len)==0)|(length(len)>2)) {
    erreur(len,"OBJECT9: 'len' must be numeric of size ONE or TWO");
}
# class check
if (!is.null(cla)) {
  if (!(class(x)%in%cla)) {
    res <- c(res,paste("The class is ",class(x)," not in /",
                       paste(cla,collapse="/"),"/",sep=""));
  }
}
# processing for the length
if (length(len)==1) {
    if (len>=0) { if (length(x)!=len) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,paste("OBJECT9: 'x' is not of length ",len));
    }}
} else {
    if ((length(x)<len[1])|(length(x)>len[2])) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,paste("OBJECT9: 'x' has got a length outside:",paste(len,collapse=":")));
    }
}
# processing for the type
if (!("any" %in% type)) {
    ty <- FALSE;
    for (tt in type) {
        #
        if (tt=="integer")   { if (is.numeric(x)) {
            if (all(x==round(x))) {
                ty <- TRUE;
            }
        }}
        if (tt=="ninteger")   { if (is.numeric(x)) {
            if (all(x==round(x))) { if (length(names(x))>0) {
                ty <- TRUE;
            }}
        }}
        #
        if (tt=="numeric")   { if (is.numeric(x))   {
            ty <- TRUE;}
        }
        if (tt=="nnumeric")   { if (is.numeric(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="character") { if (is.character(x)) {
            ty <- TRUE;}
        }
        if (tt=="ncharacter")   { if (is.character(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="logical")   { if (is.logical(x))   {
            ty <- TRUE;}
        }
        if (tt=="nlogical")   { if (is.logical(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="function")  { if (is.function(x))  { ty <- TRUE;}}
        if (tt=="list")      { if (is.list(x))      { ty <- TRUE;}}
        if (tt=="data.frame"){ if (is.data.frame(x)){ ty <- TRUE;}}
        if (tt=="matrix")    { if (is.matrix(x))    { ty <- TRUE;}}
        if (tt=="array")     { if (is.array(x))     { ty <- TRUE;}}
        if (tt=="null")      { if (void9(x))        { ty <- TRUE;}}
        #
        if (tt=="file") {
          if (all(fidi9(x) == "f")) { ty= TRUE;
          } else { res <- "Not all components were detected as file(s)";}
        }
        if (tt=="directory") {
          if (all(fidi9(x) == "d")) { ty= TRUE;
          } else { res <- "Not all components were detected as directory(ies)";}
        }
    }
    if (!ty) {
        res <- c(res,paste("Among type = ",paste(type,collapse="/")));
        rr <- "The class of 'x' is '";
        if (is.character(attributes(x)$names)) {
            rr <- c(rr,"named ");
        }
        rr <- c(rr,paste0(class(x),"'!"));
        res <- c(res,paste(rr,collapse=""));
        res <- c(res,"OBJECT9: 'x' does not belong to any of these!");
    }
}
#
# proceding for the content
if (identical(res,character(0))) { if (length(con)>0) {
    if (is.numeric(x)) {
        if (length(con) >= 2) {
            cond <- (x >= con[1]) & (x <= con[2]);
            if (!all(cond)) { res <- c(res,"Does not belong to the prescribed interval"); }
        } else {
            erreur(list(typ,con),
                   paste("OBJECT9:",
                         "For this 'typ', non empty 'con'",
                         "must define an interval with at",
                         "least two numerical values"));
        }
    }
    #
    if (is.character(x)) {
        cond <- (x %in% con);
        if (!all(cond)) { res <- c(res,"Does not belong to the prescribed values"); }
    }
}}
#
# checking the specific requirement
if (!is.null(speci)) {
  # for vectors
  if (is.vector(x)) {
    if (speci=="named") {
      if (is.null(names(x))) {
        res <- c(res,"This vector was required to be named and isn't.");
      }
    } else {
      erreur(speci,"For vectors, the only valid value for 'speci' is 'named'");
    }
  }
  # for matrices
  if (is.matrix(x)) {
    if (is.matrix(speci)) {
      if (all(dim(speci)==2)) {
        if (!any(is.na(speci[1,]))) {
          if (abs(interv7belonging(nrow(x),speci[1,]))>1) {
            inter <- paste(speci[1,],collapse="-");
            res <- c(res,paste("The number of rows is not in the specified",inter,"interval"));
          }
        }
        if (!any(is.na(speci[2,]))) {
          if (abs(interv7belonging(ncol(x),speci[2,]))>1) {
            inter <- paste(speci[2,],collapse="-");
            res <- c(res,paste("The number of columns is not in the specified",inter,"interval"));
          }
        }
      } else {
        erreur(speci,"For matrices, the only valid value for speci is a matrix of dimension '2x2'");
      }
    } else {
      erreur(speci,"For matrices, the only valid value for speci is a matrix of dimension '2x2'");
    }
  }
  # for arrays
  if (is.array(x) & !(is.matrix(x))) {
    if (is.matrix(speci)) {
      if (ncol(speci)==2) {
        if (!any(is.na(speci[1,]))) {
          if (abs(interv7belonging(length(dim(x)),speci[1,]))>1) {
            inter <- paste(speci[1,],collapse="-");
            res <- c(res,paste("The number of dimensions is not in the specified",inter,"interval"));
          }
        }
        for (dd in bf(dim(x))) { if (nrow(speci)>dd) {
          if (!any(is.na(speci[dd+1,]))) {
            if (abs(interv7belonging(dim(x)[dd],speci[dd+1,]))>1) {
              res <- c(res,paste("The dimension size number",dd,"is not in the specified",
                                 paste(speci[dd+1,],collapse="-"),"interval"));
            }
          }
        }}
      } else {
        erreur(speci,"For arrays, the only valid value for speci is a matrix with '2' columns");
      }
    } else {
      erreur(speci,"For arrays, the only valid value for speci is a matrix with '2' columns");
    }
  }
}
#
# returning
if (!identical(res,character(0))) {
    if (!is.null(mensaje)) { res <- c(res,mensaje);}
    if (fatal) {
        cat(paste(rep("~",60),collapse=""),"\n");
	cat("From object9: in fact 'x' is '",
	    deparse(substitute(x)),"'\n",sep="");
	print.default(x);
        print(res);
        erreur(NULL,"FATAL ERROR from object9");
    }
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pause <- function(what="",mensaje=NULL,top=0,ans=NULL) 
#TITLE  pauses the program until an answer is given
#DESCRIPTION
# This function issues a pause with a message allowing 
# to stop the process or to continue it (without or with and 
# according to the answer given by the user).
#DETAILS
# The answer provided by the user is interpreted as
#  a \samp{character(1)}.\cr
# When \samp{top} is \samp{NA} the call is ignored.\cr
# When \samp{top} is \samp{NULL} no stopping is possible
#      but the answer is returned. This answer can be
#      anything when \samp{is.na(ans)} but a void answer
#      is refused. This answer can be anything including
#      nothing when \samp{is.null(ans)}. The answer is
#      returned.\cr
# When \samp{top} is \samp{numeric}, \samp{ans} is not considered.
#       an empty answer allows the process to follow
#       and no void answers stop the process.\cr
# When \samp{top} and \samp{ans} are \samp{character}s, 
#      the only accepted answers are a component of \samp{top} (to 
#      stop the process) or a component of \samp{ans} (to be returned).\cr
# When \samp{top} is a \samp{character} and \samp{is.null(ans) | is.na(ans)}
#      stopping is made when the answer is one of the components of
#      \samp{top}, other answers will be returned.\cr
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{what} << Short message commenting the pause>>
#{mensaje} << A possible longer message to give details. When
#          \samp{NULL} a shortened message is issued.>>
#{top} << Precises the rules to follow for the execution to be stopped
#         (see the \samp{details} section).>>
#{ans} << Precises the accepted answers
#         (see the \samp{details} section).>>
#VALUE
# When the process is not stopped, returns the answer provided
# by the user.
#EXAMPLE
# pause("Time for lunch!",top=NA);
# \dontrun{pause("Time for lunch?")}
# \dontrun{pause("Look at your results before the process continue",top=NULL)}
# \dontrun{pause("Can we continue?",top=c("n","N","no","NO"),ans=c("y","Y","yes","YES"))}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_25
#REVISED 14_07_31
#--------------------------------------------
{
  # no action
  if(length(top)==1) { if (is.na(top)) { return(invisible());}}
  # writing the question
  if (!void9(mensaje)) {cat(">>> (",mensaje,")\n");}
  if (!void9(what))  {cat(">>> --------------> ",what,"\n");}
  # precising the possibilities and getting the answer
  if (is.numeric(top)) {
    # raw pause
    cat(">>> 'Enter' to continue | any key(s) +'Enter' to stop \n");
    quoi <- scan(what="character",nmax=1);
    if (length(quoi) != 0) {
      stop("(...YOU decided to stop the job...)",call.=FALSE);
    } else {
      return(invisible());
    }
  } else {
    if (is.null(top)) {
      # no stopping is possible
      if (is.null(ans)) {
        # any answer is possible
        cat(">>> Give your answer and 'Enter' \n");
        quoi <- scan(what="character",nmax=1);
        return(quoi);
      } else {
        if (is.na(ans[1])) {
          # any non void answer is possible
          cat(">>> Give a non-empty answer and 'Enter' \n");
          quoi <- scan(what="character",nmax=1);
          while (length(quoi) == 0) {
            cat("EMPTY ANSWER is not admitted\n");
            quoi <- scan(what="character",nmax=1);
          }
          return(quoi);
        } else {
          # a restricted answer is required
          cat(">>> Give an answer among the possible and 'Enter' \n");
          cat(">>> Possible are: /",paste(ans,collapse="/"),"/\n",sep="");
          quoi <- scan(what="character",nmax=1);
          while((length(quoi)==0) || (!(quoi %in% as.character(ans)))) {
            cat("POSSIBLE ANSWERS are in the following list:\n");
            cat(">>> /",paste(ans,collapse="/"),"/\n",sep="");
            quoi <- scan(what="character",nmax=1);
          }
          return(quoi);
        }
      }     
    } else {
      # possibly stopping
      if (is.null(ans)) {
        # any answer is possible
        cat(">>> Give an answer and 'Enter' \n");
        cat(">>> To stop: /",paste(top,collapse="/"),"/\n",sep="");
        quoi <- scan(what="character",nmax=1);
        if ((length(quoi)>0) && (quoi %in% top)) {
          stop("(...YOU decided to stop the job...)",call.=FALSE);
        } else {
          return(quoi);
        }
      } else {
        if (is.na(ans[1])) {
          # any non void answer is possible
          cat(">>> Give a non empty answer and 'Enter' \n");
          cat(">>> To stop: /",paste(top,collapse="/"),"/\n",sep="");
          quoi <- scan(what="character",nmax=1);
          while (length(quoi) == 0) {
            cat("EMPTY ANSWER is not admitted\n");
            quoi <- scan(what="character",nmax=1);
          }
          if (quoi %in% top) {
            stop("(...YOU decided to stop the job...)",call.=FALSE);
          }
          return(quoi);
        } else {
          # a restricted answer is required
          topans <- union(top,ans);
          if (length(topans)!=(length(ans)+length(top))) {
            stop("Possible answers and stopping answers are not disjoint!");
          }
          cat(">>> Give an answer among the possible and 'Enter' \n");
          cat(">>> To stop: /",paste(top,collapse="/"),"/ ",sep="");
          cat("or: /",paste(ans,collapse="/"),"/\n",sep="");
          quoi <- scan(what="character",nmax=1);
          while((length(quoi)==0) || (!(quoi %in% as.character(topans)))) {
            cat("POSSIBLE ANSWERS are in the following list:\n");
            cat(">>> To stop: /",paste(top,collapse="/"),"/ ",sep="");
            cat("or: /",paste(ans,collapse="/"),"/\n",sep="");
            quoi <- scan(what="character",nmax=1);
          }
          if (quoi %in% top) {
            stop("(...YOU decided to stop the job...)",call.=FALSE);
          }
          return(quoi);
        }
      }
    }
  }
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
now <- function(what="dh",format="red",seps=c("_","@",":"))
#TITLE  returns a character giving the present moment
#DESCRIPTION
# Returns a character giving the present moment
# with different components
#DETAILS
# based on Sys.time R function
#KEYWORDS utilities
#INPUTS
#[INPUTS]
#{what} <<\samp{character(1)} indicating which components to include
#  among:
#  \cr\samp{d} for the day (including year and month),
#  \cr\samp{h} for the hour,
#  \cr\samp{m} for the minute (including hour),
#  \cr\samp{s} for the second (including hour and minutes.>>
#{format} <<\samp{r} or \samp{"red"} for a reduced output.>>
#{seps} <<\samp{character(3)} providing the separations for the
#         different components of the reduced returned output.>>
#VALUE
# a character
#EXAMPLE
# cat("Now is",now(),"\n");
# cat(now("dhms","verbose"),"\n");  
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 14_06_04
#--------------------------------------------
{
# checking
object9(seps,"character",c(1,Inf));
if (length(seps) < 3) { seps <- c(seps,seps,seps);}
# adding the consequences to what
if (belong9("s",what)) { what <- paste0(what,"m");}
if (belong9("m",what)) { what <- paste0(what,"h");}
# getting the moment
mnt <- Sys.time();
maintenant <- as.character(mnt);
# getting each piece of the moment
an <- strsplit(maintenant,"-")[[1]][1];
mois <- strsplit(maintenant,"-")[[1]][2];
jour <-  strsplit(maintenant,"-")[[1]][3];
heure <- strsplit(jour," ")[[1]][2];
jour <- strsplit(jour," ")[[1]][1];
minute <- strsplit(heure,":")[[1]][2];
seconde <- strsplit(heure,":")[[1]][3];
heure <- strsplit(heure,":")[[1]][1];
# building the result
res <- "";
if (format[1]=="r") {
  seps <- rep(seps[1],3);
  format <- "red";
}
if (format[1]=="red") {
    if (belong9("d",what)) {
        res <- paste(res,paste(substr(an,3,100),mois,jour,sep=seps[1]),sep="");
        if (belong9("h",what)) { res <- paste0(res,seps[2]);}
    }
    if (belong9("h",what)) {
        res <- paste0(res,heure);
        if (belong9("m",what)) { res <- paste0(res,seps[3]);}
    }
    if (belong9("m",what)) {
        res <- paste0(res,minute);
        if (belong9("s",what)) { res <- paste0(res,seps[3]);}
    }
    if (belong9("s",what)) {
        res <- paste0(res,seconde);
    }
} else {
    if (belong9("d",what)) {
        res <- paste(res,"le",weekdays(mnt),jour,months(mnt),an);
        if (belong9("h",what)) { res <- paste0(res," `a ");}
    }
    if (belong9("h",what)) {
        res <- paste0(res,heure,"H");
        if (belong9("m",what)) { res <- paste0(res,"");}
    }
    if (belong9("m",what)) {
        res <- paste0(res,minute,"m");
        if (belong9("s",what)) { res <- paste0(res,"");}
    }
    if (belong9("s",what)) {
        res <- paste0(res,seconde,'s');
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pprint <- function(x,fifi="",...)
#TITLE  double print at the screen and into a text file
#DESCRIPTION
# Just doing a double printing (i) on the screen and
# (ii) appending to the file fifi
#DETAILS
#KEYWORDS IO
#INPUTS
#{x} << The object to print.>>
#[INPUTS]
#{fifi} <<  Name of the file, if \samp{""} the print
#         is only done onto the screen.>>
#{\dots} <<additional arguments for the print call.>>
#VALUE
# nothing is returned
#EXAMPLE
# pprint("to see what happen!","toto.txt");
# read.table("toto.txt");
# unlink("toto.txt");
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
# printing onto the screen
print(x,...);
# appending to the file
if (fifi != "") {
    sink(fifi,append=TRUE);
    print(x,...);
    sink();
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
posi9 <- function(which,within,strict=TRUE,repe=TRUE,
                       monitor=rbsa0$monitor$v)
#TITLE  returns the positions of identifiers
#DESCRIPTION
#  Returns the positions of identifiers (an identifier is
#  \samp{character(1)} or \samp{integer(1)}).
# From a reference set of
# identifiers (a vector in fact) returns the positions of some of them.
# If they don't belong to the list, an error is issued
# according to \samp{strict}. If there are repetitions
# an error is issued or not according to \samp{repe}.
#DETAILS
#KEYWORDS helpful
#INPUTS
#{which}    <<Identifiers, their position is looked for.
# Must be a character or a numeric.>>
#{within}    <<The ordered vector of all identifiers.
# Must be a character or a numeric of distinct values.>>
#[INPUTS]
#{strict} <<Must \samp{which} be a subset of \samp{within}.>>
#{repe} <<Are repetitions allowed?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# A numeric vector of the positions. It is named with the identifiers.
# When the identifier does not belong to the list and \samp{strict} is
# FALSE, NA values are returned.
#EXAMPLE
# posi9(10:1,1:10);
# posi9(c("Z","E","P","L","I","N"),LETTERS);
# posi9(c("B","o","F","!"),LETTERS,strict=FALSE);
# posi9(c("B","E","B","E"),LETTERS);
# \dontrun{posi9(c("B","E","B","E"),LETTERS,repe=FALSE);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_09
#REVISED 10_10_12
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    object9(which,   c("character","numeric"),-1,mensaje="   'which' is not valid");
    object9(within,  c("character","numeric"),-1,mensaje="  'within' is not valid");
    object9(strict,c("logical"),            1,mensaje="'strict' is not valid");
}
which  <- as.character(which);
within <- as.character(within);
if (length(unique(within)) < length(within)) {
  erreur(within,"The reference set contains duplicated items");
}
if (strict) {
    uu <- union(which,within);
    if (length(uu)!=length(within)) {
        erreur(list(setdiff(uu,within),within),"Some identifiers does not belong to the list");
    }
}
# computing
res <- as.numeric(outer(which,within,"==")%*%bf(within));
names(res) <- which;
res[res==0] <- NA;
# repetitions
if (!repe) {
    rr <- res[!is.na(res)];
    if (length(unique(rr)) < length(rr)) {
        erreur(rr,"Some repetitions were found while forbidden!");
    }
}  
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
source8file <- function(files,
                        display=1,
                        directory=character(0),...)
#TITLE  sources a set of R files
#DESCRIPTION
# Just sourcing \samp{files} after adding a \samp{directory}
# path and emetting a \samp{message}.
#DETAILS
# The useness of this function is to allow (1) sourcing several files with
# a unique call and (ii) to easily change the directory when
# working in different computers.
#KEYWORDS
#INPUTS
#{files} << The R file(s) to be sourced.>>
#[INPUTS]
#{display} <<If non negative, display before its sourcing, the name of each file,
#            by a call to \samp{form3title} with the \samp{empha} parameter set
#            to its value.>>
#{directory} << The directory path to add before the files.>>
#{\dots} <<Further arguments to be passed to the \samp{source} function.>>
#VALUE
# nothing is returned but sourcing and displaying file names if asked are performed.
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_16
#REVISED 14_08_16
#--------------------------------------------
{
  # looping onto the files
  for (fif in files) {
    # the files to be sourced
      if (void9(directory)) { fifi <- fif;
      } else { fifi <- paste(directory,fif,sep="/");}
      # the message
      if (display>=0) { form3title(fifi,display);}
      # sourcing
      source(fifi,...);
    }
  if (display==0) { cat("\n");}
  # returning
  invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
belong9 <- function(sch,ch,exact=FALSE,how="a",lower=FALSE,
                       monitor=rbsa0$monitor$v)
#TITLE  indicates inclusion of character string
#DESCRIPTION
# Checks if some string(s) belong(s) to a series of strings.
# When \samp{exact} is FALSE, returns TRUE if the 
# character string \samp{sch} is included at least once
# into the character string \samp{ch}.\cr
# \samp{sch} can be a vector, in that case, the check is made for each
# of its components. According to \samp{how} the returned
# value is vectorial or scalar. When \samp{sch} is zero length then
# \samp{TRUE} is returned. When \samp{ch} is zero length
# then \samp{FALSE} is returned except when \samp{length(sch)} is zero.
#DETAILS
# More possibilities exists when \samp{exact} is \samp{6} or \samp{8}.
# Namely that the substring must be at the end or at the beginning of the
# reference chain.
#KEYWORDS utilities
#INPUTS
#{sch} <<The character string(s) to be found.>>
#{ch}  <<The character string(s) to investigate.>>
#[INPUTS]
#{exact} << When exact, one component must
# be strictly identical, if not a subtring is sufficient.
# See also the \bold{Details} section for two other possibilities.>>
#{how} << Indicates what to do when \samp{length(sch)>1}. The choice are 
# \samp{v}: a logical vector gives back each check independently;
# \samp{1}: returns \samp{TRUE} when at least one of the component belongs
# to the series \samp{ch} and \samp{a} when all components must comply to get TRUE.>>
#{lower} << Must the comparisons being done after case lowering?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE A logical vector with length of \samp{sch}; or \samp{TRUE} or \samp{FALSE} when
#      \samp{how} is \samp{1} or \samp{a}.
#EXAMPLE
# belong9('a','non');
# belong9('o',c('non','oui'));
# belong9('o',c('non','oui'),6);
# belong9('o',c('non','oui'),8);
# belong9('O',c("oui"));
# belong9('O',c("oui"),lower=TRUE);
# belong9(c('o','n'),c('non','oui'),6,how='v');
# belong9(c('o','n'),c('non','oui'),8,how='v');
# belong9(c("o","oui"),c('non','oui'));
# belong9(c("o","oui"),c('non','oui'),how="v");
# belong9(c("A[SEX]","A[AGE]"),c("A[AGE]","A[SEX]"),how="a")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_28
#REVISED 14_07_31
#--------------------------------------------
{
  # degenerate cases
  if (length(sch)==0) { return(TRUE);}
  if (length( ch)==0) { return(FALSE);}
  #
  if (monitor$chk$v) {
      object9(  sch,"character",c(1,Inf));
      object9(   ch,"character",c(1,Inf));
      if (is.logical(exact)) {exact <- exact*10;}
      object9(as.character(exact),"character",1,c(0,6,8,10));
      object9(  how,"character",1);
      # because 'object9' calls 'belong9' for this type of check!
      if (!(how %in% c("v","1","a"))) {
          erreur(how,"Not accepted value for 'how'");
      }
    object9(lower,"logical",1);
  }
  if (lower) {
    sch <- tolower(sch);
    ch <- tolower(ch);
  }
  #
  res <- logical(length(sch));
  #
  #
  for (ii in bf(sch)) {
    chaine <- sch[ii];
    if (exact== 6) {
      ch <- substr(ch,pmax(1,nchar(ch)-(nchar(chaine)-1)),nchar(ch));
    }
    if (exact== 8) {
      ch <- substr(ch,1,nchar(chaine));
    }
    if (exact== 0) {
      res[ii] <- (length(grep(chaine,ch,fixed=TRUE)) > 0);
    } else {
      res[ii] <- (sum(chaine==ch) > 0);
    }
  }
  # dealing with a synthetic question
  if (how == "1") { res <- any(res);}
  if (how == "a") { res <- all(res);}
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
filter8text <- function(text,pattern,remove=TRUE,
                         exact=FALSE,how="a",lower=FALSE,
                         monitor=rbsa0$monitor$v)
#TITLE filters components of a character.
#DESCRIPTION
# For each component of a character vector, checks with function 
# \samp{belong9} if it satisfies the conditions given by the other
# arguments, the component is removed (or selected).
#DETAILS
#KEYWORDS utilities
#INPUTS
#{text} << The \samp{character} to be filtered.>>
#{pattern} <<(\samp{character}) the character string(s) to be found
#             in each component.>>
#[INPUTS]
#{remove} << Must the conforming components be removed (or selected)?>>
#{exact} << When exact, one component must
# be strictly identical, if not a subtring is sufficient.>>
#{how} << Indicates what to do when \samp{length(sch)>1}. The choice are 
# \samp{v}: a logical vector gives back each check independently;
# \samp{1}: returns \samp{TRUE} when at least one of the component belongs
# to the series \samp{ch} and \samp{a} when all components must comply to get TRUE.>>
#{lower} << Must the comparisons being done after case lowering?>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE A \samp{character} comprising the non-removed (or selected) components
#      of the initial components.
#EXAMPLE
# filter8text(rbsa0$text2$v,"little",exact=FALSE,lower=TRUE,remove=FALSE)
# filter8text(rbsa0$text2$v,"On",exact=8,lower=TRUE,remove=FALSE)
# filter8text(rbsa0$text2$v,"On",        lower=TRUE,remove=FALSE)
# filter8text(rbsa0$text2$v,"On",exact=8,lower=TRUE,remove=TRUE)
#REFERENCE
#SEE ALSO belong9
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 04_08_03
#REVISED 14_08_03
#--------------------------------------------
{
  # first checking
  if (monitor$chk$v) {
    object9(remove,"logical",1);
    object9(text,"character");
  }
  # degenerate cases
  if (length(pattern)==0) {
    if (remove) {
      return(text);
    } else {
      return(character(0));
    }
  }
  if (length(text)==0) { return(text);}
  # second checking
  if (monitor$chk$v) {
    # other checking are made within 'belong9'
  }
  # getting the conforming components
  coco <- vector("logical",length(text));
  for (ii in bf(text)) {
    coco[ii] <- belong9(pattern,text[ii],
                        exact=exact,how=how,lower=lower,
                        monitor=monitor);
  }
  if (remove) {
    coco <- (!coco);
  }
  # returning
  text[coco];
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
dipa <- function(chemins,sep=.Platform$file.sep)
#TITLE  normalize directory paths
#DESCRIPTION
#  returns the paths contained into \samp{chemins} in
#  a normalized way. No check of existence of the path(s)
#  is performed, for that the function \samp{fidi9(dipa(...))} could be used.
#DETAILS
#  Multiple \samp{sep} are removed as well as any final \samp{sep}
#  with the exception of \samp{sep} being the unique path, that is
#  the root path in Linux operating system.
#KEYWORDS directory
#INPUTS
#{chemins} << \samp{character} supposed to contain directory paths.>>
#[INPUTS]
#{sep} << The separator to use.>>
#VALUE
# a character with same length as \samp{chemins} 
#EXAMPLE
# dipa(c("/","~","~/.bashrc",getwd(),"toto.txt"));
#REFERENCE
#SEE ALSO fidi9
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_23
#REVISED 14_06_25
#--------------------------------------------
{
  # checking the arguments
  object9(chemins,"character");
  object9(sep,"character",1);
  # initializing
  se2 <- paste0(sep,sep); lse <- nchar(sep);
  res <- chemins;
  # proceeding
  for (ii in bf(res)) {
    # removing multiple "sep"
    while (length(grep(se2,res[ii],fixed=TRUE))>0) {
      res[ii] <- gsub(se2,sep,res[ii],fixed=TRUE);
    }
    # dealing with the last character
    if (nchar(res[ii])>lse) {
      laca <- substr(res[ii],nchar(res[ii])-lse+1,nchar(res[ii]));
      if (laca == sep) { res[ii] <- substr(res[ii],1,nchar(res[ii])-lse);}
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
fidi9 <- function(chemins,retour=c("-","f","d"))
#TITLE  checks files and directories
#DESCRIPTION
#  checks if \samp{chemin} is a file or a directory
#DETAILS
#  the distinction between files and directories is
# based on \samp{file.info} function.
#KEYWORDS file directory
#INPUTS
#{chemins} << \samp{character}; for each element of it, the
#       existence as file or directory must be checked.>>
#[INPUTS]
#{retour} << \samp{character(3)} indicating what to return
#            in case of (non existence), (existence of a file)
#            and (existence of a directory).>>
#VALUE
# a character of same length as \samp{chemins} with one of the
# components of \samp{retour}.
#EXAMPLE
# fidi9(c("/","~","~/.bashrc",getwd(),"toto.txt"));
#REFERENCE
#SEE ALSO dipa
#CALLING
#COMMENT
#FUTURE introduce the notion of accessibility
#AUTHOR J.-B. Denis
#CREATED 14_01_22
#REVISED 14_06_25
#--------------------------------------------
{
  # checking the arguments
  object9(chemins,"character");
  object9(retour,"character",3);
  # initializing
  res <- rep(retour[1],length(chemins));
  # proceeding
  for (ii in bf(chemins)) {
    if (nchar(chemins[ii])==0) {
      stop(paste0(" The ",ii,"th path is void!"));
    } else {
      fidi <- chemins[ii];
      if (file.exists(fidi)){
        fidir <- file.info(fidi)$isdir;
        if (!void9(fidir)) {
          res[ii] <- retour[2+fidir];
        }
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
