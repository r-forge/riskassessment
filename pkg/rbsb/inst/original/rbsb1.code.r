
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rbsb3k <- function(whi)
#TITLE  assigns the constants for the rbsb layer
#DESCRIPTION
# defines or returns the constants used within /rbsb00/. 
# The performed action depends on the argument.
#DETAILS
# All constant names start with 'rbsb.'.
# This solution was adopted to replace
# a set of global constants that I had difficulty
# to make acceptable with R packages standards.
# It is recommended not to modify these constants
# unless you are completely aware of the consequences.\cr
# The constants can be any object type.\cr
# Notice that two constants \code{rbsb.null} and \code{rbsb.pko}
#  must be updated by the \code{package3k} function of children packages of /rbsb/.
#PKEYWORDS helpful
#KEYWORDS misc
#INPUTS
#{whi}    <<a character(1) indicating either to reset or
#           to return the names or the current values. The three
#           values are \code{RESET}, \code{reset}, \code{names}, \code{definitions} or \code{values}.>>
# >>
#[INPUTS]
#VALUE
# When \code{whi=="RESET"} nothing (but the assignments are
# performed for all layers: only \code{rbsb} since it is the first one.).\cr
# When \code{whi=="reset"} nothing (but the assignments of 
# the layer \code{rbsb} are performed).\cr
# When \code{whi=="names"} the names as a character vector.\cr
# When \code{whi=="definitions"} the definitions as a named character vector.\cr
# When \code{whi=="values"} the values through a named list.
#EXAMPLE
## First assign the standard values
# rbsb3k("reset");
# print(rbsb.msi);
## to get the short labels
# rbsb3k("names");
## to obtain the current values
# rbsb3k("values");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_09_16
#REVISED 10_10_11
#--------------------------------------------
{
# checking
if (!(whi %in% c("RESET","reset","names","definitions","values"))) {
    print.default(whi);
    stop("rbsb3k does not accept this argument");
}
#
#if (whi=="RESET") { }
#
# definition of the different constants
sc <- character(0);
sc["msi"] <- "rbsb signature";
sc["pko"] <- "list of prefixes of constants (to be increased in each package)";
sc["null"] <- "list of constant of different classes declared as NULL by 'isvide' (to be increased in each package)";
sc["min"] <- "Number of spaces for indentation";
sc["mba"] <- "Must batch be activated (= no pause after displaying a result)?";
sc["mck"] <- "Must systematic checks be done?";
sc["mwa"] <- "Must warning be simple warning?";
sc["mfa"] <- "Must fatal error be fatal?";
sc["mwi"] <- "The width (nbr of characters) for printing paragraphs";
sc["mfi"] <- "Must the results be directed to files (if not to the screen)?";
sc["mgr"] <- "Type of graphics files";
sc["mnd"] <- "Number of decimals when printing";
sc["mep"] <- "When printing an object: *e*m*p*hasize level";
sc["ffg"] <- "Last number of the graphics files";
sc["fpx"] <- "Prefix for the resulting files";
sc["fou"] <- "Standard file for text outputs";
sc["fin"] <- "Standard intermediary file";
sc["tyl"] <- "Types of bullets for standard lists";
sc["l00"] <- "Symbol for the null bullets (see read8list)";
sc["cpt"] <- "Different closing parentheses as a dimnamed matrix";
sc["cni"] <- "character(1) to designate the node under consideration";
sc["cac"] <- "character(1) to indicate something to be computed";
sc["logic"] <- "Named or not logical";
sc["integ"] <- "Named or not integer";
sc["numer"] <- "Named or not numeric";
sc["chara"] <- "Named or not character";
sc["log0"] <- "Null value for logical objects";
sc["nlo0"] <- "Null value for named logical objects";
sc["cha0"] <- "Null value for character objects";
sc["nch0"] <- "Null value for named character objects";
sc["nch1"] <- "Example 1 of a named character objects";
sc["num0"] <- "Null value for numeric objects";
sc["nnu0"] <- "Null value for named numeric objects";
sc["nnu1"] <- "Example 1 of a named numeric objects";
sc["lis0"] <- "Null value for list objects";
sc["lis1"] <- "Example1 of a list objects";
sc["fun0"] <- "Null value for function objects";
sc["dfr0"] <- "Null value for data.frame objects";
sc["dfr1"] <- "Example1 of data.frame object";
sc["dfr2"] <- "Example2 of data.frame object";
sc["dfr3"] <- "Example3 of data.frame object";
sc["ion0"] <- "null /ion/"; 
sc["ion1"] <- "exemple 1 of /ion/"; 
sc["ion2"] <- "exemple 2 of /ion/"; 
sc["ion3"] <- "exemple 3 of /ion/"; 
sc["all"] <- "Symbol defining all the variates from a node"; 
sc["who"] <- "all surrounde = WHOle"; 
sc["des0"] <- "Null value for des objects";
sc["des1"] <- "Example 1 of a des object";
sc["fau0"] <- "Null value for faux objects";
sc["fau1"] <- "Example 1 of faux object";
sc["daf0"] <- "Null value for daf objects";
sc["daf1"] <- "Example /daf/ associated to 'rbsb.dfr1'";
sc["daf2"] <- "Example /daf/ associated to 'rbsb.dfr2'";
sc["daf3"] <- "Example /daf/ associated to 'rbsb.dfr3'";
sc["smn"] <- "Minimum number of observations to compute statistics";
sc["sna"] <- "The different natures for random variates";
sc["spr"] <- "The different properties for random variates";
sc["snp"] <- "Properties of the different natures of random variates as a dimnamed matrix";
sc["vma"] <- "Different types of vma (see char2vma for details)";
sc["sep0"] <- "Basic string separator";
sc["sep1"] <- "Second string separator";
sc["sep2"] <- "Third string separator";
sc["tag1"] <- "Standard tagging for text file associated to list";
sc["tag2"] <- "Alternative tagging for text file associated to list";
sc["nom0"] <- "null /nom/"; 
sc["nom1"] <- "Example 1 of /nom/"; 
sc["nom2"] <- "Example 2 of /nom/"; 
sc["nom3"] <- "Example 3 of /nom/"; 
sc["nom4"] <- "Example 4 of /nom/"; 
sc["nom5"] <- "Example 5 of /nom/"; 
sc["nom6"] <- "Example 6 of /nom/"; 
sc["nom7"] <- "Example 7 of /nom/"; 
sc["epi1"] <- "Example 1 of /empirical/"; 
sc["epi2"] <- "Example 2 of /empirical/"; 
sc["epi3"] <- "Example 3 of /empirical/"; 
sc["epi4"] <- "Example 4 of /empirical/"; 
sc["epi5"] <- "Example 5 of /empirical/"; 
sc["epi6"] <- "Example 6 of /empirical/"; 
sc["epi7"] <- "Example 7 of /empirical/"; 
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
        eee <- paste("res[[\"",noco,"\"]] <- rbsb.",noco,";",sep="");
        eval(parse(text=eee));
    }
    return(res);
}
#
# loading the standard values
#
if (tolower(whi)=="reset") {
#
    assign("rbsb.msi","/rbsb/",pos=".GlobalEnv");
    assign("rbsb.pko","rbsb",pos=".GlobalEnv");
    assign("rbsb.null",
           c("rbsb.num0","rbsb.nnu0",
             "rbsb.cha0","rbsb.nch0",
             "rbsb.log0","rbsb.nlo0",
             "rbsb.lis0","rbsb.fun0","rbsb.dfr0",
             "rbsb.ion0","rbsb.nom0",
             "rbsb.fau0","rbsb.des0","rbsb.daf0"
            ),
           pos=".GlobalEnv");
    assign("rbsb.min",3,pos=".GlobalEnv");
    assign("rbsb.mba", TRUE,pos=".GlobalEnv");
    assign("rbsb.mfa", TRUE,pos=".GlobalEnv");
    assign("rbsb.mck", TRUE,pos=".GlobalEnv");
    assign("rbsb.mwa", TRUE,pos=".GlobalEnv");
    assign("rbsb.mwi",  70,pos=".GlobalEnv");
    assign("rbsb.mfi",TRUE,pos=".GlobalEnv");
    assign("rbsb.mgr","pdf",pos=".GlobalEnv");
    assign("rbsb.mnd",3,pos=".GlobalEnv");
    assign("rbsb.mep",1,pos=".GlobalEnv");
    assign("rbsb.ffg",0,pos=".GlobalEnv");
    assign("rbsb.fpx",paste("rbsb",format(Sys.time(), "%y_%m_%d"),sep="."),pos=".GlobalEnv");
    assign("rbsb.fou",paste(rbsb.fpx,"txt",sep="."),pos=".GlobalEnv");
    assign("rbsb.fin",paste(rbsb.fpx,"int.txt",sep="."),pos=".GlobalEnv");
    assign("rbsb.tyl",c("A","a","n"),pos=".GlobalEnv");
    assign("rbsb.l00","~",pos=".GlobalEnv");
    # due to the use of easyp3cut, no nesting parenthesis are allowed!
    # also opening and closing must be different
    assign("rbsb.cpt", matrix(c("{{","}}",
                                                     "(|","|)",
                                                     "[" ,"]" ,
                                                     "<<",">>"),ncol=2,
                                  byrow=TRUE,dimnames=list(c("nodes","rounding","variables","vectors"),c("opening","closing"))),pos=".GlobalEnv");
    assign("rbsb.cni", "*Y*",pos=".GlobalEnv");
    assign("rbsb.cac", "acalculer",pos=".GlobalEnv");
    assign("rbsb.logic", c("logical","nlogical"),pos=".GlobalEnv");
    assign("rbsb.integ", c("integer","ninteger"),pos=".GlobalEnv");
    assign("rbsb.numer", c("numeric","nnumeric"),pos=".GlobalEnv");
    assign("rbsb.chara", c("character","ncharacter"),pos=".GlobalEnv");
    assign("rbsb.log0", logical(0),pos=".GlobalEnv");
    assign("rbsb.cha0", character(0),pos=".GlobalEnv");
    assign("rbsb.nch0", structure(character(0), .Names = character(0)),pos=".GlobalEnv");
    assign("rbsb.nch1", structure("1", .Names = "A"),pos=".GlobalEnv");
    assign("rbsb.num0", numeric(0),pos=".GlobalEnv");
    assign("rbsb.nnu0", structure(numeric(0), .Names = character(0)),pos=".GlobalEnv");
    assign("rbsb.nlo0", structure(logical(0), .Names = character(0)),pos=".GlobalEnv");
    assign("rbsb.nnu1", structure(1, .Names = "A"),pos=".GlobalEnv");
    assign("rbsb.lis0",    vector("list",0),pos=".GlobalEnv");
    assign("rbsb.lis1", list(A=1:3,
                            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
                            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2))))),
                        pos=".GlobalEnv");
    assign("rbsb.fun0", function(...){invisible()},pos=".GlobalEnv");
    #
    assign("rbsb.dfr0", as.data.frame(matrix(0,0,0)),pos=".GlobalEnv");
    assign("rbsb.dfr1", data.frame(F1=factor(rep(1:3,each=4)),F2=factor(rep(1:4,3))),pos=".GlobalEnv");
    assign("rbsb.dfr2", data.frame(SEX=factor(c("M","M","F","F","F","M")),
                                   AGE=20+5*(1:6),
                                   HGT=185-1:6),
           pos=".GlobalEnv");
    assign("rbsb.dfr3", data.frame(SEX=factor(c(rep("M",50),rep("F",50))),
                                   AGE=round(40+sin(1:100)*20),
                                   HGT=c(round(170+cos(1:50)*17),round(155+cos(1:50)*16)),
                                   WGT=c(round(75+cos(50:1)*15),round(60+cos(50:1)*20))),
           pos=".GlobalEnv");
    #
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
    rbsb.all <- "-";
    rbsb.who <- paste(rbsb.cpt["variables","opening"],
                           rbsb.all,
                           rbsb.cpt["variables","closing"],
                           sep="");
    assign("rbsb.all",rbsb.all,pos=".GlobalEnv");
    assign("rbsb.who",rbsb.who,pos=".GlobalEnv");
    #
    assign("rbsb.des0", new("des"),pos=".GlobalEnv");
    assign("rbsb.des1", new("des",name="des1",orig="/rbsb/",time=now(),defi="simple des",role="illustration",comm=c("Nothing more","to add")
                           ),pos=".GlobalEnv");
    assign("rbsb.fau0", new("faux"),pos=".GlobalEnv");
    assign("rbsb.fau1", new("faux",orig="ici",defi="Fatal Error",comm=c("In fact it was a joke","No error occurred!","You must be relieved")),pos=".GlobalEnv");
    assign("rbsb.smn", 30,pos=".GlobalEnv");
    assign("rbsb.sna",c("conti","integ","cateo","categ","unkno"),pos=".GlobalEnv");
    assign("rbsb.spr",c("categoric","ordered","numeric"),pos=".GlobalEnv");
    assign("rbsb.snp", 
          matrix(c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE),5,3,dimnames=list(c("conti","integ","cateo","categ","unkno"),c("categoric","ordered","numeric"))
             ),pos=".GlobalEnv");
    assign("rbsb.daf0",new("daf",des=new("des"),
                                 what="d",valu="rbsb.dfr0"),pos=".GlobalEnv");
    assign("rbsb.daf1",new("daf",des=new("des",name="daf1"),
                                 what="d",valu="rbsb.dfr1"),pos=".GlobalEnv");
    assign("rbsb.daf2",new("daf",des=new("des",name="daf2"),
                                 what="d",valu="rbsb.dfr2"),pos=".GlobalEnv");
    assign("rbsb.daf3",new("daf",des=new("des",name="daf3"),
                                 what="d",valu="rbsb.dfr3"),pos=".GlobalEnv");
    vma <- c("c","C","v","V","u","U","m","n","o","p","M","N","O","P","a","A","b","B");
    names(vma) <- vma;
    assign("rbsb.vma",vma,pos=".GlobalEnv");
    assign("rbsb.sep0"," ",pos=".GlobalEnv");
    assign("rbsb.sep1","//",pos=".GlobalEnv");
    assign("rbsb.sep2",";",pos=".GlobalEnv");
    assign("rbsb.tag1",matrix(c("<<",">>",
                               "[[","]]",
                               "((","))"),
                             ncol=2,byrow=TRUE),pos=".GlobalEnv");
    assign("rbsb.tag2",matrix(c("[[","]]",
                               "((","))"),
                             ncol=2,byrow=TRUE),pos=".GlobalEnv");
    #
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
#    
#
    empi1 <- new("empirical",des=new("des",name="empi1"),
                         sup=c(1,5),repa=c(1,0),
                         xw=matrix(c(1:5,c(1,2,0.5,0.5,1)),5)
            );
    assign("rbsb.epi1",empi1,pos=".GlobalEnv");
    empi2 <- new("empirical",des=new("des",name="empi2"),
                         sup=c(0,10),repa=c(0,1),
                         xy=matrix(c(1:8,c(0,1,10,2,2,4,4,0)),8)
            );
    assign("rbsb.epi2",empi2,pos=".GlobalEnv");
    empi3 <- new("empirical",des=new("des",name="empi3"),
                         sup=c(1,9),repa=c(1,1),
                         xw=matrix(c(1:5,c(1,2,0.5,0.5,1)),5),
                         xy=matrix(c(1:8,c(0,1,10,2,2,4,4,0)),8)
            );
    assign("rbsb.epi3",empi3,pos=".GlobalEnv");
    empi4 <- new("empirical",des=new("des",name="empi4"),
                         sup=c(0,5),repa=c(1,0),
                         xw=matrix(c(c(1,1,3,5,5),c(1,2,0.5,0.5,1)),5)
            );
    assign("rbsb.epi4",empi4,pos=".GlobalEnv");
    empi5 <- new("empirical",des=new("des",name="empi5"),
                         sup=c(0,10),repa=c(1,3),
                         xw=matrix(c(c(0:3,4.5,5,8),
                                   c(1,2,1,2,2,1,1)),ncol=2),
                         xy=matrix(c(c(1,2,4:7,9,9),
                                   c(0,4,6,0,0,2,2,0)),ncol=2)
            );
    assign("rbsb.epi5",empi5,pos=".GlobalEnv");
    empi6 <- new("empirical",des=new("des",name="empi6"),
                         sup=c(0,2),repa=c(1,0),
                         xw=matrix(c(1,1),ncol=2),
                         xy=matrix(NA,0,ncol=2)
            );
    assign("rbsb.epi6",empi6,pos=".GlobalEnv");
    empi7 <- new("empirical",des=new("des",name="empi7"),
                         sup=c(0,3),repa=c(0,1),
                         xw=matrix(NA,0,ncol=2),
                         xy=matrix(c(0,1,1,0,2,0),3,ncol=2)
            );
    assign("rbsb.epi7",empi7,pos=".GlobalEnv");
}
#
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bc <- function(nb)
#TITLE  sequence for a loop
#DESCRIPTION
# This function returns \code{1:nb} when \code{nb > 0} and
#         \code{numeric(0)} otherwise.\cr
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
#PKEYWORDS helpful
#KEYWORDS iteration
#INPUTS
#{nb}    <<length of the loop>>
#[INPUTS]
#VALUE
# \code{1:nb} if \code{nb > 0}
# else \code{numeric(0)}.
#EXAMPLE
# bc(0);
# bc(5);
#REFERENCE
#SEE ALSO bf
#CALLING
#COMMENT
# bc for BouCle (loop in French)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_18
#REVISED 09_04_05
#--------------------------------------------
{
if (is.null(nb)) {return(numeric(0));}
if (length(nb)!=1) {
    erreur(nb,"bc deals only with scalar nb");
}
if (nb > 0) {return(1:max(1,round(nb)));}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bd <- function(n1,n2)
#TITLE  sequence for insertions
#DESCRIPTION
# This function returns \code{n1:n2} when \code{n1<=n2} and
#         \code{numeric(0)} otherwise.\cr
# Quite useful when some insertion must be done within
# a sequence
#DETAILS
#PKEYWORDS helpful
#KEYWORDS iteration
#INPUTS
#{n1}    <<first element>>
#{n2}    <<second element>>
#[INPUTS]
#VALUE
# \code{n1:n2} if \code{n1<n2}
# else \code{numeric(0)}.
#EXAMPLE
# xx <- 1:5;
# for (ii in 1:6) { print(c(xx[bd(1,ii-1)],10,xx[bd(ii,5)]));}
#REFERENCE
#SEE ALSO bf
#CALLING
#COMMENT
# bc for BouCle (loop in French)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_12
#REVISED 11_01_12
#--------------------------------------------
{
if (n1 <= n2) {return(n1:n2);}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bf <- function(x)
#TITLE  sequence for looping over an object
#DESCRIPTION
# This function returns \code{1:length(x)} when \code{length(x) > 0} and
#         \code{numeric(0)} otherwise.
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
#PKEYWORDS helpful
#KEYWORDS iteration
#INPUTS
#{x}    <<vector>>
#[INPUTS]
#VALUE
# \code{1:length(x)} if \code{length(x) > 0}
# else \code{numeric(0)}.
#EXAMPLE
# bf(0);
# bf(5);
# bf(character(0));
# bf(letters);
#REFERENCE
#SEE ALSO bc
#CALLING
#COMMENT
# bf for Boucle For the elements of an object
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_05
#REVISED 08_09_05
#--------------------------------------------
{
if (length(x) > 0) { return(1:length(x));}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3liste <- function(vcara,none="-",
                       OPA="{",CPA="}",
                       opa="(",cpa=")",sep="+",
                       imp=FALSE,cr=imp)
#TITLE  formats a series of names
# or whatever
#DESCRIPTION returns a scalar character of the names
#  surrounded by any kind of parenthesis and
#  separated with the same separator.
#DETAILS
#PKEYWORDS format
#KEYWORDS print
#INPUTS
#{vcara}<<Character vector to be considered.>>
#[INPUTS]
#{none}<< The internal result if \code{vcara} is length zero.
#         When it is \code{character(0)} then the 
#         global parenthesis are not given contrary
#         when it is \code{""}.>>
#{OPA}<< The opening parenthesis to surround the entire list.>>
#{CPA}<< The closing parenthesis to surround the entire list.>>
#{opa}<< The opening parenthesis to surround each name.>>
#{cpa}<< The closing parenthesis to surround each name.>>
#{sep}<< The symbol to separate each name.>>
#{imp}<< Must the result be printed (with cat) or returned?>>
#{cr}<< Must a line feed be added?>>
#VALUE
# A character string or nothing when imp is TRUE
#EXAMPLE
# form3liste(letters[1:4])
#REFERENCE
#SEE ALSO form3etsil
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_06_25
#REVISED 08_10_30
#--------------------------------------------
{
vcara <- as.character(vcara);
vide <- (length(vcara) == 0);
if (length(vcara) == 1) { if (vcara == "") {
    vide <- TRUE;
}}
if (vide) {
    res <- none;
    if (length(none)==0) {
        OPA <- CPA <- "";
    }
} else {
    for (hd in bf(vcara)) {
        hdc <- paste(opa,vcara[hd],cpa,sep="");
        if (hd == 1) { res <- hdc;
        } else { res <- paste(res,sep,hdc,sep="");}
    }
}
res <- paste(OPA,res,CPA,sep="");
if (cr) { res <- paste(res,"\n",sep="");}
if(!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3etsil <- function(cara,none="-.-",
                       OPA="{",CPA="}",
                       opa="(",cpa=")",sep="+")
#TITLE  inverse function of form3liste
#DESCRIPTION returns a vector character of the names
#  from a character string generated by \code{form3liste}. For
# the moment, sep cannot be an empty string.\cr
#DETAILS
# Of course, it is implicitely supposed that the
# inversion is unambiguous.\cr
# The syntax (consistent use of the parentheses and 
# separators) is checked: an error is issued when
# not correct.\cr
# Important: the possible \\n added by \code{form3liste}
# is not taken into account and must be removed
# before calling this function.
#PKEYWORDS format
#KEYWORDS print
#INPUTS
#{cara}<<Character to be considered.>>
#[INPUTS]
#{none}<< idem as in form3liste.>>
#{OPA}<< idem as in form3liste.>>
#{CPA}<< idem as in form3liste.>>
#{opa}<< idem as in form3liste.>>
#{cpa}<< idem as in form3liste.>>
#{sep}<< idem as in form3liste.>>
#VALUE
# A character vector
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# uu <- form3liste(letters[1:4]);
# form3etsil(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_30
#REVISED 08_10_30
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(cara,rbsb.chara,1);
}
# removing the external braces
n1 <- nchar(OPA); n2 <- nchar(CPA);
if (n1 > 0) {
    ax <- substr(cara,1,n1);
    if (ax!=OPA) {
        erreur(c(OPA,ax),"Non consistent OPA and cara arguments.");
    }
    cara <- substr(cara,1+n1,nchar(cara));
}
if (n2 > 0) {
    nn <- nchar(cara);
    ax <- substr(cara,nn-n2+1,nn);
    if (ax!=CPA) {
        erreur(c(CPA,ax),"Non consistent OPA and cara arguments.");
    }
    cara <- substr(cara,1,nn-n2);
}
# removing the separator and constituting the vector
if (cara==none) { res <- character(0);
} else {
    # splitting
    if (nchar(sep)==0) {
        erreur(cara,"Sorry by the function does not act properly for empty separators");
    }
    res <- strsplit(cara,sep,fixed=TRUE)[[1]];
    # removing the internal braces
    n1 <- nchar(opa); n2 <- nchar(cpa);
    for (hd in bf(res)) {
        rr <- res[hd];
        if (n1 > 0) {
            ax <- substr(rr,1,n1);
            if (ax!=opa) {
                erreur(c(opa,ax),"Non consistent opa and cara arguments.");
            }
            rr <- substr(rr,1+n1,nchar(rr));
        }
        if (n2 > 0) {
            nn <- nchar(rr);
            ax <- substr(rr,nn-n2+1,nn);
            if (ax!=cpa) {
                erreur(c(cpa,ax),"Non consistent cpa and cara arguments.");
            }
            rr <- substr(rr,1,nn-n2);
        }
        res[hd] <- rr;
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expr3extrait <- function(cara,opa="(",cpa=")")
#TITLE  extracts the contents of parentheses from a character
#DESCRIPTION returns a character vector with the 
#  contents of successive parentheses. Parentheses and text outside
#  parentheses are eliminated.
#DETAILS
# Parenthesis are defined with 'opa' and 'cpa' arguments
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara}<<\code{character(1)} to be considered.>>
#[INPUTS]
#{opa}<< opening tag (\code{character(1)}).>>
#{cpa}<< closing tag (\code{character(1)}).>>
#VALUE
# A character vector or nothing when there is no parentheses
#EXAMPLE
# rbsb3k("reset"); # for R checking convenience
# expr3extrait('avant (8h) ce n_est pas l_heure, plus tard que (9h) non plus')
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_17
#REVISED 10_08_12
#--------------------------------------------
{
# preparing
res <- character(0);
i.cara <- cara;
# degenerate case
if (length(cara)==0) { return(res);}
# checking
if (rbsb.mck) {
    check4tyle(cara,rbsb.chara,1);
    check4tyle( opa,rbsb.chara,1);
    check4tyle( cpa,rbsb.chara,1);
}
# extracting
essaie <- TRUE;
jbd <- 0;
while (essaie) {
    inu <- strsplit(cara,opa,fixed=TRUE)[[1]][1];
    # this double check about inu is strange
    if (is.na(inu)) { essaie <- FALSE;
    } else {
      if (inu==cara) { essaie <- FALSE;
      } else {
          cara <- form3decadre(cara,paste(inu,opa,sep=""),"");
          uti <- strsplit(cara,cpa,fixed=TRUE)[[1]][1];
          if (is.na(uti)) {
              erreur(list(i.cara,opa,cpa),"Non accepted case: wrong syntax of your input? Most common's a bad stopping tag");
          }
          if (uti==cara) { essaie <- FALSE;
          } else {
              jbd <- jbd + 1;
              res[jbd] <- uti;
              cara <- form3decadre(cara,paste(uti,cpa,sep=""),"");
          }
      }
    }  
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
expr3present <- function(sch,ch,exact=FALSE,how="a")
#TITLE  indicates inclusion of character string
#DESCRIPTION
# Checks if some string(s) belong(s) to a series of strings.
# When \code{exact} is FALSE, returns TRUE if the 
# character string \code{sch} is included at least once
# into the character string \code{ch}.\cr
# \code{sch} can be a vector, in that case, the check is made for each
# of its components. According to \code{how} the returned
# value is vectorial or scalar. When \code{sch} is zero length then
# \code{TRUE} is returned but if not and \code{ch} is zero length
# then \code{FALSE} is returned.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{sch} <<(\code{character}) the character string(s) to be found.>>
#{ch}  <<(\code{character}) the character string(s) to investigate.>>
#[INPUTS]
#{exact} << When exact, one component must
# be strictly identical, if not a subtring is sufficient.>>
#{how} << Indicates what to do when \code{length(sch)>1}. The choice are 
# \code{'v'}: a logical vector gives back each check independently;
# \code{'1'}: returns \code{TRUE} when at least one of the component belongs
# to the series \code{ch} and \code{'a'} when all components must comply to get TRUE.>>
#VALUE A logical vector with length of \code{sch}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# expr3present('a','non');
# expr3present('o',c('non','oui'));
# expr3present(c("o","oui"),c('non','oui'));
# expr3present(c("o","oui"),c('non','oui'),how="v");
# expr3present(c("A[SEX]","A[AGE]"),c("A[AGE]","A[SEX]"),how="a")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_28
#REVISED 10_08_12
#--------------------------------------------
{
# checking is not conditionned with rbsb.mck
# since this function is called by rbsb3k which
# defines this constant!
#
# degenerate cases
if (length(sch)==0) { return(TRUE);}
if (length( ch)==0) { return(FALSE);}
#
if (rbsb.mck) {
    check4tyle(  sch,rbsb.chara,c(1,Inf));
    check4tyle(   ch,rbsb.chara,c(1,Inf));
    check4tyle(exact,"logical",1);
    check4tyle(  how,rbsb.chara,1);
    if (!(how %in% c("v","1","a"))) {
        erreur(how,"Not accepted value for 'how'");
    }
}
#
res <- logical(length(sch));
#
for (ii in bf(sch)) {
    if (exact) {
        res[ii] <- sum(sch[ii]==ch) > 0;
    } else {
        res <- length(grep(sch[ii],ch)) > 0;
    }
}
# dealing with scalar cases
if (how == "1") { res <- any(res);}
if (how == "a") { res <- all(res);}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3justifie <- function(chaine,
                          nbc=8,
                          format=3,
                          tronc=TRUE,
                          carac=" ")
#TITLE  formats a character string
#DESCRIPTION
# Formats character string(s).
# The main use of this function is to produce
# aligned columns lists while printing
# the rows not at the same time.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{chaine}<<the character string to be printed, can be a vector.>>
#[INPUTS]
#{nbc} << Desired number of characters for the result; when
#         \code{chain} is a vector can be a vector of the same length>>
#{format} << Indicates the type of alignment:\cr
#   0 no aligment (no supplementary character added to reach \code{nbc})\cr
#   1 to the left side\cr
#   2 centered\cr
#   3 to the right side>>
#{tronc} << If true, no more than
#     \code{nbc} characters are returned and
# possibly the string is truncated. In that
# case, \code{$} is added for indicating the fact.>>
#{carac} << Character to use for enlarging the string>>
#VALUE a character string
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3justifie("vers")
# form3justifie("versification",5)
# form3justifie(letters[1:5],format=2,carac="+");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 1999_05_25
#REVISED   10_07_22
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(chaine,c(rbsb.chara,"numeric"),-1);
    check4tyle(nbc,"integer",c(1,Inf));
}
# the null case
if (length(chaine)==0) { return(rbsb.cha0);}
# preparing
nbc[nbc < 3] <- 8;
if (length(nbc) < length(chaine)) {
    nnbc <- rep(nbc,length(chaine));
} else {
    nnbc <- nbc;
}
#
itr <- "$"; # truncation indicator
rres <- cchaine <- chaine;
for (rr in bf(rres)) {
    res <- rres[rr];
    nbc <- nnbc[rr];
    chaine <- cchaine[rr];
    # truncation
    if ( (nchar(res) > nbc) & tronc ) {
     if (format <= 1) {
      res <- substring(chaine,1,nbc);
      res <- paste(res,itr,sep="");
      }
     else {
      if (format == 2) {
       otg <- (nchar(chaine) - nbc) %/% 2;
       res <- substring(chaine,1+otg);
       res <- substring(res,1,nbc);
       res <- paste(itr,res,itr,sep="");
       }
      else {
       res <- substring(chaine,1+nchar(chaine)-nbc,
			nchar(chaine));
       res <- paste(itr,res,sep="");
       }
      }
     }
    if ((nchar(res) < nbc) & (format != 0)) {
     if (format == 1) {
      while (nchar(res) < nbc) res <-
	     paste(res,"",collapse="",sep=carac);
      }
     else {
      if (format == 2) {
       raj <- (nbc - nchar(res)) %/% 2;
       if (raj > 0) {
	for (jbd in 1:raj) res <-
	 paste(res,"",collapse="",sep=carac);
	}
       while (nchar(res) < nbc) {
	res <- paste("",res,collapse="",sep=carac);
	}
       }
      else {
       while (nchar(res) < nbc) {
	res <- paste("",res,collapse="",sep=carac);
	}
       }
      }
     }
    rres[rr] <- res;
}
# returning
rres;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3repeat <- function(cha="-",nb=10,imp=FALSE,cr=imp)
#TITLE  prints a repeated given string
#DESCRIPTION
# Without adding breaking line characters, prints
# \code{nb} times a given string of characters
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#[INPUTS]
#{cha} << The string to repeat>> 
#{nb} << Number of repetitions>> 
#{imp} << Printing when TRUE or returning (default)>>
#{cr} << Must a line feed be added?>>
#VALUE
# character string or printing according to \code{imp}
#EXAMPLE
# form3repeat('-+',20,TRUE)
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 08_09_25
#--------------------------------------------
{
nb <- max(0,round(nb));
res <- "";
for (jbd in bc(nb)) { res <- paste(res,cha,sep="");}
if (cr) { res <- paste(res,"\n",sep="");}
if(!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3encadre <- function(chaine,bef="(*)_",aft="",imp=FALSE,cr=imp)
#TITLE  surrounds a character string 
#DESCRIPTION
# Adds _bef_ore and _aft_er some characters to a character string 
# (the same for all components when it is a vector).
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{chaine} <<The character string to frame; can be a vector.>>
#[INPUTS]
#{bef} << What to add before>> 
#{aft} << What to add after>> 
#{imp} << Printing when TRUE or returning>>
#{cr} << Must a line feed be added?>>
#VALUE
# character string or printing according to \code{imp}.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3encadre('IMPORTANT','<<< ',' >>>');
# form3encadre('IMPORTANT','<<< ',' >>>',imp=TRUE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_21
#REVISED 10_06_16
#--------------------------------------------
{
if (rbsb.mck) {
    check4tyle(chaine,rbsb.chara,-1,message="form3encadre: 'chaine' must be a character");
    check4tyle(bef,rbsb.chara,1,message="form3encadre: 'bef' must be a character(1)");
    check4tyle(aft,rbsb.chara,1,message="form3encadre: 'aft' must be a character(1)");
    check4tyle(imp,"logical",1,message="form3encadre: 'imp' must be a logical(1)");
    check4tyle( cr,"logical",1,message="form3encadre: 'cr' must be a logical(1)");
}
if (length(chaine)>0) {
    res <- paste(bef,chaine,aft,sep="");
    if (cr) { res <- paste(res,"\n",sep="");}
} else { res <- character(0);}
if(!imp) { return(res);}
else {cat(res);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3decadre <- function(chaine,bef=rbsb.sep0,aft=bef,mxm=Inf)
#TITLE  removes framing characters from a character string
#DESCRIPTION
# removes \code{bef}s before and \code{aft}s after a character string.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{chaine} <<The character string to refine. 
#           Can be a vector.>>
#[INPUTS]
#{bef} << What to repeatedly remove before.>> 
#{aft} << What to repeatedly remove after.>>
#{mxm} << Maximum number of tags to remove.>>
#VALUE
# character string after removing
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3decadre('IMPORTANT','IM',' ANT');
# form3decadre(c('   OUF ',' FOU ',''),' ','',1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 10_06_16
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(chaine,rbsb.chara,-1,message="form3decadre: 'chaine' must be a character");
    check4tyle(bef,rbsb.chara,1,message="form3decadre: Vector are not accepted for 'bef'");
    check4tyle(aft,rbsb.chara,1,message="form3decadre: Vector are not accepted for 'aft'");
    check4tyle(mxm,"numeric",1,message="form3decadre: mxm must be numeric(1)");
}
# null case
if (length(chaine) == 0) { return(chaine);}
lb <- nchar(bef);
la <- nchar(aft);
for (ich in bf(chaine)) {
    cha <- chaine[ich];
    # removing at the beginning of the string
    if (lb>0) {
	nbr <- 0;
	repeat {
	    deb <- substr(cha,1,lb);
	    if ((deb == bef) & (nbr < mxm)) {
		cha <- substring(cha,lb+1);
		nbr <- nbr+1;
	    } else { break;}
	}
    }
    # removing at the end of the string
    if (la>0) {
	nbr <- 0;
	repeat {
	    lc <- nchar(cha);
	    fin <- substr(cha,lc-la+1,lc);
	    if ((fin == aft) & (nbr < mxm)) {
		cha <- substring(cha,1,lc-la);
		nbr <- nbr+1;
	    } else { break;}
	}
    }
    chaine[ich] <- cha;
}
# returning
chaine;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3ind <- function(niv=1,cr=TRUE,ele=" ")
#TITLE  provides indentation of different levels
#DESCRIPTION
# returns a character to be used as indentation.
# The level of indentation is given by \code{niv*rbsb.min} times
# the character \code{ele}. When \code{cr}, a line feed is made at first.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#[INPUTS]
#{niv} << Level of indentation.>>
#{cr} << Must a line feed provided first?>>
#{ele} << String to repeat.>>
#VALUE
# a scalar string
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# cat(form3ind(2),"Bien `a vous","\n");
# cat(form3ind(3),"Bien `a vous","\n");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_10
#REVISED 08_09_10
#--------------------------------------------
{
niv <- round(max(min(8,niv),0));
res <- "";
if (cr) { res <- paste(res,"\n",sep="");}
res <- paste(res,form3repeat(ele,niv*rbsb.min),sep="");
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3names <- function(nbn,nom=character(0),prefix="",
                           upca=TRUE,nume=14)
#TITLE  provides systematic names for items
#DESCRIPTION
# Provides systematic names for a series of items according their 
# number taking care of previous names.
#DETAILS (see the code)
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{nbn} <<Number of new item names to generate>>
#[INPUTS]
#{nom} << Already present names (to avoid identical names).>>
#{prefix} << Systematic prefix of all names to generate. Must
#                 comprise the dot, if one wants such a separator
#                 between it and the specific part of the name. 
#                 Of course can be 'underscore' or whatever else.>>
#{upca} << Indicates whether the letters constituting the new
#          names must be uppercase or not.>>
#{nume} << Its absolute value gives the number of the letter to use
#          when the alphabet is not sufficient. When negative, alphabet
#          is not considered as a first possibility.>>
#VALUE
# vector with \code{nbn} different strings associated to new names
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3names(2);
# form3names(2,nume=-3);
# form3names(2,prefix="rbsb.");
# form3names(2,upca=FALSE);
# form3names(5,"D");
# form3names(5,"Y");
# form3names(30);
#REFERENCE
#SEE ALSO form3nume
#CALLING
#COMMENT
#FUTURE
# Monitor the number of digits to obtain series of "Z01", "Z02",... ,"Z79", "Z80".
#AUTHOR J.-B. Denis
#CREATED 07_10_19
#REVISED 10_02_15
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(upca,"logical",1,message="Argument 'upca' not accepted");
    check4tyle(nume,"integer",1,message="Argument 'nume' not accepted");
    if ((abs(nume)<1) | (abs(nume)>26)) {
        erreur(nume,"'nume' must be comprised between 1 and 26 to indicate a Letter");
    }
}
#
if (upca) { Letters <- LETTERS;
} else { Letters <- letters;}
#
if (isempty(nbn)) { return(character(0));}
if (nbn < 1) { return(character(0));
} else {
    if (prefix != "") {
        # keeping only the names having got the prefix then
        # removing the prefixes from them (to be added further)
        decom <- sapply(strsplit(nom,prefix),function(ll){length(ll);});
        nom <- nom[decom == 2];
        nom <- sapply(strsplit(nom,prefix),function(ll){ll[2];});
    }
    # looking for the maximum letter in noms
    if ( length(nom) == 0 ) { mama <- 0;
    } else { mama <- max(1*outer(nom,Letters,"==") %*% matrix(1:26,ncol=1));}
    if ((nbn < (27-mama)) & (nume>0)) {
        # adding letters 
        res <- Letters[mama+(1:nbn)];
    } else {
        # adding numbered nodes
        ajou <- 0; nu <- 1; res <- character(0);
        while ( ajou < nbn ) {
          nono <- paste(Letters[abs(nume)],nu,sep="");
          if (all(nono != nom)) {
              ajou <- ajou + 1;
              res <- c(res,nono);
          }
          nu <- nu+1;
        }
    }
}
# adding the prefix
res <- paste(prefix,res,sep="");
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3nume <- function(nbn,type="n")
#TITLE  provides a systematic numbering of items
#DESCRIPTION
# Provides systematic names for a series of items according their 
# number giving them an identical number of character.
#DETAILS (see the code)
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{nbn} <<Number of new item names to generate>>
#[INPUTS]
#{type} << Defines the type of numbering to use. There are
#          three possibilities: \code{A} for uppercased letters,
#          \code{a} for lowercased letters and \code{n} for
#          arabic numbers.>>
#VALUE
# vector with \code{nbn} different strings associated to new names
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3names(27);
# form3names(100,"a");
#REFERENCE
#SEE ALSO form3names
#CALLING
#COMMENT
#FUTURE
# Monitor the number of digits to obtain series of "Z01", "Z02",... ,"Z79", "Z80".
#AUTHOR J.-B. Denis
#CREATED 07_10_19
#REVISED 10_02_15
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(nbn,"numeric",1,message="form3nume: argument 'nbn' not accepted");
    check4tyle(type,"character",1,c("A","a","n"),message="form3nume: argument 'type' not accepted");
}
#
# degenerate case
if (nbn < 1) { return(character(0));}
#
# determining the number of digits and them
if (tolower(type)=="a") {
  nbd <- 26;
  if (type =="a") { digi <- letters;
  } else { digi <- LETTERS;}
} else {
  nbd <- 10;
  digi <- as.character(0:9);
}
nbp <- 1;
while (nbn > nbd^nbp) { nbp <- nbp + 1;}
#
# getting the numbers
res <- rep("",nbn);
for (ii in bc(nbp)) {
  jj <- rep(digi,times=nbd^(nbp-ii),each=nbd^(ii-1))[1:nbn];
  res <- paste(jj,res,sep="");
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3line <- function(len=50,pat="-_-",wid=3,
                      gind="",pat2="_-_",
                      imp=TRUE)
#TITLE  prints a separator line from a given pattern
#DESCRIPTION
# Prints a separator line from a given pattern.
# The line can be composite with the width argument.
# The pattern can comprise more than one character.
# General indentation is possible (\code{gind}).
# Even lines can have a different pattern(\code{pat2}).
#DETAILS
# contrary to \code{form3repeat} \\n are introduced
# at the end.
#PKEYWORDS
#KEYWORDS print
#INPUTS
#[INPUTS]
#{len} << Line length (without the general indentation.>> 
#{pat} << Pattern to use.>> 
#{wid} << Number of elementary lines.>> 
#{gind} << String to introduce as indentation.>> 
#{pat2} << Pattern to use for the even lines.
#         If NULL, the pattern is common to all lines.>> 
#{imp} << Printing when TRUE or returning (FALSE)>>
#VALUE
# character string or printing according to \code{imp}
#EXAMPLE
# form3line();
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_07_31
#REVISED 08_07_31
#--------------------------------------------
{
if (is.null(pat2)) { pat2 <- pat; }
if (wid <= 0) { res <- character(0);
} else {
    res <- "";
    if (nchar(pat)<=0) { pat <- "-";}
    nbp <- ceiling((len*wid)/nchar(pat));
    ch1 <- paste(rep(pat, nbp),collapse="");
    ch2 <- paste(rep(pat2,nbp),collapse="");
    po1 <- po2 <- 1;
    for (jbd in 1:wid) {
        if ((jbd %% 2) == 0) {
            li <- substr(ch2,po2,po2+len-1);
            po2 <- po2 + len;
        } else {
            li <- substr(ch1,po1,po1+len-1);
            po1 <- po1 + len;
        }
        res <- paste(res,gind,li,"\n",sep="");
    }
}
if(!imp) { return(res);
} else {cat(res);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3file <- function(chaine,file="",append=TRUE)
#TITLE  writes a character vector to a file
#DESCRIPTION
# The aim of this function is generate files
# from successive character vectors. According to
#  \code{append} the possibly existing file is appended
# or newly created. As many lines as components to \code{chain}
#  are introduced to the file.
#DETAILS
# \\n are added to create the new lines (even when file is \code{""}).
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{chaine}<<the character string to be output>>
#[INPUTS]
#{file} << file where to write it down. When
#          \code{""}, the result is displayed on the screen.>>
#{append} << Indicates if appending or not to \code{file}.>>
#VALUE nothing but the file is written
#EXAMPLE
# form3file(letters,"useless.txt")
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
if (length(chaine) > 0) {
    if (file!="") {sink(file=file,append=append);}
    for (jbd in 1:length(chaine)) {
        cat(chaine[jbd],"\n",sep="");
    }
    if (file!="") {sink();}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3title <- function(tit,box="un",
                           lbef=0,sbef=5,
                           saft=0,laft=1,
                           charbox=c("+","|","+","-",
                                     "+","|","+","-",
                                     "."),
                           alig=2,
                           caret=TRUE,
                           imp=TRUE)
#TITLE  prints or prepares a title
#DESCRIPTION
# prints or prepares the character string \code{tit} (can comprise
# several components) with more or less emphasis.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{tit}<<the title to print, each component will be printed in a
#       different line.>>
#[INPUTS]
#{box} <<defines the type of box to put around the title. Possible
#        options are:
#         \cr\code{"no"} for nothing,
#         \cr\code{"un"} for underlined,
#         \cr\code{"ov"} for overlined,
#         \cr\code{"par"} for parentherized,
#         \cr\code{"cor"} for the four corners,
#         \cr\code{"unov"} for underlined and overlined,
#         \cr\code{"box"} for a complete box.>>
#{lbef} <<if \code{numeric(1)} defines the number of empty
#         lines before the title; when \code{character}
#         provides the lines to add before the title.>>
#{sbef} <<either \code{numeric(1)} or \code{character(1)}
#         indicating the number of spaces or the characters
#         to introduce before the box.>>
#{saft} <<same as \code{sbef} but after.>>         
#{laft} <<same as \code{lbef} but after.>>         
#{charbox} <<nine single characters to define the box starting
#            after noon, and the extra 9th beign the filling
#            character for multiple line titles.>>
#{alig} <<The aligment to be done by \code{form3justifie}: 
#         1 for left,2 for center and 3 for right.>>
#{caret} <<Indicates if \code{\\n} must be added at the end
#          of each created line.>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#VALUE
# According to \code{imp}: nothing when printing is performed,
# a character string 
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3title("Some Title");
# form3title(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2);
# form3title(c("The title","can comprise","several lines"),
#            box="box",lbef=4,laft=2,
#            charbox=c("*","+","/","=","*","+","/","="," "));
#REFERENCE
#SEE ALSO
#CALLING {form3repeat}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 10_07_25
#--------------------------------------------
{
# constants
#            1    2    3     4     5      6     7
boxes <- c("no","un","ov","par","cor","unov","box");
# checking
if (rbsb.mck) {
    check4tyle(caret,"logical",1);
    check4tyle(tit,rbsb.chara,-1);
    check4tyle(box,rbsb.chara,1);
    if (!expr3present(box,boxes,TRUE)) {
        erreur(list(box,boxes),"'box' must be one of 'boxes'");
    }
    check4tyle(lbef,c("numeric",rbsb.chara),c(1,Inf));
    check4tyle(laft,c("numeric",rbsb.chara),c(1,Inf));
    check4tyle(sbef,c("numeric",rbsb.chara),1);
    check4tyle(lbef,c("numeric",rbsb.chara),1);
    check4tyle(charbox,rbsb.chara,9);
    if (!all(nchar(charbox)==1)) {
        erreur(charbox,"For the moment only unicharacter in 'charbox'");
    }
    check4tyle(imp,"logical",1);
}
# the null case
if (isempty(tit)) { return(rbsb.cha0);}
# preparing
if (is.numeric(lbef)) { lbef <- rep(" ",max(0,round(lbef[1])));}
if (is.numeric(laft)) { laft <- rep(" ",max(0,round(laft[1])));}
if (is.numeric(sbef)) { sbef <- form3repeat(" ",max(0,round(sbef[1])));}
if (is.numeric(saft)) { saft <- form3repeat(" ",max(0,round(saft[1])));}
#
if (box %in% boxes[c(1,2,4)])   { charbox[c(7,8,1)]   <- NA;}
if (box %in% boxes[c(1,3,4)])   { charbox[c(3,4,5)]   <- NA;}
if (box %in% boxes[c(1,2,3,6)]) { charbox[c(1:3,5:7)] <- "";}
if (box %in% boxes[c(5)])       { charbox[c(4,8,2,6)] <- " ";}
#
lmax <- max(sapply(tit,nchar));
tit <- form3justifie(tit,lmax,format=alig,carac=charbox[9]);
# producing
res <- character(0);
# first lines
for (ii in bf(lbef)) { res <- c(res,lbef[ii]);}
# overline
if (box %in% boxes[c(3,5,6,7)]) {
    lili <- sbef;
    lili <- paste(lili,charbox[7],sep="");
    lili <- paste(lili,paste(rep(charbox[8],lmax),collapse=""),sep="");
    lili <- paste(lili,charbox[1],sep="");
    lili <- paste(lili,saft,sep="");
    res <- c(res,lili);
}
# title lines
for (ii in bf(tit)) {
    lili <- sbef;
    lili <- paste(lili,charbox[6],sep="");
    lili <- paste(lili,tit[ii],sep="");
    lili <- paste(lili,charbox[2],sep="");
    lili <- paste(lili,saft,sep="");
    res <- c(res,lili);
}
# underline
if (box %in% boxes[c(2,5,6,7)]) {
    lili <- sbef;
    lili <- paste(lili,charbox[5],sep="");
    lili <- paste(lili,paste(rep(charbox[4],lmax),collapse=""),sep="");
    lili <- paste(lili,charbox[3],sep="");
    lili <- paste(lili,saft,sep="");
    res <- c(res,lili);
}
# last lines
for (ii in bf(laft)) { res <- c(res,laft[ii]);}
# adding carriage returns
if (caret) { for (ii in bf(res)) {
    res[ii] <- paste(res[ii],"\n",sep="");
}}
# returning
if (imp) {
    cat(res,sep="");
    invisible();
} else { return(res);}
# 
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3titre <- function(tit,empha=3,indent=2+2*empha,imp=TRUE)
#TITLE  prints or prepares a title
#DESCRIPTION
# prints or prepares the character string \code{tit}
# with more or less emphasis.
# This function is a shortcut of \code{form3title}
# indeed, it just some specialized calls to it.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{tit}<<the title to print (just one line)>>
#[INPUTS]
#{empha} << Level of emphasize.\cr
#          0: single line without carriage return\cr
#          1: single line\cr
#          2: underlined\cr
#          3: underlined and overlined\cr
#          4: 2 + 1 line before\cr
#          5: 3 + 1 line after\cr
#          6: 2 + 2 lines before and after\cr
#          7: ...>>
#{indent} << Number of spaces to introduce before the title>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#VALUE
# either nothing or a character string according to imp
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3titre("Some Title");
#REFERENCE
#SEE ALSO
#CALLING {form3repeat}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_24
#REVISED 10_09_15
#--------------------------------------------
{
# adjusting
empha <- round(max(0,min(7,empha)));
# preparing the argument for form3title
if (length(tit)>1) { tit <- paste(tit,collapse=" ");}
if (empha == 0) { tit <- paste("<",tit,">",sep="")}
if (empha == 1) { tit <- paste("(*)",tit,"(*)",sep="")}
sbef <- round(max(indent,0));
caret <- (empha != 0);
saft=""; lbef <- 0; laft <- 0;
box <- "no";
if (empha == 2) { box <- "un";}
if (empha == 3) { box <- "unov";}
if (empha == 4) { box <- "unov"; lbef <- 1;}
if (empha == 5) { box <- "unov"; laft <- 1; lbef <- 1;}
if (empha == 6) { box <- "unov"; laft <- 2; lbef <- 2;}
if (empha == 7) { box <- "box" ; laft <- 1; lbef <- 1;}
# calling form3title
res <- form3title(tit,box=box,
                  lbef=lbef,sbef=sbef,
                  saft=saft,laft=laft,
                  charbox=c("+","|","+",
                            "-","+","|",
                            "+","-"," "),
                  alig=2,caret=caret,imp=imp);
# returning
if (imp) { return(invisible());}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3paragraphe <- function(texte,titre=-1,
                            wid=60,fli=NULL,sep=1,
                            jus=1,trunc=TRUE,
                            ed="  ",ef="",
                            imp=TRUE)
#TITLE  prints or prepares paragraphes
#         from a character vector.
#DESCRIPTION
# prints or prepares a character string \code{texte}
# as a small formatted text.\cr
#         Each component is supposed to be a
#         paragraph but the first one can be
#         considered as a title.
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{texte}<<The text to print (character vector).>>
#[INPUTS]
#{titre} << When > -2 indicates that the first
#           component is a title (if not a
#           simple paragraph). Then the value
#           of titre gives the emphasize to put
#           on the title. Notice that the title is
#           not splitted in several lines as are
#           the other components according to 'wid'
#           value.>>
#{wid} << The width (in characters) without
#        including indentation and frames.>>
#{fli} << When NULL, the first line of
#        each paragraph (in fact the second if there is 
#        a title) is issued as a standard line. If not,
#        fli[1] spaces are added before and the considered
#        width is fli[2] (not including the added spaces).
#        Also this means that your already wrote fli[3]
#        characters on the first line {this last possibility
#        can be used only when there are no title and for the 
#        the first component.
#        For instance, a French paragraph will be issued
#        with fli = c(5,wid,0). The possibility of modifying
#        wid for the first line can be of use when adding
#        the name of an item first.>>
#{sep} << Number of lines separating each paragraph.>>
#{jus} << Type of justification (1=left, 2=centred,
#             3=left).>>
#{trunc} << Must truncation be done when a word
#          is greater than the proposed wid?>>
#{ed} << Framing at the beginning of each line.>>
#{ef} << Framing at the end of each line.>>
#{imp} << Printing is performed and nothing is returned.
#                If FALSE, the character string is returned 
#                (including possible new lines)>>
#VALUE
# either nothing or a character string according to \code{imp}
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# form3paragraphe(c("My Title","My important first sentence.","Last one!"));
#REFERENCE
#SEE ALSO
#CALLING {form3repeat}
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_08_01
#REVISED 09_09_29
#--------------------------------------------
{
sep <- min(max(0,sep),5);
if (titre > -2) {
    # the possible title
    res <- form3titre(texte[1],empha=titre,indent=nchar(ed),imp=imp);
    if (imp) { cat(form3repeat("\n",sep));
    } else { res <- c(res,form3repeat("\n",sep));}
    texte <- texte[-1];
} else { res <- character(0);}
#
for (i in bf(texte)) {
    # paragraph after paragraph
    mots <- strsplit(texte[i]," ")[[1]];
    nlig <- 0;
    while (length(mots) > 0) {
        # the paragraph is not empty
        nlig <- nlig+1;
        # is it the first line and must it be different?
        spfl <- ((nlig==1)&(!isempty(fli)));
        if (spfl) {
            if (rbsb.mck) {
                check4tyle(fli,"numeric",c(3,Inf));
            }
            wiid <- fli[2];
            lili <- form3repeat(" ",fli[1]);
            trop <- fli[3];
        } else {
            wiid <- wid;
            #lili <- character(0);
            lili <- "";
            trop <- 0;
        }
        if (isempty(wiid)) { wiid <- 60;}
	while ( ((length(mots) > 0) &&
		 (nchar(paste(lili,collapse=" ")) < (wiid-nchar(mots[1])))
		) || (
		 (length(lili) == 0)
		)
	      ) {
	    lili <- c(lili,mots[1]);
	    mots <- mots[-1];
	}
	#cat("<<",length(mots),">>\n");
        lili <- paste(lili,collapse=" ");
        #cat("{{",nchar(lili),lili,"}}\n");
        lili <- form3justifie(lili,wid-trop,jus,trunc);
        if (spfl) { lili <- paste(lili,ef,"\n",sep="");
        } else { lili <- paste(ed,lili,ef,"\n",sep="");}
	if (imp) { cat(lili);
	} else {
	    res <- c(res,lili);
	    lili <- character(0);
	}
    }
    if (imp) { cat(form3repeat("\n",sep));
    } else { res <- c(res,form3repeat("\n",sep));}
}
# returning
if (!imp) { return(res);}
cat(res);
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3affiche <- function(x,pau=FALSE,cat=FALSE,...)
#TITLE  displays with its name any object
#DESCRIPTION
#  displays any object after giving the name of
# the variable containing it. A pause can be introduced
# to give the opportunity to scrutinize the result.  
#DETAILS
#PKEYWORDS
#KEYWORDS print
#INPUTS
#{x}<<The object to print.>>
#[INPUTS]
#{pau} << Must a pause be performed after the display?>>
#{cat} << Must the printing be done with 'cat' instead of print?>>
#{\dots} <<possible arguments for the print function.>>
#VALUE
# a print (or cat) is done and \code{x} is returned
#EXAMPLE
# uu <- "azerty";
# form3affiche(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_17
#REVISED 09_10_12
#--------------------------------------------
{
cat("<< Displaying ",deparse(substitute(x))," >>\n");
if (cat) { cat(x,"\n");
} else { print(x,...);}
if (pau) { pause("affichage");}
# returning
x;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3lmargin <- function(x,ma=0.15,pro=0.10)
#TITLE  adds some padding to a range
#DESCRIPTION
#   Used for the preparation of graphic outputs:
#  the range of \code{x} is extended with a proportion 
#  \code{ma} at both ends when \code{diff(range(x))}
#  is not nought, else of \code{pro*x[1]}.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{x}<<vector of values to consider>>
#[INPUTS]
#{ma} <<proportion margin to add from the range of value.>> 
#{pro} <<proportion margin to add from the range of values
#        when the range is of lenght zero.>> 
#VALUE
# A vector of two numerical giving the extended range
#EXAMPLE
# geom3lmargin(1:5);
# geom3lmargin(1:5,0.1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_16
#REVISED 11_01_19
#--------------------------------------------
{
  # degenerate case
  if (length(x)==0) { return(rbsb.num0);}
  # computation
  res <- range(x);
  if (diff(res)==0) {
    # all values are equal
    res <- res[1]*(1+c(-1,1)*pro);
  } else {
    # standard case
    res + ma*diff(res)*c(-1,1);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3xyz2pol <- function(coo)
#TITLE  transforms cartesian coordinates in polar coordinates
#DESCRIPTION
#  Transforms, into R to three, cartesian coordinates
# in polar coordinates
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{coo}<<the x,y,z coordinates.>>
#[INPUTS]
#VALUE
# A numerical vector with (rho, theta, phi)
#EXAMPLE
# geom3xyz2pol(c(1/sqrt(2),1/sqrt(2),0));
#REFERENCE
#SEE ALSO geom3pol2xyz
#CALLING
#COMMENT
#FUTURE
# to be vectorialized
#AUTHOR J.-B. Denis
#CREATED 07_07_16
#REVISED 07_10_10
#--------------------------------------------
{
    if (any(is.na(coo))) { return(rep(NA,3));}
    rho <- sqrt(sum(coo^2));
    if (abs(rho ) < 10^-10) { return(rep(0,3));}
    phi <- asin(coo[3]/rho);
    x <- coo[1]/rho/cos(phi);
    y <- coo[2]/rho/cos(phi);
    theta <- acos(x);
    if (y < 0) { theta <- 2*pi - theta;}
    c(rho,theta,phi);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3pol2xyz <- function(poo)
#TITLE  transforms polar coordinates in cartesian coordinates
#DESCRIPTION
#  Transforms polar coordinates into cartesian coordinates in R to the three
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{poo}<<the polar coordinates (rho, theta, phi)>>
#[INPUTS]
#VALUE
# A numerical vector with (x, y, z)
#EXAMPLE
# uu <- geom3xyz2pol(c(1/sqrt(2),1/sqrt(2),0));
# geom3pol2xyz(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# to be vectorialized
#AUTHOR J.-B. Denis
#CREATED 07_07_16
#REVISED 07_09_07
#--------------------------------------------
{
    if (any(is.na(poo))) { return(rep(NA,3));}
    rho <- poo[1];theta <- poo[2]; phi <- poo[3];
    rho*c(cos(theta)*cos(phi),sin(theta)*cos(phi),sin(phi));
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
geom3pointi <- function(x0,y0,x1,y1,d)
#TITLE  interpolation in R to the 2
#DESCRIPTION
# Interpolation in the plane: given two points p0=(x0,y0) and p1=(x1,y1)
# returns the point p=(x,y) such that
#  vector(p0,p) = z*vector(p0,p1)
#  where z is such that the distance between
#  p0 and p is abs(d). Also sign(d) == sign(z)
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS 
#{x0} <<see the description>>
#{y0} <<see the description>>
#{x1} <<see the description>>
#{y1} <<see the description>>
#[INPUTS]
#{d} <<see the description>>
#VALUE
# Returns c(x,y), the interpolated points.
#EXAMPLE
# geom3pointi(1,1,10,10,sqrt(2));
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE
# vectorialize the function using two matrices \code{p0} and \cde{p1}.
#AUTHOR J.-B. Denis
#CREATED 07_05_23
#REVISED 07_06_14
#--------------------------------------------
{
    z <- d / sqrt((x1-x0)^2+(y1-y0)^2);
    z*c(x1-x0,y1-y0) +c(x0,y0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
conti2categ <- function(val,lim=0,pod=c("-","+"))
#TITLE  transforms continuous values into a 
# categoric values.
#DESCRIPTION
# Transforms continuous values into a 
# categoric values.  A first step towards the discretization
# of numeric variables into n classes
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{val}<<vector of values to be discretized.>>
#{lim}<< Vector of the n-1 compulsory limits 
#            to define the n classes. When x == lim[i]
#            it belongs to the ith class.>>
#{pod}<< Vector of the labels associated 
#                    to the n classes. Its length
#        provides the number of classes.>>
#[INPUTS]
#VALUE
# the \code{character} of the resulting categoric values
#EXAMPLE
# set.seed(1234);
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# conti2categ(runif(10)-0.5);
# conti2categ(runif(10),seq(0.1,0.9,length=9),LETTERS[1:10]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_06
#REVISED 09_03_20
#--------------------------------------------
{
if (isempty(val)) { return(character(0));}
if ((1+length(lim)) != length(pod)) {
    form3affiche(lim);
    erreur(pod,"Non compatible lengths of lim and pod.");
}
if (length(lim)==0) {
    return(rep(pod,length(val)));
}
res <- rep(1,length(val));
for (jbd in 1:length(lim)) {
    res <- res + (lim[jbd] <= val);
}
pod[res];
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
open8graph <- function(prefix=NULL,...) 
#TITLE  opens the graph device 
#DESCRIPTION
# According to the global constant \code{rbsb.mfi} a
# graphical device is open or not.
# Must be called before plotting something that ones want
# to keep under \code{rbsb.mgr} type.
#DETAILS
# The file opened for storing the graph is named with
# three components separated with dots: \code{rbsb.fpx}, prefix 
# and \code{rbsb.mgr}. \code{rbsb.fpx} is a prefix to be modified by 
# the user as convenient. \code{rbsb.ffg} is the number of the
# current figure incremented by this function. rbsb.mgr is
# the suffix associated to the type of graph (either 'pdf'
# or 'png').
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{prefix} << When NULL the \code{1+rbsb.ffg} numeric is taken
#                to give the number of the file with three
#                digits. If not, it is a character giving
#                the complete prefix to use before the suffix.>>
#{\dots} << argument(s) to be transmitted to the openning device
#         of graphics. Quite useful for specific character and
#         picture sizes, or to get more than one graph into
#         the file.>>
#VALUE
# Nothing but when \code{rbsb.mfi} is \code{TRUE} 
# and \code{is.null(prefix)}, the graphical device is open and the global 
# constant \code{rbsb.ffg} is increased _before_ with one.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# print(rbsb.ffg);
# open8graph();
# close8graph();
# print(rbsb.ffg);
#REFERENCE
#SEE ALSO close8graph
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 09_11_17
#--------------------------------------------
{
if (rbsb.mfi) {
    if (is.null(prefix)) {
        assign("rbsb.ffg",rbsb.ffg + 1,pos=".GlobalEnv");
        prefix <- paste(rbsb.fpx,form3justifie(rbsb.ffg,nbc=4,
                                               format=3,tronc=FALSE,
                                               carac="0"),
                        sep=".");
    }
    fifi <- paste(prefix,rbsb.mgr,sep=".");
    gopen <- FALSE;
    if (rbsb.mgr == "pdf") { pdf(fifi,...); gopen <- TRUE;}
    if (rbsb.mgr == "png") { png(fifi,...); gopen <- TRUE;}
    if (!gopen) {
        erreur(rbsb.mgr,
               "This type of graph is not yet implemented in /rbsb/");
    }
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8graph <- function(message=rbsb.cha0) 
#TITLE  closes the file open by open8graph
#DESCRIPTION
# According
# to the global constant \code{rbsb.mfi} closes the file
# open by \code{open8graph}. Also if \code{!rbsb.mba}
#  a pause is issued.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{message} << Message to display on the terminal.
# When empty, no message will be displayed.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_29
#REVISED 10_03_16
#--------------------------------------------
{
# closing the device
if (rbsb.mfi) { dev.off();}
# displaying the message
if (!isempty(message)) {
    if (!rbsb.mba) { pause(message,"pause from close8graph");
    } else { cat("<<<",message,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
open8text <- function(append=TRUE) 
#TITLE  opens the standard output text for rebastaba
#DESCRIPTION
#  Opens the standard output text for /rbsb/.
# According to the global constant \code{rbsb.mfi} the
# standard output text of rebastaba is open (in
# append mode) or not. The name of this file is provided
# by the constant \code{rbsb.fou}.
# Must be called before printing something ones want
# to keep on file. 
#DETAILS
#  Usual derivation is done with \code{sink} until \code{close8text} is called.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{append} << Must the current file rbsb.fou be continued ?>>
#VALUE
# nothing but the indicated actions are performed
#EXAMPLE
#REFERENCE
#SEE ALSO close8text
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 08_08_28
#--------------------------------------------
{
if (rbsb.mfi) { sink(rbsb.fou,append);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8text <- function(message=rbsb.cha0) 
#TITLE  pauses (and more) the program until an answer is given
#DESCRIPTION
# Closing the output file \code{rbsb.fou} according to \code{rbsb.mfi}. A pause allowing 
# to stop the process is issued if \code{rbsb.mba} is false.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{message} << Message to display on the terminal. 
# When empty, no message will be displayed.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_29
#REVISED 10_03_16
#--------------------------------------------
{
# closing the file
if (rbsb.mfi) { sink();}
# displaying the message
if (!isempty(message)) {
    if (!rbsb.mba) { pause(message,"pause from close8text");
    } else { cat("<<<",message,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
depeche <- function(what="???",answer=TRUE) 
#TITLE  issues a message on the screen [and returns the answer]
#DESCRIPTION
# This function issues a message and pause if an answer is required.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{what} << Message to be issued>>
#{answer} << When an answer is awaited.>>
#VALUE
# the answer as character if answer is TRUE (but no answer is transformed 
# into "") if not ""
#EXAMPLE
# uu <- depeche("What time is it?");
# print(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_02_04
#REVISED 09_02_04
#--------------------------------------------
{
cat(">>> --------------> ",what,"\n");
if (answer) {
    cat(">>> Give the answer and 'Enter' to continue \n");
    res <- scan(what="character",nmax=1);
    if (length(res) == 0) { res <- "";}
} else { res <- "";} 
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
pause <- function(what="",message=NULL) 
#TITLE  pauses the program until an answer is given
#DESCRIPTION
# This function issues a pause with a message allowing 
# to stop the process or to continue it.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{what} << Short message commenting the pause>>
#{message} << A possible longer message, generally
#             to give details about from where the pause
#             is issued. When
#          NULL a shortened message is issued.>>
#VALUE
# Message(s) are displayed on the screen awaiting for
# an answer to stop or continue.  
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# pause("Time for lunch!");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_25
#REVISED 09_01_27
#--------------------------------------------
{
if (!isempty(message)) {cat(">>> (",message,")\n");}
if (!isempty(what))  {cat(">>> --------------> ",what,"\n");}
cat(">>> 'Enter' to continue | any key(s) +'Enter' to stop \n");
what <- scan(what="character",nmax=1);
if (length(what) != 0) {
    stop("(...YOU decided to stop the job...)",call.=FALSE);
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
erreur <- function(x,...,w=FALSE)
#TITLE  issues an error message and concludes accordingly
#DESCRIPTION
# when called this function prints x, then displays a message before stopping 
# the process except if it is a warning or if the general constant
# \code{rbsb.mfa} is true.
#DETAILS
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{x} <<object to be printed before the message. When \code{isempty(x)}
#      nothing is printed. When it is a list, all components
#      of the list are successively printed.>>
#{\dots}<<pieces of message to display after pasting>>
#[INPUTS]
#{w} << Indicates if it is a simple warning>> 
#VALUE
# nothing is returned
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
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
#REVISED 10_06_24
#--------------------------------------------
{
form3repeat("~",60,TRUE);
if (!isempty(x)) {
    if (is.list(x)) {
        for (i in bf(x)) {
            form3repeat("~",40,TRUE);
            cat("<< Displaying ",deparse(substitute(x[[i]]))," >>\n")
            print(x[[i]]);
        }
    } else {
        cat("<< Displaying ",deparse(substitute(x))," >>\n")
        print(x);
    }
}
message <- paste(...);
cat("<<<<< MESSAGE >>>>>\n");
print(message);
if (w) {
    cat(rbsb.msi,"SIMPLE WARNING:\n");
} else {
    on.exit(traceback());
    cat(rbsb.msi,"ERREUR FATALE\n");
    form3repeat("~",60,TRUE);
    if (rbsb.mfa) { stop("stopped by rebastaba");}
}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rapport <- function(origine)
#TITLE  issues an error message when rebastaba fails
#DESCRIPTION
# Ask the user to send the author a detailed report.
#DETAILS
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{origine}<<A character indicating where the difficulty
#           occured.>>
#[INPUTS]
#VALUE
# nothing
#EXAMPLE
# # rapport("For the moment, we are doing the tests...");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_10_23
#REVISED 09_01_12
#--------------------------------------------
{
    message <- new("des",
                   name="rebastaba error",
                   orig=paste(origine,collapse=" "),
                   time=now("d"),
                   defi="",
                   role="Improve rebastaba code",
                   comm=c(paste("Congratulations! You were clever enough",
                              "to stick this code."),
                          paste("Sorry because you didn't get your",
                                "result"),
                          paste("Perhaps you can have a look at what you",
                                "are wanting, to modify your own code?"),
                          paste("In any case, I would be pleased if you",
                                "could report me the error",
                                "with a reproductible example"),
                          "Jean-Baptiste.Denis@Jouy.Inra.Fr"));
    form3repeat("+",60,TRUE);
    form3repeat("+",60,TRUE);
    print(message,what="a");
    form3repeat("+",60,TRUE);
    form3repeat("+",60,TRUE);
    cat("<<D'esol'e de ce contretemps pour vous !>>\n\n");
    stop();
    invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
now <- function(what="dh",format="red")
#TITLE  returns a character giving the present moment
#DESCRIPTION
# Returns a character giving the present moment
# with different components
#DETAILS
# based on Sys.time R function
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#[INPUTS]
#{what} <<\code{character(1)} indicating which components to include
#  among:
#  \cr\code{d} for the day,
#  \cr\code{h} for the hour,
#  \cr\code{m} for the minute,
#  \cr\code{s} for the second.>>
#{format} <<\code{"red"} for reduced if not in a verbose way.>>
#VALUE
# a character
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# cat("Now is",now(),"\n");
# cat(now("dhms","verbose"),"\n");  
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 10_10_12
#--------------------------------------------
{
# adding the consequences to what
if (expr3present("s",what)) { what <- paste(what,"m",sep="");}
if (expr3present("m",what)) { what <- paste(what,"h",sep="");}
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
if (format[1]=="red") {
    if (expr3present("d",what)) {
        res <- paste(res,paste(substr(an,3,100),mois,jour,sep="_"),sep="");
        if (expr3present("h",what)) { res <- paste(res,"@",sep="");}
    }
    if (expr3present("h",what)) {
        res <- paste(res,heure,sep="");
        if (expr3present("m",what)) { res <- paste(res,":",sep="");}
    }
    if (expr3present("m",what)) {
        res <- paste(res,minute,sep="");
        if (expr3present("s",what)) { res <- paste(res,":",sep="");}
    }
    if (expr3present("s",what)) {
        res <- paste(res,seconde,sep="");
    }
} else {
    if (expr3present("d",what)) {
        res <- paste(res,"le",weekdays(mnt),jour,months(mnt),an);
        if (expr3present("h",what)) { res <- paste(res," `a ",sep="");}
    }
    if (expr3present("h",what)) {
        res <- paste(res,heure,"H",sep="");
        if (expr3present("m",what)) { res <- paste(res,"",sep="");}
    }
    if (expr3present("m",what)) {
        res <- paste(res,minute,"m",sep="");
        if (expr3present("s",what)) { res <- paste(res,"",sep="");}
    }
    if (expr3present("s",what)) {
        res <- paste(res,seconde,'s',sep="");
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2chars <- function(cara,sep=rbsb.sep0,
                       rsep="no_action",no_value=character(0))
#TITLE  transforms a single character into a character vector 
#DESCRIPTION
# Just doing a character vector from a single
# character using \code{sep} to separate the different
# components.
#DETAILS
# Argument \code{sep} is a possible separator. If empty
# no splitting is performed.
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara} << The single character to transform.>>
#[INPUTS]
#{sep} <<  Separator for the splitting
# when \code{""}, no splitting is done.>>
#{rsep} << Indicates if repetitions of \code{sep} must be considered as signicant or not.
#          If \code{"no_action"} then their repetitions will be ignored
#          if not \code{no_value} components will be introduced.>>
#{no_value} <<value to introduce for empty components.>>
#VALUE
# a character vector if everything was good.
# a 'faux' object otherwise.
#EXAMPLE
# rbsb3k("reset"); # for R checking convenience
# char2chars("A B C");
# char2chars("A B C","  ");
# char2chars("A B  C  ");
# char2chars("A B  C  ",rsep="");
# char2chars("0.1 0.9 // 0.5 0.5 // 0.9 0.1","//");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# This function is one of the evolution of the former char2vect function.
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_22
#REVISED 10_10_11
#--------------------------------------------
{
# checking
if (length(cara)>1) {
    return(new("faux",orig="char2chars",
           comm=as.character(cara),
           defi="A single character was expected as input"));
}
if (length(cara) == 0) {
    return(character(0));
}
if (!is.character(cara)) {
    return(new("faux",orig="char2chars",
           orig=class(cara),
           comm=as.character(cara),
           defi="A character even null was expected!"));
}
# extracting the vector
if (sep!="") {
    res <- strsplit(cara,sep)[[1]];
    # removing parasite values
    if (rsep == "no_action") {res <- res[res!=""];
    } else { res[res==""] <- no_value;}
} else { res <- cara;}
#
# returning without error
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
li2cha <- function(liste,tw=" ",nam=NULL)
#TITLE  transforms a list into a character vector
#DESCRIPTION
# The resulting vector will have as many components as 
# list. Each one being the concatenation of its own
# components interpreted \code{as.character} with \code{tw}
# as seperator.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{liste} << The list to transform.
#{nam} << When \code{is.null(nam)} and they exist, the names 
# of the list are reported to the vector.>>
#[INPUTS]
#{tw} <<  Separator for each component.>>
#{nam} << Names to be apply to the created list.
# The exact number is expected, as well as non
# repeated names.>>
#VALUE
# a character vector with possibly
# named components
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# li2cha(cha2li(c("A B C","D E")));
# li2cha(cha2li(c("A B C","D E"),""),"");
# cha2li(li2cha(cha2li(c("A B C","D E"))));
# cha2li(li2cha(cha2li(c("A B C","D E"),""),""),"");
#REFERENCE
#SEE ALSO cha2li
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_08
#REVISED 10_03_08
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(liste,"list",-1);
    if (!is.null(nam)) {
        check4tyle(nam,rbsb.chara,length(liste));
        if (length(unique(nam)) != length(nam)) {
            erreur(sort(nam),"Not all names are different!");
        }
    }
}
# creating the list
 res <- character(length(liste));
# naming the list
if (!is.null(nam)) {
    names(res) <- nam;
} else {
    names(res) <- names(liste);
}
# filling the vector
for ( nn in bf(res)) {
    res[nn] <- paste(as.character(liste[[nn]]),collapse=tw);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cha2li <- function(cara,tw=" ",nam=NULL)
#TITLE  transforms a character into a list
#DESCRIPTION
# Just doing a list of single (or not) characters from a
# a character vector, taking care of the names
# in a due way.
#DETAILS
# Argument 'tw' is a possible separator. If empty
# only one character each component. If not
# each caracter is splitted with it...
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{cara} << The vector of characters to transform.
# When is.null(nam) and they exist, the names 
# of the vector are reported to the list.>>
#[INPUTS]
#{tw} <<  Separators for the second step splitting.>>
#{nam} << Names to be apply to the created list.
# The exact number is expected, as well as non
# repeated names.>>
#VALUE
# a list of single character with possibly
# named components
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# cha2li(c("A B C","D E"));
# cha2li(c("A B C","D E")," ");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_18
#REVISED 10_04_07
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(cara,rbsb.chara,-1);
    if (!is.null(nam)) {
        check4tyle(nam,rbsb.chara,length(cara));
        if (length(unique(nam)) != length(nam)) {
            erreur(sort(nam),"Not all names are different!");
        }
    }
}
# creating the list
 res <- vector("list",length(cara));
# naming the list
if (!is.null(nam)) {
    names(res) <- nam;
} else {
    names(res) <- names(cara);
}
# filling the list
for ( nn in bf(cara)) {
    recu <- char2chars(cara[nn],tw);
    if (is(recu,"faux")) {
        print(recu);
        erreur(cara[nn],"The transformation into vector was refused!");
    } else {
        res[[nn]] <- recu;
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
# The file must reopen to empty it if necessary
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{x} << The object to print.>>
#[INPUTS]
#{fifi} <<  Name of the file, if '' the print
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
var3standard <- function(nvar,bef="-",aft="-")
#TITLE  provides nvar standard names of variables
#DESCRIPTION
#   numbering surrounded with 'bef' and 'aft'
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{nvar} <<Number of variables names to provide.
#         When less than 2, \code{""} is returned.>>
#[INPUTS]
#{bef} << To put before the number.>>
#{aft} << To put after  the number.>>
#VALUE
# A character of length \code{nvar} with standard
# names for repetitions. When \code{nvar} is one, an empty
# character is returned.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# var3standard(5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# add left '0' to get names with the same 
# number of characters whatever is the 
# number of variable names to generate.
#AUTHOR J.-B. Denis
#CREATED 09_04_04
#REVISED 09_05_29
#--------------------------------------------
{
# checking
if (isempty(nvar)) { return("");}
if (rbsb.mck) {
    check4tyle(nvar,"numeric",c(0,Inf));
    check4tyle(bef,rbsb.chara,1);
    check4tyle(aft,rbsb.chara,1);
}
# returning
if (nvar<2) { return("");}
paste(bef,bc(nvar),aft,sep="");
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check4tyle <- function(x,typ,len=-1,con=rbsb.num0,
                       message=NULL,fatal=TRUE,na.rm=FALSE)
#TITLE  checks the type, the length and the content
# of some standard object
#DESCRIPTION
#  checks the type, the length and the content
# of some standard object.
# If not correct, a message and a fatal error are issued.
# NA are detected and considered as wrong.
#DETAILS
# 'integer' has not got the meaning in \code{is.integer} R
# standard function. 'null' must be understood as
# resulting TRUE with function \code{isempty}.\cr
# 'Named character' is different from  
# 'character': \code{rbsb.chara} can be used for this purpose.
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{x} <<object to be checked.>>
#{typ} <<The list of correct types, among
# 'null', 'integer', 'numeric', 'character',
# 'logical', 'list', 'any', 'function', 'data.frame',
# 'matrix'. Also are 'nlogical', 'ninteger', 'nnumeric' and 
# 'ncharacter' for named structures.
# As understood, 'any' implies that no
# check of the type is performed.>>
#[INPUTS]
#{len} <<If \code{length(len)==1}, the exact length
# to be checked, if not must be of length two for
# the possible range of \code{length(x)}. When -1,
# no check on the length is performed.\cr
# For data.frame, it is the number of columns.
# When \code{na.rm} the check is made after
# removing the NA values.>>
#{con} << If \code{length(con)>0}, some check about
# the content of \code{x} is done for some of the 
# types. More precisely for (integer, numeric): all
# values must belong to the interval \code{[con[1],con[2]]}
# and for (character), the set of possible \code{character(1)}
# is described by \code{con}.>>
#{message} << Some additional message to be
#            issued before stopping in case of error.>>
#{fatal} << what to do when discovering
#           an inconsistency ? TRUE: this function prints the message
#           and stops; FALSE: this function returns
#           the message as a character.>>
#{na.rm} << Must the \code{NA} values be removed before checking?
#           This is implemented only for the types integer, numeric,
#           character and logical.>>
#VALUE
# a character with the message(s), when everything is OK
# returns a \code{character(0)}.
# But when \code{fatal} if not OK prints the
# message and stops.
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# check4tyle(letters,c("numeric","character"),c(20,30),message="!OK");
# check4tyle(structure("A",.Names="a"),"ncharacter",-1,LETTERS);
# check4tyle("A","ncharacter",-1,message="BAD",fatal=FALSE);
# check4tyle(structure("A",.Names="a"),"character",-1,letters,"bad",fatal=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE improves the treatment of 'NA's
#AUTHOR J.-B. Denis
#CREATED 09_05_02
#REVISED 10_09_14
#--------------------------------------------
{
# checking the arguments and preparation
# accepted types by check4tyle
typena <- c("integer","numeric","character",
           "logical","ninteger","nnumeric",
           "ncharacter","nlogical");
# In typena are those types from which NA values 
# can be removed
types <- c(typena,"list","function","any",
           "data.frame","matrix","null");
type <- match(typ,types);
if (all(is.na(type))) {
    erreur(list(types,typ),
           "CHECK4TYLE: None of the proposed type was in the list!"
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
    if ("null" %in% typ) { return(TRUE);
    } else { 
        res <- paste("The proposed object is NULL not in '",
                     paste(typ,collapse="/"),"'",sep="");
	if (!is.null(message)) { res <- c(res,message);}
	form3repeat("=",60,TRUE);
	cat("From check4tyle: in fact 'x' is '",
	    deparse(substitute(x)),"'\n",sep="");
	print.default(x);
	if (fatal) {erreur("Message:",paste(res));}
    }
}
if (all(!is.na(match(typ,typena)))) { if (any(is.na(x))) { 
    erreur(typ,"CHECK4TYLE: 'x' is or comprises 'NA' which were not removed!");
}}
# possible type for the check
type <- types[type[!is.na(type)]];
if (!is.numeric(len)) {
    erreur(len,"CHECK4TYLE: 'len' must be NUMERIC of size one or two");
}
if ((length(len)==0)|(length(len)>2)) {
    erreur(len,"CHECK4TYLE: 'len' must be numeric of size ONE or TWO");
}
# processing for the length
if (length(len)==1) {
    if (len>=0) { if (length(x)!=len) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,paste("CHECK4TYLE: 'x' is not of length ",len));
    }}
} else {
    if ((length(x)<len[1])|(length(x)>len[2])) {
        if (narm>0) { res <- c(res,paste(narm,"components were removed"));}
        res <- c(res,paste("CHECK4TYLE: 'x' has got a length outside:",paste(len,collapse=":")));
    }
}
# processing for the type
if (!("any" %in% type)) {
    ty <- FALSE;
    for (tt in type) {
        #
        if (tt=="integer")   { if (is.numeric(x)) {
            if (all(x==round(x))) { if (length(names(x)) == 0) {
                ty <- TRUE;
            }}
        }}
        if (tt=="ninteger")   { if (is.numeric(x)) {
            if (all(x==round(x))) { if (length(names(x))>0) {
                ty <- TRUE;
            }}
        }}
        #
        if (tt=="numeric")   { if (is.numeric(x))   {
            if (is.null(attributes(x)$names)) { ty <- TRUE;}
        }}
        if (tt=="nnumeric")   { if (is.numeric(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="character") { if (is.character(x)) {
            if (is.null(attributes(x)$names)) { ty <- TRUE;}
        }}
        if (tt=="ncharacter")   { if (is.character(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="logical")   { if (is.logical(x))   {
            if (is.null(attributes(x)$names)) { ty <- TRUE;}
        }}
        if (tt=="nlogical")   { if (is.logical(x)) {
            if (is.character(attributes(x)$names)) { ty <- TRUE;}
        }}
        #
        if (tt=="function")  { if (is.function(x))  { ty <- TRUE;}}
        if (tt=="list")      { if (is.list(x))      { ty <- TRUE;}}
        if (tt=="data.frame"){ if (is.data.frame(x)){ ty <- TRUE;}}
        if (tt=="matrix")    { if (is.matrix(x))    { ty <- TRUE;}}
        if (tt=="null")      { if (isempty(x))      { ty <- TRUE;}}
    }
    if (!ty) {
        res <- c(res,paste("Among type = ",paste(type,collapse="/")));
        rr <- "The class of 'x' is '";
        if (is.character(attributes(x)$names)) {
            rr <- c(rr,"named ");
        }
        rr <- c(rr,paste(class(x),"'!",sep=""));
        res <- c(res,paste(rr,collapse=""));
        res <- c(res,"CHECK4TYLE: 'x' does not belong to any of these!");
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
                   paste("CHECK4TYLE:",
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
# returning
if (!identical(res,character(0))) {
    if (!is.null(message)) { res <- c(res,message);}
    if (fatal) {
	form3repeat("=",60,TRUE);
	cat("From check4tyle: in fact 'x' is '",
	    deparse(substitute(x)),"'\n",sep="");
	print.default(x);
        erreur(NULL,"FATAL ERROR from check4tyle");
    }
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
listev <- function(liste,ith)
#TITLE  returns the ith element of a list supposed to be numeric
#DESCRIPTION
# Returns the ith element of a list supposed to be numeric.  
# As \code{liste[[ith]]} but returns \code{numeric(0)} if the length of 
# the list is shorter than \code{ith}.
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{liste}<<the list>>
#{ith}<<number of the element to return>>
#[INPUTS]
#VALUE
# The \code{ith} element of liste if it exists
# \code{numeric(0)} otherwise.
#EXAMPLE
# listev(list(A=123,B=234,C=345),2);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# I was expecting this a property of R!
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_09
#REVISED 07_09_18
#--------------------------------------------
{
if (length(liste) < ith) { return(numeric(0));}
else {
    if (is.null(liste[[ith]])) { return(numeric(0));}
    else { return(liste[[ith]]);}
}
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp3cut <- function(x,pth=matrix(c("(","{","[",")","}","]"),3))
#TITLE  splits an expression into non nested blocks 
#DESCRIPTION
# x must be a character string which is parsed to 
#   extracts \code{n} types of blocks where n is \code{nrow(pth)+1}.
#   Something in between \code{pth[i,1]} and \code{pth[i,2]} is coded \code{i},
#   everything else is coded \code{0}. See the examples.
#DETAILS
# The braces can contain more than one character, they must be
# distinct. Be aware that no check is done about the possible nesting
# of the parentheses.
#PKEYWORDS syntax
#KEYWORDS utilities
#INPUTS
#{x} <<character string to split; must be of length one.>>
#[INPUTS]
#{pth} <<character matrix with two columns defining
# the opening parentheses in the first column and the
# closing parentheses in the second column.>>
#VALUE
# A list of two equal length vectors:
#{$blo} <<The vector of character strings (braces are taken off)>>
#{$typ} <<Corresponding codes of the blocks and 0 when outside of
# any defined block.>>
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# easyp3cut("(a+b)^[2]");
# easyp3cut("abs({{*Y*}})*{{A}}^{{B}}",matrix(c("{{","}}"),1));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_07_11
#REVISED 09_11_05
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(   x,rbsb.chara,1);
    check4tyle(pth,rbsb.chara,-1);
    if (!is.matrix(pth)) {
        erreur(pth,"pth must a character MATRIX with two columns");
    }
    if (ncol(pth)!=2) {
        erreur(pth,"pth must a character matrix with TWO columns");
    }
    ncas <- nrow(pth);
    if (sum(outer(pth[],pth[],"=="))!=2*ncas) { erreur(pth,"ALL tags must be distinct");}
}
# going back to the previous arguments
opar <- pth[,1]; cpar <- pth[,2];
# ncas is the number of different possible types
# of parenthezided blocks (can be 0)
ncas <- nrow(pth);
ics <- rep(NA,ncas);
blo <- character(0);
if (!is.null(x)) { if (length(x)==0) {x <- character(0);}}
chaine <- x;
fini <- FALSE; # indicates the end of the algorithm
nepa <- TRUE;  # indicates if no parenthesis is currently open
if (length(chaine) == 0) { fini <- TRUE;}
# a loop to discover the blocks
while(!fini) {
    if (nepa) {
        # No parenthesis is currently openned, looking for one to be
        # and putting the associated closing parenthesis into what
        # ic gives the position of the first one, -1 if no more.
        for (jd in 1:ncas) {
            # looking for the positions of all opening parenthesis
            ics[jd] <- regexpr(opar[jd],chaine,fixed=TRUE)[1];
        }
        # determining the first one
        if (sum(ics > 0) > 0) {
            ic <- min(ics[ics > 0]);
            qui <- (1:ncas)[ics == ic][1];
            # what is the closing parenthesis to find next
            what <- cpar[qui];
            # lop/lcp is the length of the opening and closing parentheses
            lop <- nchar(opar[qui]);
            lcp <- nchar(cpar[qui]);
        } else {ic <- -1;}
    } else {
        # Looking for the closing parenthesis of the presently openned
        # ic will give its position
        ic <- regexpr(what,chaine,fixed=TRUE)[1];
    }
    #
    if ( ic < 0) {
        # No new block was identified
        blo <- c(blo,chaine);
        fini <- TRUE;
    } else {
        # a block is under discovery
        if (nepa) {
            # the block is just openned
            blo <- c(blo,substr(chaine,1,ic-1));
            chaine <- substr(chaine,ic,nchar(chaine));
            nepa <- FALSE;
        } else {
            # the block must be closed
            blo <- c(blo,substr(chaine,1,ic+lcp-1));
            chaine <- substr(chaine,ic+lcp,nchar(chaine));
            nepa <- TRUE;
        }
    }
} # while(!fini);
# at this level the blocks were cutted with their braces...
# and some additional empty blocks were introduced
# (1) suppressing the empty blocks
bnul <- (blo == "");
blo <- blo[!bnul];
# (2) now coding and preparing the codes and removing the braces
cod <- numeric(0);
if (length(blo) > 0) {
    # removing empty blocks
    for (jbd in length(blo):1) {
        if (blo[jbd] == "") {blo <- blo[-jbd];}
    }
    # identifying each block
    cod <- rep(NA,length(blo));
    for (co in bf(blo)) {
        qqq <- blo[co];
        for (jd in 1:ncas) {
            lop <- nchar(opar[jd]); lcp <- nchar(cpar[jd]);
            if (nchar(qqq)>=(lop+lcp)) {
            if ((substr(qqq,1,lop) == opar[jd]) & (substr(qqq,nchar(qqq)-lcp+1,nchar(qqq))==cpar[jd])) {
                blo[co] <- substr(qqq,lop+1,nchar(qqq)-lcp);
                cod[co] <- jd;
            }}
        }
        if (is.na(cod[co])) {cod[co] <- 0;}
    }
}
#returning
list(blo=blo,typ=cod);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
easyp3stickback <- function(d,pth=matrix(c("(","{","[",")","}","]"),3))
#TITLE  converse operation of easyp3cut
#DESCRIPTION
# d must be a list with $typ and $blo components
# (see easyp3cut for the details)
#DETAILS
#PKEYWORDS
#KEYWORDS utilities
#INPUTS
#{d} <<d$typ for the type of components, 
#      d$blo for the content of each component.>>
#[INPUTS]
#{pth} <<character matrix with two columns defining
# the opening parentheses in the first column and the
# closing parentheses in the second column.>>
#VALUE
# A character(1) of the reconstituted easyprograming
#EXAMPLE
# rbsb3k("RESET"); # for R checking compliance (useless)
# uu <- easyp3cut("abs({Y})*[A]^(B)");
# easyp3stickback(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_03_31
#REVISED 09_11_05
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    if (!("blo" %in% names(d))) {erreur(d,"$blo is missing");}
    if (!("typ" %in% names(d))) {erreur(d,"$typ is missing");}
    if (length(d$typ)!=length(d$blo)) {erreur(d,"$typ & $blo has got != lengths");}
    check4tyle(pth,rbsb.chara,-1);
    if (!is.matrix(pth)) {
        erreur(pth,"pth must a character MATRIX with two columns");
    }
    if (ncol(pth)!=2) {
        erreur(pth,"pth must a character matrix with TWO columns");
    }
}
ncas <- nrow(pth);
if (sum(outer(pth[],pth[],"=="))!=2*ncas) { erreur(pth,"ALL tags must be distinct");}
# going back to the previous arguments
opar <- pth[,1]; cpar <- pth[,2];
# ncas is the number of different possible types of parenthezided blocks (can be 0)
mncas <- max(d$typ);
if (mncas > ncas) {
    erreur(list(opar,d$typ),"Some parentheses are not described into 'opar/cpar' arguments");
}
# reconstituting
nb <- length(d$typ);
res <- "";
for (ii in bf(d$typ)) {
    ty <- d$typ[ii]; bl <- d$blo[ii];
    if (ty==0) {
        res <- paste(res,bl,sep="");
    } else {
        res <- paste(res,opar[ty],bl,cpar[ty],sep="");
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
unex <- function(xv,di,check=TRUE)
#TITLE  collapsing indices
#DESCRIPTION
#   Transforms multiple indices into a unique index
#   following the storage order of array elements
#   by R.
#DETAILS
#KEYWORDS iteration
#INPUTS
#{xv}<<a N by K matrix, each row corresponding to uple of indices.
#      each column must be a factor.>>
#{di} <<dimensions of the K indices>>
#[INPUTS]
#{check}<< must be the consistency between the argument \code{xv} and \code{di}
#                 be checked?>>
#VALUE
# A vector of size N with the unique index values
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# uu <- rbsb.dfr1;
# print(cbind(uu,unex(uu,3:4)));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# Make the inverse function
#AUTHOR J.-B. Denis
#CREATED 07_07_06
#REVISED 08_07_01
#--------------------------------------------
{
K <- ncol(xv);
res <- 0;
if (K > 0) {
    # some checks
    if (check) {
        if (K != length(di)) {erreur(di,"number of indices different!");}
        for (k in 1:K) {
            if (!is.factor(xv[,k])) { erreur(NULL,"index",k,"is not a factor!");}
            if (min(as.numeric(xv[,k])) < 1) {erreur(NULL,"index",k,"is not positive!");}
            if (max(as.numeric(xv[,k])) > di[k]) {erreur(NULL,"index",k,"is greater than the dimension!");}
        }
    }
    # computing the collapsing
    res <- as.numeric(xv[,1]); KK <- di[1];
    if (K > 1) {for (k in 2:K) {
        res <- res + KK*(as.numeric(xv[,k])-1);
        KK <- KK*di[k];
    }}
}
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
comp3interpolation <- function(values,x,xx=seq(min(x),max(x),length=100))
#TITLE  interpolation of known values
#DESCRIPTION From a set of values given at known x,
# interpolation is done for other values (xx). Extrapolation 
# is rejected.
#DETAILS
# \code{x} values can be given in any order.
#KEYWORDS utilities
#INPUTS
#{values}<<values to be interpolated (means y values)>>
#{x}<<abscissae associated to the known values>>
#[INPUTS]
#{xx} <<dates for which values must be found by 
#       linear interpolation.>>
#VALUE
# The resulting values (in the same order that xx)
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_10_08
#REVISED 10_10_12
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(values,"numeric",c(2,Inf),message="argument 'values' not accepted");
    check4tyle(     x,"numeric",c(2,Inf),message="argument 'x' not accepted");
    nbv <- length(values);
    if (length(x)!=nbv) {
        erreur(list(length(values),length(x)),"values and x must have the same lengths");
    }
    check4tyle(xx,"numeric",-1,message="argument 'xx' must be numeric");
}
#
if (length(xx)==0) { return(numeric(0));}
if (!identical(as.numeric(range(x)),as.numeric(range(c(x,xx))))) {
    erreur(list(range(x),range(xx)),"Only interpolation is possible!");
}
# sorting the reference values
oo <- order(x);
values <- values[oo];
x <- x[oo];
# interpoling
res <- rep(NA,length(xx));
nbc <- length(x)-1;
for (clc in bc(nbc)) {
    bi <- x[clc]; bs <- x[clc+1];
    qui <- (bi <= xx) & (xx <= bs);
    if (length(qui) > 0) {
        rr <- values[clc] + (values[clc+1]-values[clc]) * (xx[qui] - bi) / (bs - bi);
        res[qui] <- rr;
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
get8object <- function(whi)
#TITLE  returns an object from a character(2) coding
#DESCRIPTION
# Returns the object associated to two character strings. 
# \code{whi[1]} designates a global variate and \code{whi[2]}
# indicates the way to interpret it.
#DETAILS
# Very similar in spirit to the function \code{get8daf} but
# much less specialized.
#KEYWORDS misc
#INPUTS
#{whi} <<The designation of the object.\cr\cr
# \code{whi[1]} is the name of the object and \code{whi[2]} indicates
# the way interpret it. The different possibilities are:
# \cr When "o", the object is returned as it is (when it does
# not exist, NULL is returned).
# \cr When "c", must be a character.
# \cr When "n", must be a numeric.
# \cr When "d", must be a data.frame.
# \cr When "m", must be a matrix.
# \cr When "a", must be an array.
# \cr When "f", must be a function.
# \cr When "t1", must be a text file to be read with \code{read.table}.
# \cr When "t2", must be a text file to be read with \code{read.csv}.
# \cr When "t3", must be a text file to be read with \code{read.csv2}.
# \cr (when reading a file, the corresponding function is called with
# \code{header=TRUE,comment.char="#"}.)
# >>
#[INPUTS]
#VALUE
# The object. Except when \code{whi[2]=="o"} an error is
# issued if the object does not exist or is not from the 
# expected class.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# get8object(c("rbsb.cha0","o"));
# #will rise an error
# #get8object(c("rbsb.cha0","c"));
# uu <- "Bonjour";
# get8object(c("uu","c"));
# get8object(c("rbsb.dfr1","d"));
# get8object(c("rbsb.fun0","f"));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_04_20
#REVISED 10_04_20
#--------------------------------------------
{
# constant
possibles <- c("any object","a character","a numeric",
               "a data.frame","a matrix","an array",
               "a function",
               "a text file to be read with read.table",
               "a text file to be read with read.csv",
               "a text file to be read with read.csv2"
              );
names(possibles) <- c("o","c","n","d","m","a","f","t1","t2","t3");
# checking
if (rbsb.mck) {
    check4tyle(whi,rbsb.chara,2,message="A character(2) is expected");
    #
    if (!expr3present(whi[2],names(possibles),TRUE)) {
        erreur(list(possibles,whi),"'whi[2]' must belong to the 'names(possibles)'...");
    }
}
# getting the values
if (!exists(whi[1])) {
    if (whi[2]=="o") {
        return(NULL);
    } else {
        erreur(whi,paste("The object 'whi[1]' supposed to be",
                         possibles[whi[2]],"does not exist.",
                         "Remind that NULL objects does not exist."));
    }
} else {
    coda <- paste("res <-",whi[1]);
    eval(parse(text=coda));
    if (whi[2] == "o") {
        # nothing to do
    }
    #
    if (whi[2]=="c") {
        if (!is.character(res)) {
            erreur(whi,"The object 'whi[1]' is not a character.");
        }
    }
    #
    if (whi[2]=="n") {
        if (!is.numeric(res)) {
            erreur(whi,"The object 'whi[1]' is not a numeric.");
        }
    }
    #
    if (whi[2]=="d") {
        if (!is.data.frame(res)) {
            erreur(whi,"The object 'whi[1]' is not a data.frame.");
        }
    }
    #
    if (whi[2]=="m") {
        if (!is.matrix(res)) {
            erreur(whi,"The object 'whi[1]' is not a matrix.");
        }
    }
    #
    if (whi[2]=="a") {
        if (!is.array(res)) {
            erreur(whi,"The object 'whi[1]' is not a array.");
        }
    }
    #
    if (whi[2]=="f") {
        if (!is.function(res)) {
            erreur(whi,"The object 'whi[1]' is not a function.");
        }
    }
    #
    if (whi[2] %in% c("t1","t2","t3")) {
        # the associated file must exist
        if (file.access(whi[1]) < 0) {
            erreur(whi,"File 'whi[1]' does not exist");
        }
    }
    if (whi[2] == "t1") {
	res <- read.table(whi[1],header=TRUE,comment.char="#");
    }
    #
    if (whi[2] == "t2") {
	res <- read.csv(whi[1],header=TRUE,comment.char="#");
    }
    #
    if (whi[2] == "t3") {
	res <- read.csv2(whi[1],header=TRUE,comment.char="#");
    }
    #
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df2ustat <- function(data,
                     quant=c(0.01,0.05,0.10,0.25,0.50,
                             0.75,0.90,0.95,0.99),
                     nbmin=30)
#TITLE  computes univariate statistics from a data frame
#DESCRIPTION
# Data being supposed to be a data frame: observation by rows,
# variables by columns being numeric or factor; standard univariate
# statistics are computed for each variable.
#DETAILS
#PKEYWORDS statistics
#KEYWORDS 
#INPUTS
#{data} <<Date frame containing the data set. NA values are accepted.>>
#[INPUTS]
#{quant} <<(\code{numeric}) The desired quantiles to be computed
#          for the continuous variables.>>
#{nbmin} <<(\code{numeric(1)}) Minimum number of observations required to 
#                compute the statistics for each variable.>>
#VALUE
# a named list with as many components as variables (getting the variable names).\cr
# For numeric variables it is a named vector with standard statistics.\cr
# For categoric variables it is a matrix having a row for each category
# sorted according to the frequencies and four columns: frequencies, rounded 
# percentages, rounded cumulated percentages in both directions (based on the
# level orders.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# set.seed(1234);
# don <- matrix(round(runif(100)*100),20);
# dimnames(don)[[2]] <- LETTERS[1:5];
# print(df2ustat(as.data.frame(don),quant=c(0,0.5,1),nbmin=5));
# set.seed(1234);
# don <- matrix(letters[1+round(runif(100)*10)],20); 
# dimnames(don)[[2]] <- LETTERS[1:5];
# print(df2ustat(as.data.frame(don),nbmin=5));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
# use for arguments function of the style of the present \code{ustat}.
#AUTHOR J.-B. Denis
#CREATED 08_10_05
#REVISED 10_03_10
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(quant,"numeric",-1,message="'quant' must be numeric");
    if ((min(quant) < 0) |
        (max(quant) > 1)) {
        erreur(quant,"Some values of 'quant' are not probabilities");
    }
    check4tyle(nbmin,"integer",1,message="'nbmin' must be integer");
    nbmin <- abs(nbmin);
    if (!is.data.frame(data)) {
        erreur(data,"Must be a data frame!");
    }
}
# preparing the returned list
nbv <- length(data);
res <- vector("list",nbv);
names(res) <- names(data);
# quantile names
qna <- as.character(round(quant*100,1));
qna <- paste("Q",qna,"%",sep="");
qna[quant==0.5] <- "Median";
# standard names
cona <- c("number","missing",
          "mean","std-dev.","skewness","kurtosis",
          "min","MAX",
          qna);
cana <- c("Counts","Percentage","Cum.Per","cuM.per");
# computing for each variable
for (vv in bc(nbv)) {
    if (is.numeric(data[[vv]])) {
        # continuous case
        res[[vv]] <- rep(NA,length(cona));
        names(res[[vv]]) <- cona;
        res[[vv]]["number"] <- sum(!is.na(data[,vv]));
        res[[vv]]["missing"] <- sum(is.na(data[,vv]));
        res[[vv]]["min"] <- min(data[,vv],na.rm=TRUE);
        res[[vv]]["MAX"] <- max(data[,vv],na.rm=TRUE);
        if (res[[vv]]["number"] >= nbmin) {
            res[[vv]]["mean"] <- mean(data[,vv],na.rm=TRUE);
	    res[[vv]]["std-dev."] <- sqrt(var(data[,vv],na.rm=TRUE));
	    res[[vv]]["skewness"] <- skewness(data[,vv],na.rm=TRUE);
	    res[[vv]]["kurtosis"] <- kurtosis(data[,vv],na.rm=TRUE);
            pq <- 8 + bf(quant);
	    res[[vv]][pq] <- quantile(data[,vv],quant,na.rm=TRUE);
	}
    }
    if (is.factor(data[[vv]])) {
        # categoric case
	nbni <- length(levels(data[[vv]]));
	res[[vv]] <- matrix(0,nbni,4);
	cou <- sort(table(data[[vv]]));
	dimnames(res[[vv]]) <- list(names(cou),cana);
	res[[vv]][,1] <- cou;
	res[[vv]][,2] <- round(cou*100/sum(cou));
	res[[vv]][,3] <- round(cumsum(cou*100/sum(cou)));
	res[[vv]][,4] <- rev(round(cumsum(rev(cou*100/sum(cou)))));
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df2bstat <- function(data,nbmin=80)
#TITLE  computes bivariate statistics from a data frame
#DESCRIPTION
# Data being supposed to be a data frame: observations by rows,
# variables by columns being numeric or factor; standard bivariate
# statistics are computed for each couple of variables.
#DETAILS
#PKEYWORDS statistics
#KEYWORDS 
#INPUTS
#{data} <<Date frame containing the data set. NA values are accepted.>>
#[INPUTS]
#{nbmin} <<(\code{numeric(1)}) Minimum number of observations required to 
#                compute the statistics for each variable couple.>>
#VALUE
# a symmetric matrix having rows and columns associated to the variables.
# When the couple is numeric/numeric: the Pearson correlation; 
# categoric/categoric: the first eigen values of {n_ij/(n_i+.n_+j)};
# numeric/categoric: the Pearson correlation giving to the categoric the
# values of mean of the numeric.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# set.seed(1234);
# don <- as.data.frame(matrix(round(runif(100)*10),20));
# names(don) <- LETTERS[1:5];
# don[[4]] <- as.factor(don[[4]]);
# don[[5]] <- as.factor(don[[5]]);
# df2bstat(don,5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
# The underlying theory for the non standard correlations is
# to be established
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_12_29
#REVISED 09_12_29
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(nbmin,"integer",1,message="'nbmin' must be integer");
    nbmin <- abs(nbmin);
    if (!is.data.frame(data)) {
        erreur(data,"Must be a data frame!");
    }
}
# preparing the returned matrix
nbv <- length(data);
res <- matrix(NA,nbv,nbv);
dimnames(res) <- list(names(data),names(data));
# computing the 'correlations'
for (v1 in bc(nbv)) { for (v2 in bc(nbv)) {
    if (v1==v2) { res[v1,v2] <- 1;}
    if (v1 <v2) {
        rho <- NA;
        V1 <- data[[v1]]; V2 <- data[[v2]];
        ou1 <- !is.na(V1); ou2 <- !is.na(V2);
        ou <- which(ou1 & ou2);
        if (length(ou)>=nbmin) {
            if (is.numeric(V1) & is.numeric(V2)) {
                rho <- cor(V1[ou],V2[ou]);
            }
            if (is.factor(V1) & is.factor(V2)) {
                NN <- table(V1[ou],V2[ou]);
                nl <- nrow(NN); nc <- ncol(NN);
                nn <- sum(NN)*(solve(diag(apply(NN,1,sum),nl,nl)) %*%
                               NN %*%
                               solve(diag(apply(NN,2,sum),nc,nc))
                              );
                dd <- svd(nn)$d;
                rho <- dd[1]/sum(dd);
            }
            VN <- "non";
            if (is.numeric(V1) & is.factor(V2)) { VN <- V1[ou]; VF <- V2[ou];}
            if (is.numeric(V2) & is.factor(V1)) { VN <- V2[ou]; VF <- V1[ou];}
            if (is.numeric(VN)) {
                # computing the mean by level
                scsc <- rep(NA,length(levels(VF)));
                for (ll in bf(levels(VF))) {
                    scsc[ll] <- mean(VN[VF==levels(VF)[ll]],na.rm=TRUE);
                }
                # creating the corresponding numerical vector
                VFN <- scsc[as.numeric(VF)];
                rho <- cor(VN,VFN);
            }
        }
        res[v1,v2] <- rho;
        res[v2,v1] <- res[v1,v2];
    }
}}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check8list <- function(lili,nana)
#TITLE  checks the components of a list
#DESCRIPTION
# checks that all \code{nana} are names of the components of
# the list \code{lili}.
#DETAILS
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{lili}<<(\code{list}). The list the names of which have to be checked.>>
#{nana}<<(\code{character}). The names to be found.>>
#[INPUTS]
#VALUE
# Nothing but an error is issued if the check
# is not positive.
#EXAMPLE
#REFERENCE 
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_11_09
#REVISED 09_11_09
#--------------------------------------------
{
# checking
nini <- names(lili);
for (ii in nana) {
    if (!expr3present(ii,nini)) {
        erreur(list(ii,nini),"'ii' was not found in 'names(lili)'");
    }
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
comp3dtriang <- function(ddel,ste=1,pos=TRUE)
#TITLE  returns the discrete density of a triangular distribution
#DESCRIPTION
# returns the discrete density of a triangular distribution.
# More precisely returns a list with \code{$xval} indicating
# the discretized values and \code{$prop} the associated
# probabilities. When \code{ddel} and 
# \code{ste} are integer, the \code{$xval} are
# also integer.
#DETAILS
# The function \code{seq} is used to compute \code{$xval}.
#KEYWORDS misc
#PKEYWORDS 
#INPUTS
#{ddel}<<(\code{numeric(3)}) Definition of the distribution 
# (start, mode and end abscissae).>>
#[INPUTS]
#{ste} <<(\code{numeric(1)}) Definition of the
# progression between two values.>>
#{pos} <<(\code{logical(1)}) When TRUE the interval
# is first expanded of \code{ste} each side to give
# non null \code{$xval}.>>
#VALUE
# A list with two components. \code{$xval} gives
# the values; \code{$prop} gives the proportion
# to apply to each of them (\code{sum($prop)=1}.
#EXAMPLE
#REFERENCE 
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 09_11_13
#REVISED 09_11_13
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(ddel,"numeric",3,message="Bad delay");
    if (!all(diff(ddel)>=0)) {erreur(ddel,"must be non decreasing");}
    if (all(diff(ddel)==0)) {erreur(ddel,"cannot be constant");}
    check4tyle(ste, "numeric",1,message="Bad ste");
    if (ste<0) {erreur(ste,"must not be negative");}
    check4tyle(pos, "logical",1,message="Bad pos");
}
# dealing with pos
if (pos) {
    ddel[1] <- ddel[1] - ste;
    ddel[3] <- ddel[3] + ste;
}
# computing the xval
xval <- seq(ddel[1],ddel[3],ste);
# computing the prop
if (ddel[1]!=ddel[2]) {
    asc <- (xval-ddel[1]) / (ddel[2]-ddel[1]);
} else {
    asc <- rep(Inf,length(xval));
}
if (ddel[3]!=ddel[2]) {
    des <- (xval-ddel[3]) / (ddel[2]-ddel[3]);
} else {
    des <- rep(Inf,length(xval));
}
prop <- pmin(asc,des);
prop <- prop / sum(prop);
# dealing with pos
if (pos) {
    use <- 2:(length(prop)-1);
    prop <- prop[use];
    xval <- xval[use];
}
# returning
list(xval=xval,prop=prop);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
comp3convolu <- function(pro1,pro2=pro1)
#TITLE  computes a convoluted distribution
#DESCRIPTION
# From two probability vector associated to
# sequential integers starting from ZERO,
# returns the probability of the sum.
#DETAILS
#PKEYWORDS helpful
#KEYWORDS iteration
#INPUTS
#{pro1}    <<Probability vector of the first variable.
# needs not to be normalized.>>
#[INPUTS]
#{pro2}    <<As \code{pro1} for the second variable>>
#VALUE
# A named probability vector of length \code{length(pro1)+length(pro2)-1}.
# The names are the values of the sum.
#EXAMPLE
# comp3convolu(1:10);
# comp3convolu(comp3convolu(0:5),0:5);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_01_21
#REVISED 10_10_12
#--------------------------------------------
{
# checking
if (any(pro1 < 0)) {
    erreur(pro1,"Probability cannot be negative");
}
if (any(pro2 < 0)) {
    erreur(pro2,"Probability cannot be negative");
}
res <- rep(NA,length(pro1)+length(pro2)-1);
ppro <- outer(pro1,pro2);
iind <- outer(1:length(pro1),1:length(pro2),"+") - 1;
for (irr in bf(res)) {
    res[irr] <- sum(ppro[iind==irr]);
}
# naming with the values
names(res) <- as.character(bf(res)-1);
# returning
res/sum(res);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
numero <- function(qui,dans,strict=TRUE,repe=TRUE)
#TITLE  returns the positions of identifiers
#DESCRIPTION
#  Returns the positions of identifiers.
# From a reference set of
# identifiers returns the positions of some of them.
# If they don't belong to the list, an error is issued
# according to \code{strict}. If there are repetitions
# an error is issued or not according to \code{repe}.
#DETAILS
#PKEYWORDS helpful
#KEYWORDS
#INPUTS
#{qui}    <<Identifiers the position of them is looked for.
# Must be a character or a numeric.>>
#{dans}    <<The ordered vector of all identifiers.
# Must be a character or a numeric.>>
#[INPUTS]
#{strict} <<Must \code{qui} be a subset of \code{dans}.>>
#{repe} <<Are repeatitions allowed?>>
#VALUE
# A numeric vector of the positions. It is named with the identifiers.
# When the identifier does not belong to the list and \code{strict} is
# FALSE, NA values are returned.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# numero(10:1,1:10);
# numero(c("Z","E","P","L","I","N"),LETTERS);
# numero(c("B","o","F","!"),LETTERS,strict=FALSE);
# numero(c("B","E","B","E"),LETTERS);
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
if (rbsb.mck) {
    check4tyle(qui,   c(rbsb.chara,"numeric"),-1,message="   'qui' is not valid");
    check4tyle(dans,  c(rbsb.chara,"numeric"),-1,message="  'dans' is not valid");
    check4tyle(strict,c("logical"),            1,message="'strict' is not valid");
}
qui  <- as.character(qui);
dans <- as.character(dans);
if (strict) {
    uu <- union(qui,dans);
    if (length(uu)!=length(dans)) {
        erreur(list(setdiff(uu,dans),dans),"Some identifiers does not belong to the list");
    }
}
# computing
res <- as.numeric(outer(qui,dans,"==")%*%bf(dans));
names(res) <- qui;
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
ss <- function(file,
              message=rbsb.cha0,directory=rbsb.cha0,
              ...)
#TITLE  sources an R file
#DESCRIPTION
# Just sourcing \code{file} after adding a \code{directory}
# path and emmetting a \code{message}.
#DETAILS
#PKEYWORDS
#KEYWORDS
#INPUTS
#{file} << The R file to be sourced.>>
#[INPUTS]
#{message} <<An indicating message to display, possibly with a pause
# according to \code{rbsb.mba}.>>
#{directory} << The directory path to add before the file.>>
#{\dots} <<Further arguments to be passed to the \code{source} function.>>
#{message} << Message to display on the terminal.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 10_03_16
#REVISED 10_03_16
#--------------------------------------------
{
# the file to be sourced
if (isempty(directory)) { fifi <- file;
} else { fifi <- paste(directory,file,sep="/");}
# sourcing
source(fifi,...);
# the message
if (!isempty(message)) {
    if (!rbsb.mba) { pause(message,"pause from s(ource)");
    } else { cat("<<<",message,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
file2char <- function(file,path="./",
                      clean=TRUE,
                      ended=")STOP(",
                      comme="#",
                      skip=matrix(c(")START_SKIPPING(",")END_SKIPPING(",
                                    "/*","*/"),ncol=2,byrow=TRUE),
                      include=c(")FILE(")
                     )
#TITLE  reads a file and transforms it in a single character
#DESCRIPTION
# reads a conveniently tagged file to produce a vector of character
# where all non used lines are eliminated. Each component of the resulting
# vector is associated to an original line.
#DETAILS
# Performed actions are : (i) eliminating commented lines, (ii) eliminating
# lines after a 'stop', (iii) including indicated files, (iv) skipping 
# sequences of lines and (v) cleaning the line, i.e. removing starting and
# ending spaces.\cr
# All tags are supposed to be in the first position of the line after
# cleaning when asked.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{file} << file which have to be read and transformed into a list.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{clean} <<Indicates if starting and ending spaces must be eliminated at first.>>
#{ended} << To indicate the line from which to stop the reading.>>
#{comme} <<At the beginning of a line, it indicates that this line must not be
#          considered. More than one commenting character can be considered when
#          it is a vector. For instance \code{c("#","%")} means that
#          lines starting with an hash or a percent are comment lines.>>
#{skip} << To indicate set(s) of lines to be skipped. Must be a character matrix
#          where the two columns correspond respectively to the opening and 
#          closing tags, and where each row is associate to a couple of tags.
#          Tags are considered successively following the order of these matrix rows.>>
#{include} << Tags to indicate a file (including possible path) by
#             a \code{character(1)} to include at this point its contents
#             as a text file with the same tags specifications.
#             Including files can be recursive. >>
#VALUE
# a character of length, the number of retained lines.
#EXAMPLE
# rbsb3k("reset"); # for R checking convenience
# sink("rbsb.char.txt")
# cat("# comments can be included as well\n")
# cat(" something\n");
# cat("/* skipping from here\n");
# cat("blablabla\n");
# cat("  */ until here (this line is ALSO eliminated\n");
# cat(" interesting:\n");
# cat("un dos tres\n");
# cat(")STOP(\n");
# cat(" It is finished!\n");
# cat(" Don't insist!\n");
# sink();
# file2char("rbsb.char.txt");
# unlink("rbsb.char.txt");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 10_09_13
#--------------------------------------------
{
#
# checking
if (rbsb.mck) {
    check4tyle(file,rbsb.chara,1,message="file must indicate the name of one file");
    #
    check4tyle(path,rbsb.chara,1,message="path must indicate the name of one directory");
    #
    if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
    if (is.na(file.info(fifi)$size)) { erreur(fifi,"This file seems not to exist.");}
    #
    check4tyle(clean,"logical",1,message="clean must be a logical(1)");
    #
    check4tyle(ended,rbsb.chara,1,message="ended must be a character of length one");
    #
    check4tyle(comme,rbsb.chara,-1,message="comme must be a character");
    #
    check4tyle(skip,rbsb.chara,c(2,Inf),message="skip must be a CHARACTER matrix with two columns");
    if (!is.matrix(skip)) {erreur(skip,"skip must be a character MATRIX of two columns");}
    if (ncol(skip)!=2) {erreur(skip,"skip must be a character matrix OF TWO COLUMNS");}
    #
    check4tyle(include,rbsb.chara,1,message="include must be a character of length one");
    #
}
#
# reading the proposed file
if (path!="") {file <- paste(path,file,sep="/");}
lu <- readLines(file);
#
# cleaning
if (clean) { for (ii in bf(lu)) {
    # removing framing spaces
    lu[ii] <- form3decadre(lu[ii]," "," ");
}}
#
# removing the lines after a possible stop
sto <- which(substr(lu,1,nchar(ended))==ended);
if (length(sto)>0) {
    lu <- lu[1:(sto[1]-1)];
}
#
# removing the empty lines
lu <- lu[nchar(lu)>0];
#
# removing the commented lines
for (ii in bf(comme)) {
    debu <- substr(lu,1,nchar(comme[ii]));
    lu <- lu[debu!=comme[ii]];
}
#
# removing the skipped lines
for (nn in bc(nrow(skip))) {
    deb <- skip[nn,1]; fin <- skip[nn,2];
    debu <- substr(lu,1,nchar(deb)) == deb;
    fini <- substr(lu,1,nchar(fin)) == fin;
    sk1 <- apply(outer(which(debu)  ,bf(lu),"<="),2,sum);
    sk2 <- apply(outer(which(fini)+1,bf(lu),"<="),2,sum);
    nsk <- ((sk1-sk2) < 1);
    lu <- lu[nsk];
}
#
# including the indicated files
inclus <- which(substr(lu,1,nchar(include))==include);
for (ii in bf(inclus)) {
    jj <- length(inclus) + 1 - ii;
    kk <- inclus[jj];
    fi <- strsplit(lu[kk]," ",fixed=TRUE)[[1]][2];
    plus <- Recall(fi,path=path,
                      clean=clean,
                      ended=ended,
                      comme=comme,
                      skip=skip,
                      include=include);
    plus <- c(lu[bc(kk-1)],plus);
    if (kk < length(lu)) { plus <- c(plus,lu[(kk+1):length(lu)]);}
    lu <- plus;
}
#
# returning
lu;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2list <- function(chara,
                      tags=rbsb.tag1,
                      sep=rbsb.sep0,
                      rsep="no_action",
                      stag=c("/","/")
                     )
#TITLE  transforms a character into a list (of lists) of characters
#DESCRIPTION
# from a conveniently tagged character vector, returns nested lists.
# Most often such a character is obtained by reading a file with
# the function \code{file2char}, this explains why it is convenient
# to speak about 'lines' rather about 'components of the character
# vector'.\cr
# All lists are named lists and the tags give the names of their
# component. The maximum number of nested levels of lists is given by the
# number of rows of the matrix \code{tags}. The corresponding two columns
# give the opening and closing sequence of the tags. Final lists 
# contain \code{character} vectors, each component of them being
# on the same line and/or on the following line (before a new tag).\cr
# All tags must start at the very beginning of a line. All separator
# tags must be used sticked to the list tag.
#DETAILS
# It is compulsory to tag each level of the lists, this implies that 
# when starting a new list, a character vma (see \code{char2vma}
# for the details: vma means vector or matrix or array) is provided meaning 
# that this is the final level for this branch, or a new sublist 
# is started with the corresponding tag, or a new component of the
# list is given at a level less or equal to the present.\cr
# Separator between different character component is given by the
# \code{sep} argument or indicated by the separator tag (\code{stag}), 
# in the last case, it can be different from a leaf list to another.\cr
# Be aware that before reading a line for a character translation,
# all starting and ending spaces are eliminated.\cr
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{chara} << character vector to be transformed into a list.>>
#[INPUTS]
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row numbers gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the character vectors of every
# line. Notice that \code{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \code{sep} must be considered as 
#          signicant or not and which null value to introduce.
#          If \code{no_action} then the repetitions will be ignored
#          if not \code{rsep} component(s) will be introduced.>>
#{stag} << Two character strings indicating the tag to define different \code{sep} for a given
#           [sub]list. These two correspond to \code{stag[c(1,3)]} of \code{list2file} function.>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
# or matrices or arrays.
#EXAMPLE
# rbsb3k("reset"); # for R checking convenience
# chara <- c("<<A>>",
#            "[[a]]/*/v 1*un deux trois",
#            "[[b]]/*/v 1*2*3",
#            "un uno one",
#            "deux dos two",
#            "trois tres three",
#            "<<B>>",
#            "[[a]] un deux trois",
#            "[[b]] un  uno  one",
#            " deux dos two",
#            "trois tres three",
#            "<<C>> 1 2 3");
# char2list(chara);
#REFERENCE
#SEE ALSO \code{char2file}, \code{file2list}.
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 10_09_13
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(chara,rbsb.chara,-1,message="chara must be a character");
    #
    check4tyle(tags,rbsb.chara,c(2,Inf),message="tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {erreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {erreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { erreur(tags,"Opening tags are not unique!");}
    #
    check4tyle(sep,rbsb.chara,1,message="sep must indicate one string character");
    if (nchar(sep) == 0) { erreur(sep,"sep must not be an empty string");}
    #
    check4tyle(rsep,rbsb.chara,1,message="rsep must be a character(1)");
    #
    check4tyle(stag,rbsb.chara,2,message="stag must be a character of length two");
    if (any(nchar(stag) == 0)) { erreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag)) > 0) { erreur(stag,"stag must not comprise spaces");}
    #
}
#
#
# getting the tagged lines 
tagged <- matrix(NA,0,6);
dimnames(tagged) <- list(NULL,c("num","lev","name","leaf","sep","type"));
#
# tagged will contain all useful informations about the tags.
#    num: line number into chara
#    lev: level number of the tag
#    name: name of the tag
#    leaf: "yes" when final branch, "no" if not. The last
#          two columns will be significant only for leaf sub-list.
#    sep: separator string to be used to get the vma character
#    type: indicates the type of the associated sub-sub-...-sub list
#          we are dealing with : "lists" indicates that more
#          levels are present, if not it is a leaf of the structure
#          and \code{rbsb.vma["c","v","V","w","m",...]} one of the type of vma character
#          is expected. 
nbtag <- 0;
for (i in bf(chara)) {
    lev <- 0;
    # looking for a tag
    for (j in bc(nrow(tags))) { if (lev == 0) {
        pot <- form3decadre(chara[i],tags[j,1],"");
        if (nchar(pot) < nchar(chara[i])) { lev <- j;}
    }}
    if (lev > 0) {
        # some tag was perhaps identified looking for the name
        put <- strsplit(pot,tags[lev,2],fixed=TRUE)[[1]];
        if (nchar(put[1]) < nchar(pot)) {
            # indeed it was a tag
            fch <- substr(put[2],1,1);
            if (fch %in% c(""," ",NA)) {
                # default tags and separator
                ssep <- sep;
                typing <- rbsb.vma["v"];
                chara[i] <- put[2];
            } else {
                # getting the possible explicit separator
                if (substr(put[2],1,nchar(stag[1]))==stag[1]) {
                    # looking for the separator
                    pit <- form3decadre(put[2],stag[1]);
                    pat <- strsplit(pit,stag[2],fixed=TRUE)[[1]];
                    ssep <- pat[1];
                    pyt <- pat[2];
                } else {
                    # default separator
                    ssep <- sep;
                    pyt <- put[2];
                }
                # getting the possible information
                ouou <- strsplit(pyt," ",fixed=TRUE)[[1]];
                if (length(ouou) > 1) {
                    chara[i] <- paste(ouou[-1],collapse=" ");
                } else {
                    chara[i] <- "";
                }
                pyt <- ouou[1];
                # getting the type of the tag
                tta <- which(pyt==rbsb.vma);
                if (length(tta)==0) {
                    # default type
                    typing <- rbsb.vma["v"];
                } else {
                    # explicite type
                    typing <- names(rbsb.vma)[tta];
                }
            }
        tagged <- rbind(tagged,c(i,lev,put[1],"???",ssep,typing));
        } # end of nchar(put[1]) < nchar(pot)
    } # end of lev > 0
} # end of i in bf(chara)
#
#
# in case of no list
if (nrow(tagged) == 0) { return(vector("list",0));}
# detecting the final lists
tagged[,"leaf"] <- "yes";
for (ii in bc(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if (lev2-lev1==1) { tagged[ii,"leaf"] <- "no";}
}
#
#   matrix 'tagged' is now available
#
# checking the sequential progress of the levels
for (ii in bc(nrow(tagged)-1)) {
    lev1 <- as.numeric(tagged[  ii,"lev"]);
    lev2 <- as.numeric(tagged[ii+1,"lev"]);
    if ((lev2-lev1) > 1) {
        erreur(tagged,"The progression of levels is not accepted: all levels must be introduced");
    }
}
# checking that no leaf tags are followed by another tag
for (ii in bc(nrow(tagged)-1)) { if (tagged[ii,"leaf"]=="no") {
    actuel <- as.numeric(tagged[  ii,"num"]);
    suivan <- as.numeric(tagged[ii+1,"num"]);
    if ((suivan-actuel) != 1) {
        erreur(list(tagged,ii),"In line 'ii', a 'no-leaf' tags is not immediately followed by another tag");
    }
}}
#
if (tagged[1,"lev"] != "1") {
    erreur(tagged,"The first level must be 1");
}
# replacing the introduced NA
for (ii in bf(chara)) { if (is.na(chara[ii])) { chara[ii] <- "";}}
#
#
# constructing the character strings associated to each leaf list
#
#
oul1 <- which(tagged[,"leaf"]=="yes");
oul2 <- as.numeric(tagged[oul1,"num"]);
lulu <- character(length(oul1));
oul2 <- c(oul2,length(chara)+1);
for (ss in bf(lulu)) {
    leaf <- oul1[ss];
    ssep <- tagged[leaf,"sep"];
    for (sss in oul2[ss]:(oul2[ss+1]-1)) {
        nou <- form3decadre(chara[sss]," ");
        if (nchar(nou)>0) {
            if (nchar(lulu[ss])>0) {  
                # here multiple lines are concatenated
                lulu[ss] <- paste(lulu[ss],nou,sep=ssep);
            } else {
                lulu[ss] <- nou;
            }
        }
    }
}
#
# building the list of lists...
res <- vector("list",0);
vari <- "res"; niv <- 0; ulul <- 0;
for (ll in bc(nrow(tagged))) {
    # getting the name
    leve <- as.numeric(tagged[ll,"lev"]);
    if (niv<leve) {
        if (niv+1!=leve) { rapport("Error(1) found into 'file2test'");}
        vari <- paste(vari,"[['",tagged[ll,"name"],"']]",sep="");
    }
    if (niv>=leve) {
        va <- strsplit(vari,"[['",fixed=TRUE)[[1]];
        vari <- paste(va[1:leve],collapse="[['");
        nom <- tagged[ll,"name"];
        #???if (nom == "<NA>") { nom <- NULL;}
        vari <- paste(vari,"[['",nom,"']]",sep="");
    }
    niv <- leve;
    # filling the value
    if (tagged[ll,"leaf"]=="yes") {
        ulul <- ulul+1;
        jsep <- tagged[ll,"sep"];
        # getting the character vector
        choc <- strsplit(lulu[ulul],jsep,fixed=TRUE)[[1]];
	if (rsep=="no_action") {
	    choc <- choc[choc!=""];
	}
        # building the code to interpret the character vector
        chacha <- char2vma(choc,tagged[ll,"type"],rbsb.sep1);
        eval(parse(text=paste(vari,"<- chacha")));
    } else {
        eval(parse(text=paste(vari,"<- vector('list',0)")));
    }
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
file2list <- function(file,path="./",
                      clean=TRUE,ended=")STOP(",comme="#",
                      skip=matrix(c(")SKIPPING(",")READING("),1),
                      include=c(")FILE("),
                      tags=rbsb.tag1,
                      sep=rbsb.sep0,
                      rsep="no_action",
                      stag=c("/","/")
                     )
#TITLE  reads a file and transforms it in a list (of lists) of characters
#DESCRIPTION
# reads a conveniently tagged file into nested lists.
# It is just the linking of the two functions \code{file2char}
# and \code{char2list}, see their comments for the description of the arguments.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{file} << file to be read and transformed into a list.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{clean} <<Indicates if starting and ending spaces must be eliminated at first.>>
#{ended} << To indicate the line from which to stop the reading.>>
#{comme} <<At the beginning of a line, it indicates that this line must not be
#          considered. More than one commenting character can be considered when
#          it is a vector. For instance \code{c("#","%")} means that
#          lines starting with an hash or a percent are comment lines.>>
#{skip} << To indicate set(s) of lines to be skipped. Must be a character matrix
#          where the two columns correspond respectively to the opening and 
#          closing tags, and where each row is associate to a couple of tags.
#          Tags are considered successively following the order of these matrix rows.>>
#{include} << Tags to indicate a file (including possible path) by
#             a \code{character(1)} to include at this point its contents
#             as a text file with the same tags specifications.
#             Including files can be recursive. >>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row numbers gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the character vectors of every
# line. Notice that \code{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \code{sep} must be considered as 
#          signicant or not and which null value to introduce.
#          If \code{no_action} then the repetitions will be ignored
#          if not \code{rsep} component(s) will be introduced.>>
#{stag} << Two character strings indicating the tag to define different \code{sep} for a given
#           [sub]list. These two correspond to \code{stag[c(1,3)]} of \code{list2file} function.>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
# or matrices or arrays.
#EXAMPLE
# rbsb3k("reset"); # for R checking convenience
# sink("rbsb.list.txt")
# cat("# comments can be included as well\n")
# cat("<<A>>\n");
# cat("[[a]]/*/v 1*un deux trois\n");
# cat("[[b]]/*/v 1*2*3\n");
# cat("un uno one\n");
# cat("deux dos two\n");
# cat("trois tres three\n");
# cat("<<B>>\n");
# cat("[[a]] un deux trois\n");
# cat("# the following three are interesting\n");
# cat("[[b]] un  uno  one\n");
# cat(" deux dos two\n");
# cat("trois tres three\n");
# cat("<<C>> 1 2 3\n");
# sink();
# file2list("rbsb.list.txt");
# unlink("rbsb.list.txt");
#REFERENCE
#SEE ALSO \code{list2file}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 08_12_10
#REVISED 10_09_13
#--------------------------------------------
{
# everything, including the check are deported
# to the called functions
#
# from file to character
res <- file2char(file,
                 path=path,clean=clean,ended=ended,
                 comme=comme,skip=skip,
                 include=include
                );
#
# from character to list
res <- char2list(res,                      #
                 tags=tags,sep=sep,
                 rsep=rsep,stag=stag
                );

# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8list <- function(file,path="./",bullet=")BULLETS(",
                      clean=TRUE,ended=")STOP(",comme="#",
                      skip=matrix(c(")SKIPPING(",")READING("),1),
                      include=c(")FILE("),
                      tags=rbsb.tag1,
                      sep=rbsb.sep0,
                      rsep="no_action",
                      stag=c("/","/")
                     )
#TITLE  reads a file and transforms it in a list (of lists) of characters
#DESCRIPTION
# This is a slight generalization of \code{file2list} in the
# sense that the component of the list have not to be given
# in the file but are determined by the function from an additionnal
# line (everywhere in the file) tagged with the second argument
# starting at the first character.
# This line contains the list of bullets, giving then their number.
# Notice that all proposed bullets must be different from \code{rbsb.l00}
# which can be modified if of use.\cr
# It is worth explaining why such a function was written.
# The obvious one is that not always names for the components of the
# list are natural. In future version, it could be good to mixed with
# and without names. Another reason is that the contraint of 'value
# only at the final level' is not more natural. \code{read8list} adds
# itself 'null' components (named \code{rbsb.l00})
# to allow it. Finally, levels can be skipped
# and missing levels are introduced.\cr
# Nevertheless, everything has to be paid: now the constraint is
# that every component must have a non empty content.
#DETAILS
# An intermediary file is generated (and not deleted) to be read by
# \code{file2list} function; its names is given by the global constant 
# \code{rbsb.fin}.\cr
# The numbering of the each level of the list is determined by cycling the
# global constant \code{rbsb.tly}.\cr
# Only the first three arguments plus the tags are used by this function, all others are
# directly passed to \code{file2list}.\cr
# For the moment, the numbering of the different levels does not take 
# into account the level hierarchy, this must be offered in
# next versions of this function.\cr
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{file} << file to be read and transformed into a list.>>
#[INPUTS]
#{bullet} <<Indicates the bullets to be considered. This tags must
#           be at the very beginning of a line. But the bullets
#           can be preceeded by spaces.>>
#{path} << Directory containing the file.>>
#{clean} <<Indicates if starting and ending spaces must be eliminated at first.>>
#{ended} << To indicate the line from which to stop the reading.>>
#{comme} <<At the beginning of a line, it indicates that this line must not be
#          considered. More than one commenting character can be considered when
#          it is a vector. For instance \code{c("#","%")} means that
#          lines starting with an hash or a percent are comment lines.>>
#{skip} << To indicate set(s) of lines to be skipped. Must be a character matrix
#          where the two columns correspond respectively to the opening and 
#          closing tags, and where each row is associate to a couple of tags.
#          Tags are considered successively following the order of these matrix rows.>>
#{include} << Tags to indicate a file (including possible path) by
#             a \code{character(1)} to include at this point its contents
#             as a text file with the same tags specifications.
#             Including files can be recursive. >>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row numbers gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the character vectors of every
# line. Notice that \code{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \code{sep} must be considered as 
#          signicant or not and which null value to introduce.
#          If \code{no_action} then the repetitions will be ignored
#          if not \code{rsep} component(s) will be introduced.>>
#{stag} << Two character strings indicating the tag to define different \code{sep} for a given
#           [sub]list. These two correspond to \code{stag[c(1,3)]} of \code{list2file} function.>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
# or matrices or arrays.
#EXAMPLE
# rbsb3k("reset"); # for R checking convenience
# sink("rbsb.list.txt")
# cat(")BULLETS( * + -\n")
# cat("* FIRST LEVEL 1\n");
# cat(" + Second Level 1\n");
# cat("   - third level 1\n");
# cat("   - third level 2\n");
# cat(" + Second Level 2\n");
# cat(" + Second Level 3\n");
# cat("* FIRST LEVEL 2\n");
# cat(" + Second Level 1\n");
# cat(" + Second Level 2\n");
# cat("   - third level 1\n");
# cat("   - third level 2\n");
# cat("* FIRST LEVEL 3\n");
# sink();
# read8list("rbsb.list.txt");
# unlink("rbsb.list.txt");
# unlink(rbsb.fin);
#REFERENCE
#SEE ALSO \code{list2file}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 11_01_17
#REVISED 11_01_17
#--------------------------------------------
{
# everything, including the check are deported
# to the called functions
#
#
# checking
if (rbsb.mck) {
  check4tyle(file,rbsb.chara,1,message="file must indicate the name of one file");
  #
  check4tyle(path,rbsb.chara,1,message="path must indicate the name of one directory");
  #
  if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
  if (is.na(file.info(fifi)$size)) { erreur(fifi,"This file seems not to exist.");}
  }
#
# reading the file to get the possible bullet
if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
uu <- readLines(fifi);
bu <- grep(bullet,uu,fixed=TRUE);
#
# possibly transforming the character
if (length(bu)>=1) {
  # analyzing the bullets
  bubu <- uu[bu[length(bu)]];
  nabu <- form3etsil(bubu,OPA="",CPA="",opa="",cpa="",sep=" ")[-1];
  if (rbsb.l00 %in% nabu) {
    erreur(list(rbsb.l00,nabu),
           "The proposed bullets comprise the standard bullet defined by 'rbsb.l00', this is not possible");
  }
  lebu <- nchar(nabu);
  nbul <- length(nabu);
  uu <- uu[-bu];
  # getting the replacements
  if (nbul>nrow(tags)) {
    erreur(list(tags,nabu),"More levels of the list are given that the number of provided tags!");
  }
  debu <- rep(tags[,1],nbul)[1:nbul];
  finn <- rep(tags[,2],nbul)[1:nbul];
  nunu <- rep(rbsb.tyl,nbul)[1:nbul];
  orajou <- numeric(0);
  lrajou <- numeric(0);
  qrajou <- character(0);
  for (bb in bc(nbul)) {
    # dealing the bullets of level bb
    lo <- lebu[bb];
    ou <- grep(nabu[bb],uu,fixed=TRUE);
    # first looking where there are and how they are
    ouou <- numeric(0);
    for (ii in ou) {
      # removing the possible first spaces
      lili <- form3decadre(uu[ii]);
      if (substr(lili,1,lo)==nabu[bb]) {
        ouou <- c(ouou,ii);
      }
    }
    # preparing everything
    if (length(ouou)>0) {
      nou <- form3nume(length(ouou),nunu[bb]);
      iii <- 0;
      for (ii in ouou) {
        iii <- 1+iii;
        # getting rid of first spaces
        lili <- form3decadre(uu[ii]);
        if (substr(lili,1,lo)!=nabu[bb]) {
          rapport("in 'read8list' about 'ouou'");
        }
        lulu <- lili;
        lili <- substr(lili,lo+1,nchar(lili));
        # the string cannot be empty
        if (form3decadre(lili)=="") {
          erreur(list(fifi,lulu),
                 "This tagged line is empty which is not allowed!");
        }
        # when we are not at the last level,
        # additional levels must be introduced
        if (bb<nbul) {
          orajou <- c(orajou,ii);
          lrajou <- c(lrajou,bb);
          qrajou <- c(qrajou,nou[iii]);
          uu[ii] <- paste(debu[nbul],rbsb.l00,finn[nbul],lili,sep="");
        } else {
          uu[ii] <- paste(debu[bb],nou[iii],finn[bb],lili,sep="");
        }
      }
    }
  }
  #
  # adding the level to insert
  if (length(orajou)>0) {
    # FIRST ordering things into order
    oo <- order(orajou);
    orajou <- orajou[oo];
    qrajou <- qrajou[oo];
    lrajou <- lrajou[oo];
    for (ii in bf(orajou)) {
      iii <- length(orajou) - ii + 1;
      iiu <- orajou[iii];
      nbajou <- (nbul - lrajou[iii]);
      # shifting at the end to introduce new lines
      uu[bd(iiu+nbajou,length(uu)+nbajou)] <- uu[bd(iiu,length(uu))];
      # adding the chief line
      uu[iiu] <- paste(debu[lrajou[iii]],
                       qrajou[iii],
                       finn[lrajou[iii]],sep="");
      # adding possible intermediary lines
      jk <- 0;
      for (jjj in bd(lrajou[iii]+1,nbul-1)) {
        jk <- 1+jk;
        uu[iiu+jk] <- paste(debu[jjj],
                            rbsb.l00,
                            finn[jjj],sep="");
      }
    }
  }
}
#
# writing the intemediary file
if (path!="") { fofo <- paste(path,rbsb.fin,sep="/");} else { fofo <- rbsb.fin;}
writeLines(uu,fofo);
#
# finally calling 'file2list'
res <- file2list(
                 file=rbsb.fin,path=path,
                 clean=clean,ended=ended,comme=comme,
                 skip=skip,
                 include=include,
                 tags=tags,
                 sep=sep,
                 rsep=rsep,
                 stag=stag
                                     );

# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2char <- function(lili,
                      tags=rbsb.tag1,
                      stag=c("/",";","/"),
                      comment="#",
                      comments=character(0)
                     )
#TITLE  transforms a list into a character
#DESCRIPTION
# The reverse operation of \code{char2list}. The list must be a
# rsbs-list, that is compying
# some properties : all components of the [sub-]lists must be either
# a list or a [named] character vector/matrix/array. The number of nested list
# must not be greater than the number of rows in matrix \code{tags}.\cr
# Every list component must be named.\cr
# The idea is to get a character compatible with \code{char2list} to produce back
# the object \code{lili}.\cr
# Some comments are added to the content of the list by the function itself and/or
# according to the whish of the user, this is why an escaping character is asked.
#DETAILS
# Also, the character strings of the structure must not comprise
# the \code{rbsb.sep0} constant except when this global constant is
# conveniently modified. The same for \code{rbsb.sep1}.\cr
# Use is made of the general constant \code{rbsb.mck} for the checking.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be transformed into a 
#          \code{character}.>>
#[INPUTS]
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the [sub]lists at different levels. Its row numbers gives the maximum
#          accepted number of levels. Opening tags must be different.>>
#{stag} << Three character strings indicating the tagging to define the separator for each
#          character vector \code{stag[2]} between \code{stag[1]} and \code{stag[3]}.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered. This function will introduce its signature at the beginning
#          of the file.>>
#{comments} <<Comments that the user want to be added at the beginning of the file.>>
#VALUE
# The resulting character.
#EXAMPLE
# rbsb3k("reset");
# list2char(rbsb.lis1);
#REFERENCE
#SEE ALSO  \code{char2list}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_25
#REVISED 10_09_13
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,message="lili must be a list");
    #
    check4tyle(tags,rbsb.chara,c(2,Inf),message="tags must be a CHARACTER matrix with two columns");
    if (!is.matrix(tags)) {erreur(tags,"tags must be a character MATRIX of two columns");}
    if (ncol(tags)!=2) {erreur(tags,"tags must be a character matrix OF TWO COLUMNS");}
    if (length(unique(tags[,1]))!= nrow(tags)) { erreur(tags,"Opening tags are not unique!");}
    #
    check4tyle(stag,rbsb.chara,3,message="stag must be a character of length three");
    if (any(nchar(stag) == 0)) { erreur(stag,"stag must not have zero length components");}
    if (length(grep(" ",stag[c(1,3)])) > 0) { erreur(stag,"stag must not comprise spaces in first and/or third position");}
    #
    check4tyle(comment,rbsb.chara,1,message="'comment' must be a character of length one");
    #
    check4tyle(comments,rbsb.chara,-1,message="'comment' must be a character of any length");
}
# 
# preparing
res <- character(0);
#
# issuing the starting comments
res <- c(res,comment);
res <- c(res,paste(comment,"  This character was created on",now("d"),"by list2file of rbsb package"));
res <- c(res,paste(comment,"  from the object :",deparse(substitute(x))));
res <- c(res,comment);
res <- c(res,comment);
for (ii in bf(comments)) {
    res <- c(res,paste(comment,comment,rbsb.sep0,rbsb.sep0,comments[ii]));
}
res <- c(res,comment);
res <- c(res,comment);
#
# processing the list to translate it
if (length(lili) == 0) {
    res <- c(res,paste(comment,"The proposed list was of length zero!"));
    res <- c(res,comment);
} else {
    # exploring the list
    liliex <- explore8list(lili);
    # checking the list-vma nature 
    if(!all(liliex[,"classes"] %in% c("list",
                                      "logical","integer","numeric","character",
                                      "matrix","array"))) {
        erreur(liliex,"The 'lili' list is not composed of list/vector/matrix/array");
    }
    # checking the existence of names for all levels
    if (any(is.na(liliex[,"name"]))) {
        erreur(liliex,"Not all 'lili' components have got a name");
    }
    # checking the levels
    xlev <- max(as.numeric(liliex[,"depth"]));
    if (xlev > nrow(tags)) {
        erreur(list(liliex,tags),"'lili' has got too many levels for the proposed 'tags'");
    }
    #
    # ordering the table
    uv <- order(liliex[,"numbers"]);
    # writing down the file
    for (ii in uv) {
        niv <- as.numeric(liliex[ii,"depth"]);
        if (niv == 1) { res <- c(res,paste(comment,form3repeat("=",20),sep=""));}
        if (niv == 2) { res <- c(res,comment);}
        rrr <- paste(tags[niv,1],liliex[ii,"name"],tags[niv,2],sep="");
        if (liliex[ii,"classes"] != "list") {
            coco <- get8listcomp(lili,liliex[ii,])[[1]];
            caca <- vma2char(coco,rbsb.sep1);
            rrr <- paste(rrr,stag[1],stag[2],stag[3],caca$type,sep="");
            res <- c(res,rrr);
            res <- c(res,paste(caca$character,collapse=stag[2]));
        } else {
            res <- c(res,rrr);
        }
    }
}
#
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
char2file <- function(chara,file,path="./",
                      ap=FALSE
                     )
#TITLE  writes a character onto a file
#DESCRIPTION
# The reverse operation of \code{file2char}. This function is not
# very tricky, it was written for the completeness.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{chara} << The \code{character} to be written into
#          \code{file}.>>
#{file} << file to be written. According to \code{ap} when already exist, 
# it will be destroyed and recreated or completed.
# When the \code{file} is \code{rbsb.cha0}, no file is considered but the
# corresonding \code{character} is returned.
# >>
#[INPUTS]
#{path} << Directory containing the file.>>
#{ap} <<Must the file be appended if it already exist?>>
#VALUE
# Nothing but a file is created or modified when everything is right -or-
# according to the value of \code{file}, a character is returned.
#EXAMPLE
# rbsb3k("reset");
# char2file(letters,"toto.txt");
# unlink("toto.txt");
# char2file(letters,rbsb.cha0);
#REFERENCE
#SEE ALSO  \code{file2char}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_25
#REVISED 10_11_24
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(chara,rbsb.chara,-1,message="'chara' must be a character");
    #
    check4tyle(file,rbsb.chara,c(0,1),message="file must indicate the name of one file");
    #
    check4tyle(path,rbsb.chara,1,message="path must indicate the name of one directory");
    #
    if (is.na(file.info(path)$size)) { erreur(path,"This directory seems not to exist.");}
    #
    check4tyle(ap,rbsb.logic,1,message="'ap' must be a single logical");
}
#
# The returning case
if (isempty(file)) { return(chara);}
#
# opening the proposed file
file <- paste(path,file,sep="/");
sink(file,append=ap);
#
# writting each component
for (ii in bf(chara)) {
    cat(chara[ii],"\n");
}
#
# closing the file
sink();
#
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list2file <- function(lili,file,path=".",
                      tags=rbsb.tag1,
                      stag=c("/",";","/"),
                      comment="#",
                      comments=character(0),
                      ap=FALSE
                     )
#TITLE  transforms a list and writes it onto a text file
#DESCRIPTION
#  Transforms a list and writes it onto a text file.
# The reverse operation of \code{file2list} and the linking of
# \code{list2char} and \code{char2file}. See their description for
# complete details.
#DETAILS
# No use is made of the general constant
# \code{rbsb.mck}, being reported in the called functions.\cr
# In any case, permission of writting must exist in the directory of the proposed path.  
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be transformed and written in
#          \code{file}.>>
#{file} << file to be written. According to \code{ap} when already exist, 
# it will be destroyed and recreated or completed. But when equal to \code{rbsb.cha0}
# no file is considered but the potential content of the file is returned as a character.
# >>
#[INPUTS]
#{path} << Directory containing the file.>>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the [sub]lists at different levels. Its row numbers gives the maximum
#          accepted number of levels. Opening tags must be different.>>
#{stag} << Three character strings indicating the tagging to define the separator for each
#          character vector \code{stag[2]} between \code{stag[1]} and \code{stag[3]}.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered. This function will introduce its signature at the beginning
#          of the file.>>
#{comments} <<Comments that the user want to be added at the beginning of the file.>>
#{ap} <<Must the file be appended if it already exist?>>
#VALUE
# Nothing but a file is created or modified when everything is right; except when
# \code{file==rbsb.cha0}, in that case a character is returned.
#EXAMPLE
# rbsb3k("reset");
# list2file(rbsb.lis1,"toto.txt");
# unlink("toto.txt");
# list2file(rbsb.lis1,file=rbsb.cha0);  
#REFERENCE
#SEE ALSO  \code{file2list}
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_03_25
#REVISED 10_11_24
#--------------------------------------------
{
#
# no checking (will be done in char2file)
#
# creating the intermediary character
chara <- list2char(lili,tags=tags,
                        stag=stag,
                        comment=comment,
                        comments=comments
                  );
#
# creating the file and returning
char2file(chara,file=file,
          path=path,ap=ap
         );
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
explore8list <- function(lili)
#TITLE  returns the structure of a list in matrix
#DESCRIPTION
# Recursively explores the branches of a list, returning them into a 
# character matrix: a row for each branch, the columns describing
# some characteristics.\cr
# The columns are 
# 'number' (the number of this component from its branch),
# 'numbers' (the succesions of all numbers leading to 
#            this component separated with spaces),
# 'name' (<NA> if does not exist),
# 'names' (succession of names separated with spaces),
# 'level' (the branch level, i.e. a numbering of the branches),
# 'depth' (the branch depth from the root, implicitely 0 for the root),
# 'class' (the classes of the component separated with spaces).
#DETAILS
# \code{rbsb.sep1} is used to join the names, as a consequence it must
# not be present into the names. In case an error is issued.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be explored.>>
#[INPUTS]
#VALUE
# The resulting character matrix (see the description section).
#EXAMPLE
# rbsb3k("reset");
# explore8list(rbsb.lis1);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_01
#REVISED 10_06_23
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,message="lili must be a list");
}
#
# starting the table
nta <- c("numbers","number","names","name","depth","level","classes");
tab <- matrix(NA,0,length(nta));
dimnames(tab) <- list(NULL,nta);
if (length(lili) == 0) { return(tab);
} else {
    for (ii in bf(lili)) {
        nom <- names(lili)[ii];
        if (is.null(nom)) { nom <- "<NA>";}
        if (nom=="") { nom <- "<NA>";}
        if (expr3present(rbsb.sep1,nom)) {
            erreur(list(rbsb.sep1,nom),
                   "This name for the list comprises 'rbsb.sep1' which is not accepted by 'explore8list'");
        }
        tab <- rbind(tab,c(ii,ii,nom,nom,1,1,NA));
    }
}
#
qq <- which(is.na(tab[,"classes"]));
if (length(qq) > 0) { qq <- qq[1];
} else { qq <- 0; }
# filling the table
niv <- 1;
while (qq > 0) {
    # the qq-th component must be explored
    coco <- paste("lili[[",
                  paste(strsplit(tab[qq,"numbers"]," ")[[1]],
                        collapse="]][["),
                  "]]",sep="");
    coco <- paste("coco <-",coco);
    eval(parse(text=coco));
    # completing the class
    tab[qq,"classes"] <- paste(class(coco),collapse=" ");
    # if a list adding more components
    if (tab[qq,"classes"] == "list") {
        pro <- length(strsplit(tab[qq,"numbers"]," ")[[1]])+1;
        niv <- niv + 1;
	for (ii in bf(coco)) {
	    nom <- names(coco)[ii];
	    if (is.null(nom)) { nom <- "<NA>";}
	    if (nom=="") { nom <- "<NA>";}
            noms <- paste(tab[qq,"names"],nom,sep=rbsb.sep1);
            iii <- paste(tab[qq,"numbers"],ii);
	    tab <- rbind(tab,c(iii,ii,noms,nom,pro,niv,NA));
	}
    }
    # something more to explore?
    qq <- which(is.na(tab[,"classes"]));
    if (length(qq) > 0) { qq <- qq[1];
    } else { qq <- 0; }
}
# returning
tab;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
name8list <- function(lili)
#TITLE  returns the same list but with names
#DESCRIPTION
# Explores the branches of a list, returning the same valued list
# but adding names to the components if not existing. This is a 
# way to comply one of the two requirements to be a rbsb-list (see 
# \code{is8rbsblist}).
#DETAILS
# The new names are obtained with \code{form3names}.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be possibly named.>>
#[INPUTS]
#VALUE
# The resulting named list.
#EXAMPLE
# rbsb3k("reset");
# uu <- list(list(1:3,4:6),
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            list(a=1:3,b=letters,list(array(1:8,c(2,2,2)))));
# name8list(uu);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 10_04_19
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,message="lili must be a list");
}
#
# exploring the list
tata <- explore8list(lili);
if (nrow(tata) == 0) {return(lili);}
#
# looking for missing names and 
# if so, providing them
qq <- which(tata[,"name"]=="<NA>");
while (length(qq)>0) {
    qqq <- qq[1];
    # selecting all concerned names
    qqqq <- tata[tata[,"level"]==tata[qqq,"level"],"numbers"];
    # proposing replacement names
    nana <- form3names(length(qqqq));
    ouou <- strsplit(qqqq[1]," ")[[1]];
    ouou <- ouou[-length(ouou)];
    if (length(ouou) > 0) {
        coco <- paste("lili[[",
                      paste(ouou,collapse="]][["),
                      "]]",sep="");
    } else {
        coco <- "lili";
    }
    coco <- paste("names(",coco,") <- nana;",sep="");
    eval(parse(text=coco));
    #
    tata <- explore8list(lili);
    qq <- which(tata[,"name"]=="<NA>");
}
#
# returning
lili;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
get8listcomp <- function(lili,tata)
#TITLE  returns components from a list structure
#DESCRIPTION
# Returns components from a list structure as a one level list.
# The list \code{lili} must have been explored with \code{explore8list}
# and the branch(es) to return are indicated through their line numbers
#  (\code{tata}) in the table it generates.
#DETAILS
# Names of the produced list are the stacked names of the initial list 
# \code{lili}.
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure components of which have to be extracted.>>
#{tata} << The lines of the table provided by \code{explore8list}.>>
#[INPUTS]
#VALUE
# The resulting list with as many component as indicated rows.
#EXAMPLE
# rbsb3k("reset");
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# get8listcomp(uu,vv[7,])[[1]];
##
# uu <- list(1:3,
#            matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# get8listcomp(uu,vv[7,])[[1]];
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_01
#REVISED 10_04_20
#--------------------------------------------
{
# in case of
if (length(tata)==0) {
    return(vector("list",0));
}
nta <- c("numbers","number","names","name","depth","level","classes");
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,message="lili must be a list");
    #
    check4tyle(tata,rbsb.chara,-1,
               message="'tata' must be a 'character'");
    if (!is.matrix(tata)) {
        check4tyle(tata,rbsb.chara,length(nta),
                   message="When not a matrix, 'tata' must have the same length that 'nta'");
    } else {
        if (ncol(tata) != length(nta)) {
            erreur(tata,"The 'tata' matrix must have got as many columns as the length of nta");
        }
        if (is.null(dimnames(tata)[[2]])) {
            erreur(tata,"The 'tata' matrix must have named columns");
        }
        if (length(union(dimnames(tata)[[2]],nta))>length(nta)) {
            erreur(list(tata,nta),"The 'tata' matrix must have named columns with 'nta'");
        }
    }
}
# preparation
if (!is.matrix(tata)) {
   tata <- matrix(tata,1,length(nta));
   dimnames(tata) <- list(NULL,nta);
}
#
# starting the resulting list
res <- vector("list",nrow(tata));
nono <- tata[,"names"];
nunu <- tata[,"numbers"];
if (length(unique(nono))<length(nono)) {
    nono <- nunu;
}
names(res) <- nono;
for (ii in bc(nrow(tata))) {
    coco <- paste("lili[[",
                  paste(strsplit(nunu[ii],rbsb.sep0)[[1]],
                        collapse="]][["),
                  "]]",sep="");
    coco <- paste("coco <-",coco);
    eval(parse(text=coco));
    res[[ii]] <- coco;
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
set8listcomp <- function(x,lili,tata)
#TITLE  modifies one component from a list structure
#DESCRIPTION
# Replaces one component from a list structure which must not be a list
# (neither before or after the replacement).
# The list \code{lili} must have been explored with \code{explore8list}
# and the branch to modify is indicated through its line in the 
# generated table by \code{tata}.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{x} << The object to be inserted in place of the former one.>>
#{lili} << The list structure a component of which is to be modified.>>
#{tata} << The line (not the line number) of the table provided by \code{explore8list}
#          indicating the component to replace.>>
#[INPUTS]
#VALUE
# The new resulting list
#EXAMPLE
# rbsb3k("reset");
# uu <- list(A=1:3,
#            B=matrix(letters[1:20],nrow=5,dimnames=list(1:5,c("on","tw","th","fo"))),
#            C=list(a=1:3,b=letters,c=list(final=array(1:8,c(2,2,2)))));
# vv <- explore8list(uu);
# set8listcomp(LETTERS,uu,vv[7,]);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_08
#REVISED 10_04_08
#--------------------------------------------
{
# in case of
if (length(lili)==0) {
    return(vector("list",0));
}
nta <- c("numbers","number","names","name","depth","level","classes");
# checking
if (rbsb.mck) {
    check4tyle(lili,"list",-1,message="lili must be a list");
    #
    check4tyle(tata,rbsb.chara,length(nta),
               message="'tata' must be a 'character' of length length(nta)");
    #
    if (tata[length(nta)] == "list") {erreur(tata,"the component to replace must not be a list");}
}
# preparation
tata <- matrix(tata,1,length(nta));
dimnames(tata) <- list(NULL,nta);
#
# replacing the desired component
coco <- paste("lili[[",
                  paste(strsplit(tata[1,"numbers"]," ")[[1]],
                        collapse="]][["),
                  "]]",sep="");
coco <- paste(coco,"<- x;");
eval(parse(text=coco));
# returning
lili;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
form3norma <- function(cha,redu=" ",bef=" ",aft=" ")
#TITLE removes redundant characters into a character string
#DESCRIPTION
#  Removes redundant characters into a character string.
# The most immediate use (default) is to transform
# sequences of \code{" "} into a simple \code{" "}.
# More generally, removes all sequences of \code{redu} before
# \code{bef} or after \code{aft}.
#DETAILS
#PKEYWORDS
#KEYWORDS IO
#INPUTS
#{cha} << The character to normalize, can be of length greater
# than one.>>
#[INPUTS]
#{redu} << The character to remove, single or not.>>
#{bef} << The ante-tag, single or not.>>
#{aft} << The post-tag, single or not.>>
#VALUE
# The transformed character (with the same length than \code{cha}.
#EXAMPLE
# rbsb3k("reset"); # only necessary for R checking
# form3norma(" pour   voir  ");
# form3norma(c(" A > B ","B > C","D"),bef=">");
# form3norma(c(" A > B ","B > C","D"),bef=">",aft=">");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_06_16
#REVISED 10_06_16
#--------------------------------------------
{
# checking
if (rbsb.mck) {
    check4tyle(cha, rbsb.chara,-1,message=" cha must be a character");
    check4tyle(redu,rbsb.chara, 1,message="redu must be a character(1)");
    check4tyle(bef, rbsb.chara, 1,message=" bef must be a character(1)");
    check4tyle(aft, rbsb.chara, 1,message=" aft must be a character(1)");
}
if ((length(cha)>0) & (nchar(redu)>0)) {
    # removing before
    what <- paste(redu,bef,sep="");
    ou <- grep(what,cha);
    while (length(ou)>0) {
        cha <- gsub(what,bef,cha);
        ou <- grep(what,cha);
    }
    # removing before
    what <- paste(aft,redu,sep="");
    ou <- grep(what,cha);
    while (length(ou)>0) {
        cha <- gsub(what,aft,cha);
        ou <- grep(what,cha);
    }
}
# returning
cha;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
systematic <- function(x,nb,kk,ord="random")
#TITLE returns a systematic exploration of a vector
#DESCRIPTION
# returns a vector of indices of \code{x} such that this
# is explored in systematic way by \code{nb} draws, spaced
# of \code{kk} according to its order.\cr
# Even is \code{ord} is not "random", a call to
# \code{sample} is done so it is advised to call
# first \code{set.seed}.
#DETAILS
# When \code{nb*kk} is less than \code{length(x)} then the 
# sequence is 1, kk+1, 2*kk+1,..., (nb-1)*kk+1 taking into
# account the ordered order of values of \code{x}. If not either
# look at the code or experiment the function.
#PKEYWORDS
#KEYWORDS misc
#INPUTS
#{x} << The vector to systematically sample.>>
#{nb} << desired number of draws.>>
#{kk} << frequency of successive draws.
#        Something giving quasi-uniform draw is
#        is \code{min(length(x)-1,length(x)\%/\%(nb\%\%length(x)))}
#        ???notice that the direct application of this formula
#        failed for rounding phenomenum???.>>
#[INPUTS]
#{ord} <<indicates which type of ordering must be done on
#        the output: \code{randor} for a circular random permutation;
#        \code{random} for a complete random permutation;
#        \code{order} for a sorting with \code{sort};
#        \code{such} for nothing (in that case, it depends on the
#        algorithm which can be changed).
#VALUE
# A vector of length \code{nb} of indices of \code{x}.
#EXAMPLE
# rbsb3k("reset"); # only necessary for R checking
# systematic(1:20,5,1);
# systematic(1:20,5,3);
# systematic(1:20,15,3);
# systematic(1:20,15,3,"order");
# systematic(1:20,15,3,"random");
# sort(systematic(1:20,55,3));
# uu <- c(2*(1:10),1+2*(0:9));
# uu[systematic(uu,20,3)];
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_07_27
#REVISED 10_07_30
#--------------------------------------------
{ 
# degenerate cases
if (length(x)==0) { return(numeric(0));}
# checking
cord <- c("randor","random","order","such");
if (rbsb.mck) {
    check4tyle(x, "numeric",-1);
    check4tyle(nb,"integer", 1);
    check4tyle(kk,"integer", 1);
    if (nb < 0) { erreur(nb,"'nb' must be non negative");}
    if (kk < 1) { erreur(kk,"'kk' must be greater than or equal to zero");}
    if (!(ord %in% cord)) {
        erreur(list(ord,cord),"'ord' must be one of 'cord'");
    }
}
# null case
if (isempty(x)) { return(rbsb.num0);}
# length of available values
nn <- length(x);
# adjustment of not sensible cases
if (kk == nn) { kk <- 1;}
kk <- kk %% nn;
# degenerate cases
if (nn==1) { return(rep(1,nb));}
# a complete draw with random starting point
nc <- nn %/% kk;
if (nc*kk < nn) { nc <- nc+1;}
mm <- matrix(1:(nc*kk),nrow=kk);
aa <- sample(kk,1);
if (aa > 1) { mm <- mm[c(aa:kk,1:(aa-1)),];}
rr <- as.vector(t(mm));
rc <- rr[rr<=nn];
# initializing
res <- numeric(0);
# complete draws
if (nb >= nn) {
    nbc <- nb %/% nn;
    nbn <- nb %% nn;
    res <- c(res,rep(rc,nbc));
} else { nbn <- nb;}
# final incomplete draw
ri <- rc[bc(nbn)];
res <- c(res,ri);
# taking into account the values of x
res <- order(x)[res];
# checking in case
if (rbsb.mck) {
    if (length(res)!=nb) {
        erreur(list(nb,length(res)),"length(res) not right",w=TRUE);
        rapport("(i) Something bad in 'systematic'");
    }
    if (any(res>length(x))) {
        erreur(list(range(res),length(x)),"too large value of 'res'",w=TRUE);
        rapport("(ii) Something bad in 'systematic'");
    }
    if ((nb>length(res)) & (length(unique(res))!=length(res))) {
        rapport("(iii) Something bad in 'systematic'");
    }
}
# ordering
if (ord == cord[1]) {
    ax <- sample(length(res),1);
    if (ax > 1) {
        res <- res[c(ax:length(res),1:(ax-1))];
    }
}
if (ord == cord[2]) {
    res <- res[sample(length(res),length(res))];
}
if (ord == cord[3]) {
    res <- sort(res);
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
check4valid <- function(x,message=NULL,fatal=TRUE)
#TITLE  checks the returned value of valid8 functions
#DESCRIPTION
# When \code{x} is character, then it is printed
# as well as \code{message} and according to \code{fatal}
# the program can be stopped.
#DETAILS
#PKEYWORDS
#KEYWORDS error
#INPUTS
#{x} <<object to be checked.>>
#[INPUTS]
#{message} << Some additional message to be
#            issued before stopping.>>
#{fatal} << what to do when discovering
#           a character? TRUE: this function prints the message
#           and stops; FALSE: this function returns
#           the message as a character.>>
#VALUE
# When the check is validated returns TRUE.
# If not, according to \code{fatal} prints the
# message and stops or returns the message.
#EXAMPLE
# rbsb3k("RESET"); # needed only for R checking, to be forgotten
# check4valid("That's not good enough",fatal=FALSE);
# check4valid(TRUE);
# check4valid("A","B C and so on.",fatal=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE improves the treatment of 'NA's
#AUTHOR J.-B. Denis
#CREATED 10_08_06
#REVISED 10_08_09
#--------------------------------------------
{
# no checking
if (!is.character(x)) { return(TRUE);}
#
res <- c(x,message);
if (fatal) {
    form3title(res,box="no");
    erreur(NULL,"from check4valid");
}
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
