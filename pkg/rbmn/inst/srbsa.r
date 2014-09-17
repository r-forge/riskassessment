#
# 14_09_15
#
## loading the libraries
#  (in fact /rbsa/ is not available in /documair/
#   you can get it from the R forge
#   https://r-forge.r-project.org/projects/riskassessment/ )
library(rbsa); library(documair);
#
# the prefix associated to the /rbsa/ functions
kkk <- "sss";
#
# /rbsa/ code file path
rbsapath <- "/home/jbdenis/attente.liana/inra/paquets/rbsa/pro/perso";
rbsapath <- "/home/jbdenis/bananier/inra/p/r/paquets/rbsa/pro/perso";
#
# the necessary object list
nefu <- c("form3title","form3titre","form3display","pause",
          "vma2text",
          "form3join","list2file","text2file","list2text",
          "file2text","text2list","filter8text","text2vma",
          "posi9","read8list","now","form3split","form3crop","file2list",
          "form3title","form3titre","void9","fidi9","explore8list",
          "interv7belonging","belong9","form3repeat","form3justify",
          "get8comp7list","dipa","text3preparation",
          "form3names","form3numbering","rbsa7list9","display8k",
          "rbsa0","object9","bc","bd","bf","erreur"
         );
#
# preparing the new files to receive the functions
form3title("Preparing The Intermediate File",4);
newint <- "tutu.inte.r";
newfi2 <- "rrbsa.code.r";
if (fidi9(newint) == "f") { unlink(newint);}
if (fidi9(newfi2) == "f") { unlink(newfi2);}
#
# getting the proposed object from /rbsa/ directory
rcopy <- copy8objects(Sys.glob(paste0(rbsapath,"/*.code.r")),nefu,newint);
if (length(rcopy)>0) {
  print(rcopy);
  form3title("Found difficulties",6);
  stop("Have a look");
}
#
# proposing newnames for the designated objects
newnames <- cbind(nefu,paste0(kkk,nefu));
#
# translating the functions
form3title("Translating Object Names",4);
faire <- change8names(c(newint,newfi2),newnames);
if (length(faire)>0) {
  print(faire);
  form3title("The translations of the function names went wrong!",5);
  stop("Better to see what happened");
}
#
# checking by building the subpackage
form3title("Checking the sub-package by R itself",4);
rrbbii <- "rrbsa.essairrbsa.r";
if (fidi9(rrbbii)=="f") { unlink(rrbbii);}
if (!file.copy(newfi2,rrbbii)) {
  erreur(list(newfi2,rrbbii),"Not possible to copy 'rrbbii'?");
}
construire <- build8pkg("rrbsa",".","rrbsa",what="pzl");
if (length(construire)>0) {
  print(construire);
  form3title("The Building of the package was not good!",4);
  stop("Look also at the R messages");
}
#
## the end
if (fidi9(newint) == "f") { unlink(newint);}
form3title("rrbsa.r* finished its job!",8);
