chemin <- searchpaths()[grep("prr",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# examples of use of /nod/ objects
# 
# 10_11_25
#
#

prr3k("RESET");
tpau <- FALSE;

# usual warning
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}

############################################################
tit <- "extracting the parents from a /nod/";
form3titre(tit);
#
nono <- new("nod",name="nono",
                  probab=prr.probab2,
                  assess=new("assess",x="min({{A}},{{B}})",
                                     cV=0,cU=0),
                  format=c(1,0,0)
           );
print(nono);
cat("\n\n  Indeed, its parents are:",parents5nod(nono),"\n\n");
if (tpau) { pause(paste(tit,"is finished"));}
#

