#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("prr");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("prr",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# exemples of reading some prepared files
# 
# 10_09_14 10_11_24
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
tit <- "reading a first /chain/ from a file";
form3titre(tit);
#
#
tt1 <- paste(chemin,"prr.demo.ex00.txt",sep="");
#
lu <- readLines(tt1);
print(lu);
if (tpau) { pause("The file, as it will be read");}
#
#
lili <- file2list(tt1,path="",
                  tags=matrix(c("<<", ">>",
                                "((", "))"),
                  ncol=2, byrow=TRUE));
print(lili);
if (tpau) { pause("The file, read as a list");}
#
#
chai1 <- read8chain(tt1);
print(chai1);
if (tpau) { pause("The file, read as a chain");}


############################################################
tit <- "reading a second /chain/ from a file";
form3titre(tit);
#
#
tt2 <- paste(chemin,"prr.demo.ex01.txt",sep="");
#
#
chai2 <- read8chain(tt2);
print(chai2);
if (tpau) { pause("The second file, read as a chain");}

