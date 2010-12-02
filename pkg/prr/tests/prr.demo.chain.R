#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("prr");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("prr",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# examples of use of /chain/ objects
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
tit <- "Transforming /nod/";
form3titre(tit);
#
lino1 <- nod2list(prr.nod1);
print(lino1);
print(list2nod(lino1));
if (tpau) { pause("prr.nod1 list");}
lino2 <- nod2list(prr.nod2);
print(lino2);
print(list2nod(lino2));
if (tpau) { pause("prr.nod2 list");}
if (tpau) { pause(paste(tit,"is finished"));}
#

tpau <- FALSE;
############################################################
tit <- "Transforming /chain/";
form3titre(tit);
#
lich1 <- chain2list(prr.chain1);
print(lich1);
print(list2chain(lich1));
if (tpau) { pause("prr.chain1 list");}
lich2 <- chain2list(prr.chain2);
print(lich2);
print(list2chain(lich2));
if (tpau) { pause("prr.chain2 list");}
if (tpau) { pause(paste(tit,"is finished"));}
#
