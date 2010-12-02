#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rebastaba");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# plotting dn objects
# 
# 10_04_22
#
#


############################################################
form3titre("Demonstrating the plotting of a /dn/");

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
fifi <- "rsdn.demo.bn.standard.txt";
fifi <- paste(chemin,fifi,sep="");
cat("chemin = <<<",chemin,">>>\n",sep="");
cat("fifi = <<<",fifi,">>>\n",sep="");
cat("wd = <<<",getwd(),">>>\n",sep="");
form3titre("Looking at the prepared file");
print(readLines(fifi));

form3titre("Loading the associated /bn/");
bc <- read8bn(fifi);
form3titre("Having a look to the resulting /bn/",0);
print(bc,"n");

form3titre("Simulating to get a /dn/");
dc <- bn2dn(bc);
form3titre("Having a look to the resulting /dn/",0);
print(dc,quoi="i",simu=10);

form3titre("At last Different PLOTTINGS");
#
plot(dc,titles="TITRE",stitles="Sous-Titre",xxlab="abscisses",yylab="ordonnees",varx="N1",vary="N2");


