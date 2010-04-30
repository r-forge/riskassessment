#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rbsb");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rbsb",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# creating bdn objects
# 
# 10_02_12 10_02_15 10_03_09 10_04_16
#
#


############################################################
form3titre("Creation of /dn/ by simulating with a /bn/");

form3titre("Generating the /dn/ named dc",0);

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
fifi <- "rsdn.demo.bn.two_nodes.txt";
fifi <- paste(chemin,fifi,sep="");
print(fifi);
#
bc <- read8bn(fifi);
dc <- bn2dn(bc);

form3titre("Having a look to the simulated values",0);
print(dc,quoi="i",simu=10);
plot(dc,varx="N1",vary=-1);
plot(dc,varx="N1",vary="N2");

set.seed(9876);
ga <- rgn("random graph",type="hier",nno=7);
plot(ga);
#
ba1 <- rnorbn(ga,"continuous random Bayesian network");
da1 <- bn2dn(ba1,23);
print(da1);
print(da1,quoi="i",simu=100);

#
ba2 <- rcatbn(ga,"discrete random Bayesian network");
print(ba2,quoi="n");
da2 <- bn2dn(ba2,23);
print(da2);
print(da2,quoi="i",simu=100);

#
fifi <- paste(chemin,"rsdn.demo.bn.anemones.txt",sep="");
bane <- read8bn(fifi);
print(bane,quoi="nv");
dane <- bn2dn(bane,16);
print(dane,quoi="i",simu=100);

