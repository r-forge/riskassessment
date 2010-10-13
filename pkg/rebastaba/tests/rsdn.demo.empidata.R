#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rebastaba");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# simulating with empidata nodes
# 
# 10_07_29 10_07_30 10_08_06 10_08_13
#
#


############################################################
form3titre("Simulating with an ancestor 'empidata' node");

if (!exists("chemin")) {
    erreur(paste("The package must be installed to run this demo",
                 "(or an appropriate 'chemin' must be defined)")
          );
}
# defining the data frame through a function
empi1.gen <- function()
{
nbd <- 100;
v2 <- -3 + (1:nbd)/20;
v1 <- 15 + abs(v2);
v0 <- 1:nbd;
data.frame(v0=v0,v1=v1,v2=v2);
}
#
fifi1 <- "rsdn.demo.empi1.txt";
fifi1 <- paste(chemin,fifi1,sep="");
print(fifi1);
#
form3titre("Associated bn",0);
bn1 <- read8bn(fifi1);
print(bn1,quoi="nv");
print(cor(empi1.gen()));
#
# varying the type of drawing
for (d1 in c("*","0","1")) {
for (d2 in c("random","systematic")) {
#for (d1 in c("0")) {
#for (d2 in c("systematic")) {
    form3line();
    bn1@nwin[[1]]@rty <- c(d1,d2);
    form3titre(paste("Simulated values",d1,d2,sep=" - "),0);
    dn1 <- bn2dn(bn1,10);
    print(dn1,quoi="i",simu=10);
    print(cor(dn1@df[,1:3]));
}}

#pause("Firt done!");
############################################################
form3titre("Simulating with a leaf 'empidata' node");

if (!exists("chemin")) {
    erreur(paste("The package must be installed to run this demo",
                 "(or an appropriate 'chemin' must be defined)")
          );
}
# defining the data frame through a function
empi2.gen <- function()
{
nbd <- 69;
v0 <- rbinom(nbd,1,0.4);
v2 <- -3 + (1:nbd)/20;
v1 <- 15 + abs(v2);
v1 <- v1 + 3*v0;
v2 <- v2 - v0;
#
nive <- c("n","y");
v0 <- factor(nive[1+v0]);
#
data.frame(positive=v0,v1=v1,v2=v2);
}
#
fifi2 <- "rsdn.demo.empi2.txt";
fifi2 <- paste(chemin,fifi2,sep="");
print(fifi2);
#
form3titre("Associated bn",0);
bn2 <- read8bn(fifi2);
print(bn2,quoi="nv");
#
form3titre("Simulated values",0);
set.seed(3456);
dn2 <- bn2dn(bn2,10);
print(dn2,quoi="i",simu=100);
