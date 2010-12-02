chemin <- searchpaths()[grep("prr",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# exemples of printing /distri/ objects 
# 
# 10_11_17 10_11_22
#
#

#prr3k("RESET");

# from an existing text file
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo",
           "or chemin must be defined accordingly");
}

############################################################
form3titre("Drawing directly with the 'u*' functions");
#
pau <- FALSE;
nbdraws <- 10000;
#
#
tru <- c(0.2,0.5);
form3titre("A truncated Beta distribution (1)",2);
bbb <- ubeta(rep(0.2,2*nbdraws),30);
bbt <- ubeta(rep(0.2,nbdraws),30,trunc=tru);
par(mfrow=c(2,1));
plot(density(bbb),xlim=c(0,1),main="complete beta 1");
abline(v=tru,lty=2);
plot(density(bbt),xlim=c(0,1),main="truncated beta 1");
abline(v=tru,lty=2);
if (pau) { pause("Beta1");}
#
tru <- c(0.6,0.8);
form3titre("A truncated Beta distribution (2)",2);
bbb <- ubeta(rep(0.2,2*nbdraws),30);
bbt <- ubeta(rep(0.2,nbdraws),30,trunc=tru);
par(mfrow=c(2,1));
plot(density(bbb),xlim=c(0,1),main="complete beta 2");
abline(v=tru,lty=2);
plot(density(bbt),xlim=c(0,1),main="truncated beta 2");
abline(v=tru,lty=2);
if (pau) { pause("Beta2");}
#
#
tru <- c(6,8);
form3titre("A truncated Normal distribution (1)",2);
nnn <- unormal(rep(10,2*nbdraws),30);
nnt <- unormal(rep(10,nbdraws),30,trunc=tru);
par(mfrow=c(2,1));
plot(density(nnn),xlim=c(0,20),main="complete normal 1");
abline(v=tru,lty=2);
plot(density(nnt),xlim=c(0,20),main="truncated normal 1");
abline(v=tru,lty=2);
if (pau) { pause("Normal1");}
#
tru <- c(7,12);
form3titre("A truncated Normal distribution (2)",2);
nnn <- unormal(rep(10,2*nbdraws),30);
nnt <- unormal(rep(10,nbdraws),30,trunc=tru);
par(mfrow=c(2,1));
plot(density(nnn),xlim=c(0,20),main="complete normal 2");
abline(v=tru,lty=2);
plot(density(nnt),xlim=c(0,20),main="truncated normal 2");
abline(v=tru,lty=2);
if (pau) { pause("Normal2");}
#
#
tru <- c(2E6,4E6);
form3titre("A truncated LogNormal distribution (1)",2);
lll <- ulnormal(rep(10,2*nbdraws),30);
llt <- ulnormal(rep(10,nbdraws),30,trunc=tru);
par(mfrow=c(2,1));
plot(density(lll),xlim=c(0,10^7),main="complete lognormal 1");
abline(v=tru,lty=2);
plot(density(llt),xlim=c(0,10^7),main="truncated lognorma 1");
abline(v=tru,lty=2);
if (pau) { pause("LogNormal1");}
#
#
tru <- c(2E6,4E6);
form3titre("Categorical distributions",2);
cc1 <- ucateg(rep(5,nbdraws),30,lim=0:10);
cc2 <- ucateg(rep(5,nbdraws),90,lim=0:10);
cc3 <- ucateg(rep(1,nbdraws),30,lim=0:10);
cc4 <- ucateg(rep(1,nbdraws),90,lim=0:10);
par(mfrow=c(2,2));
plot(density(cc1),xlim=c(0,10),main="categorical 1");
abline(v=1:9,lty=2);
plot(density(cc2),xlim=c(0,10),main="categorical 2");
abline(v=1:9,lty=2);
plot(density(cc3),xlim=c(0,10),main="categorical 3");
abline(v=1:9,lty=2);
plot(density(cc4),xlim=c(0,10),main="categorical 4");
abline(v=1:9,lty=2);
if (pau) { pause("categorical");}
