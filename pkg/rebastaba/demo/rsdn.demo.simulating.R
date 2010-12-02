chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# plotting dn objects
# 
# 10_07_13
#
#


############################################################
form3titre("Demonstrating the simulations of /dn/s");

### <generating the initial population>
###
lis_voir <- function() {
    N <- 50;
    sex <- rep("F",N);
    sex[runif(N) < 0.5] <- "M";
    sex <- as.factor(sex);
    age <- round(runif(N,20,79));
    hgt <- round(rnorm(N,160,10)+8*(sex=="M"));
    wgt <- round(hgt-100+rnorm(N,0,10));
    data.frame(sex=sex,age=age,hgt=hgt,wgt=wgt);
}
###
### <--------------------------------->

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
fifi <- "rsbn.demo.bn.dirac.txt";
fifi <- paste(chemin,fifi,sep="");
cat("chemin = <<<",chemin,">>>\n",sep="");
cat("fifi = <<<",fifi,">>>\n",sep="");
cat("wd = <<<",getwd(),">>>\n",sep="");
form3titre("Looking at the prepared file");
print(readLines(fifi));

form3titre("Loading the associated /bn/");
bc <- read8bn(fifi);
form3titre("Having a look to the resulting /bn/",0);
print(bc,quoi="nv");

form3titre("Simulating to get a /dn/");
dc <- bn2dn(bc);
form3titre("Having a look to the resulting /dn/",0);
print(dc,quoi="i",simu=10);
