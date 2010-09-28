#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rebastaba");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# creating bn objects
# 
# 10_02_12 10_02_15 10_03_10 10_04_16 10_04_20
# 10_05_25 10_06_09 10_06_29 10_08_13 10_08_30
#


############################################################
form3titre("Demonstrating the creation of /bn/ with another /alk/");

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
#
bc4 <- and4bn(rebastaba.bn2,rebastaba.alk4);
#
form3titre("Having a look to the resulting /bn/",0);
print(bc4);
#
pause("Entonces:  categ? / bn2dn(bc4) ?");

############################################################
form3titre("Demonstrating the creation of /bn/ from a prepared file");
form3titre("Including an empidata defined with a file",0)

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
fifi <- "rsbn.demo.bn.multivariate_node.txt";
fifi <- paste(chemin,fifi,sep="");
cat("chemin = <<<",chemin,">>>\n",sep="");
cat("fifi = <<<",fifi,">>>\n",sep="");
cat("wd = <<<",getwd(),">>>\n",sep="");
form3titre("Looking at the file defining the /bn/");
print(readLines(fifi));
form3titre("Looking at the file defining the data base");
fafa <- "rsbn.demo.data.small_population.txt";
fafa <- paste(chemin,fafa,sep="");
print(readLines(fafa));

form3titre("Loading the associated /bn/");
bc2 <- read8bn(fifi);
form3titre("Having a look to the resulting /bn/",0);
print(bc2,quoi="n");
#


############################################################
form3titre("Demonstrating the creation of /bn/ with easyprogramming");

bc77 <- zero2bn(char2des("EasyP"));
#
no1 <- new8alk(char2des("A"),"normal",lpara=list(mu=10,sigma=2),
               lpod=list(c(8,15)),ltransfo="(|2|)");
bc77 <- and4bn(bc77,no1);
#
no2 <- new8alk(char2des("B"),"normal",lpara=list(mu="{{A}}",sigma="abs({{A}})"),
               lpod=list(c(5,20)),ltransfo="(|2|)");
bc77 <- and4bn(bc77,no2);
#
no3 <- new8alk(char2des("C"),"Dirac",lpara=list(k=c("{{A}}+{{B}}",
                                                    "{{A}}-{{B}}")),
               lvar=c("sum","dif"),
               lpod=list(c(-100,100),c(-100,100)));
bc77 <- and4bn(bc77,no3);
#
no4 <- new8alk(char2des("D"),"Dirac",lpara=list(k=c("0.5*({{C[sum]}}+{{C[dif]}})",
                                                    "0.5*({{C[sum]}}-{{C[dif]}})")),
               lvar=c("A","B"),
               lpod=list(c(-100,100),c(-100,100)));
bc77 <- and4bn(bc77,no4);
#
no5 <- new8alk(char2des("E"),"Dirac",lpara=list(k="({{A}}*({{B}}+{{C[dif]}}))/2"),
               ltransfo="(|0|)",
               lpod=list(c(-100,100)));
bc77 <- and4bn(bc77,no5);
#
print(bc77,quoi="nv");
#

############################################################
form3titre("Demonstrating the creation of /bn/ from a prepared file");

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}

fifi <- "rsbn.demo.bn.two_nodes.txt";
fifi <- paste(chemin,fifi,sep="");
cat("chemin = <<<",chemin,">>>\n",sep="");
cat("fifi = <<<",fifi,">>>\n",sep="");
cat("wd = <<<",getwd(),">>>\n",sep="");
form3titre("Looking at the prepared file");
print(readLines(fifi));

form3titre("Loading the associated /bn/");
bc <- read8bn(fifi);
form3titre("Having a look to the resulting /bn/",0);
print(bc,quoi="n");
plot(bc);



############################################################
form3titre("Demonstrating the creation of /bn/ calling a function");

set.seed(9876);
ga <- rgn("random graph",type="hier",nno=7);
plot(ga);
#
ba1 <- rnorbn(ga,"continuous random Bayesian network");
print(ba1,quoi="n");
#
ba2 <- rcatbn(ga,"discrete random Bayesian network");
print(ba2,quoi="n");



############################################################
form3titre("Demonstrating the creation of /bn/ by simple programming");

form3titre("Creating a /bn/ without any node",0);
bb <- zero2bn(char2des("A new bn"));

form3titre("Introducing it a first node",0);
bb <- anode4bn(bb,"A");

form3titre("Adding it a second node with the first node as parent",0);
bb <- anode4bn(bb,"B",parent="A");

form3titre("Randomly adding new nodes",0);
set.seed(3456);
nn <- 12; nno <- letters[bc(nn)]; ppa <- c("A","B");
if (nn>length(letters)) { erreur(nn,"Too large");}
for (ii in bc(nn)) {
    qui <- which(rbinom(2,1,0.5)==1);
    bb <- anode4bn(bb,nno[ii],parent=ppa[qui]);
}
form3titre("Having a look to the resulting /bn/",0);
print(bb,quoi="n");
plot(bb);


############################################################
form3titre("Demonstrating the creation of /bn/s by programming");

form3titre("Creating a /bn/ without any node",0);
#
uu <- new("des",name="BN 8",
          defi="A more elaborated /bn/ generated by programming",
          orig="rebastaba documentation",time=today(),
          role="to give a more elaborated example",
          comm=c("The comment slot is the unique slot of /des/ where you can have vectors of characters.",
                 "Each component will be associated to a new paragraph when printing it.",
                 "This allows more readable outputs."))
print(uu,quoi="a");
#
bp <- zero2bn(uu);

form3titre("Putting in it two root nodes",0);
aalk <- new8alk(ldes=char2des("a"),
                ltype="uniform",
                lpara=list(a=1,b=3),
                lpod=list(c(1,3)),
                lred=list(c(1.1,2.9)),
                lcod=list(c(1.5,2.5))
               );          
balk <- new8alk(ldes=char2des("b"),
                ltype="uniform",
                lpara=list(a=1,b=10),
                lpod=list(c(1,10))
               );          

bp <- and4bn(bp,aalk);
bp <- and4bn(bp,balk);

form3titre("Putting in it two deduced probability nodes",0);
p1alk <- new8alk(ldes=char2des("p1"),
                ltype="Dirac",
                lpara=list(k="{{a}}/({{a}}+{{b}})"),
                lpod=list(c(0,1))
               );          
p2alk <- new8alk(ldes=char2des("p2"),
                ltype="beta",
                lpara=list(a="{{a}}",b="{{b}}"),
                lpod=list(c(0,1))
               );          

bp <- and4bn(bp,p1alk);
bp <- and4bn(bp,p2alk);

form3titre("Putting in it two following binomial nodes of equal size",0);
nbino <- 17;
B1alk <- new8alk(ldes=char2des("B1"),
                ltype="binomial",
                lpara=list(p="{{p1}}",n=nbino),
                lpod=list(c(0,nbino))
               );          
B2alk <- new8alk(ldes=char2des("B2"),
                ltype="binomial",
                lpara=list(p="{{p2}}",n=nbino),
                lpod=list(c(0,nbino))
               );          

bp <- and4bn(bp,B1alk);
bp <- and4bn(bp,B2alk);

form3titre("Having a look to the resulting /bn/",0);
print(bp,quoi="n");
plot(bp);

############################################################
form3titre("Demonstrating the creation of /bn/ with a three nodes /bn/");
form3titre("from standard distributions",0)

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
fifi <- "rsbn.demo.bn.three_nodes.txt";
fifi <- paste(chemin,fifi,sep="");
cat("chemin = <<<",chemin,">>>\n",sep="");
cat("fifi = <<<",fifi,">>>\n",sep="");
cat("wd = <<<",getwd(),">>>\n",sep="");
form3titre("Looking at the file defining the /bn/");
print(readLines(fifi));
form3titre("Looking at the file defining the data base");
print(readLines("rsbn.demo.data.small_population.txt"));

form3titre("Loading the associated /bn/");
bc3 <- read8bn(fifi);
form3titre("Having a look to the resulting /bn/",0);
print(bc3,quoi="n");
