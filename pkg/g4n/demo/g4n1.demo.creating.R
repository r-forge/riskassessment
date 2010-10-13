chemin <- searchpaths()[grep("g4n",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# creating bdn objects
# 
# 10_02_16 10_06_14 10_06_17
#
#


############################################################
form3titre("Creation of /gn/ by reading a file");

if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
fifi <- "g4n1.demo.gn.simple.txt";
fifi <- paste(chemin,fifi,sep="");
cat("chemin = <<<",chemin,">>>\n",sep="");
cat("fifi = <<<",fifi,">>>\n",sep="");
cat("wd = <<<",getwd(),">>>\n",sep="");
form3titre("Looking at the file defining the /gn/");
print(readLines(fifi));

form3titre("Loading the associated /gn/");
ge <- read8gn(fifi);
form3titre("Having a look to the resulting /gn/",0);
print(ge,quoi="n");
#
#

############################################################
form3titre("Creation of /gn/ by pseudo-random generation");

set.seed(9876);
ga <- rgn("random graph 1",type="hier",nno=7);
print(ga);
plot(ga);
#
set.seed(9876);
gb <- rgn("random graph 2",type="audit",nno=4,nar=1:4);
print(gb);
plot(gb);
#
set.seed(9876);
gc <- rgn("random graph 3",type="square",nno=19);
print(gc);
plot(gc);
#
set.seed(9876);
gd <- rgn("random graph 4",type="chain",nno=3,nom=c("Bonjour","Bonsoir","Bonne Nuit"));
print(gd);
plot(gd);
#
