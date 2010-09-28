chemin <- searchpaths()[grep("g4n",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# playing with /gn/ objects
# 
#  10_06_22
#
#


############################################################
form3titre("Transforming /gn/ into lists and vice versa");

form3titre("Initial /gn/",0);
print(g4n.gn6);
form3titre("Associated list",0);
li1 <- gn2list(g4n.gn6);
print(li1);
form3titre("Back to the /gn/",0);
gn1 <- list2gn(li1);
print(gn1);
#
#

############################################################
form3titre("Transforming /gn/ lists into file and vice versa");

form3titre("Initial List from g4n.gn3",0);
li2 <- gn2list(g4n.gn3);
print(li2);
form3titre("Writing it on 'toto.txt'",0);
list2file(li2,"toto.txt",tags=rbsb.tag2);
form3titre("Readind this file",0);
print(readLines("toto.txt"));
form3titre("Getting back the list",0);
li3 <- file2list("toto.txt",tags=rbsb.tag2);
print(li3);
#
#

############################################################
form3titre("Reading/writting /gn/ objects");

form3titre("Initial /gn/ is g4n.gn7",0);
print(g4n.gn7);
form3titre("Writing it on 'toto.txt'",0);
write8gn(g4n.gn7,"toto.txt");
form3titre("Getting it back from the file",0);
gg7 <- read8gn("toto.txt");
print(gg7);
#
#
