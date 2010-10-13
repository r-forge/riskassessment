#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rebastaba");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# playing with /bn/ objects
# 
#  10_06_23 10_06_30
#
#


############################################################
form3titre("Transforming /bn/ into lists and vice versa");

form3titre("Initial /gn/",0);
print(rebastaba.bn3);
form3titre("Associated list",0);
li1 <- bn2list(rebastaba.bn3);
print(li1);
form3titre("Back to the /bn/",0);
bn1 <- list2bn(li1);
print(bn1);
#
#

############################################################
form3titre("Transforming /gn/ lists into file and vice versa");

form3titre("Initial List from rebastaba.bn4",0);
li2 <- bn2list(rebastaba.bn4);
print(li2);
form3titre("Writing it on 'toto.txt'",0);
list2file(li2,"toto.txt",tags=rbsb.tag1);
form3titre("Readind this file",0);
print(readLines("toto.txt"));
form3titre("Getting back the list",0);
li3 <- file2list("toto.txt",tags=rbsb.tag1);
print(li3);
#
#

############################################################
form3titre("Reading/writting /bn/ objects");

form3titre("Initial /bn/ is rebastaba.bn1",0);
print(rebastaba.bn1);
form3titre("Writing it on 'toto.txt'",0);
write8bn(rebastaba.bn1,"toto.txt");
form3titre("Getting it back from the file",0);
bb1 <- read8bn("toto.txt");
print(bb1);
#
#
