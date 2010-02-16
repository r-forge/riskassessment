#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rs00");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rs00",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# script test for easyp3cut et al.
# 
# 09_10_12
#
#
form3titre("Testing the use of 'easyp3cut' and 'easyp3stickback'");

# preparing the expressions to parse
ex <- character(0);
ex[1] <- "(A)+{B}+[C]";

# parsing and recompacting
for (ii in sjl(ex)) {
    res0 <- ex[ii];
    form3line(wid=1);
    form3titre("Initial Expression",0);
    print(res0);
    res1 <- easyp3cut(ex[ii]);
    form3titre("Decomposed Expression",0);
    print(res1);
    res2 <- easyp3stickback(res1);
    form3titre("Recomposed Expression",0);
    print(res2);
    form3line(wid=1);
}
