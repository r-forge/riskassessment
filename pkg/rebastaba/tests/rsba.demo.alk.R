#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rebastaba");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# script test for rsba
# 
# 10_06_08 10_06_09 10_07_28 10_07_29 10_08_06
#
#

#
rebastaba3k("RESET");
#

#
form3titre("Testing valid8alk");
#
# creating an alk
#
dd <- data.frame(A=rep(1:2,each=2),B=rep(1:2,2));
alk1 <- new8alk(ldes=char2des("fautif"),
                ltype="empidata",
                lpod=list(A=1:2,B=1:2),
                lwin=new("win",nat=structure(c("conti","conti"),.Names=c("A","B")),
                               rty=c("*","systematic")),
                ldaf=new("daf",des=char2des("dada"),
                               what="d",
                               valu="dd"
                        )
               );
#
# making it incorrect by modifying the data frame
#
dd$A <- as.factor(dd$A);
#
# detecting the point
#
print(valid8alk(alk1));


#
form3titre("Testing repeated nodes");
#
# creating an alk
#
alk2.0 <- new8alk(ldes=char2des("repete"),
                  ltype="normal",
                  lrep=2,
                  lvar=c("A","B"),
                  lpara=list(mu=10,sigma=1),
                  lpod=list(c(0,12),c(8,20))
                 );
print(alk2.0);
#
alk2 <- complete8alk(rebastaba.bn2,alk2.0);
#
# making it incorrect by modifying the data frame
#
dd$A <- as.factor(dd$A);
#
# detecting the point
#
print(valid8alk(alk2));
