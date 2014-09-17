#
# 13_07_09 13_07_10 13_10_31 13_11_04 13_12_27
#
library(rbsa);
#
#
#

uu <- c("12345678901234567890123456789012345678901234567890",
        "12345678901234567890123456789012345678901234567890",
        "12345678901234567890123456789012345678901234567890");
vv <- matrix(c(1,3,5,
               1,8,15,
               3,20,27),
             ncol=3,byrow=TRUE);
ww <- texts4text(uu,vv);
print(ww);
pause("texts4text (1)");

uu <- paste(letters,collapse=""); UU <- toupper(uu);
ww <- matrix(c(1,2,2,1,12,13,2,5,10),ncol=3,byrow=TRUE);
texts4text(c(uu),ww[1,,drop=FALSE]);
texts4text(c(uu),ww[1:2,]);
texts4text(c(uu,UU),ww);
texts4text(c(uu),ww);
texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE);
texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE,addend=FALSE);
#
pause("texts4text (2)");
