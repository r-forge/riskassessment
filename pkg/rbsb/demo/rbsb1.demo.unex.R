chemin <- searchpaths()[grep("rbsb",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# script test for unex
# 
# 09_09_22 09_09_28 09_09_29
#
#
form3titre("Demonstrating the use of 'unex'");

# preparing the data.frame
nb <- 23; nf <- c(2,7,3);
set.seed(1234);
xv <-  as.data.frame(matrix(0,nb,0));
for (ii in 1:length(nf)) {
    xv <- cbind(xv,factor(sample(nf[ii],nb,TRUE),levels=1:nf[ii]));
}
names(xv) <- LETTERS[1:length(nf)];
# computing the index
Index <- unex(xv,nf);
# printint the result
print(cbind(xv,Index));
