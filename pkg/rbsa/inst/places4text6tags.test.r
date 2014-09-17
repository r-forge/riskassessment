#
# test of places4text6tags
#
library(rbsa);
uu <- c("L1: Juste pour voir",
        "L2: Un exemple de balisage",
        "L3: Et cela devrait suffire");
vv <- c("L1","L3","L2");
w1 <-  places4text6tags(uu,vv);
names(vv) <- c("l1","l3","l2");
w2 <-  places4text6tags(uu,vv);
print(uu);
print(vv);
print(w1);
print(w2);
cat("Test is finished\n");
