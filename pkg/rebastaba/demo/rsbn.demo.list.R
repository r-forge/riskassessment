chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# creating bn objects
# 
# 10_02_12 10_02_15 10_03_10 10_04_16 10_04_20
# 10_05_25 10_06_09 10_06_21 10_06_30 10_08_12
# 10_09_03


############################################################
form3titre("Demonstrating going from a list to a bn and vice versa");

#
# the basics
print(list2bn(bn2list(rbsb.bn0)));
print(list2bn(bn2list(rbsb.bn1)));
print(list2bn(bn2list(rbsb.bn2)));
print(list2bn(bn2list(rbsb.bn3)));
print(list2bn(bn2list(rbsb.bn4)));
print(list2bn(bn2list(rbsb.bn5)));
print(list2bn(bn2list(rbsb.bn6)));

#
# getting a /bn/ from a data file
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
fifi <- "rsbn.demo.bn.multivariate_node.txt";
fifi <- paste(chemin,fifi,sep="");
bn1 <- read8bn(fifi);
form3titre("Having a look to the initial /bn/",0);
print(bn1,quoi="nv");
#
# transforming it to a list
li1 <- bn2list(bn1);
form3titre("Looking at the resulting list");
str(li1);
#
# coming back to the /bn/
bn2 <- list2bn(li1);
form3titre("Having a look to the final /bn/",0);
print(bn2,quoi="nv");
