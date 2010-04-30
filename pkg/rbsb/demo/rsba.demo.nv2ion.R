chemin <- searchpaths()[grep("rbsb",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# script test for rsba
# 
# 09_10_12 09_10_13 10_02_10
#
#

form3titre("Testing sort8ion");
uu <- new("nom",x=list(A=letters[1:3],C=c("1","2"),B=""));
vv <- nv2ion(0,uu);
print(sort8ion(vv,uu));
print(sort8ion(vv,uu,"a"));


form3titre("Demonstrating and testing the use of 'nv2ion'");

# preparing the /nom/s to parse
nm <- vector("list");
nm[[1]] <- new("nom", x=list(A=letters[1:3], B="", C=c("1", "2")));

# parsing and recompacting
for (ii in sjl(nm)) {
    res0 <- nm[[ii]];
    form3line(wid=1);
    form3titre("Current value of 'rs0'",0);
    print(res0);
    form3affiche(nv2ion("B",res0,"n"));
    form3affiche(nv2ion("B",res0,"N"));
    form3affiche(nv2ion("B",res0,"v"));
    #
    form3affiche(nv2ion("A",res0,"n"));
    form3affiche(nv2ion("A",res0,"N"));
    form3affiche(nv2ion("A",res0,"v"));
    #
    form3affiche(nv2ion(1,res0,"n"));
    form3affiche(nv2ion(1,res0,"N"));
    form3affiche(nv2ion(1,res0,"v"));
    #
    form3affiche(nv2ion(0,res0,"n"));
    form3affiche(nv2ion(0,res0,"N"));
    form3affiche(nv2ion(0,res0,"v"));
    #
    form3affiche(nv2ion("-",res0,"n"));
    form3affiche(nv2ion("-",res0,"N"));
    form3affiche(nv2ion("-",res0,"v"));
    form3line(wid=1);
}
