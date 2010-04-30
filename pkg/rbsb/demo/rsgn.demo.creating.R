chemin <- searchpaths()[grep("rbsb",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# creating bdn objects
# 
# 10_02_16
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
