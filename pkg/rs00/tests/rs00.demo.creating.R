#
# This was added by paquet.pl (version 0.04) (beginning)
#
library("rs00");
#
# This was added by paquet.pl (version 0.04) (end)
#
chemin <- searchpaths()[grep("rs00",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# creating rs00 S4 object
# 
# 10_02_11
#
#


############################################################
form3titre("Demonstrating the creation of 'des' objects");

des1 <- new("des",name="To describe any object you want",
                  orig="For instance, a reference",
                  time=today("nor"),
                  defi="Just a charcter(1) providing the definition",
                  role="here, just an illustration",
                  comm=c("The comment field",
                         "It is the only ones which can comprise several paragraphs")
            );
print(des1,quoi="a");

print(char2des("Just The name"));

############################################################
form3titre("Demonstrating the creation of 'daf' objects");

# from an existing data frame
ddd <- data.frame(A=11:36,B=letters,C=sin(1:26),row.names=NULL);
dafd <- new("daf",des=char2des("From a data.frame"),
                  what="d",
                  valu="ddd"
           );
print(dafd);

# from an existing text file
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
ttt <- paste(chemin,"rs00.demo.creating.txt",sep="");
print(ttt);
print(readLines(ttt));

daft <- new("daf",des=char2des("From a text file"),
                  what="t",
                  valu=ttt
           );
print(daft);


# from an existing function without argument
fff <- function() {
    data.frame(A=11:36,B=letters,C=sin(1:26),row.names=NULL);
}
daff <- new("daf",des=char2des("From a function"),
                  what="f",
                  valu="fff"
           );
print(daff);
