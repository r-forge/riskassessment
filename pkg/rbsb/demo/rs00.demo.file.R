chemin <- searchpaths()[grep("rbsb",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# creating rs00 S4 object
# 
# 10_02_11 10_03_10 10_06_16 10_06_22
#
#


############################################################
form3titre("Demonstrating the reading of lists from files");

# from an existing text file
if (!exists("chemin")) {
    cat("A file into the private 'files' directory is needed\n");
    erreur("The package must be installed to run this demo");
}
#
ttt <- paste(chemin,"rs00.demo.list.txt",sep="");
#
lili <- file2list(ttt,path="");
print(lili);


############################################################
form3titre("Demonstrating the writing of lists to files");

list2file(lili,"toto");
print(readLines("toto"));
lolo <- file2list("toto");
print(lolo);