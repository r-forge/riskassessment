chemin <- searchpaths()[grep("rbsb",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# transforming character vectors into vector/matrix/array
# 
# 10_06_28 10_06_29
#
#


############################################################
form3titre("Transforming characters to structures");
############################################################
form3titre("to vectors with collapsing");
uu <- char2vma(letters,"c");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to vectors with collapsing and separating");
uu <- char2vma(LETTERS,"C",xsep="D");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to numerical vectors non collapsed");
uu <- char2vma(as.character(1:10),"v");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to vectors with names (1)");
uu <- char2vma(c(letters[1:10],as.character(1:10)),"V");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to vectors with names (2)");
uu <- char2vma(c(letters[1:10],as.character(1:10)),"U");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to matrix with no names");
uu <- char2vma(c(letters[1:5],"//",letters[6:10],"//",letters[11:15]),"m");
print(uu);
print(vma2char(uu));
uu <- char2vma(c(letters[1:5],"//",letters[6:10],"//",letters[11:15]),"M");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to matrix with column names");
uu <- char2vma(c(letters[1:5],"//",6:10,"//",11:15),"n");
print(uu);
print(vma2char(uu));
uu <- char2vma(c(letters[1:5],"//",6:10,"//",11:15),"N");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to matrix with row names");
uu <- char2vma(c("A1",1:5,"//","B2",6:10,"//","C3",11:15),"o");
print(uu);
print(vma2char(uu));
uu <- char2vma(c("A1",1:5,"//","B2",6:10,"//","C3",11:15),"O");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to matrix with row and column names");
uu <- char2vma(c(1:3,"//","b",4:6,"//","c",14:16,"//","d",24:26),"p");
print(uu);
print(vma2char(uu));
uu <- char2vma(c(1:3,"//","b",4:6,"//","c",14:16,"//","d",24:26),"P");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to 3-way array without names");
uu <- char2vma(c("2","4","5","//",1:40),"a",nat="N");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to 3-way array with dimnames");
uu <- char2vma(c("2","4","5","//",letters[1:2],"//",LETTERS[1:4],"//",1:5,"//",1:40),"A",nat="N");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to 3-way array with names and no dimnames");
uu <- char2vma(c("2","4","5","//","one","two","three","//",1:40),"b",nat="N");
print(uu);
print(vma2char(uu));
############################################################
form3titre("to 3-way array with names and dimnames");
uu <- char2vma(c("2","4","5","//",
                 "one","two","three","//",
                 letters[1:2],"//",LETTERS[1:4],"//",1:5,"//",
                 1:40),"B",nat="N");
print(uu);
print(vma2char(uu));
