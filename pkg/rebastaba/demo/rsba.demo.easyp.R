chemin <- searchpaths()[grep("rebastaba",searchpaths())];
chemin <- paste(chemin,"/files/",sep="");
# script test for rsba
# 
# 10_06_09
#
#

#
rsba3k("RESET");
#

#
form3titre("Testing easyp2code2");
#
# Trying some expressions
#
cat("\n");
cat(easyp2code2("((1+(1+({{A}}+2)))*({{B}}+5))^2"),"\n");
#
cat("\n");
cat(easyp2code2("(|2|)"),"\n");