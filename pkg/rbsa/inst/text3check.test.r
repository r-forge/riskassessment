#
# 14_06_10 14_06_12
#
# checking text3 functions and list4text
#
library(rbsa);
#
ptop <- 0;
ptop <- NA;
#
uu <- paste(letters,collapse=""); UU <- toupper(uu);
ww <- matrix(c(1,2,2,1,12,13,2,5,10),ncol=3,byrow=TRUE);
texts4text(c(uu),ww[1,,drop=FALSE]);
texts4text(c(uu),ww[1:2,]);
texts4text(c(uu,UU),ww);
texts4text(c(uu,"",UU),ww);
texts4text(c(uu,"",UU),ww,rmeli=FALSE);
texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE);
texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE,addend=FALSE);
print(texts4text(c(uu),c(1,7,6)));
pause("texts4text",top=ptop);
#
#ptop <- NA;
#
uu <- c("1rst paragraph","comprising two lines",
        "{#} <<un>>","{#}","<<deux>>","2d and last paragraph");
print(parse8text(uu));
pause("parse8text (uu)",top=ptop);
#
uuu <- c("1rst paragraph","-","2d paragraph","",
        "{#} <<un>>","{#}","<<deux>>","","3rd paragraph",
         "{#} << dernier item...>>","last found paragraph");
print(parse8text(uuu));
pause("parse8text (uuu)",top=ptop);
#
vv <- c("1rst paragraph","-","2d paragraph","-",
        "{AA} <<un>>","{BBB}","<<deux>>","","3rd and last paragraph");
print(parse8text(vv));
pause("parse8text (vv)",top=ptop);
print(parse8text(rbsa0$text4$v));
pause("parse8text (rbsa0$text4$v)",top=ptop);
#
tt <- rbsa0$text2$v;
nposi <- seq(1,601,50);
ijpos <- text3n2ij(nposi,tt);
print(cbind(nposi,ijpos));
pause("(row,column)",top=ptop);
ij2np <- text3ij2n(ijpos,tt);
print(cbind(nposi,ijpos,ij2np,nposi-ij2np));
pause("(row,column)",top=ptop);
#
bb1 <- text3brackets(paste(letters,collapse=""),c("j","u"));
print(bb1);
pause("bracket1",top=ptop);
bb2 <- text3brackets(c(" juste {un","deux ou trois} suffiront !"));
print(bb2);
pause("bracket2",top=ptop);
bb3 <- text3brackets(c(" juste {u}n","d{eux ou ","trois}{"," su","ff","iront !}"));
print(bb3);
pause("bracket3",top=ptop);
#
ee <- c("{ee} << Premier.>>","{oui} << et encore une",
        " qui tient sur deux lignes.>>","{f}","<<fuinui.>>");
ff <- list4text(ee);
print(ff);
pause("list",top=ptop);
#
uu <- c(" Je veux   voir  ","{a}<<OUI>>",""," et re-voir  ");
print(uu);
for (aa in c("r","b","B","R","w","c","v","j")) {
  vv <- text3preparation(uu,preparation=aa);
  print(vv);
  pause(aa,top=ptop);
}
#
cat("<<<-------------->>>\n");
for (ii in bf(rbsa0$text3$v)) {
  ee <- text3stext(rbsa0$text3$v,c(ii-1,1,ii+1,Inf));
  print(ee);cat("<<<>>>\n");
  pause(ii,top=ptop);
}
cat("<<<-------------->>>\n");
