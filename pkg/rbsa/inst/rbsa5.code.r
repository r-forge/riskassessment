
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
list4text <- function(text,content=c(1,length(text)),ccom="#",
                      lndel=c("{","}"),lvdel=c("<<",">>"))
#TITLE Analyses and prepares a list from a /text/ 
#DESCRIPTION from a text with convenient tags
# returns a named list. Labelling tags must be 
# in first position of the lines, possibly after removing
# some precised character.
#DETAILS
# Useless lines can exist before and after the list, and also
# between the list items.
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# from which to extract the list.>>
#[INPUTS]
#{content} << Indicates which components of \samp{text} to consider.
#             Usually \samp{numeric(2)} to indicate the interval of lines to consider.
#             When \samp{numeric(1)}, only this line. When \samp{0} or \samp{diff(content)<0} 
#             a \samp{list(0)} is returned.>>
#{ccom} <<Character(s) at the beginning of lines to possibly be removed.
#         When \samp{character(0)} no removing is performed.>>
#{lndel} <<Starting and ending delimiters for the name of the item (at the beginning of the line).>>
#{lvdel} <<Starting and ending delimiters for the description of the item.>>
#VALUE
# A named list.
#EXAMPLE
# list4text(c("{a}<<Il etait une fois>>",
#                "{b}<<   un petit et rouge chaperon>>{c}",
#                "<< pas bon>>",
#                "{d}    <<et le m'echant loup...>>"))
# list4text(c("{a}","<<","pour voir",">>"));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 13_10_14
#REVISED 14_06_10
#--------------------------------------------
{
  # initializing
  res <- vector("list",0);
  # checking
  object9(content,
             "numeric",len=1:2,con=c(0,Inf),
             mensaje="Bad 'content' argument (1)");
  if (content[1]==0)   { return(res);}
  if (diff(content)<0) { return(res);}
  if (length(content)!=2) { content <- rep(content[1],2);}
  object9(content,"numeric",len=2,con=c(1,length(text)),
               mensaje="Bad 'content' argument (2)");
  #
  object9(ccom,
             "character",len=0:1,
             mensaje="Bad 'ccom' argument");
  # restricting the treatment
  text <- text[content[1]:content[2]];
  ## removing starting tags
  if (length(ccom)==1) {
    nbc <- nchar(ccom);
    if (nbc>0) {
      for (tt in bf(text)) {
        tex <- text[tt];
        if (substr(tex,1,nbc)==ccom) {
          text[tt] <- substr(tex,nbc+1,nchar(tex));
        }
      }
    }
  }
  #
  object9(lndel,
             "character",len=2,
             mensaje="Bad 'lndel' argument");
  if (any(lndel=="")) {
    erreur(lndel,"bad labelling tags");
  }
  object9(lvdel,
             "character",len=2,
             mensaje="Bad 'lvdel' argument");
  if (any(lvdel=="")) {
    erreur(lndel,"bad valuing tags");
  }
  # extracting the list
  #
  # localizing the list delimiters
  oudon <- places4text6tags(text,tags=c(lndel[1],lndel[2],
                                        lvdel[1],lvdel[2]));
  # getting the lines of item names
  ou1 <- which((oudon[,"tag"] == lndel[1]) & (oudon[,"co1"] == 1));
  ou2 <- which(oudon[,"tag"] == lndel[2]);
  oun <- intersect(oudon[ou1,"li1"],oudon[ou2,"li1"]);
  nbi <- length(oun);
  # getting the item names
  inames <- rep("",nbi);
  for (ii in bc(nbi)) {
    tex <- text[oun[ii]];
    inames[ii] <- substr(tex,nchar(lndel[1])+1,as.numeric(regexpr(lndel[2],tex))-1);
  }
  # getting the value for each item name
  ivalues <- rep("",nbi);
  for (ii in bc(nbi)) {
    # interval where must be the values
    i1 <- oun[ii];
    i2 <- c(oun,length(text))[ii+1];
    # first position for opening the value item
    v1 <- which(oudon[,"tag"] == lvdel[1]);
    v1 <- oudon[v1,"li1"];
    v1 <- v1[(v1>=i1)&(v1<=i2)];
    if (length(v1) == 0) {
      ivalues[ii] <- NA;
    } else {
      # first position for closing the value item
      v1 <- v1[1];
      w1 <- which((oudon[,"li1"]==v1)&(oudon[,"tag"]==lvdel[1]));
      if (length(w1)==0) { erreur(v1,"Internal Error 1!");}
      w1 <- oudon[w1[1],"co1"];
      v2 <- which(oudon[,"tag"] == lvdel[2]);
      v2 <- oudon[v2,"li1"];
      v2 <- v2[(v2>=v1)&(v2<=i2)];
      if (length(v2) == 0) {
        ivalues[ii] <- NA;
      } else {
        v2 <- v2[1];
        w2 <- which((oudon[,"li1"]==v2)&(oudon[,"tag"]==lvdel[2]));
        if (length(w2)==0) { erreur(v2,"Internal Error 2!");}
        w2 <- oudon[w2[1],"co2"];
        ivalues[ii] <- list(text3stext(text,c(v1,w1,v2,w2)));
      }
    }
    # removing the remaining braces
    resu <- ivalues[[ii]];
    if (length(resu) > 0) {
      if (nchar(resu[1]) == nchar(lvdel[1])) {
        resu <- resu[-1];
      } else {
        resu[1] <- substr(resu[1],nchar(lvdel[1])+1,nchar(resu[1]));
      }
      der <- length(resu);
      if (nchar(resu[der]) == nchar(lvdel[2])) {
        resu <- resu[-der];
      } else {
        resu[der] <- substr(resu[der],1,nchar(resu[der])-nchar(lvdel[2]));
      }
    }
    #
    res[[ii]] <- resu;
  }
  names(res) <- inames;
  
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3stext <- function(text,sub=c(l1=1,p1=1,l2=Inf,p2=Inf))
#TITLE returns a subtext
#DESCRIPTION from a text,
# gives back a portion according to the \samp{sub} argument.
# Delimiting positions are included, so \samp{sub=c(1,1,1,1)}
# gives back the first character of \samp{text}.
#DETAILS
# Definition of the subtext is flexible (outside positions
# are interpreted as minimum / maximum positions).
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#[INPUTS]
#{sub} << A \samp{numeric(4)} indicating the portion of the \samp{text} to consider; can be a matrix but only one row is allowed.
# See the function \samp{text3interval} for details.>>
#VALUE
# The resulting sub-text
#EXAMPLE
# aa <- c(paste(letters,collapse=""),paste(LETTERS,collapse=""),paste(0:9,collapse=""));
# text3stext(aa);
# text3stext(aa,c(1,12,1,15));
# text3stext(aa,c(1,12,2,15));
# text3stext(aa,c(1,12,3,15));
# # In this last example, the last character of line 2 is kept (flexibility)!
# text3stext(aa,c(2,68,3,15));
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_10_16
#REVISED 14_06_12
#--------------------------------------------
{
  # checking finished by text3interval
  if (is.matrix(sub)) {
    if (nrow(sub)>1) {
      erreur(sub,"Only one interval is allowed");
    }
  }
  # getting the interval
  sub <- text3interval(text,sub);
  # extracting
  ii <- 1;
  if (is.na(sub[ii,1])) {
    res <- character(0);
  } else {
    res <- text[bd(sub[ii,1],sub[ii,3])];
    if (sub[ii,1]==sub[ii,3]) {
      res <- substr(res,sub[ii,2],sub[ii,4]);
    } else {
      res[1] <- substr(res[1],sub[ii,2],nchar(res[1]));
      res[length(res)] <- substr(res[length(res)],1,sub[ii,4]);
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3places8word <- function(text,word,
                            column=c(1,Inf),
                            which=c(1,Inf)
                            )
#TITLE returns the places of one word
#DESCRIPTION from a text,
# gives back the positions of a word indicated by
# the associated intervals as defined in the function \samp{text3interval}. 
# The search can be constrained to specific columns. The output can
# also be restricted.
#DETAILS
# The word cannot be extended upon two successive lines but the same line
# can have more than one word. Be aware that overlapping patterns are not 
# all detected (see one of the examples).
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#{word}<< \samp{character(1)} the word to be found.>>
#[INPUTS]
#{column} << The columns where the first character of the word must found.
# \samp{c(1,1)} means that it must be at the very start of a line. \samp{c(10,12)}
# means that it must start on the 10th, 11th or 12th column of a line.>>
#{which} <<Which occurences of \samp{word} (not the line numbers)
# must be returned defined by
# the number of the first one and the number of the last one.\cr
# So \samp{c(2,2)} will designate the second and only the second;
# \samp{c(1,5)} will ask for the first five. When the components are
# both negative, the numbering is done from the end, so \samp{c(-1,-1)}
# means the last one and \samp{c(-1,-3)} asks for the last three ones
# given starting from the last. >>
#VALUE
# A four column matrix, each row corresponding to a word place with the help of
# an interval.\cr
# For negative values of \samp{which}, the order of occurences is reversed: the last
# found will be in the first row of the matrix output.
#EXAMPLE
# text3places8word(letters,"j");
# text3places8word(letters,"J");
# text3places8word(c("Il etait une fois","un petit et rouge chaperon"),"et");
# text3places8word(c("Il etait une fois","un petit et rouge chaperon"),"et",which=c(2,3));
# text3places8word(c("Il etait une fois","un petit et rouge chaperon"),"et",which=-c(1,3));
# text3places8word(c("# Il etait une fois"," #un petit et rouge chaperon"),"#");
# text3places8word(c("# Il etait une fois"," #un petit et rouge chaperon"),"#",column=c(1,2));
# text3places8word(c("# Il etait une fois"," #un petit et rouge chaperon"),"#",column=c(2,2));
# # overlapping pattern  
# text3places8word("aaaa","aa");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE Think of a way to introduce "end of line" as a possible word.
#       Improve the case of overlapping patterns.
#AUTHOR J.-B. Denis
#CREATED 13_07_08
#REVISED 13_10_30
#--------------------------------------------
{
  # checking
  object9(text,"character");
  object9(word,"character",len=1);
  object9(which,"numeric",len=2);
  object9(column,"numeric",len=2);
  # initializing
  res <- matrix(NA,0,4,dimnames=list(NULL,c("i1","p1","i2","p2")));
  # degenerate cases
  if (length(text)==0) { return(res);}
  if (length(word)==0) { return(res);}
  # reversing if negative numbering
  if (which[1] < 0) {
    reve <- TRUE;
    text <- rev(text);
    which <- -which;
  } else { reve <- FALSE;}
  # checking
  if (diff(which) < 0) {
    stop("'which' not consistent");
  }
  # looking for the word
  ouhou <- gregexpr(word,text,fixed=TRUE);
  # looping onto the lines
  nbw <- 0; nlw <- nchar(word); nbtl <- length(text);
  for (nbl in bf(text)) {
    # looking in the line
    if (ouhou[[nbl]][1]>0) {
      # at least an occurrence
      for (oo in bc(length(ouhou[[nbl]]))) {
        nbw <- nbw + 1;
        if (nbw >= which[1]) {
          if ((ouhou[[nbl]][oo] >= column[1]) &
              (ouhou[[nbl]][oo] <= column[2])) {
            res <- rbind(res,c(nbl,ouhou[[nbl]][oo],
                               nbl,ouhou[[nbl]][oo]+nlw-1));
          }
        }
      }
    }
    if (nbl==which[2]) { return(res);}
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3brackets <- function(text,bra=c("{","}"),
                          col1=c(1,Inf),col2=c(1,Inf),
                          rembra=TRUE,
                          which=c(1,Inf))
#TITLE returns the content of pairs of brackets
#DESCRIPTION from a text,
# gives back the contents of a couple of tags (opening and
# closing tags) indicated by \samp{bra} under the constraint
# that the first character of each delimiter be in the column
# interval of each line indicated with \samp{col1} (respectively
# \samp{col2}).
#DETAILS
# Tags cannot be upon two successive lines. The same line
# can have more than one tag. The result is built by an analysis
# of the result provided by \samp{text3places8word}.\cr
# When an opening tag is discovered, the following opening
# tag is not considered before a closing tag has been encountered 
# for the previous one.
# So the sequence \samp{"\{ toto \{tut\} bof\} \{deux\}"} will gives two
# contents, respectively \samp{" toto \{tut"} and \samp{"deux"}.\cr
# When an opening tags remains not consistently closed a fatal
# error is issued with some indication.
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#[INPUTS]
#{bra}<< \samp{character(2)} the pair of tags to use.>>
#{col1} <<Positions within a line where the opening bracket has to be found.>>
#{col2} <<Positions within a line where the closing bracket has to be found.>>
#{rembra} <<Must the brackets be removed?>>
#{which} <<Which occurence of \samp{tag} (not the line numbers)
# must be returned defined by the
# the number of the first one and the number of the last one.>>
#VALUE
# A \samp{list} of texts. Portion within two braces are eliminated.
#EXAMPLE
# text3brackets(paste(letters,collapse=""),c("j","u"));
# text3brackets(c(" juste {un","deux ou trois} suffiront !"));
#REFERENCE
#SEE ALSO
#CALLING text3places8brackets
#COMMENT
#FUTURE Think to a way to introduce "end of line" as a possible tag.
#AUTHOR J.-B. Denis
#CREATED 13_10_28
#REVISED 13_10_31
#--------------------------------------------
{
  # checking
  object9(text,"character");
  object9(bra,"character",len=2);
  object9(which,"numeric",len=2);
  object9(col1,"numeric",len=2);
  object9(col2,"numeric",len=2);
  # looking for bracket places
  uuu <- text3places8brackets(text,bra,col1,col2,which);
  # getting the different components
  res <- vector("list",0);
  for (con in bc(dim(uuu)[3])) {
    # getting the intervals
    res[[con]] <- text3stext(text,as.numeric((uuu[,,con])));
    # removing the closing parenthese
    if (rembra) {
      if (length(res[[con]]) > 0) {
        # closing one
        dli <- length(res[[con]]); dca <- nchar(res[[con]][dli]);
        res[[con]][dli] <- substr(res[[con]][dli],1,dca-nchar(bra[2]));
        # opening one
        dca <- nchar(res[[con]][1]);
        res[[con]][1] <- substr(res[[con]][1],nchar(bra[1])+1,dca);
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
parse8text <- function(text,item1=c("{","}"),item2=c("<<",">>"),
                       numb="#",bull="*",lsep="-")
#TITLE returns the parsed content of a text
#DESCRIPTION from a text comprising paragraphs and items
# finds the different components and returns them by means of a list.
# When the component is a paragraph, it is a \samp{character},
# When the component is an item list, it is a named list.\cr
# This function is not intended for standard users.
#DETAILS
# Each item of a list must comprise two values, framed with \samp{item1} and
# \samp{item2}. When the first value is \samp{numb}, it is a numbered
# item; when the first value is \samp{bull}, it is a bullet item;
# if not it is a labelled item.\cr
# Each component, paragraphs and items are supposed to be proposed on
# non overlapping lines.\cr
# Successive items are considered to belong to the list of items knowing 
# that empty lines (comprising zero characters) are first eliminated 
# (a line with a blank is not empty and will be considered as a paragraph).
# Also are eliminated lines starting with \samp{lsep}, their role is to
# separate distinct paragraphs and lists.\cr
# When the braces for items are not consistent, no error is reported but the
# staff is interpreted as part of a paragraph.\cr
# When two list items have got identical labels, an error is reported.
#KEYWORDS 
#INPUTS
#{text}<< The \samp{text} to be parsed. For the moment just a \samp{character}
# vector.>>
#[INPUTS]
#{item1}<< \samp{character(2)} the pair of tags to use to define the
# first value of an item. When \samp{numb} interpreted as an enumeration,
# when \samp{*} interpreted as an itemized list, if not a description
# list. The first character of \samp{item1[1]} must start at the beginning
# a line and the two braces must be on the same line.>>
#{item2}<< \samp{character(2)} the pair of tags to use to define the
# second value of an item.>>
#{numb} << \samp{character(1)} code to indicate automatically numbered items.>>
#{bull} << \samp{character(1)} code to indicate bullet items.>>
#{lsep} << \samp{character(1)} Each line starting with \samp{lsep} is
# considered a tagging line to separate two paragraphs or two item lists.
# They can be used to separate a paragraph and an item list but are useless.
# Separating lines within list items are not considered as separating.
# Successive separating lines are considered as a unique separating line.
# They are eliminated in the resulting list.>>
#VALUE
# A named \samp{list}. The names for paragraphs start with \samp{P}, those
# for item lists with \samp{L}.
#EXAMPLE
# parse8text(c("{a}","<<","pour voir",">>"));
# uu <- c("1rst paragraph","","2d paragraph","",
#         "{#} <<un>>","{#}","<<deux>>","","3rd and last paragraph");
# parse8text(uu);
# vv <- c("1rst paragraph","","2d paragraph","",
#         "{AA} <<un>>","{BBB}","<<deux>>","","3rd and last paragraph");
# parse8text(vv);
# parse8text(rbsa0$text4$v);
#REFERENCE
#SEE ALSO
#CALLING 
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 13_12_30
#REVISED 14_08_07
#--------------------------------------------
{
  # checking
  object9(text,"character");
  object9(item1,"character",len=2);
  if (any(item1=="")) {
    erreur(item1,mensaje="(1) A closing cannot be ''");
  }
  object9(item2,"character",len=2);
  if (any(item2=="")) {
    erreur(item2,mensaje="(2) A closing cannot be ''");
  }
  object9(numb,"character",len=1);
  object9(bull,"character",len=1);
  object9(lsep,"character",len=1);
  # initialization
  res <- ouca <- vector("list",0);
  nt <- np <- nn <- ni <- nd <- nitem <- 0;
  lili <- bf(text);
  # removing empty lines
  vides <- which(text=="");
  if (length(vides)>0) { text <- text[-vides];}
  # corner case
  if (length(text) == 0) { return(vector("list",0));}
  # sticking to its previous line
  # every line starting with an \samp{item2[1]}
  # (the used algorithm fails in that circumstance)
  colle <- (substr(text,1,nchar(item2[2]))==item2[2]);
  colle[1] <- FALSE;
  for (ii in rev(which(colle))) {
    text[ii-1] <- paste0(text[ii-1],text[ii]);
    text <- text[-ii];
  }
  # looking for the positions of valid list items
  b1 <- text3places8word(text,item1[1],column=c(1,1));
  if (nrow(b1)>0) {
    # possibly some lists are present
    eli <- TRUE;
    nb1 <- text3ij2n(b1[,1:2],text);
    b2 <- text3places8word(text,item1[2]);
    if (nrow(b2)>0) {
      nb2 <- text3ij2n(b2[,3:4],text);
      b3 <- text3places8word(text,item2[1]);
      if (nrow(b3)>0) {
        nb3 <- text3ij2n(b3[,1:2],text);
        b4 <- text3places8word(text,item2[2]);
          if (nrow(b4)>0) {
            nb4 <- text3ij2n(b4[,3:4],text);
            # finding which braces must be considered
            bra <- matrix(NA,0,4);
            while (length(nb1) > 0) {
              bb1 <- nb1[1]; nb1 <- nb1[-1];
              nb2 <- nb2[which(nb2>bb1)];
              if (length(nb2)>0) {
                bb2 <- nb2[1]; nb2 <- nb2[nb2>bb2];
                nb3 <- nb3[which(nb3>bb2)];
                if (length(nb3)>0) {
                  bb3 <- nb3[1]; nb3 <- nb3[nb3>bb3];
                  nb4 <- nb4[which(nb4>bb3)];
                  if (length(nb4)>0) {
                    bb4 <- nb4[1]; nb4 <- nb4[nb4>bb4];
                    bra <- rbind(bra,c(bb1,bb2,bb3,bb4));
                  }
                }
              }
            }
          } else {
            eli <- FALSE;
          }
      } else {
        eli <- FALSE;
      }
    } else { 
      eli <- FALSE;
    }
    
  } else {
    # no list
    eli <- FALSE;
  }
  ## identifying the role of the different lines
  # The default paragraphs
  lig <- rep("P",length(text));
  # The separating lines
  if (nchar(lsep)>0) {
    lig[substr(text,1,nchar(lsep))==lsep] <- "S";
  }
  # The item lists
  if (eli) {
    # adjusting for the brace widths
    bra[,1] <- bra[,1]+nchar(item1[1]);
    bra[,2] <- bra[,2]-nchar(item1[2]);
    bra[,3] <- bra[,3]+nchar(item2[1]);
    bra[,4] <- bra[,4]-nchar(item2[2]);
    # going back to (line,column) positions
    pbra <- array(NA,dim=c(4,2,nrow(bra)),
                  dimnames=list(c("s1","e1","s2","e2"),
                                c("row","col"),bc(nrow(bra))));
    pbra["s1",,] <- t(text3n2ij(bra[,1],text));
    pbra["e1",,] <- t(text3n2ij(bra[,2],text));
    pbra["s2",,] <- t(text3n2ij(bra[,3],text));
    pbra["e2",,] <- t(text3n2ij(bra[,4],text));
    #
    for (li in bc(dim(pbra)[3])) {
      lig[pbra["s1","row",li]:pbra["e2","row",li]] <- paste0("L",li,"-");
    }
    # checking that the item names are on the same line
    egal <- (pbra["s1","row",]!=pbra["e1","row",]);
    if (any(egal)) {
      print(text[sort(unique(c(pbra["s1","row",],pbra["e1","row",])))]);
      erreur(pbra[,,egal,drop=FALSE],"Some item name not on the same line");
    }
  }
  ## numbering the different roles
  ## and initialization of the resulting list
  npa <- nli <- 0; courant <- "X";
  res <- vector("list",0); loli <- 0;
  for (lili in bf(lig)) {
    if (lig[lili] == "P") {
      if (courant != "P") {
        npa <- npa+1;
        loli <- loli+1;
        res[[loli]] <- character(0);
        names(res)[loli] <- paste0("P",npa);
      }
      lig[lili] <- paste0(lig[lili],npa);
      courant <- "P";
    }
    if (substr(lig[lili],1,1) == "L") {
      if (courant != "L") {
        nli <- nli+1;
        loli <- loli+1;
        res[[loli]] <- vector("list",0);
        names(res)[loli] <- paste0("L",nli);
      }
      lig[lili] <- paste0(lig[lili],nli);
      courant <- "L";
    }
    if (lig[lili] == "S") {
      courant <- "S";
    }
  }
  ## decoding the lig coding
  llig <- lig[substr(lig,1,1)=="L"];
  llig <- substr(llig,2,nchar(llig));
  llig <- strsplit(llig,"-");
  lli1 <- as.numeric(sapply(llig,function(a)a[1]));
  lli2 <- as.numeric(sapply(llig,function(a)a[2]));
  ## filling the different components
  for (coco in bf(res)) { 
    quoi <- names(res)[coco];
    if (substr(quoi,1,1)=="P") {
      # a paragraph
      res[[coco]] <- text[lig==quoi];
    } else {
      # a list
      # looking for the corresponding items
      nuli <- as.numeric(substr(quoi,2,nchar(quoi)));
      quelles <- (lli2==nuli);
      quelles <- unique(lli1[quelles]);
      labt <- character(0);
      for (ququ in bf(quelles)) {
        labnum <- as.numeric(t(pbra[c("s1","e1"),,quelles[ququ]]));
        labite <- text3stext(text,labnum);
        if (labite==numb) { labite <- as.character(ququ);}
        if (labite==bull) { labite <- paste0("*",ququ);}
        if (labite %in% labt) {
          erreur(labite,"This label was not unique as list item name");
        } else {
          labt <- c(labt,labite);
        }
        valnum <- as.numeric(t(pbra[c("s2","e2"),,quelles[ququ]]));
        valite <- text3stext(text,valnum);
        res[[coco]][[labite]] <- valite;
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
places4text6tags <- function (text,tags,check=FALSE)
#TITLE returns the places of a series of tags within a /text/
#DESCRIPTION From a text, 
# independently for a series of tags, gives back the positions 
# where they are in the text. A position is given by four
# values \samp{(li1,co1,li2,co2)}, respectively the numbers of
# \samp{li}ne and \samp{co}lumn of the first character and last
# character of the tag. \cr
# Non-overlapping between tags can be checked.
#DETAILS
# One tags cannot be upon two successive lines. The same line
# can have more than one tag. This is a clear generalization of the 
# function \samp{text3places8word} which is called to elaborate the result.\cr
# If some tags have an intersection in the text, the case is considered 
# as no acceptable and a fatal error is issued. For instance in
# "The computation is performed" the two tags "perf" and "formed" are
# not compatible; that is\cr
#  \samp{places4text6tags("is performed",c("perf","formed"))}
# stops with a fatal error.
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text.>>
#{tags}<< \samp{character()} the series of tags to be considered.>>
#[INPUTS]
#{check} << Must overlapping be checked?>>
#VALUE
# A five column data frame: a line for each found tag indicated by a factor;
# four columns giving respectively the \samp{line},
# the \samp{beg}inning and \samp{end}ing positions plus the fifth column
# to indicate the corresponding tag value. When they
# exist \samp{names(tags)} are used for the row dimnames.
#EXAMPLE
# places4text6tags("Bonjour Monsieur","on");
# places4text6tags("Bonjour Monsieur",c("on","ons","mon"));
# \dontrun{places4text6tags("Bonjour Monsieur",c("on","ons","mon"),check=TRUE)};
# places4text6tags(rbsa0$text1$v,c("1","2","7"));
# places4text6tags(rbsa0$text2$v[1:3],"t");
# places4text6tags(rbsa0$text3$v,"uu");
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_10_04
#REVISED 14_08_07
#--------------------------------------------
{
  # getting all positions of the tags
  posi <- matrix(NA,0,5);
  dimnames(posi)[[2]] <- c("tag","li1","co1","li2","co2");
  ftag <- character(0);
  for (ww in bf(tags)) {
    rr <- text3places8word(text,tags[ww]);
    if (!all(rr[,1]==rr[,3])) {
      stop("Unexpected result found in 'places4text6tags'");
    }
    posi <- rbind(posi,cbind(rep(ww,nrow(rr)),rr));
    fftag <- rep(names(tags[ww]),nrow(rr));
    # adding numbering when more than one occurrence
    if (length(fftag)>1) {
      fftag <- paste(bf(fftag),fftag,sep=".");
    }
    ftag <- c(ftag,fftag);
  }
  # processing the results
  if (nrow(posi)>1) {
    # ordering the result
    ooo <- order(posi[,"li1",drop=FALSE],posi[,"co1",drop=FALSE]);
    posi <- posi[ooo,,drop=FALSE];
    # data.framing
    res <- as.data.frame(posi[,c("li1","co1","li2","co2"),drop=FALSE]);
    res$tag <- as.factor(tags[posi[,"tag",drop=FALSE]]);
    if (!is.null(names(tags))) {
      dimnames(res)[[1]] <- ftag[ooo];
    }
    # checking possible non overlapping
    if (check) {
      mama <- max(posi[,"co1"],posi[,"co2"])+2;
      debu <- posi[,"li1"] * mama + posi[,"co1"];
      fini <- posi[,"li1"] * mama + posi[,"co2"];
      crit <- c((debu[-1] <= fini[-length(fini)]),FALSE);
      if (!all(!crit)) {
        ouou <- which(crit);
        ouou <- sort(unique(pmin(pmax(1,c(ouou,ouou-1,ouou+1)),nrow(posi))));
        print(res[ouou,,drop=FALSE]);
        erreur(NULL,"Overlapping discovered...");
      }
    }
  } else {
    res <- as.data.frame(cbind(posi[,c("li1","co1","li2","co2"),drop=FALSE],tag=character(0)));
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
texts4text <- function(text,where,
                       addbeg=TRUE,addend=TRUE,rmeli=TRUE)
#TITLE returns a list of texts from a text
#DESCRIPTION from a text, 
# gives back a list of texts according to the delimitations
# proposed into \samp{where}; typically \samp{where} can be deduced from 
# output of \samp{places4text6tags}. Empty lines can be removed.\cr
# The idea is from a text comprising several lines, to get pieces of it
# after eliminating the delimitating tags; when the tags have already been
# identified since they are indicated through their positions with
# the argument \samp{where}. A delimitation is given in each row of \samp{where},
# so according to the values of \samp{addbeg} and \samp{addend} the maximum
# length of the returned list is \samp{nrow(where)-1}, \samp{nrow(where)}
# or \samp{nrow(where)+1}.
#DETAILS
# When \samp{nrow(where)==0} an empty list is returned without
# warning.\cr
# The component number \samp{i} of the resulting list, is the portion of \samp{text}
# in between the two positions indicated by rows \samp{i} and \samp{i+1} of
# \samp{where} of course, it can comprise several lines, that is be a
# character vector.\cr
# The two positions defining a delimitation are included (i.e. second and third columns
# of the \samp{where} matrix) in the tag. Nevertheless a split can be obtained at
# position \samp{7} in row \samp{3} by the following row in matrix \samp{where}:
# \samp{c(3,7,6)}.
# When delimitations are not consistent, a fatal error is issued.\cr
#KEYWORDS 
#INPUTS
#{text}<< A named \samp{character} vector containing the text
# (a component, a line).>>
#{where}<< A two or three column matrix (when a two column matrix, the second column
# is first duplicated as third column and substracted with 1 to generate a simple splitting
# at this point). The first column gives the line number;
# the second column, the first character of the position (excluded in the previous text
# component); the third column, the last character of the position (excluded in the
# next text component). Overlapping of the extraction is not admitted so the difference
# between the second and third column can be greater than one.>>
#[INPUTS]
#{addbeg} <<Add (or not) what is before the first position?>>
#{addend} <<Add (or not) what is after the last position?>>
#{rmeli} <<Must empty lines of the found texts (\samp{""}) must be eliminated?>>
#VALUE
# A list of \samp{character}s each corresponding to an extracted text.
#EXAMPLE
# uu <- paste(letters,collapse=""); UU <- toupper(uu);
# ww <- matrix(c(1,2,2,1,12,13,2,5,10),ncol=3,byrow=TRUE);
# texts4text(c(uu),ww[1,,drop=FALSE]);
# texts4text(c(uu),c(1,7,6));
# texts4text(c(uu),ww[1:2,]);
# texts4text(c(uu,UU),ww);
# texts4text(c(uu,"",UU),ww);
# texts4text(c(uu,"",UU),ww,rmeli=FALSE);
# texts4text(c(uu),ww);
# texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE);
# texts4text(c(uu),ww[1,,drop=FALSE],addbeg=FALSE,addend=FALSE);
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_07_10
#REVISED 14_06_13
#--------------------------------------------
{
  # transforming
  if (is.data.frame(where)) {
    where <- as.matrix(where);
  }
  if (!is.matrix(where)) { if (length(where)%in%2:3) {
    where <- matrix(where,ncol=length(where));
  }}
  # checking
  if (length(object9(where,c("matrix","data.frame"))) > 0) {
    erreur(where,"'where' must be a matix!");
  }
  if ((ncol(where) < 2) | (ncol(where)>3)) {
    erreur(where,"'where' must have two or three columns");
  }
  if (nrow(where) < 1) {
    erreur(where,"'where' must have at least one row");
  }
  # completing with a third column
  if (ncol(where)==2) {
    where <- cbind(where,where[,2]-1);
  }
  # further check
  if (any(where[,3] < where[,2]-1)) {
    erreur(where,"Third column cannot be less that the second one!");
  }
  # dimnaming if necessary
  if (is.null(dimnames(where))) {
    dimnames(where) <- list(paste0("r.",bc(nrow(where))),
                            paste0("c.",bc(ncol(where))));
  } else {
    if (is.null(dimnames(where)[[1]])) {
      dimnames(where)[[1]] <- paste0("r.",bc(nrow(where)));
    }
  }
  # possibly adding the ends
  if (addbeg) {
    where <- rbind(c(1,0,0),where);
    dimnames(where)[[1]][1] <- "<antes>";
  }
  if (addend) {
    ll <- length(text);
    lc <- nchar(text[ll]);
    where <- rbind(where,c(ll,lc+1,lc+1));
    dimnames(where)[[1]][nrow(where)] <- "<despues>";
  }
  # eliminating irrelevant rows
  where <- where[where[,1]>=1,,drop=FALSE];
  where <- where[where[,1]<=length(text),,drop=FALSE];
  #where <- where[where[,2]<1,,drop=FALSE];
  #where <- where[where[,2]>nchar(text[where[,1]]),,drop=FALSE];
  #where <- where[where[,3]<1,,drop=FALSE];
  #where <- where[where[,3]>nchar(text[where[,1]]),,drop=FALSE];
  if (nrow(where) < 1) {
    erreur(where,"'where' must have at least one valid row");
  }
  # sorting
  rrr <- text3ij2n(where[,1:2,drop=FALSE],text);
  where <- where[order(rrr),,drop=FALSE];
  # initializing
  nbt <- nrow(where)-1;
  res <- vector("list",nbt);
  # looking for each text in turn
  for (itt in bc(nbt)) {
    bb <- where[itt   ,1:3];
    be <- where[itt+1,1:3];
    afaire <- TRUE;
    if (bb[1]  > be[1]) { afaire <- FALSE;}
    if (bb[1] == be[1]) {
      if (bb[3]>be[2]) { afaire <- FALSE;}
    }
    if (afaire) {
      if (bb[1] == be[1]) {
        res[[itt]] <- substr(text[bb[1]],bb[3]+1,be[2]-1);
      } else {
        res[[itt]] <- substr(text[bb[1]],bb[3]+1,nchar(text[bb[1]]));
        for (iii in bd(bb[1]+1,be[1]-1)) {
          res[[itt]] <- c(res[[itt]],text[iii]);
        }
        res[[itt]] <- c(res[[itt]],
                    substr(text[be[1]],      1,be[2]-1));
      }
    } else {
      res[[itt]] <- character(0);
    }
    # possibly removing empty lines
    if (rmeli) {
      for (ll in rev(bf(res[[itt]]))) {
        if (nchar(res[[itt]][ll])==0) {
          res[[itt]] <- res[[itt]][-ll];
        }
      }
    }
  }
  # naming
  names(res) <- dimnames(where)[[1]][-(nrow(where))];
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3preparation <- function(text,preparation="rbR",
                         content=c(1,length(text)),
                         ccom="#",
                         llength=50)
#TITLE Analyses and prepares a /text/ 
#DESCRIPTION from a text returns another text after some
# transformations.\cr
# Not intended for the standard user.
#DETAILS
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# to prepare.>>
#[INPUTS]
#{preparation} <<A \samp{character(1)} whose characters indicates the actions
# to perform (in this order):
#     \cr\samp{r} to remove the starting characters of \samp{ccom}.
#     \cr\samp{b} to remove 'blank' characters at both ends of lines.
#     \cr\samp{B} to remove repeated 'blank' characters within lines.
#     \cr\samp{R} to remove empty lines.
#     \cr\samp{w} to return the first word of the first line delimited with either blank, \samp{=}
#                 of \samp{<-}.
#     \cr \cr The following options are hierarchized (for instance \samp{j} is equivalent to \samp{cvVSj}).
#     \cr\samp{c} to concatanate lines (between lists if any) into a unique line;
#                 one space is added between two initial lines.
#     \cr\samp{v} to return the vector with one word one component from the concatenated line of \samp{c}.
#     \cr\samp{V} the same as \samp{v} but eliminated repeated words.
#     \cr\samp{S} the same as \samp{v} but sorting the found words.
#     \cr\samp{j} to gather the words obtained after \samp{v} into text components having less that
#                 \samp{llength} characters or only one word.>>
#{content} << Indicates which component of \samp{text} to prepare.
#             Usually \samp{numeric(2)} to indicate the interval of lines to consider.
#             when \samp{numeric(1)}, only this line. When \samp{0} or \samp{diff(content)<0} 
#             \samp{character(0)} is returned.>>
#{ccom} <<A \samp{character(1)} indicating which character(s) at the beginning
#         of lines must possibly be removed (the character(s), not the complete line). >>
#{llength} <<Maximum number of characters for a line (except when it comprises only one word).>>
#VALUE
# The transformed text, that is a \samp{character}.
#EXAMPLE
# text3preparation(rbsa0$text2$v[1:3],preparation="j",llength=10)
# uu <- c("Il etait une fois un petit et rouge chaperon",
#         "qui voulait aller voir sa mere-grand");
# text3preparation(uu,"j",llength=20);
# text3preparation(uu,"j",llength=80);
# text3preparation( c(" Je veux   voir  "," et re-voir  "),"rbBc")
# text3preparation(c("# Je veux   voir  "," et re-voir  "),"rbBc")
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Other functionalities could be added.
#AUTHOR J.-B. Denis
#CREATED 13_10_14
#REVISED 14_07_10
#--------------------------------------------
{
  # initializing
  res <- character(0);
  if (length(text)==0) { return(res);}
  # checking
  object9(text,
             "character",
             mensaje="Bad 'text' argument");
  object9(content,
             "numeric",len=1:2,con=c(0,Inf),
             mensaje="Bad 'content' argument (1)");
  if (content[1]==0)   { return(res);}
  if (diff(content)<0) { return(res);}
  if (length(content)!=2) { content <- rep(content[1],2);}
  object9(content,"numeric",len=2,con=c(1,length(text)),
               mensaje="Bad 'content' argument (2)");
  #
  object9(preparation,
             "character",len=1,
             mensaje="Bad 'preparation' argument");
  object9(ccom,
             "character",len=1,
             mensaje="Bad 'ccom' argument");
  object9(llength,
             "numeric",len=1,con=c(1,Inf),
             mensaje="Bad 'llength' argument");
  # restricting the treatment
  text <- text[content[1]:content[2]];
  ## removing starting tags
  if (belong9("r",preparation)) {
    nbc <- nchar(ccom);
    if (nbc>0) {
      for (tt in bf(text)) {
        tex <- text[tt];
        if (substr(tex,1,nbc)==ccom) {
          text[tt] <- substr(tex,nbc+1,nchar(tex));
        }
      }
    }
  }
  ## removing ending blanks
  if (belong9("b",preparation)) {
    for (tt in bf(text)) {
      tex <- text[tt];
      while (substr(tex,1,1)==" ") {
        tex <- substr(tex,2,nchar(tex));
      }
      while (substr(tex,nchar(tex),nchar(tex))==" ") {
        tex <- substr(tex,1,nchar(tex)-1);
      }
      text[tt] <- tex;
    }
  }
  ## removing repeated blanks
  if (belong9("B",preparation)) {
    for (tt in bf(text)) {
      tex <- text[tt];
      while (belong9("  ",tex)) {
        tex <- gsub("  "," ",tex);
      }
      text[tt] <- tex;
    }
  }
  res <- text;
  ## removing empty lines
  if (belong9("R",preparation)) {
    for (tt in rev(bf(text))) {
      if (nchar(text[tt])==0) {
        text <- text[-tt];
      }
    }
  }
  res <- text;
  ## looking for the first word
  if (belong9("w",preparation)) {
    if (length(text)== 0) {
      erreur(NULL,"No line found to get a first word");
    }
    tex <- text[1];
    n1 <- nchar(strsplit(tex," ",fixed=TRUE)[[1]][1]);
    n2 <- nchar(strsplit(tex,"<-",fixed=TRUE)[[1]][1]);
    n3 <- nchar(strsplit(tex,"=",fixed=TRUE)[[1]][1]);
    ftr <- min(n1,n2,n3);
    return(substr(tex,1,ftr));
  }
  ## concatenation
  if (belong9("c",preparation)|
      belong9("v",preparation)|
      belong9("V",preparation)|
      belong9("S",preparation)|
      belong9("j",preparation)) {
    res <- paste(res,collapse=" ");
  }
  ## vectorisation
  if (belong9("v",preparation)|
      belong9("V",preparation)|
      belong9("S",preparation)|
      belong9("j",preparation)) {
    res <- strsplit(res," ",fixed=TRUE)[[1]];
  }
  ## eliminating redundancy
  if (belong9("V",preparation)|
      belong9("S",preparation)|
      belong9("j",preparation)) {
    res <- unique(res);
  }
  ## sorting
  if (belong9("S",preparation)|
      belong9("j",preparation)) {
    res <- sort(res);
  }
  ## justification
  if (belong9("j",preparation)) {
    if (length(res)>0) {
      rrr <- res[-1]; res <- res[1];
      for (cc in bf(rrr)) {
        if (nchar(res[length(res)])+1+nchar(rrr[cc]) <= llength) {
          res[length(res)] <- paste0(res[length(res)]," ",rrr[cc]);
        } else {
          res <- c(res,rrr[cc]);
        }
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3translate <- function(words,translation,
                           left=TRUE,eline=TRUE)
#TITLE translate words
#DESCRIPTION 
# returns a \samp{character()} deduced from
# \samp{words} replacing some of the words
# by the translation proposed into \samp{translation}.
# A component of \samp{words} can comprise several
# common words. 
#DETAILS
# When a word has no translation, it is left as it is
# when \samp{left} is \samp{TRUE}, suppressed if not.
# Initial spacing is not respected, ending blanks are
# suppressed and no repeated blanks are separating the
# final words.
#KEYWORDS 
#INPUTS
#{words} << A \samp{character()} of words .>>
#{translation}<< A named \samp{character} vector containing the
# translations through the names:
# \samp{names(translation)[i]} becomes \samp{translation[i]}.>>
#[INPUTS]
#{left} << When \samp{TRUE} non translated words are left.>>
#{eline} << Indicates if resulting empty lines must be left.>>
#VALUE
# The resulting \samp{character} after translation.
#EXAMPLE
# tra <- c(a="A",e="E",i="I",o="O",u="U")
# text3translate(letters,tra);
# text3translate(letters,tra,FALSE);
# text3translate(letters,tra,FALSE,FALSE);
# text3translate(rbsa0$text3$v,c(The="xxx",the="xxx"));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_06_14
#REVISED 14_06_14
#--------------------------------------------
{
  # checking
  object9(words,"character");
  object9(translation,"character");
  object9(left,"logical",1);
  object9(eline,"logical",1);
  # standardizing the words to translate
  words <- text3preparation(words,"bBR");
  # degenerate cases
  if (length(translation)==0) {
    return(words);
  }
  # more checking
  if (length(unique(names(translation)))!=length(translation)) {
    erreur(translation,"Some words have different translations");
  }
  # translating
  for (ww in bf(words)) {
    www <- words[ww];
    www <- text3preparation(www,"V");
    for (vv in bf(www)) {
      ou <- which(www[vv]==names(translation));
      if (length(ou)==0) {
        if (!left) {
          www[vv] <- "";
        }
      } else {
        www[vv] <- translation[ou];
      }
    }
    words[ww] <- paste(www,collapse=" ");
  }
  if (eline) {
    words <- text3preparation(words,"R");
  }
  # returning
  words;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
