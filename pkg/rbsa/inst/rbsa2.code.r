
####### rbsa2.code.r ########################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
read8list <- function(file,path=getwd(),bullet=")BULLETS(",
                      clean=TRUE,ended=")STOP(",comment="#",
                      skip=matrix(c(")SKIPPING(",")READING("),1),
                      include=c(")FILE("),
                      tags=rbsa0$tag1$v,
                      sep=rbsa0$sep0$v,
                      rsep="no_action",
                      stag=c("/","/"),
                       monitor=rbsa0$monitor$v
                     )
#TITLE  reads a file and transforms it in a list (of lists) of characters
#DESCRIPTION
# Reads a file and transforms it in a list (of lists) of characters
# It is a slight generalization of \samp{file2list}.\cr
# It is worth explaining why such a function was written.
# The obvious one is that not always names for the components of the
# list are natural. (In future version, it could be good to mixed with
# and without names). Another reason is that the contraint of 'value
# only at the final level is not always natural. \samp{read8list} adds
# itself 'null' components (named \samp{rbsa0$l00$v})
# to allow it. Finally, levels can be skipped
# and missing levels are introduced.\cr
# So the generalization is that
# the components of the list have not to be given
# in the file but are determined by the function from an additionnal
# line (everywhere in the file) tagged with the third argument
# starting at the first character.
# This line contains the list of bullets, giving then their number.
# Notice that all proposed bullets must be different from
#  \samp{rbsa0$l00$v}.
# But everything has to be paid: now the constraint is
# that every component must have a non empty content.
#DETAILS
# An intermediary file is generated (and not deleted) to be read by
# \samp{file2list} function; its name is obtained from the global constant 
# \samp{rbsa0$fin$v}.\cr
# The numbering of the levels of the list is determined by cycling the
# global constant \samp{rbsa0$tyl$v} (run the example to have an idea of what this means).\cr
# Only the first three arguments plus the tags are used by this function, all others are
# directly passed to \samp{file2list}.\cr
# For the moment, the numbering of the different levels does not take 
# into account the level hierarchy, this must be offered in
# future versions of this function.\cr
#KEYWORDS IO
#INPUTS
#{file} << file to be read and transformed into a list.>>
#[INPUTS]
#{path} << Directory containing the file.>>
#{bullet} <<Indicates the bullets to be considered. This tags must
#           be at the very beginning of a line. But the bullets
#           can be preceeded by spaces.>>
#{clean} <<Indicates if starting and ending spaces of every lines
#          must be eliminated at first.>>
#{ended} << To indicate the line from which to stop the reading.>>
#{comment} <<At the beginning of a line, it indicates that this line must not be
#          considered. More than one commenting character can be considered when
#          it is a vector. For instance \samp{c("#","\%")} means that
#          lines starting with an hash or a percent are comment lines.>>
#{skip} << To indicate set(s) of lines to be skipped. Must be a character matrix
#          where the two columns correspond respectively to the opening and 
#          closing tags, and where each row is associate to a couple of tags.
#          Tags are considered successively following the order of these matrix rows.>>
#{include} << Tags to indicate a file (including possible path) by
#             a \samp{character(1)} to include at this point its contents
#             as a text file with the same tags specifications.
#             Including files can be recursive. >>
#{tags} << Character matrix with two columns indicating the opening and closing
#          tags of the lists at different levels. Its row number gives the maximum
#          number of levels. Opening tags must be different.>>
#{sep} << Character sequence used to split the components of character vectors
#         provided within the same 
# line. Notice that \samp{LF} is always considered as a separator.>>
#{rsep} << Indicates if repetitions of \samp{sep} must be considered as 
#          significant or not and which null value to introduce.
#          If \samp{no_action} then the repetitions will be ignored
#          if not \samp{rsep} component(s) will be introduced.>>
#{stag} << Two character strings indicating the tag to define different \samp{sep} for a given
#           [sub]list. These two correspond to \samp{stag[c(1,3)]} of \samp{list2file} function.>>
#{monitor} <<List of constants indicating the monitoring choices,
#            see the \samp{rbsa0$monitor$v} provided object as an example.>>
#VALUE
# a list [of lists [of lists [...] ] ] of character (possibly named) vectors
# or matrices or arrays.
#EXAMPLE
# sink("rbsa.list.txt")
# cat(rbsa0$text5$v,sep="\n");
# sink();
# read8list("rbsa.list.txt");
# # deleting the created file
# unlink("rbsa.list.txt");
# # deleting the intermediate file
# unlink(rbsa0$fin$v);
#REFERENCE
#SEE ALSO list2file
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 11_01_17
#REVISED 11_01_17
#--------------------------------------------
{
# everything, including the check are deported
# to the called functions
#
#
# checking
if (monitor$chk$v) {
  object9(file,"character",1,mensaje="file must indicate the name of one file");
  #
  object9(path,"character",1,mensaje="path must indicate the name of one directory");
  #
  if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
  if (is.na(file.info(fifi)$size)) { erreur(fifi,"This file seems not to exist.");}
  }
#
# reading the file to get the possible bullet
if (path!="") { fifi <- paste(path,file,sep="/");} else { fifi <- file;}
uu <- readLines(fifi);
bu <- grep(bullet,uu,fixed=TRUE);
#
# possibly transforming the character
if (length(bu)>=1) {
  # analyzing the bullets
  bubu <- uu[bu[length(bu)]];
  nabu <- form3split(bubu,OPA="",CPA="",opa="",cpa="",sep=" ")[-1];
  if (rbsa0$l00$v %in% nabu) {
    erreur(list(rbsa0$l00$v,nabu),
           "The proposed bullets comprise the standard bullet defined by 'rbsa0$l00$v', this is not possible");
  }
  lebu <- nchar(nabu);
  nbul <- length(nabu);
  uu <- uu[-bu];
  # getting the replacements
  if (nbul>nrow(tags)) {
    erreur(list(tags,nabu),"More levels of the list are given that the number of provided tags!");
  }
  debu <- rep(tags[,1],nbul)[1:nbul];
  finn <- rep(tags[,2],nbul)[1:nbul];
  nunu <- rep(rbsa0$tyl$v,nbul)[1:nbul];
  orajou <- numeric(0);
  lrajou <- numeric(0);
  qrajou <- character(0);
  for (bb in bc(nbul)) {
    # dealing the bullets of level bb
    lo <- lebu[bb];
    ou <- grep(nabu[bb],uu,fixed=TRUE);
    # first looking where there are and how they are
    ouou <- numeric(0);
    for (ii in ou) {
      # removing the possible first spaces
      lili <- form3crop(uu[ii]);
      if (substr(lili,1,lo)==nabu[bb]) {
        ouou <- c(ouou,ii);
      }
    }
    # preparing everything
    if (length(ouou)>0) {
      nou <- form3numbering(length(ouou),nunu[bb]);
      iii <- 0;
      for (ii in ouou) {
        iii <- 1+iii;
        # getting rid of first spaces
        lili <- form3crop(uu[ii]);
        if (substr(lili,1,lo)!=nabu[bb]) {
          stop("in 'read8list' about 'ouou'");
        }
        lulu <- lili;
        lili <- substr(lili,lo+1,nchar(lili));
        # the string cannot be empty
        if (form3crop(lili)=="") {
          erreur(list(fifi,lulu),
                 "This tagged line is empty which is not allowed!");
        }
        # when we are not at the last level,
        # additional levels must be introduced
        if (bb<nbul) {
          orajou <- c(orajou,ii);
          lrajou <- c(lrajou,bb);
          qrajou <- c(qrajou,nou[iii]);
          uu[ii] <- paste0(debu[nbul],rbsa0$l00$v,finn[nbul],lili);
        } else {
          uu[ii] <- paste0(debu[bb],nou[iii],finn[bb],lili);
        }
      }
    }
  }
  #
  # adding the level to insert
  if (length(orajou)>0) {
    # FIRST ordering things into order
    oo <- order(orajou);
    orajou <- orajou[oo];
    qrajou <- qrajou[oo];
    lrajou <- lrajou[oo];
    for (ii in bf(orajou)) {
      iii <- length(orajou) - ii + 1;
      iiu <- orajou[iii];
      nbajou <- (nbul - lrajou[iii]);
      # shifting at the end to introduce new lines
      uu[bd(iiu+nbajou,length(uu)+nbajou)] <- uu[bd(iiu,length(uu))];
      # adding the chief line
      uu[iiu] <- paste0(debu[lrajou[iii]],
                       qrajou[iii],
                       finn[lrajou[iii]]);
      # adding possible intermediary lines
      jk <- 0;
      for (jjj in bd(lrajou[iii]+1,nbul-1)) {
        jk <- 1+jk;
        uu[iiu+jk] <- paste0(debu[jjj],
                            rbsa0$l00$v,
                            finn[jjj]);
      }
    }
  }
}
#
# writing the intemediary file
if (path!="") { fofo <- paste(path,rbsa0$fin$v,sep="/");} else { fofo <- rbsa0$fin$v;}
writeLines(uu,fofo);
#
# finally calling 'file2list'
res <- file2list(
                 file=rbsa0$fin$v,path=path,
                 clean=clean,ended=ended,comment=comment,
                 skip=skip,
                 include=include,
                 tags=tags,
                 sep=sep,
                 rsep=rsep,
                 stag=stag
                                     );

# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
rbsa7list9 <- function(lili,monitor=rbsa0$monitor$v)
#TITLE checks whether a list is rbsa-compatible list
#DESCRIPTION
# To be a rbsa-list, a list must satisfy the following two properties:
# (i) all components and sub-components are named.
# (ii) all components and sub-components are either a list
# or a character vector/matrix/array (i.e. vma components); they are 
# the leafs of the rbsa-list.\cr
# To be handled by \samp{list2file} or \samp{file2list} functions, a list must
# rbsa-compatible.
#DETAILS
#KEYWORDS IO
#INPUTS
#{lili} << The list structure to be checked.>>
#[INPUTS]
#{monitor} << List of monitoring constants, see \samp{rbsa0$monitor$v} to
#             know its structure.>>
#VALUE
# \samp{TRUE} or \samp{FALSE} according to the results of the checks.
#EXAMPLE
# rbsa7list9(rbsa0$lis1$v);
# rbsa7list9(list(rbsa0$lis1$v));
# rbsa7list9(list(rbsa0$lis1$v,nu=NULL));
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 10_04_19
#REVISED 10_04_20
#--------------------------------------------
{
# checking
if (monitor$chk$v) {
    object9(lili,"list",-1,mensaje="lili must be a list");
}
# exploring the list
eee <- explore8list(lili);
# checking the presence on unamed components
#          and the types of the list
comment <- character(0);
for (ii in bc(nrow(eee))) {
    if ((eee[ii,"name"]=="<NA>")|(eee[ii,"name"]=="")) {
        comment <- c(comment,
                     paste("The component",eee[ii,"numbers"],
                           "has got no name:",
                           paste("'",eee[ii,"names"],"'",sep=""))
                    );
    }
    coco <- get8comp7list(lili,eee[ii,,drop=FALSE],monitor=monitor);
    if (length(coco) > 0) { coco <- coco[[1]];}
    if (!(is.list(coco) |
          is.numeric(coco) |
          is.character(coco) | 
          is.matrix(coco) | 
          is.array(coco))) {
        comment <- c(comment,
                     paste("The component (",eee[ii,"numbers"],
                           ") with name: '",eee[ii,"names"],
                           "is not list/vector/matrix/array")
                    );
    }
}
# preparing the result
res <- void9(comment);
# returning
res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

