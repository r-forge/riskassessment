
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3acceptance <- function(line,posi,
                           b.accepted=NULL,a.accepted=NULL,
                           b.rejected=NULL,a.rejected=NULL,
                           b.line=TRUE,e.line=TRUE)
#TITLE acceptance of word within a line 
#DESCRIPTION 
# Indicates the acceptance of a sequence within a line
# based on some positive and negative criteria.
#DETAILS
# \samp{posi[1]>=posi[2]} case is accepted. Outside
# position are also accepted being valued to the bounds.
#KEYWORDS 
#INPUTS
#{line}<< A \samp{character(1)} within which is the sequence.>>
#{posi}<< The two positions within \samp{line} of the sequence.>>
#[INPUTS]
#{b.accepted} << A \samp{character} providing the surrounding characters 
#                before the words to determine the acceptance of the
#                sequence.>>
#{a.accepted} << A \samp{character} providing the surrounding characters 
#                after the words to determine the acceptance of the
#                sequence.>>
#{b.rejected} << A \samp{character} providing the surrounding characters 
#                before the words to determine the rejection of the
#                sequence.>>
#{a.rejected} << A \samp{character} providing the surrounding characters 
#                after the words to determine the rejection of the
#                sequence.>>
#{b.line} << Must sequences at the beginning of a line be accepted?>>
#{e.line} << Must sequences at the end of a line be accepted?>>
#VALUE
# \samp{TRUE} or \samp{FALSE}.
#EXAMPLE
# text3acceptance("Et pourquoi pas ?",c(1,3));
# text3acceptance("Et pourquoi pas ?",c(4,11),b.accepted=" ");
# text3acceptance("Et pourquoi pas ?",c(4,11),b.accepted=letters);
# text3acceptance("Et pourquoi pas ?",c(5,11),b.accepted=letters);
# \dontrun{text3acceptance("Et",1:2,b.accepted="letters",b.rejected=LETTERS)}
#REFERENCE
#SEE ALSO text3replace
#CALLING
#COMMENT This function was written to be used within \samp{text3replace}.
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 14_09_03
#REVISED 14_09_03
#--------------------------------------------
{
  # checking
  object9(line,"character");
  object9(posi,"integer",2);
  posi <- pmax(pmin(posi,nchar(line)),1);
  object9(b.accepted,c("null","character"));
  object9(b.rejected,c("null","character"));
  object9(a.accepted,c("null","character"));
  object9(a.rejected,c("null","character"));
  if ((!is.null(b.accepted)) & (!is.null(b.rejected))) {
    erreur(list(b.accepted,b.rejected),"'b.accepted' and 'b.rejected' cannot be active simultaneously");
  }
  if ((!is.null(a.accepted)) & (!is.null(a.rejected))) {
    erreur(list(a.accepted,a.rejected),"'a.accepted' and 'a.rejected' cannot be active simultaneously");
  }
  object9(b.line,"logical");
  object9(e.line,"logical");
  # looking for the before acceptability
  befo <- FALSE;
  if (b.line & (posi[1]==1)) {
    befo <- TRUE;
  } else {
    if (!is.null(b.accepted)) {
      for (mot in b.accepted) {
        if (nchar(mot) < posi[1]) {
          if (substr(line,posi[1]-nchar(mot),posi[1]-1)==mot) { befo <- TRUE;}
        }
      }
    } else {
      befo <- TRUE;
      if (!is.null(b.rejected)) {
        for (mot in b.rejected) {
          if (nchar(mot) < posi[1]) {
            if (substr(line,posi[1]-nchar(mot),posi[1]-1)==mot) { befo <- FALSE;}
          }
        }
      }
    }
  }
  # looking for the before acceptability
  afte <- FALSE;
  if (befo) {
    if (e.line & (posi[2]==nchar(line))) {
      afte <- TRUE;
    } else {
      if (!is.null(a.accepted)) {
        for (mot in a.accepted) {
          if (posi[2]+nchar(mot) <= nchar(line)) {
            if (substr(line,posi[2]+1,posi[2]+nchar(mot))==mot) { afte <- TRUE;}
          }
        }
      } else {
        afte <- TRUE;
        if (!is.null(a.rejected)) {
          for (mot in a.rejected) {
            if (posi[2]+nchar(mot) <= nchar(line)) {
              if (substr(line,posi[2]+1,posi[2]+nchar(mot))==mot) { afte <- FALSE;}
            }
          }
        }
      }
    }
  } 
  # returning
  (befo & afte);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3replace <- function(text,translate,lowering=FALSE,
                           b.accepted=NULL,a.accepted=NULL,
                           b.rejected=NULL,a.rejected=NULL,
                           b.line=TRUE,e.line=TRUE)
#TITLE translating some words
#DESCRIPTION from a text,
# gives it back after replacing some words (in fact sequences of characters)
# by others. The translation can be conditionned by the surrounding
# characters of the sequence.
#DETAILS
# The words to translate cannot be extended upon two successive lines.\cr
# The translations are done sequentially along the rows of \samp{translate}
# so modifying their order can produce different results.\cr
# For obvious reason, \samp{b.accepted} and \samp{b.rejected} cannot be
# non \samp{NULL} simultaneously; the same for \samp{a.accepted} and
# \samp{a.rejected}.\cr
# Resulting empty lines are not removed neither already present empty lines.
#KEYWORDS 
#INPUTS
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#{translate}<< A two-columns character matrix with, row by row, a word to
# translate and its translations. The second column can comprise \samp{""}
# not the first.>>
#[INPUTS]
#{lowering} << Must a lowering being applied before the selection
#              of the words to translate?>>
#{b.accepted} << A \samp{character} providing the surrounding characters 
#                before the words to determine the acceptance of the
#                translation.>>
#{a.accepted} << A \samp{character} providing the surrounding characters 
#                after the words to determine the acceptance of the
#                translation.>>
#{b.rejected} << A \samp{character} providing the surrounding characters 
#                before the words to determine the rejection of the
#                translation.>>
#{a.rejected} << A \samp{character} providing the surrounding characters 
#                after the words to determine the rejection of the
#                translation.>>
#{b.line} << Must words at the beginning of a line be translated?>>
#{e.line} << Must words at the end of a line be translated?>>
#VALUE
# The translated \samp{text}
#EXAMPLE
# let <- c("a","e","i","o","u","y");
# tra <- matrix(c(let,tolower(let)),ncol=2);
# text3replace(letters,tra);
# text3replace(c("Il etait une fois","un petit et rouge chaperon"),tra);
# text3replace(c("Il etait une fois","un petit et rouge chaperon"),tra,b.accepted=" ");
#REFERENCE
#SEE ALSO text3acceptance
#CALLING
#COMMENT This function was written to change the names of defined objects
#        within an R script
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 14_09_03
#REVISED 14_09_03
#--------------------------------------------
{
  # checking
  object9(text,"character");
  if (length(translate)==0) { return(text);}
  object9(translate,"character");
  if (length(translate)==2) { translate <- matrix(translate,1);}
  object9(translate,"matrix",speci=matrix(c(NA,2,NA,2),2));
  object9(b.accepted,c("null","character"));
  object9(b.rejected,c("null","character"));
  object9(a.accepted,c("null","character"));
  object9(a.rejected,c("null","character"));
  if ((!is.null(b.accepted)) & (!is.null(b.rejected))) {
    erreur(list(b.accepted,b.rejected),"'b.accepted' and 'b.rejected' cannot be active simultaneously");
  }
  if ((!is.null(a.accepted)) & (!is.null(a.rejected))) {
    erreur(list(a.accepted,a.rejected),"'a.accepted' and 'a.rejected' cannot be active simultaneously");
  }
  object9(b.line,"logical");
  object9(e.line,"logical");
  # looping on each word
  for (nn in bc(nrow(translate))) {
    amo <- translate[nn,1];
    nmo <- translate[nn,2];
    # looking for the presence of the word to translate
    if (lowering) {
      ouou <- text3places8word(tolower(text),amo);
    } else {
      ouou <- text3places8word(text,amo);
    }
    # dealing with each occurrence
    for (oo in rev(bc(nrow(ouou)))) {
      lign <- text[ouou[oo,1]];
      # determining the acceptance
      acce <- text3acceptance(lign,c(ouou[oo,2],ouou[oo,4]),
                              b.accepted=b.accepted,a.accepted=a.accepted,
                              b.rejected=b.rejected,a.rejected=a.rejected,
                              b.line=b.line,e.line);
      if (acce) {
        # making the translation
        nlig <- "";
        if (ouou[oo,2]>1) { nlig <- paste0(nlig,substr(lign,1,ouou[oo,2]-1));}
        nlig <- paste0(nlig,nmo);
        if (ouou[oo,4] < nchar(lign)) {
          nlig <- paste0(nlig,substr(lign,ouou[oo,4]+1,nchar(lign)));
        }
        text[ouou[oo,1]] <- nlig;
      }
    }
  }
  # returning
  text;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
