
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3ij2n <- function(ij,text,flexible=TRUE)
#TITLE gives the absolute positions of (row,column) positions within a /text/
#DESCRIPTION 
# returns numerical values equal to the rank of a position when all 
# lines of the \samp{text} are concatenated without separators.
#DETAILS
# Definition of the positions is flexible (outside positions
# is interpreted as minimum / maximum positions) or not, according to \samp{flexible}.
#KEYWORDS 
#INPUTS
#{ij} << A \samp{numeric(2)} indicating (line,column) of the position.
#          May also be a matrix with two columns.>>
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#[INPUTS]
#{flexible} << When \samp{TRUE} values of \samp{n} outside the acceptable
#              range are accepted and replaced with first or last positions.>>
#VALUE
# The resulting ranks associated to the rows of \samp{ij}.
#EXAMPLE
# aa <- c(paste(letters,collapse=""),paste(LETTERS,collapse=""),paste(0:9,collapse=""));
# text3ij2n(c(1,3),aa);
# text3ij2n(c(2,1),aa);
# text3ij2n(rbind(c(1,3),c(2,1)),aa);
#ALIAS text-indexing
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 13_10_17
#REVISED 13_10_29
#--------------------------------------------
{
  # checking and normalizing
  if (is.matrix(ij)) {
    if (ncol(ij) != 2) {
      stop("'ij' must have two columns");
      object9(ij,"numeric");
    }
  } else {
    object9(ij,"numeric",len=2);
    ij <- matrix(ij,ncol=2);
  }
  object9(text,"character");
  # degenerate case
  if (nrow(ij)==0) { return(numeric(0));}
  # possibly adjusting
  if (flexible) {
    i1 <- pmin(length(text),pmax(1,ij[,1]));
    j1 <- pmin(nchar(text[i1]),pmax(1,ij[,2]));
  } else {
    object9(ij[,1],"numeric",con=c(1,length(text)));
    if ((any(ij[,2] < 1)) | (any(ij[,2]>nchar(text[ij[,1]])))) {
      erreur(cbind(ij,nchar(text[ij[,1]])),"Bad column numbering");
    }
  }
  # computing 
  csu <- c(0,cumsum(nchar(text)));
  # returning
  csu[i1] + j1;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
text3n2ij <- function(n,text,flexible=TRUE)
#TITLE gives the (row,column) position from the absolute position within a /text/
#DESCRIPTION 
# returns the (line, column) position associated to an absolute position when the
# lines of the \samp{text} are concatanated.\cr
# In fact, the reverse of \samp{text3ij2n}.
#DETAILS
#KEYWORDS 
#INPUTS
#{n} << A \samp{numeric(1)} indicating the absolute position.
#          May also be a vector. Values outside the possible range
#          are bounded to the extrema according to \samp{flexible}.  >>
#{text}<< A \samp{character} vector containing the text
# (a component, a line).>>
#[INPUTS]
#{flexible} << When \samp{TRUE} values of \samp{n} outside the accetable
#              range are accepted and replaced with first and last positions.>>
#VALUE
# The resulting positions associated to \samp{n}. When \samp{length(n)==1}
# it is a \samp{numeric(2)} if not a matrix with two columns.
#EXAMPLE
# aa <- c(paste(letters,collapse=""),paste(LETTERS,collapse=""),paste(0:9,collapse=""));
# text3n2ij(text3ij2n(c(1,3),aa),aa);
# text3n2ij(text3ij2n(c(2,1),aa),aa);
# text3n2ij(text3ij2n(rbind(c(1,3),c(2,1)),aa),aa);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 14_06_12
#REVISED 14_06_13
#--------------------------------------------
{
  # checking
  object9(text,"character");
  if (flexible) {
    object9(n,"integer");
    n <- pmax(n,1);
    n <- pmin(n,sum(nchar(text)));
  } else {
    object9(n,"integer",con=c(1,sum(nchar(text))));
  }
  # initializing
  res <- matrix(NA,length(n),2);
  # degenerate cases
  if (length(n)==0) { return(res);}
  if (length(text)==0) { return(res);}
  # computing the limits
  limi <- cumsum(c(0,nchar(text)));
  # computing the line numbers
  iin <- apply(outer(n,limi,">"),1,sum);
  # computing the column numbers
  jjn <- n - limi[iin];
  #
  if (length(n)==1) {
    res <- c(iin,jjn);
  } else {
    res <- cbind(iin,jjn);
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
