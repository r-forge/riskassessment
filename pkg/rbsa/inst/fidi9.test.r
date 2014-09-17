
# to be independent of them
object9 <- function(a,b,c) {character(0);}

bf <- function(x){
  if (length(x) > 0) { return(1:length(x));}
  numeric(0);
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
fidi9 <- function(chemins,retour=c("-","f","d"))
#TITLE  checks files and directories
#DESCRIPTION
#  checks if \code{chemin} is a file or a directory
#DETAILS
#  the distinction between files and directory is
# based on \code{file.info} function.
#KEYWORDS file directory
#INPUTS
#{chemins} << \code{character} each element of it, the
#       existence as file or directory must be checked.>>
#[INPUTS]
#{retour} << \code{character(3)} indicating what to return
#            in case of (non existence), (existence of a file)
#            and (existence of a directory).>>
#VALUE
# a character of same length as \code{chemins} with one of the
# components of \code{retour}.
#EXAMPLE
# fidi9(c("/","~","~/.bashrc",getwd(),"toto.txt"));
#REFERENCE
#SEE ALSO dipa
#CALLING
#COMMENT
#FUTURE (i) make it properly behave with "~", "my_rep",
#       (ii)  introduce the notion of accessibility
#AUTHOR J.-B. Denis
#CREATED 14_01_22
#REVISED 14_06_03
#--------------------------------------------
{
  # checking the arguments
  object9(chemins,"character");
  object9(retour,"character",3);
  # initializing
  res <- rep(retour[1],length(chemins));
  # proceeding
  for (ii in bf(chemins)) {
    if (nchar(chemins[ii])==0) {
      stop(paste0(" The ",ii,"th path is void!"));
    } else {
      fidi <- chemins[ii];
      if (file.exists(fidi)){
        res[ii] <- retour[2];
        if (file.info(fidi)$isdir) {
          res[ii] <- retour[3];
        }
      }
    }
  }
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

tester <- c(".",
            "C:/Users/",
            "C:/Users",
            getwd(),
            "fidi9.test.r"
           );

print(fidi9(tester));

