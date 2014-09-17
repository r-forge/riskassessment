
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
nbn2pam <- function(nbn) 
#TITLE gets the /pam/ associate to a /nbn/
#DESCRIPTION returns a \samp{pam} object of /g4n/ 
# package with the parentship of \samp{nbn}.
#DETAILS
#KEYWORDS 
#INPUTS
#{nbn}<< The initial \samp{nbn} object.>>
#[INPUTS]
#VALUE
# A \samp{pam} object
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_05_16
#REVISED 12_05_16
#--------------------------------------------
{
  # checking
  # To be done
  # getting the parentship matrix
  nbno <- length(nbn);
  nbna <- names(nbn);
  rlt <- matrix(0,nbno,nbno);
  for (nn in sssbf(nbn)) {
    if (length(nbn[[nn]]$parents) > 0) {
      rlt[match(nbn[[nn]]$parents,nbna),nn] <- 1;
    }
  }
  # creating the pam object
  res <- new("pam",rlt=rlt);
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
