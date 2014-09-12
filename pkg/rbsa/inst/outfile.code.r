
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
open8graph <- function(prefix=rbsa0$fpx$v,...) 
#TITLE  opens/closes graph/text files
#DESCRIPTION
# According to the global constant \samp{rbsa0$mfi$v} a
# graphical device is open or not.
# To be called before plotting something that ones want
# to keep under \samp{rbsa0$mgr$v} type.
#ALIAS outputting
#DETAILS
# The file opened for storing the graph is named with
# two components separated with dots: the prefix and the suffix.
# The prefix is either given by the first argument or \cr
# \samp{paste(rbsa0$fpx$v,prefix,sep=".")}; the suffix
# is rbsa0$mgr$v associated to the type of the graph (either 'pdf'
# or 'png').
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{prefix} << When \samp{numeric}, it is used with three
#                digits to complete the constant \samp{rbsa0$fpx$v}.
#                If not, must be a character giving
#                the complete prefix to use before the suffix.>>
#{\dots} << argument(s) to be transmitted to the opening device
#         of graphics. Quite useful for specific character and
#         picture sizes, or to get more than one graph into
#         the file.>>
#VALUE
# \samp{0} when \samp{is.character(prefix)} and
# \samp{prefix+1} when \samp{is.numeric(prefix)}.
# The last option is intended to generate a series of numbered graph
# file names.
#EXAMPLE
# open8graph("my_graph");
# close8graph();
# open8graph(5);
# close8graph();
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 14_09_01
#--------------------------------------------
{
  # checking
  object9(prefix,c("numeric","character"),1,mensaje="Bad value for 'prefix'");
  if (is.numeric(prefix)) {
    object9(prefix,"numeric",1,c(0,Inf),mensaje="Bad numeric value for 'prefix'");
  }
  # opening the device
  if (rbsa0$mfi$v) {
      if (is.numeric(prefix)) {
          vprefix <- paste(rbsa0$fpx$v,form3justify(prefix,nbc=3,
                                                 format=3,tronc=FALSE,
                                                 carac="0"),
                          sep=".");
      } else {
        vprefix <- prefix;
        prefix <- -1;
      }
      fifi <- paste(vprefix,rbsa0$mgr$v,sep=".");
      gopen <- FALSE;
      if (rbsa0$mgr$v == "pdf") { pdf(fifi,...); gopen <- TRUE;}
      if (rbsa0$mgr$v == "png") { png(fifi,...); gopen <- TRUE;}
      if (!gopen) {
          erreur(rbsa0$mgr$v,
                 "This type of graph is not yet implemented in /rbsa/");
      }
  }
  # returning
  prefix+1;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8graph <- function(mensaje=character(0),monitor=rbsa0$monitor$v) 
#TITLE  closes the file open by open8graph
#DESCRIPTION
# Closes the file
# open by \samp{open8graph}. When \samp{monitor$pau$v} is \samp{TRUE},
#  a pause is issued.
#DETAILS
# This action is conditioned by the value of
# \samp{rbsa0$mfi$v}. The pausing is conditionned by (i)
# a non empty \samp{mensaje} and (ii) \samp{monitor$pau$v}.
#ALIAS 
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{mensaje} << Message to display on the terminal.
# When empty, no message will be displayed.>>
#{monitor} << List of monitoring constants, see \samp{rbsa0$monitor$v} to
#             know its structure.>>
#VALUE
# Nothing but the actions indicated in the description field are performed.
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_29
#REVISED 10_03_16
#--------------------------------------------
{
# closing the device
if (rbsa0$mfi$v) { dev.off();}
# displaying the message
if (!void9(mensaje)) {
    if (monitor$pau$v) { pause(mensaje,"pause from close8graph");
    } else { cat("<<<",mensaje,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
open8text <- function(append=TRUE) 
#TITLE  opens the standard output text for rbsa
#DESCRIPTION
#  Opens the standard output text for /rbsa/.
# According to the global constant \samp{rbsa0$mfi$v} the
# standard output text of rbsa is open (in
# append mode) or not. The name of this file is provided
# by the constant \samp{rbsa0$fou$v}.
# Must be called before printing something ones want
# to keep on file. 
#DETAILS
#  Usual derivation is done with \samp{sink} until \samp{close8text} is called.
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{append} << Must the current file rbsa0$fou$v be continued ?>>
#VALUE
# nothing but the indicated actions are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_10_10
#REVISED 08_08_28
#--------------------------------------------
{
if (rbsa0$mfi$v) { sink(rbsa0$fou$v,append);}
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
close8text <- function(mensaje=character(0),monitor=rbsa0$monitor$v) 
#TITLE  pauses (and more) the program until an answer is given
#DESCRIPTION
# Closing the output file \samp{rbsa0$fou$v} according to \samp{rbsa0$mfi$v}. A pause allowing 
# to stop the process is issued if \samp{rbsa0$pau$v} is \samp{TRUE}.
#DETAILS
# This action is conditioned by the value of
# \samp{rbsa0$mfi$v}. The pausing is conditionned by (i)
# a non empty \samp{mensaje} and (ii) \samp{monitor$pau$v}.
#KEYWORDS IO
#INPUTS
#[INPUTS]
#{mensaje} << Message to display on the terminal. 
# When empty, no message will be displayed.>>
#{monitor} << List of monitoring constants, see \samp{rbsa0$monitor$v} to
#             know its structure.>>
#VALUE
# nothing but the actions indicated in the description field are performed
#EXAMPLE
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_29
#REVISED 10_03_16
#--------------------------------------------
{
# closing the file
if (rbsa0$mfi$v) { sink();}
# displaying the message
if (!void9(mensaje)) {
    if (monitor$pau$v) { pause(mensaje,"pause from close8text");
    } else { cat("<<<",mensaje,">>>\n");}
}
# returning
invisible();
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
