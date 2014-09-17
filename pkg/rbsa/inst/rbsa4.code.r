
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
inquiry <- function(question,
                    q7help="H",
                    help=NULL,
                    q7stop="S",
                    a7type="character",
                    a7default="Y",
                    a7length=1,
                    a7separator="//",
                    a7possibility="P",
                    a7possibilities=c("y","Y","n","N"),
                    patience= 3,
                    format=1
                    ) 
#TITLE  gets some information after a question
#DESCRIPTION
# This function issues a question onto the screen
# and get the answer from the user keyboard. The answer can be checked among
# a set of possibilities.\cr
# The basic principle is to provide some restrictions and checks on the answers.
# If an answer is not complete (because more components are asked) the user must
# follow to type it. If an answer is not accepted, it is repeated a limited
# number of times before a fatal error is issued. Also some help can be
# provided in case the user asks for it.
#DETAILS
# Answers \samp{q7help} or \samp{a7possibility} are not possible (only
# when these options are activated, that is \samp{help} and \samp{a7possibilities} are not \samp{NULL}.
#KEYWORDS IO
#INPUTS
#{question} <<The question to issue on the screen before getting the answer.>>
#[INPUTS]
#{q7help} << Answer providing the display of the helping complement
#             to the question if any
#            (then cannot be a possible answer returned by the function).>>
#{help} <<Additional information provided to the user if requested with \samp{q7help}. >>
#{q7stop} << Answer to give to stop the inquiry and a \samp{NULL} is returned
#            (then cannot be a acceptable answer).>>
#{a7type} << indicates the type of the answer: either "integer", "numeric" or "character",
#                 when not "character" a vector can be returned with blank as separator. >>
#{a7default} << Default value when no answer is issued; must be \samp{numeric} if \samp{a7type}
#               is not \samp{character}. Must be \samp{NULL} if no default value.>>
#{a7length} << number of returned values; either a unique number or the minimum and maximum numbers of items.
#              For character answers, the separator between
#              paragraphs is \samp{a7separator}; for numerical values it is the blank(s).
#              To include several blanks within character strings, you must use quoting marks (\samp{\"}). >>
#{a7separator} << character string indicating a separation between paragraphes for
#                 \samp{character} answers.>>
#{a7possibility} << Answer providing the display of the accepted possibilities.
#             (when they are constrained, then cannot be a right answer).>>
#{a7possibilities} << When the type is "character" a character
#                        vector containing the different possibilities,
#                        or a named character, the names are the possibilities and the
#                        values are the explanations of each possibility.
#                        When the type is not "character" a numerical vector of size 2
#                        providing the minimum and maximum for accepted values.
#                        \samp{NULL} for no check on the possibilities.>>
#{patience} << number of accepted bad answers before a fatal stop.>>
#{format} << 0: no help and reduced display; 1: reduced display; 2: enlarged display.   >>
#VALUE
# The answer, or \samp{NULL} if (i) the answer was void or (ii) no accepted answer was provided by the user.
# The answer can be a vector, if so and if constrained are imposed for accepted values, then the acceptance
# is checked for every component of the vector.
#EXAMPLE
# \dontrun{inquiry("Hungry ?",q7help="Si 'oui', je mets 'a cuire le repas",
#         a7possibilities=c("oui","non"),a7default=NULL);}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE Add the possibility to answer with intervals for numerical answers.
#AUTHOR J.-B. Denis
#CREATED 14_02_19
#REVISED 14_08_29
#--------------------------------------------
{
  # checking the arguments
  object9(question,"character",1);
  object9(q7help,"character",1);
  if (!is.null(help)) {
    object9(help,"character",c(1,Inf));
  }
  if (!is.null(q7stop)) {
    object9(q7stop,"character",1);
  }
  object9(a7type,"character",1,
          c("numeric","integer","character"));
  object9(a7length,"integer",c(1,2),c(1,Inf));
  if (length(a7length)==1) { a7length=rep(a7length,2);}
  object9(a7separator,"character",1);
  if (!is.null(a7possibilities)) {
    if (a7type=="character") {
      object9(a7possibilities,"character",c(2,Inf));
      if (is.null(names(a7possibilities))) {
        a7explanations <- NULL;
      } else {
        a7explanations <- a7possibilities;
        a7possibilities <- names(a7possibilities);
        names(a7explanations) <- NULL;
      }
    } else {
      if (a7type=="integer") {
        object9(a7possibilities,"integer",2);
      } else {
        object9(a7possibilities,"numeric",2);
      }
      a7possibilities <- range(a7possibilities);
    }
  } else {
    a7possibility <- "No restrictions on the possible answers";
  }
  object9(a7possibility,"character",1);
  if (!is.null(a7default)) {
    object9(a7default,a7type,1,a7possibilities);
  }
  object9(patience,"integer",1,c(1,Inf));
  object9(format,"integer",1,c(0,2));
  # end of checking
  #
  # looping to get the answer
  toanswer <- TRUE;
  while (toanswer) {
    # getting an answer
    answer <- character(0); uu <- "";
    while(length(uu)>0) {
      if (length(answer)==0) {
        # issuing the question
        cat(">>>>>>>> ",question,"\n");
        # providing help about the answer to give
        if (!is.null(a7default)) { cat("[default]",a7default);}
        if (!is.null(q7stop)) { cat(" / [",q7stop,"] to stop",sep="");}
        cat(" / [empty line] finished");
        if (!is.null(help)) {
          cat(" / [",q7help,"] help",sep="");
        }
        if (!is.null(a7possibilities)) {
          cat(" / [",a7possibility,"] possibilities",sep="");
        }
        cat(" / [quoting] for spaces");
        cat("\n");
      }
      uu <- scan(what="character",nlines=1,quiet=TRUE);
      # Possibly stopping 
      if (!is.null(q7stop)) { if (length(uu)==1) { if (uu==q7stop) {
        return(NULL);
      }}}
      if (length(uu)>0) {
        answer <- c(answer,uu);
      } else {
        if (length(answer)==0) { answer <- a7default;}
      }
      if (length(answer) == 1) {
        # Possibly asking for help
        if (format!=0) { if (!is.null(help)) { if (answer==q7help) {
          # precising the question
          if (format==2) { cat("\n");}
          for (hh in help) {
            if (format==2) { cat(".....   ");}
            cat(hh,"\n");
          }
          if (format==2) { cat("\n");}
          answer <- character(0);
          toanswer <- TRUE;
        }}}
      }
      if (length(answer) == 1) {
        # Possibly asking for  the possibilities
        if (!is.null(a7possibilities)) { if (answer==a7possibility) {
          # showing the possibilities
          if (a7type == "character") {
            cat("There are",length(a7possibilities),"possibilities\n");
            for (ee in bf(a7possibilities)) {
              if (format==2) { cat(".....   ");}
              cat("<",ee,"> ",a7possibilities[ee]);
              if (!is.null(a7explanations)) {
                cat(" {{",a7explanations[ee],"}}",sep="");
              }
              if (format==2) { cat("\n");}
            }
            if (format<2) { cat("\n");}
          } else {
            if (a7type == "integer") {
              if (format<2) { cat("\n");}
              if (format==2) { cat(".....   ");}
              cat(" Integer value(s) ");
            } else {
              if (format<2) { cat("\n");}
              if (format==2) { cat(".....   ");}
            cat(" Numerical value(s) ");
            }
            cat("between",a7possibilities[1],"and",a7possibilities[2],"\n");
            if (format<2) { cat("\n");}
          }
          if (format==2) { cat("\n.....   ");}
          if (is.null(a7length)) {
            cat("Any length is accepted");
          } else {
            cat("The expected length is in [",a7length[1],
              ",",a7length[2],"]",sep="");
          }
          if (format==2) { cat("\n");}
          cat("\n");
          toanswer <- TRUE;
          answer <- character(0);
        }}
      }
    } # ending length(uu) > 0
    toanswer <- FALSE;
    # transforming the answer
    if (a7type == "character") {
      answer <- paste(answer,collapse=" ");
      answer <- strsplit(answer,a7separator)[[1]];
    } else {
      answer <- as.numeric(answer);
      # removing "NA"
      answer <- answer[!is.na(answer)];
    }
    # checking the answer
    bon <- object9(answer,a7type,a7length,a7possibilities,fatal=FALSE,na.rm=TRUE);
    if (length(bon) > 0) {
      patience <- patience - 1;
      cat("The proposed answer was not accepted\n");
      if (format == 2) {
        cat("\nThe reason is :\n\n");
        print(bon);
        cat("\n");
      }
      if (patience < 1) {
        cat(" It was your last answer... a NULL answer is returned\n");
        return(NULL);
      } else {
        cat("You are left with",patience,"answer(s) be careful!\n");
        if (format == 2) { cat("\n");}
        toanswer <- TRUE;
      }
    }
  } # ending while(toanswer)
  # return
  answer;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df4kbd <- function(def)
#TITLE  Produces a data.frame from the keyboard.
#DESCRIPTION
# This function issues repeatedly a series of questions onto the screen
# and from the answers builds a data.frame.
#DETAILS
# Questions are proposed with the function \samp{inquiry}. Activating
# the 'stop' in any of the questions is interpreted as the end of the
# data capture.
#KEYWORDS IO
#INPUTS
#{def} <<A named list describing the different variables of the data
#   frame to produce.  Its names give the names of the variables. Each
#   component of \samp{def} are the list of arguments to provide to function
#   \samp{inquiry} to get the values for each individual. More
#   precisely \samp{question} which is compulsory, \samp{help}
#   (default \samp{NULL}), \samp{a7type} (compulsory either
#   \samp{numeric} or \samp{character}), \samp{a7possibilities}
#   (default \samp{NULL}).>>
#[INPUTS]
#VALUE
# The resulting data frame, possibly with zero row when no individual
# was correctly recorded.
#EXAMPLE
# nom <- list(question="The name?",a7type="character");
# edad <- list(question="How old?",a7type="numeric");
# defi <- list(NAME=nom,AGE=edad);
#\dontrun{df4kbd(defi)}
#REFERENCE
#SEE ALSO
#CALLING
#COMMENT
#FUTURE 
#AUTHOR J.-B. Denis
#CREATED 14_03_26
#REVISED 14_08_29
#--------------------------------------------
{
  # constants
  q7help <- "H"; q7stop <- "S";
  patience <- 5;
  # checking the arguments
  object9(def,"list",c(1,Inf));
  object9(names(def),"character",length(def));
  mauvais <- FALSE;
  for (qq in names(def)) {
    cc <- object9(def[[qq]]$question,"character",1,fatal=FALSE);
    cc <- c(cc,object9(def[[qq]]$a7type,"character",1,c("character","numeric"),fatal=FALSE));
    if (length(cc) > 0) {
      form3title(paste("No aceptable '$question' for",qq));
      print(def[qq]);
      print(cc);
      mauvais <- TRUE;
    }
  }
  if (mauvais) {
    stop("Non accepted list of questions");
  }
  # end of checking
  #
  # initializing the data frame
  res <- data.frame(matrix(NA,0,length(def)));
  names(res) <- names(def);
  #
  # starting the working loop of indivduals
  toanswer <- TRUE;
  while (toanswer) {
    #
    nbind <- nrow(res)+1;
    vasy <- pause(paste(" <[( Enquiring for Individual Number",nbind,"?)]>"),
                  ans=c("y","Y","n","N"),top=NULL);
    if (vasy %in% c("n","N")) {
      toanswer <- FALSE;
    } else {
      # initialisation
      rere <- vector("list",length(def));
      names(rere) <- names(def);
      # starting the loop of the questions
      for (qq in bf(def)) {
        ques <- def[[qq]];
        qn <- names(def)[qq];
        user <- inquiry(
                 question=ques$question,
                 q7help=q7help,
                 help=NULL,
                 q7stop=q7stop,
                 a7type=ques$a7type,
                 a7default=ques$a7default,
                 a7length=1,
                 a7separator="//",
                 a7possibility="P",
                 a7possibilities=ques$a7possibilities,
                 patience= 3,
                 format=1
                       );
        res[nbind,qq] <- user;
      }
    }
  } # end of while
  # return
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
relpath <- function(pfrom,pto)
#TITLE returns a relative path
#DESCRIPTION
# returns the relative path associated to \samp{pto}
# starting from \samp{pfrom}; both paths being supposed
# to be defined from the same point; possibly the \samp{/}
# one.
#DETAILS
# Seems compulsory to forbid \samp{../} components into \samp{pto}
#KEYWORDS 
#INPUTS
#{pfrom}<<Starting path.>>
#{pto}<<Target path. Must not comprise any \samp{../} component.>>
#[INPUTS]
#VALUE
# A \samp{character} with the computed path.
#EXAMPLE
# relpath("A/B/C/D/","A/B/Q/R/");
# relpath("../../../../","A/B/Q/R/");
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 12_12_04
#REVISED 13_09_17
#--------------------------------------------
{
  # systematic trailing path
  ff <- function(toto) {
    if (substr(toto,nchar(toto),nchar(toto))!="/") {
      toto <- paste(toto,"/",sep="");
    }
    toto;
  }
  pfrom <- ff(pfrom); pto <- ff(pto);
  # multiple "/" elimination
  gg <- function(toto) {
    while(length(strsplit(toto,"//",fixed=TRUE)[[1]])>1) {
      toto <- paste(strsplit(toto,"//",fixed=TRUE)[[1]],collapse="/");
    }
    toto;
  }
  pfrom <- gg(pfrom); pto <- gg(pto);
  # decomposing
  fdec <- strsplit(pfrom,"/",fixed=TRUE)[[1]];
  tdec <- strsplit(pto,"/",fixed=TRUE)[[1]];
  # checking that no decreasing parts are present into 'pto'
  if (".." %in% tdec) {
    form3display(pto);
    stop("This path must be non decreasing");
  }
  # removing the '.' convention if any
  if (length(fdec)>0) {
    if (fdec[1]==".") {
      fdec <- fdec[-1];
    }
  }
  if (length(tdec)>0) {
    if (tdec[1]==".") {
      tdec <- tdec[-1];
    }
  }
  # removing the common branch
  while ((length(fdec)>0) & (length(tdec)>0) & (fdec[1]==tdec[1])) {
    fdec <- fdec[-1]; tdec <- tdec[-1];
  }
  # building the relative path
  if (length(fdec)>0) {
    res <- rep("..",length(fdec));
  } else {
    res <- ".";
  }
  res <- c(res,tdec);
  res <- paste(paste(res,collapse="/"),
               "/",sep="");
  # returning
  res;
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
