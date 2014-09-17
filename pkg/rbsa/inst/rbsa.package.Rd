\name{rbsa-package}
\alias{rbsa-package}
\alias{rbsa}
\docType{package}
\title{
  Set of basic but convenient functions
}

\description{ The main idea was to provide some convenient functions to 
  avoid some disturbing points when programming with R. Perhaps the most
  striking example is when you need to make an action on each component
  of an object and use some loop like \samp{for (i in 1:length(object)){...}}
  since later or former the loop will be used with an object of
  length zero...
  
  The \pkg{rbsa} package gives access to

  \enumerate{
    \item logical looping with \samp{bc}, \samp{bd} and
    \samp{bf}.
    \item simply building a \samp{list} from a simply tagged
    text file; very convenient to parameterize jobs (see \samp{read8list}).
    \item checking objects and error displaying (\samp{object9} and
    \samp{erreur}).
    \item pausing to look at intermediate results (\samp{pause}).
    \item opening and closing of text/graph
    files (\samp{open8text}, \samp{close8text}, \samp{open8graph} and
    \samp{close8graph}).
    \item analyze and handle character vectors seen as text files.
    \item and more facilities of this kind.
}

  It is worth knowing that \pkg{rbsa} has got a series of constants
  that can be modified by the user:
  \itemize{
    \item especially the component \samp{rbsa0$monitor} to
     monitor the behaviour of some functions. It can be scrutinized by calling
     \samp{display8k(rbsa0$monitor$v)}.
  }
  
  The name of \pkg{rbsa} comes from \pkg{rbsb} with last letter
  changed from 'b' to 'a' to indicate that it is only the basic part of
  \pkg{rbsb}. The denomination of \pkg{rbsb} was a shortcut of
  \pkg{rebastaba} to indicate that it was the set of auxiliary functions
  of the package. \pkg{rebastaba} was standing for the
  French expression of \code{R\'eseaux Bay\'esiens pour la Statistique
  Bay\'esienne}, a finally abandonned project.
  
}


\section{Naming conventions}{

  The object denomination follows optional rules defined for
  \pkg{documair}, a package to build package \samp{tar.gz}
  files. The main idea is to concatenate keywords with digits, loading the
  digits with some operating meaning.

  \itemize{
      \item [0] to designate a variable object (predefined constants).
      \item [1] to concatenate two objects (similar to the use of \code{and}).
      \item [2] to indicate a one-to-one transformation.
      \item [3] to mean a component of a series.
      \item [4] equivalent to \code{from} (and not \code{for}).
      \item [5] <not used for the moment>.
      \item [6] equivalent to \code{with}.
      \item [7] to replace an \code{hyphen}.
      \item [8] to link a verb and a noun.
      \item [9] equivalent to a question mark (\samp{?}).
    }

    More explanations are given in \pkg{documair} presentation.
  
}

\details{

 After reading the documentation, the best way to get details is first
 to try the functions, second to look at their code in the files
 \samp{*.code.r} which are stored in the directory \samp{inst}. Most of
 the time the programming is quite straightforward and always commented.

}


\author{
  Jean-Baptiste Denis (MIAj - Inra - Jouy-en-Josas).
}

      
\examples{
  library(rbsa)

  ## looping
  for (ii in bf(letters)) {
    cat(ii,letters[ii],"|",sep="")
  }
  cat("Look at the result\n");
  ## pausing
  cat("Try the interactive command:",
      " \"pause('To end the process'",
      ",top=c('yes','oui','si'))\"\n");
  ## filling a text file
  open8text(FALSE);
  cat(letters,"\n");
  close8text();
  cat(bc(4),"\n");
  open8text(FALSE);
  cat(LETTERS,"\n");
  close8text();
  cat("Look at the newly created text file\n");
}

\keyword{ package }
