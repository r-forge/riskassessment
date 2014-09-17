
\section{Developer corner}{

\subsection{TO DO list}{

  \itemize{

    \item Describe how to deal with constant in this file
    (section 'usage' can or not be computed... )

    \item Make items possible in the description section (doesn't work);
    also it works in the value section, but the 'mark up' issues a
    warning, it seems that only warning are possible...    

    \item empty comment lines in the 'INPUTS' and '[INPUTS]' sections
    produces an error.

    \item Make or improve the example sections of all functions

    \item Make \code{check8object} functions.

    \item Prepare a true example, comprising a file with some function
    included in the package through a \code{character} vector and
    introduce it in this presentation.

    \item Produce a \code{pkg.r} file to source all components of
    the \pkg{pkg} package, to facilitate the programming developement
    when some objects are modified.

    \item Find some trick to improve the portability to other OS than
    linux since the function to detect the existence of a file and a
    directory (\code{fidi9} of \pkg{rbsa}) is based on the \code{ls}
    command. 

    \item Generate from all the stuff an help function named
    \samp{objects7pkg} which allows to list the object names (all or
    associated to some keywords), as well as titles, descriptions...

    \item Make debugging of code tagging much more explicit.

    \item Improve the presentation of arguments for aliased functions,
   distinguishing one way or the other, the correspondances.

   \item Check the 'See Also' items and suppressed those corresponding
   to masked objects.

   \item The use of \samp{\cr} seems a random event when introduced in
   the description of some arguments...

  }
  
}

\subsection{General process}{

  To avoid the constant manipulation of text files containing the code,
  most of the functions of \pkg{documair} work on what are denominated
  \code{text} objects which basically are \code{character} vectors. The
  different components of these objects are denominated 'lines' for
  reference of the file line, they come from. Basic functions to handle
  such objects are available in the \pkg{rbsa} package and their name
  start with \code{text3...}.

  To produce a package from a set of \code{text} files (comprising each
  as many objects as desired) supposed to be in the same directory with
  standard denomination, the following steps are performed by \pkg{documair}:

  \enumerate{

    \item get each function of each designated file (see function
    \code{code7objects4text6tags}),    
    \item from a function, find the different components of the
    documentation given at the beginning of the function, or within the
    code to complete the description component (see function
    \code{parse8code}),
    \item produce the corresponding \code{Rd} files taking care of
    possible aliases (see function \code{make8rd}).
    
  }
 }

 \subsection{File organisation}{

  The functions are gathered into different text files (with
  \code{code.r} as extension) according to the level of basicity.

  \itemize{

    \item \code{text}: functions devoted to the manipulation of
    character strings, could be as well in \pkg{rbsa} package.

    \item \code{objects}: objects defining the tags names used in
    \pkg{documair} or simple \code{text} objects used to perform the
    examples.
    
    \item \code{basic}: very basic functions for the package could
    be forgotten if provided facilities in the following files are
    sufficient. 
    
    \item \code{builder}: functions performing standard operations of
    decompositions of text files containing the documented codes.
     
    \item \code{user}: top level functions providing standard
    facilities to make straightforward scripts.

  }

 }

}
