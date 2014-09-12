\name{documair-package} 
\alias{documair-package} 
\alias{documair} 
\docType{package} 
\title{ 
  Automatic Documentation for (and confection of) R packages 
}

\description{ 

  The development of this package was led by the idea that it is a bad
practice to separate code and documentation.  Code and documentation
should be at the same place so that both can easily be modified
simultaneously. We proceed according to this principle since the middle
of the 90ties using a Perl script (a very convenient language for this
kind of tasks). Now, we share it with the community by the more
convenient way of an R package. The recent development of R make things
quite easy.\cr To use \pkg{documair}, each code defining an object
(either function or variable) must be encapsulated within a series of
tagged comments; we propose tags but their values can be adapted as you
wish (by modifying the \samp{documair0$tag$v} list). From these tagged
comments, \pkg{documair} automatically writes \samp{Rd} files and
gathers them with a few more files to produce the complete package until
the \samp{tar.gz}.  For some specific objects, the user can write
manually the \samp{Rd} file. All the necessary files must be gathered in
a unique directory. In it, the user must place the following set of
mandatory files (in the following \samp{pkg} will designate the name of
the package):

\enumerate{ 

  \item A \samp{pkg.DESCRIPTION} file: the standard text DESCRIPTION
file to be associated to the package. The NAMESPACE file is
automatically created from the exported objects and the presence of C
and Fortran files.

 \item A \samp{pkg.package.Rd} file: a text file describing in Rd syntax
the general description of the package to appear in the
documentation. This file can be slightly supplemented by \pkg{documair}
to add some additional information.

 \item As many as wanted \samp{foo.code.r} files where are placed 
the documented code of each object. Each files can 
include more than one object. The extension \samp{code.r} can 
be modified within the \samp{pkg.which.txt} file. 

} 

Additional optional files can also be included: 

\itemize{ 

  \item \samp{foo.test.r} files including some scripts to test the
functions. The extension \samp{test.r} can be modified within the
\samp{pkg.which.txt} file.

 \item \samp{pkg.foo.rda} files where are placed possible data
sets. These binary files must be loadable with the function
\samp{load}. The associated \samp{pkg.foo.Rd} documentation files must
be provided.

 \item \samp{object(s).Rd} files the user wants to produce by hand. They
will be used by \pkg{documair} instead of the one based on the tagged
code for (and only for) exported objects. By default, all the objects
are exported, but some can be declared hidden with the
\samp{pkg.which.txt} file (see below).

 \item \samp{C} functions must be stored into individual files with the
same name and extension \samp{.c}; the same for \samp{Fortran} functions
with extension \samp{.f}.

 \item An optional \samp{pkg.which.txt} file allows the user to override
the standard use of files and functions to prepare different versions of
the package with the same set of code files. It gives the possibility to
hide or not functions in the package as well as to get sets of aliased
objects. For details see the example below.

 \item Every additional file the author wants to share with the other
 users of the package.

}
 
The complete content of this directory will be copied in the free
\samp{inst} directory of the package arborescence.  Once this directory
is prepared, the user successively calls the \samp{documair} functions
\samp{prepare8pkg} and \samp{compile8pkg}.\cr

\cr

The denomination of \pkg{documair} stands for \emph{documentation for
R}, 'air' having the same pronunciation as 'R' in French.

\cr\cr

Other R packages to generate Rd documentations files and building
packages from comments inserted in R code are available. Two outstanding
examples are \pkg{roxygen} and \pkg{inlinedocs}. The former is based on
header comments, uses powerful parsers and propose interesting analyses
like the call tree of the set of functions. The latter is very light,
using simple tagging in logical places of the code. \pkg{documair} is in
between these two cases, tags are also within the code but are many and
varied givng rise to more possibilities.


}

\section{Documenting an object}{ 

  Here is an example of a simple masked function of \pkg{documair}.\cr\cr


\samp{#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<}\cr 
\samp{r.bd <- function(n1,n2)}\cr 
\samp{#TITLE  sequence of increasing numbers}\cr 
\samp{#DESCRIPTION}\cr 
\samp{# This function returns {n1:n2} when {n1<=n2} and}\cr 
\samp{#         {numeric(0)} otherwise.}\cr 
\samp{# Quite useful when some insertion must be done within}\cr 
\samp{# a sequence}\cr 
\samp{#DETAILS}\cr 
\samp{#KEYWORDS iteration}\cr 
\samp{#INPUTS}\cr 
\samp{#\{n1\}    <<first integer>>}\cr 
\samp{#\{n2\}    <<second integer>>}\cr 
\samp{#[INPUTS]}\cr 
\samp{#VALUE}\cr 
\samp{# {n1:n2} if {n1<n2}}\cr 
\samp{# else {numeric(0)}.}\cr 
\samp{#EXAMPLE}\cr 
\samp{# xx <- 1:5;}\cr 
\samp{# for (ii in 1:6) { print(c(xx[bd(1,ii-1)],10,xx[bd(ii,5)]));}}\cr 
\samp{#REFERENCE}\cr 
\samp{#SEE ALSO bf}\cr 
\samp{#CALLING}\cr 
\samp{#COMMENT}\cr 
\samp{#FUTURE}\cr 
\samp{#AUTHOR J.-B. Denis}\cr 
\samp{#CREATED 11_01_12}\cr 
\samp{#REVISED 11_05_21}\cr 
\samp{#--------------------------------------------}\cr 
\samp{\{}\cr 
\samp{if (n1 <= n2) {return(n1:n2);}}\cr 
\samp{numeric(0);}\cr 
\samp{\}}\cr 
\samp{#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}\cr

One can notice the different tags used to structure the information
provided in the comments lines:

\itemize{ 

  \item \samp{#<<<<<<<}\ldots\samp{<<<<<<<<} to open the object, 

 \item \samp{#TITLE} to specify the title of the object, 

 \item \samp{#DESCRIPTION} to describe the object, 

 \item \ldots, 

 \item \samp{#-------}\ldots\samp{--------} to close the documentation part 

 \item \ldots, 

 \item \samp{#>>>>>>>}\ldots\samp{>>>>>>>>} to close the object.
 
}
 
Those five tags are the only compulsory ones (Note: the
\samp{#DESCRIPTION} tag can be missing for the children objects of a set
of aliased objects).

} 

\section{Monitoring \pkg{documair}}{ 

  \subsection{Introduction}{ 

    \pkg{documair} can build different packages from the same set of
files by simply selecting which files/objects have to be compiled and
which ones must be proposed or hidden to the end user. This is performed
through the \samp{pkg.which.txt} text file. An example of such a file
(with comments) is \samp{documair.which.txt} of the \pkg{documair}
package. If you are generating different packages with the same
functions, it is recommended to change the name of the package, this is
the reason why you can indicate in the \samp{pkg.which.txt} file other
\samp{DESCRIPTION} and PRESENTATION files than the standard ones deduced
from the package name.

}
 
\subsection{syntax of \samp{pkg.which.txt}}{ 

  The documentation of \pkg{documair} was performed by itself. Below is
provided, as an example, a possible \samp{documair.which.txt}.  The
comments introduced in the file are self-sufficient to understand what
are the different possibilities:\cr\cr


\samp{#}\cr 
\samp{# created       on 14_01_28}\cr 
\samp{# last modified on 14_05_27 }\cr 
\samp{#}\cr 
\samp{# + the order of the items is irrelevant}\cr 
\samp{#   but they are exploited in that order}\cr 
\samp{# + according to an option, the existence}\cr 
\samp{#   of the specified file, objects is}\cr 
\samp{#   checked or not.}\cr 
\samp{# + '_ALL_' means all occurences}\cr 
\samp{#}\cr 
\samp{# specifying the description file}\cr 
\samp{<<DESCRIPTION>> documair.DESCRIPTION}\cr 
\samp{#}\cr 
\samp{# the Rd file to describe the package}\cr 
\samp{<<PRESENTATION>> documair.package.Rd}\cr 
\samp{#}\cr 
\samp{# specifying the extension for the code files}\cr 
\samp{<<C.EXTE>> code.r}\cr 
\samp{#}\cr 
\samp{# specifying the extension for the test files}\cr 
\samp{<<T.EXTE>> test.r}\cr 
\samp{#}\cr 
\samp{# specifying hidden code files}\cr 
\samp{# (in the example, all objects are hidden)}\cr 
\samp{<<HIDDEN.F>> _ALL_}\cr 
\samp{#}\cr 
\samp{# specifying exported code files and}\cr 
\samp{# modifying previous prescriptions}\cr 
\samp{<<EXPORTED.F>> user.code.r}\cr 
\samp{#}\cr 
\samp{# specifying exported code files containing alias sets}\cr 
\samp{<<ALIASED.F>> exterieur.code.r}\cr 
\samp{#}\cr 
\samp{# specifying hidden objects}\cr 
\samp{# (in the example no specific object should be hidden}\cr 
\samp{#  so the item here is suppressed with an '#'.)}\cr 
\samp{#<<HIDDEN.O>> }\cr 
\samp{#}\cr 
\samp{# specifying exported objects}\cr 
\samp{# (in the example an object belonging to a hidden file}\cr 
\samp{#  (documair0) is exported)}\cr 
\samp{<<EXPORTED.O>> documair0}\cr 
\samp{#}\cr 
\samp{# specifying objects for which}\cr 
\samp{#  the content will be displayed on the}\cr 
\samp{#  screen during the process.}\cr 
\samp{# (the same can be done at the level of the}\cr 
\samp{#  files with '<<DISPLAY.F>>'.)}\cr 
\samp{<<DISPLAY.O>> documair0 display8tags}\cr 
\samp{#}\cr 
\samp{# specifying keywords from the components}\cr 
\samp{# (here 'compile' must be interpreted as}\cr 
\samp{#  'compilation', 'pkg' as 'package',...}\cr 
\samp{#  be aware that the two words must be stuck}\cr 
\samp{#  with '=' and that a word must not comprise}\cr 
\samp{#  any blank. Also that the special word '_NO_'}\cr 
\samp{#  means that this component must not appear}\cr 
\samp{#  as a keyword.)}\cr 
\samp{<<KEYWORDS>>/=/U}\cr 
\samp{compile=compilation}\cr 
\samp{pkg=package}\cr 
\samp{prepare=preparation}\cr 
\samp{documair=_NO_}\cr 
\samp{#}\cr 
\samp{# end of the which.documair.txt file}\cr 

} 

\subsection{Aliasing}{ 

  \pkg{documair} accepts the aliasing of set of objects but some rules 
  must be followed for that.
  
  \enumerate{
    

 \item Objects sharing the same alias must be proposed into a single 
file, and only those objects should be present in this file. 

 \item The parent object must be in the first position into this file. 

 \item The first alias name of the parent object must be the common alias 

 \item The name of the file must be declared as containing aliased 
objects in the (in that case mandatory) \samp{pkg.which.txt} file 
after the tag \samp{<<ALIASED.F>>}. 

} 

The alias \samp{Rd} file can be either provided by the user or composed
by \pkg{documair} from the documenting tags. When the \samp{Rd} file is
hand-written, it must be named under the first object (the parent
alias). When the documentation is built by \pkg{documair}, the
descriptions of identical arguments are taken in the first object using
them according to the ordering within the file.

} 
\subsection{Examples}{
 
To get more insights about the flexibility of \pkg{documair}, the reader
can have a glance to \pkg{documair} itself since all necessary files are
gathered in the \samp{inst} directory. In the same directory is prepared
within the script \samp{make.r} four examples of building package
variations based on \pkg{documair} objects; the first one being
\pkg{documair} itself. The second one (named \pkg{documair1} proposes
the building of the package without \samp{pkg.which.txt} file meaning
that all objects are exported. The fourth example (\pkg{documair3} gives
an example of using \samp{C} and \samp{Fortran} functions which can be
not effective for some configurations. Due to minor inconsistencies,
some examples generate warnings.

} 

} 

\section{Errors with \pkg{documair}}{ 

  Currently, \pkg{documair} is quite sensitive to errors in the input
files! Some are detected but indications are not always very clear,
others are not detected. For instance the double \samp{#} in\cr
\samp{##{argument} << explanation...>>} when describing the arguments of
a function causes a non explicit error. Also, it can be easily affected
by a mismatched parenthesis...  To help the user in seeing where the
mistake is located, it is suggested to put the \samp{check} argument of
\samp{prepare8pkg} to \samp{TRUE} and introduce the line
'\samp{<<DISPLAY.O>> _ALL_}' in the used \samp{pkg.which.txt} file. This
way the interpreted content of each object of the package will be
displayed on the screen during the process with a pause to give an
opportunity to see if everything seems consistent.  It is strongly
advised:

 \itemize{ 

 \item to introduce only standard ascii characters, even in the
comments.

 \item to test regularly the preparation of a \samp{tar.gz} with
\pkg{documair} during the development of a package, rather than once at
the end, in order detect more easily the origins of potantial issues.

 \item to avoid functions with name having more than one dot
(\samp{.}). It is considered as a method for an S3 object by
\pkg{documair} but only the last dot is taken into account.

 \item to be aware that the tagging of \samp{documair0$tags$v$deb$v} 
 which is\cr
 
 \samp{"#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"}.\cr
must be exactly respected as well as those of \samp{documair0$tags$v$fin$v} 
and \cr

\samp{"#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"} \cr

Of course, you can modify them as you wish.

} 

} 

\section{Naming conventions}{ 

  Remembering object names in a given package is not always an easy
task, the first reason being their number. To relieve it, we propose to
follow some conventions, if so \pkg{documair} will propose deduced
keywords.


 \itemize{ 

 \item Names will be composed of \emph{name components} separated with
digits. For instance \samp{print8object} has \samp{print} and
\samp{object} as components. The number of components can be one,
two,\ldots or more.

 \item When nouns, the components can be singular or plural, making
differences.

 \item The separating digits have a meaning: 


 \itemize{ 

 \item [0] pkg0 (object proposed by the package \pkg{pkg}) /0 ~ Object/

 \item [1] res4objA1objB (and; here 'res' obtained from 'objA' and
'objB') /1 = one = an ~ and/

 \item [2] a2b (conversion from object a to object b, even if the
conversion is not one-to-one, a reverse function b2a is supposed to
exist) / 2 ~ to/

 \item [3] series3fun (function fun belongs to the family series) /3 ~
\\in/

 \item [4] trait4object (extract some characteristic from an object) /4
= Four ~ From/

 \item [5] <free for the moment> 

 \item [6] split8text6tag (split a text object [with | by means of]
tags) /6 = sIx ~~ wIth/

 \item [7] image7path (image path) /upper bar of 7 is similar to an
hyphen used to join two nouns or an adjective and a noun./

 \item [8] action8object (make an action on an (or several) object(s))
 /8 ~ a/

 \item [9] empty7object9 (is it an empty object?: question mark) /9 ~~
 ?/ } } }


\section{Projected evolution of \pkg{documair}}{

  
\itemize{ 


 \item Make possible to have operator functions like \samp{\%T\%}.

 \item Allow the user not to have too many \samp{*.code.r} files. For
that, the code files could be first splitted into elementary sub-files
with a splitting tag like \samp{#<<<--->>>}. This would be convenient
when there are numerous aliased object sets. Indeed, currently, each
identically aliased set of objects must be in a distinct file.

 \item Get the main steps of the algorithm used in the function by
collecting some tag contents within the code (introduced as special
titles) and added either as a special section or as a new paragraph in
the details section.

 \item Allows the possibility of including files for repeated pieces of
code, at least for one level.

 \item Allows the introduction of enumerations in the comments.

 \item As an option, check and impose typographical rules like upper
      cases at the beginning and final dot to the argument
      description... with some indication in a separate file of the
      changes to give the user the opportunity to check them.

 \item Improve the way aliased objects are documented, allowing
      collective fields like titles...


} 

}

\section{Acknowledgment}{

  The authors want to thank Annie Bouvier and Caroline Bidot for their
  useful supports in solving some technical difficulties we had when
  elaborating the package.

}

\author{ 
Jean-Baptiste Denis (MIAj - Inra - Jouy-en-Josas),\cr 
R\'{e}gis Pouillot and \cr 
Ki\^{e}n Ki\^{e}u (MIAj - Inra - Jouy-en-Josas) 
} 
\keyword{ package } 
 
