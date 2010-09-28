\name{form3title}
\alias{form3title}
\title{
prints or prepares a title
}
\description{
prints or prepares a character string "tit" (can comprise several
components) with more or less emphasis.
}
\usage{
form3title(tit, box="un", lbef=0, sbef=5, saft=0, laft=1, charbox=c("+", "|",
	  "+", "-", "+", "|", "+", "-", "."), alig=2, caret=TRUE,
	  imp=TRUE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3title("Some Title");
 form3title(c("The title", "can comprise", "several lines"), 
 box="box", lbef=4, laft=2);
 form3title(c("The title", "can comprise", "several lines"), 
 box="box", lbef=4, laft=2, 
 charbox=c("*", "+", "/", "=", "*", "+", "/", "=", " "));
}
\arguments{
\item{tit}{the title to print, each component will be printed in a
different line.}
\item{box}{defines the type of box to put around the title. Possible
options are: \code{"no"} for nothing, \code{"un"} for underlined,
\code{"ov"} for overlined, \code{"par"} for parentherized,
\code{"cor"} for the four corners, \code{"unov"} for underlined and
overlined, \code{"box"} for a compl}
\item{lbef}{if \code{numeric(1)} defines the number of empty lines
before the title; when \code{character} provides the lines to add
before the title.}
\item{sbef}{either \code{numeric(1)} or \code{character(1)}
indicating the number of spaces or the characters to introduce before
the box.}
\item{saft}{same as \code{sbef} but after.}
\item{laft}{same as \code{lbef} but after.}
\item{charbox}{nine single characters to define the box starting
after noon, and the extra 9th beign the filling character for
multiple line titles.}
\item{alig}{The aligment to be done by \code{form3justifie}: 1 for
left,2 for center and 3 for right.}
\item{caret}{Indicates if \code{\\n} must be added at the end of each
created line.}
\item{imp}{ Printing is performed and nothing is returned. If FALSE,
the character string is returned (including possible new lines)}
}
\value{
either nothing but a printing is performed or a character string
according to imp
}
\keyword{print}
