\name{form3paragraphe}
\alias{form3paragraphe}
\title{
(f0) prints or prepares paragraphes from a character vector. Each
component is supposed to be a paragraph but the first one can be
considered as a title.
}
\description{
prints or prepares a character string "texte" as a small formatted
text.
}
\usage{
form3paragraphe(texte, titre=-1, wid=60, fli=NULL, sep=1, jus=1, tronc=TRUE, ed="",
	  ef="", imp=TRUE)
}
\examples{
 form3paragraphe(c("My Title", "My important first sentence.", "Last one!"));
}
\arguments{
\item{texte}{The text to print (character vector).}
\item{titre}{ When \eqn{>} -2 indicates that the first component is a
title (if not a simple paragraph). Then the value of titre gives the
emphasize to put on the title. Notice that the title is not spletted
in several lines as are the other components according to 'wid'
value.}
\item{wid}{ The width (in characters) without including indentation
and frames.}
\item{fli}{ When NULL, the first line of each paragraph (in fact the
second if there is a title) is issued as a standard line. If not,
fli[1] spaces are added before and the considered width is fli[2]
(not including the added spaces). Also this means that your already
wrote fli[3] characters on the first line \{this last possibility can
be used only when there are no title and for the the first component.
For instance, a French paragraph will be issued with fli =
c(5,wid,0). The possibility of modifying wid for the first line can
be of use when adding the name of an item first.}
\item{sep}{ Number of lines separating each paragraph.}
\item{jus}{ Type of justification (1=left, 2=centred, 3=left).}
\item{tronc}{ Must troncation be done when a word is greater than the
proposed wid?}
\item{ed}{ Framing at the beginning of each line.}
\item{ef}{ Framing at the end of each line.}
\item{imp}{ Printing is performed and nothing is returned. If FALSE,
the character string is returned (including possible new lines)}
}
\value{
either nothing or a character string according to imp
}
\keyword{print}
