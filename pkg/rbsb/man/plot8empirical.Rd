\name{plot8empirical}
\alias{plot8empirical}
\title{
plots an /empirical/
}
\description{
This function plots an /empirical/ object.
}
\usage{
plot8empirical(x, y="useless", ..., what="dc", ticks="s", xli=NULL, ylid=NULL,
	  ylic=NULL, main=NULL)
}
\arguments{
\item{x}{The \code{empirical} object to plot.}
\item{y}{ For compatibility with the generic plot function. Useless}
\item{\dots}{Further arguments to be passed to the plot function.}
\item{what}{What to plot? \code{d} for density and \code{c} for
cumulative.}
\item{ticks}{Markers of the discontinuity of the function. When
numerical, 0 means no ticks if not the height of the ticks. When
character, \code{s} means until the curve, \code{S} the same but
ticks will have a minimum length, if not nothing.}
\item{xli}{possible imposed \code{xlim} for the density and the
cumulative}
\item{ylid}{possible imposed \code{ylim} for the density}
\item{ylic}{possible imposed \code{ylim} for the cumulative}
\item{main}{Some prepared title for the plot.}
}
\value{
nothing but a plot is performed
}
\keyword{classes}
