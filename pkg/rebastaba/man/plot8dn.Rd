\name{plot8dn}
\alias{plot8dn}
\title{
Plots the numeric or categoric variables of a dn object
}
\description{
(dn) produces the (X,Y) plots from a given dn with possible selection
of variables and simulations. red and cod slots are used. To get a
better picture for cartesian representations, some shaking can be
applied to the integ variables.\cr When X!=Y in the couple (X,Y) a
bivariate representation is issued according to the nature of the two
variables. When X==Y, then a univariate representation is performed.
}
\usage{
plot8dn(x, y="useless", ..., titles=NULL, stitles=NULL, xxlab=NULL,
	  yylab=NULL, varx=1, vary=0, quels=0, lab=1, shaking=0.100,
	  file="")
}
\examples{
 rebastaba3k("RESET"); # for R checking convenience
 plot(rebastaba.dn1);
 plot(rebastaba.dn1, vary=-1);
 plot(rebastaba.dn4, varx="A", vary="B");
 plot(rebastaba.dn4, varx="A", vary="D");
 plot(rebastaba.dn4, varx="D", vary="A");
 plot(rebastaba.dn4, varx="E", vary="F");
}
\arguments{
\item{x}{the rbsb object to plot}
\item{y}{ For compatibility with the generic plot function. Useless
here.}
\item{\dots}{Further arguments to be passed to the plot function.}
\item{titles}{ Defines the titles to provide on each Cartesian
diagram(s) with a character vector cycled as necessary to cover all
diagrams. If NULL the name of the dn with pertinent information is
issued.}
\item{stitles}{ Defines the subtitles to provide on each Cartesian
diagram(s) with a character. If NULL standard sub-title.}
\item{xxlab}{ As \code{stitles} but for \code{xlab} arguments of the
plots.}
\item{yylab}{ As \code{xxlab} for \code{ylab}.}
\item{varx}{ Indicates the variables to use as abscissae. If 1, all
numeric variables of the dn are involved. If 2, all categoric
variables of the dn are involved. If 3, all variables of the dn are
involved. If not, indicates by their names which variables must be
used.}
\item{vary}{ Indicates which variables to use as ordinates. If -1,
only univariate graphs of \code{varx} are done, If 0, \code{vary} is
set identical to \code{varx}, If 1, all numeric variables of the dn
are involved, If 2, all categoric variables of the dn are involved,
If 3, all variables of the dn are involved. For these numerical
orders, reciprocal bivariate graphs are discarded. If not numeric,
must have the same size as varx providing the names of the variables
to use in ordinates.}
\item{quels}{ Indicates the selection of the points to be displayed.
(i) 0 then all simulations are displayed. (ii) A positive integer
indicating the number of simulations to sample within the data frame.
(iii) Can be a character string and must be on the format "V1==3"
indicating that only the simulated observations for which the
variable "V1" has got 3 as value will be displayed. So "V3" must
exist and probably is an integ or categ variable. Be careful that no
spaces are possible, e.g. "V1 == 3" will not work. (iv) Can be a
boolean vector with as many components as simulated observations,
only those with TRUE will be selected. (v) When its length is zero,
no plot is performed.}
\item{lab}{ Indicates the labels with which the points must be
identified in case of binumeric plots. (i) Can be a numeric scalar:
the symbol for localization. (ii) Can be a numerical vector with the
same size that the number of rows of x@df, used to indicate which
standard symbols of R plot to use as label (in case of selection,
only those selected will be used). (iii) Can be a single character:
then must indicates a name variables (if continuous the values of it
will be used after rounding as labels). (iv) Can be a character
vector with the same size that the number of rows of x@df, used as
there are (in case of selection, only those selected will be used).}
\item{shaking}{ In standard deviation of each variable, the
coefficient for giggling the 'integ' variables in order to better see
identically plotted points in binumeric plots.}
\item{file}{ When not empty produces a text file of this name
containing the numbering of all page representations. Quite useful to
find a specific diagram when the diagrams are numerous.}
}
\details{
Some
}
\value{
returns nothing but plots are drawn and possibly a text file is
created.
}
