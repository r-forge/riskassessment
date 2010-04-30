\name{draw8empidata}
\alias{draw8empidata}
\title{
(dn) performs an empidata [multivariate] draw
}
\description{
According to parent values provided in X, returns a draw for
simulation from E data frame. The returned variables are defined by
ny, the subset selection id defined by win. See the detail section
for the management of missing values.\cr Each time a drawn is done, a
line indicating the size in which the drawn was made is appended to
the file 'rebastaba.empidat.txt'. This file is destroyed when
compiling 'rebastaba.la.r'
}
\usage{
draw8empidata(nam, ny, E, X, nat, win, bnid, strict=FALSE, mnbo=10)
}
\examples{
 rsdn3k("RESET"); # to comply R checking
 E <- data.frame(A=1:100, c=100:1); # the empirical distribution
 X <- as.data.frame(matrix(20:29, 10, 1, dimnames=list(NULL, "A"))); # the covariate values
 win <- new("win", wgt=1, k=0, di=c(0, 1), nb=c(1, 2));
 draw8empidata("A", "B[c]", E, X, "conti", win, bnid="rbsb.bn2", mnbo=1);
}
\arguments{
\item{nam}{ Vector of the parent names of the node to be simulated.
When the parent names comprise node name and brackets, there are
eliminated to look for the correspondence into E data frame. Said in
a different way, never brackets for data.frame names. A value of
character(0) means no parents. In that case the draw is random
whateber be win@ty.}
\item{ny}{ Name(s) of the variable(s) to simulate (WITH node name and
brackets).}
\item{E}{ The data frame to be used for the empidata distribution. It
must comprise all possible parents and variables indicated in ny (but
WITHOUT the node name and brackets).}
\item{X}{ The matrix of values of the already simulated values. It
must comprises the parents. If there is no parents, the matrix must
be given (even with no columns) just to give the number of
simulations by its row number.}
\item{nat}{ Vector of the parent natures, same order as nam.}
\item{win}{ /win/ object to be used for the selection (see
'draw8pred').}
\item{bnid}{ character identifying the bn (for indication in the
rbsb.efi sinking.}
\item{strict}{(=FALSE) Must the elimination of missing values be
common to all variables of the node?}
\item{mnbo}{(=10) Minimum of number of observations for a draw to be
performed. If less than mnbo, a NA is returned.}
}
\details{
To allow the successive call of empidata with corresponding variables
the node name was eliminated from the correspondance. E.g. let
suppose that we are asking a first correspondance with the sex to get
the height of some individuals. We could match the previous drawn
"SEX" with "SEX" in the empidata E to get "HGT" in data frame but
which will be "E[HGT]" for the /bn/. Now, if we want to use the
obtained "E[HGT]" to ge the weight from another empidata set F, we
will introduce "E[HGT]" to match "HGT" into data frame F getting
"WGT" named as "F[WGT]" at the /bn/ level. This explains why, at the
code level 'nam' is transformed into "namv" with "SEX" giving "SEX"
and "E[HGT]" giving "HGT". This is the consequence that square
brackets can be problematic for variable names in data.frames. Also
this supports some consistency. \cr Missing values can be implied in
several ways. Here are the rules which are applied (the principle is
to keep the correlations between the variables, if the user does not
want them, it must call draw8empidata several times with the same
data base.).\cr Let us denote X[parents], E[parents] and
E[variables], respectively, the inherited parents from the previous
simulated values, the corresponding parents in the data base
according to win selection and the corresponding variables we are
drawing. 

\describe{
\item{1}{When there are missing values in E[variables], either they
are left (strict=FALSE) either they are prevented, forbidding these
observations to be drawn.}
\item{2}{When there are parents, any missingness in X[parents] leads
to missing values in all E[variables] since there is no way to know
the corresponding observations in E data base..}
\item{3}{For the same reason, when there are parents, all
observations with missing values in E[parents] are discarded.}
\item{4}{When there are less observations available to draw than
mnbo, then NA are issued. Notice that the important point of the size
of the available sample for a given combination is not considered
more than with the mnbo parameter with no posterior control by the
user.}}
}
\value{
A vector (matrix) of size nrow(X) with the simulated value in the
right order. When the subset which is selected is less than mnbo,
then NA is introduced.
}
