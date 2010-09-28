\name{draw8empidata}
\alias{draw8empidata}
\title{
performs an empidata [multivariate] draw or prediction
}
\description{
(dn) According to parent values provided in \code{X}, returns draws
for simulation from the \code{E} data frame. The returned variables
are defined by \code{names(win@nat)}, the subset selection is defined
by \code{win}. See the detail section for the management of missing
values.\cr When draws are done individually (within a loop), each
time a draw is done, a record indicating the size of the candidate
subset is appended to the file of name 'rebastaba.efi'.\cr Notice
that the parent names (\code{names(win@swg)}) and variable to extract
(\code{names(win@nat)}) are given at the variable level.
}
\usage{
draw8empidata(X, E, win, nat, bnid, strict=FALSE)
}
\examples{
 rebastaba3k("RESET"); # to comply R checking
 E <- data.frame(A=1:100, c=100:1, w=round(sin(1:100), 2)); # the empirical distribution
 X <- as.data.frame(matrix(20:29, 10, 1, dimnames=list(NULL, "A"))); # the covariate values
 win <- new("win", nat=structure("conti", .Names="c"), 
 swg=structure(1, .Names="A"), skk=0, sdi=c(0, 1), snb=c(1, 2));
 draw8empidata(X, E, win, nat=structure("conti", .Names="A"), bnid="rebastaba.bn2");
}
\arguments{
\item{X}{ The matrix of values of the already simulated values. It
must comprises the parents. If there is no parents, the matrix must
be given (even with no columns) just to provide the number of
simulations by its row number.}
\item{E}{ The data frame to be used for the empidata distribution. It
must comprise all possible parents (WITH node name and brackets) and
variables (WITHOUT the node name and brackets).}
\item{win}{ /win/ object to be used for the selection. Notice that
\code{win@swg} indicates the parents and \code{names(win@nat)}
indicates the variables of the nodes.}
\item{nat}{ Named vector of the nature parents (possibly more,
usually those of the \code{X} columns).}
\item{bnid}{ character identifying the bn (for indication in the
rebastaba.efi sinking.}
\item{strict}{ Must the elimination of missing values be common to
all variables of the node?}
}
\details{
To allow the successive links of empidata nodes the node name was
eliminated from the correspondance. E.g. let suppose that we are
dealing witht the sequence of \code{SEX -> E[HGT] -> F[WGT]} where
\code{E} and \code{F} are associated to empidata bases. In the
adopted way, this implies that the corresponding parents
\code{E[SEX]} and \code{F[HGT]} exists with the reduced names
\code{SEX} for the base \code{E} and \code{HGT} for the base
\code{F}.\cr Notice that implies that the same variable name can be
used as co-parents for a given node. E.g. \code{E1[HGT]} and
\code{E2[HGT]} cannot be simultaneous parents of \code{F[WGT]}. \cr
Missing values can be dealt in two ways. Here are the rules which are
applied.\cr Let us denote X[parents], E[parents] and E[variables],
respectively, the inherited parents from the previous simulated
values, the corresponding parents in the data base and the variables
which are drawn. 

\describe{
\item{1}{When there are missing values in E[variables], either they
are left (strict=FALSE) either they are prevented, forbidding these
observations to be drawn.}
\item{2}{When there are parents, any missingness in X[parents] leads
to missing values in all E[variables] since there is no way to know
the corresponding observations in E data base..}
\item{3}{For the same reason, when there are parents, all
observations with missing values in E[parents] are discarded.}}
}
\value{
A data.frame of size \code{nrow(X)} times \code{length(win@nat)} with
the simulated values in the right order.
}
