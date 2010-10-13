\name{win-class}
\docType{class}
\alias{win-class}
\alias{print,win-method}

\title{Class "win" to define a node from a data.frame}

\description{	Three almost independent points are described in /win/ to define a new node with a non parametric regression style from a data base. (I) the variable names and their natures. (II) The windows to select a subset of prediction individuals, usually based on some covariables (the parents in the Bayesian network perspective); the associated slots start with a \code{s}. (III) The way from which a value is computed to represent the selected set, sometimes called the prediction; the associated slots start with an \code{r}. Be aware that most of the sophistication of the algorithm is linked with the fact that there can be several parents and the prediction be multivariate.
}
\section{Objects from the Class}{
Examples of such objects are \code{rbsb.win?}. The usual way to create a new object of this class is by calling the generic function \code{new("win",swg=...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{nat}:}{A named \code{"character"} providing the natures of the variables with their names (without node indication) by its names. At least one variable is expected when it is not the null /win/.}
    \item{\code{swg}:}{A named \code{"numeric"} providing by its names the parent variables (in the format \code{node[variable]}) and by its values their weights to compute the distance determining the subset of candidates. When there are no parents, all individuals are set to the same distance.}
    \item{\code{skk}:}{A \code{"numeric(1)"}: the power coefficient in the distance formulae (cannot be negative).}
    \item{\code{sdi}:}{A \code{"numeric(2)"}: the minimum and 
      maximum distances between the data base individuals and the individual to be predicted to belong to the candidate subset.}
    \item{\code{snb}:}{A \code{"numeric(2)"}: the minimum and
      maximum numbers of individuals in the candidate subset. When less are proposed with the distance criteria a \code{NA} is returned; when more, the closest ones are retained (in case of ties, random drawing is performed).}
    \item{\code{rty}:}{A \code{"character(2:3)"} to indicate the way the prediction is done from the selected subset.
%
 \code{rty[1]} precises the way the prediction is done:
'*' each column independently, 
'0' the real row, the closest to '*' (according to the defined distance
for the representativeness) 
and '1' a real row selected from '\code{rmo}' variable using 
rty[2] prediction type.
%
\code{rty[2]} precises the type of prediction: 
'random' a random draw with possibly a weighting 
given by the absolute value of \code{rmo} when rty[1]=='1' or '*' 
(if not equal weighting is given to all observations), 
'mean' the value of the individual closest in mean, for the variable \code{rmo} when rty[1]=='1'or'*', 
'median' like 'mean' but using the median,  
'systematic', the frequency of the drawing is given by rk2, 
if it does not exist it is set to one; 
when rty[1] is '1'or'*', \code{rmo} variable is taken to define
the order of drawing if not the order of the data basis is used; 
'quantile', as median but for any probability level indicated in rk2.}
%
 \item{rmo}{The name of the possible variable which monitor the algorithm. }
 \item{rk2}{The frequency for a systematic drawing or the probability for a quantile drawing.}
    \item{\code{rwg}:}{A \code{"numeric"} providing the variable weights to compute the distance determining the best representative to be determined when \code{rty[1]} is '0' (their order is supposed to be this of slot \code{nat}).}
    \item{\code{rkk}:}{A \code{"numeric(1)"}: the power coefficient in the distance formulae (cannot be negative).; necessary only when slot \code{rwg} exists.}
  }
}
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8win} for the details. }
         }
}

\author{ J.-B. Denis }

\examples{
showClass("win")
}
\keyword{classes}
