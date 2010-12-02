\name{expert4chain}
\alias{expert4chain}
\title{
reads the expert assessement of an expertise and builds the
corresponding chain
}
\description{
From a file completed by the assessement of expert(s), and an
existing chain, this function constitutes a chain. All assessment
must be done.
}
\usage{
expert4chain(file, chain)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 ## chain2fileexpert(prr.chain3, "tutu");
 ## file 'tutu' must be completed by hand 
 ## cha <- expert4chain("tutu", prr.chain);
 ## print(cha);
 ## unlink("tutu");
}
\arguments{
\item{file}{name of the file where must be extracted the expert
assessemnt.}
\item{chain}{The /chain/ serving as template for the new chain. It
must be completely consistent with the file assessment.}
}
\details{
Usually the expert assessment file comes from a call to
\code{chaine2fileexpert}. Nodes can be in any order but the first
component must be devoted to the chain description.\cr Tags to be
used for the file are \code{prr.tag1}.
}
\value{
The resulting /chain/.
}
