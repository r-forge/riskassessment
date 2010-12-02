\name{repartition3dn}
\alias{repartition3dn}
\title{
scans the repartition of the values of dn@df
}
\description{
(dn) Using the three domains, returns the repartition of the
simulated values according to the limits
}
\usage{
repartition3dn(dn)
}
\examples{
 rebastaba3k("RESET"); # For R convenience when checking
 repartition3dn(rebastaba.dn4);
}
\arguments{
\item{dn}{the rbsb object to scan}
}
\value{
returns a list comprising (i) two components for the continuous
variables \$colimi = matrix of variables x (the 6 limits) \$corepa =
matrix of variables x (the 6 cases) (ii) two components for the
categoric variables \$calimi = list of lists for each variables
\$carepa = matrix of variables x (the 4 cases)
}
