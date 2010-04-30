\name{easyp3expanded}
\alias{easyp3expanded}
\title{
(bn) returns an expanded easyp from an easyp
}
\description{
Changing the nodes into the node variables. This is done when
necessary, e.g. eas can be returned as it is. Detected
inconsistencies provoke fatal errors.
}
\usage{
easyp3expanded(ldim, eas, bn)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(easyp3expanded(1, "{{A}}+{{B[a]}}", rbsb.bn2));
 print(easyp3expanded(2, "{{A}}+{{B[a]}}", rbsb.bn2));
 print(easyp3expanded(3, "{{B}}", rbsb.bn2));
 print(easyp3expanded(3, "{{A}}+{{B}}", rbsb.bn2));
 \dontrun{easyp3expanded(5, "{{B}}", rbsb.bn2)};
}
\arguments{
\item{ldim}{When greater than one, must be the length of the
resulting easyp.}
\item{eas}{The scalar easyp expression.}
\item{bn}{The associated /bn/.}
}
\value{
A character containing the expanded easyp
}
