\name{modify4gn}
\alias{modify4gn}
\title{
(gn) interactive modifications of a gn
}
\description{
graphical interface to modify (i) the positions, (ii) the arcs and
(iii) the nodes of an object of class gn. Besides, it provides the
facility to change the viewpoint from which is drawn the graph
(positions of a gn object are defined in R to the 3). \cr The
modified gn is returned. \cr To exit, just click the grey button 'E'
\cr Action are started by the left hand buttons. Their codes are:\cr
"rst" for resetting (cancel all modifications),\cr "MN" for moving
the position of a node (the user must click twice: for indicating the
node and then the new desired position),\cr "AA" for adding an arc
(the user must click twice to indicate the starting node then the
ending node), \cr "RA" for removing an existing arc (the user must
click twice to indicate the arc to be removed), \cr "AN" for adding a
node (the user must click once to indicate the position of the new
node), its name is automatically provided but can be changed
afterwards, \cr "RN" for removing a node (the user must click once,
on the node to be removed), \cr "+" for narrowing the zoom (the user
must click once, on the point where the zoom must be centered; the
zooming coefficient is driven by the @pgr@kzoom parameter),\cr "-"
for widing the zoom (the user must click once, on the point where the
zoom must be centered; the zooming coefficient is driven by the
@pgr@kzoom parameter),\cr "Rot" for rotating the representation in R
to the three (the user must click once, in the direction of the
rotation and more or less close to the center to indicate the angle
to perform; the two big circles which appears on the diagram are
associated to rotations of 5 and 10 degrees.
}
\usage{
modify4gn(gn)
}
\arguments{
\item{gn}{the gn object gn to be interactively modifyed.}
}
\value{
The modified gn by the actions performed by the user.
}
