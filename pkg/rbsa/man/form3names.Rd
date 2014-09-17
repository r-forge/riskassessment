% 
% created by /documair/ at 14_09_15@11:19 
% 
\name{form3names} 
% 
\alias{form3names} 
% 
\title{provides systematic names for items} 
% 
\usage{ 
  form3names(nbn,nom=character(0),prefix="",
             upca=TRUE,nume=14,
             monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  form3names(2); 
  form3names(2,nume=-3); 
  form3names(2,prefix="rbsa."); 
  form3names(2,upca=FALSE); 
  form3names(5,"D"); 
  form3names(5,"Y"); 
  form3names(30); 
} 
% 
\description{ 
   
  Provides systematic names for a series of items according to their 
  number and taking care of previous names. 
   
} 
% 
\value{ 
   
  vector with \samp{nbn} different strings associated to new names 
   
} 
% 
 
\arguments{ 
    \item{nbn}{Number of new item names to generate} 
    \item{nom}{ Already present names (to avoid identical names).} 
    \item{prefix}{ Systematic prefix of all names to generate. Must comprise the dot, if one wants such a separator between it and the specific part of the name. Of course can be 'underscore' or whatever else.} 
    \item{upca}{ Indicates whether the letters constituting the new names must be uppercase or not.} 
    \item{nume}{ Its absolute value gives the number of the letter to use when the alphabet is not sufficient. When negative, alphabet is not considered as a first possibility. For instance 2 will indicate "B" and the default is "N". When 0, no letter is considered.} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{utilities} 
\keyword{format} 
\keyword{name} 
\seealso{ 
\code{\link{form3numbering}} 
} 
