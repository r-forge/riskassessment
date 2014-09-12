% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{inquiry} 
% 
\alias{inquiry} 
% 
\title{gets some information after a question} 
% 
\usage{ 
  inquiry(question,
          q7help="H",
          help=NULL,
          q7stop="S",
          a7type="character",
          a7default="Y",
          a7length=1,
          a7separator="//",
          a7possibility="P",
          a7possibilities=c("y","Y","n","N"),
          patience=3,
          format=1
          ) 
} 
% 
\examples{ 
  \dontrun{inquiry("Hungry ?",q7help="Si 'oui', je mets 'a cuire le repas", 
  a7possibilities=c("oui","non"),a7default=NULL);} 
} 
% 
\details{ 
   
  Answers \samp{q7help} or \samp{a7possibility} are not possible (only 
  when these options are activated, that is \samp{help} and \samp{a7possibilities} are not \samp{NULL}. 
   
} 
% 
\description{ 
   
  This function issues a question onto the screen 
  and get the answer from the user keyboard. The answer can be checked among 
  a set of possibilities.\cr 
  The basic principle is to provide some restrictions and checks on the answers. 
  If an answer is not complete (because more components are asked) the user must 
  follow to type it. If an answer is not accepted, it is repeated a limited 
  number of times before a fatal error is issued. Also some help can be 
  provided in case the user asks for it. 
   
} 
% 
\value{ 
   
  The answer, or \samp{NULL} if (i) the answer was void or (ii) no accepted answer was provided by the user. 
  The answer can be a vector, if so and if constrained are imposed for accepted values, then the acceptance 
  is checked for every component of the vector. 
   
} 
% 
 
\arguments{ 
    \item{question}{The question to issue on the screen before getting the answer.} 
    \item{q7help}{ Answer providing the display of the helping complement to the question if any (then cannot be a possible answer returned by the function).} 
    \item{help}{Additional information provided to the user if requested with \samp{q7help}. } 
    \item{q7stop}{ Answer to give to stop the inquiry and a \samp{NULL} is returned (then cannot be a acceptable answer).} 
    \item{a7type}{ indicates the type of the answer: either "integer", "numeric" or "character", when not "character" a vector can be returned with blank as separator. } 
    \item{a7default}{ Default value when no answer is issued; must be \samp{numeric} if \samp{a7type} is not \samp{character}. Must be \samp{NULL} if no default value.} 
    \item{a7length}{ number of returned values; either a unique number or the minimum and maximum numbers of items. For character answers, the separator between paragraphs is \samp{a7separator}; for numerical values it is the blank(s). To include several blanks within character strings, you must use quoting marks (\samp{\"}). } 
    \item{a7separator}{ character string indicating a separation between paragraphes for \samp{character} answers.} 
    \item{a7possibility}{ Answer providing the display of the accepted possibilities. (when they are constrained, then cannot be a right answer).} 
    \item{a7possibilities}{ When the type is "character" a character vector containing the different possibilities, or a named character, the names are the possibilities and the values are the explanations of each possibility. When the type is not "character" a numerical vector of size 2 providing the minimum and maximum for accepted values. \samp{NULL} for no check on the possibilities.} 
    \item{patience}{ number of accepted bad answers before a fatal stop.} 
    \item{format}{ 0: no help and reduced display; 1: reduced display; 2: enlarged display.   } 
} 
 
% 
\keyword{input/output} 
\section{Future}{ 
Add the possibility to answer with intervals for numerical answers. 
} 
