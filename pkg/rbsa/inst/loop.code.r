
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bc <- function(nb)
#TITLE  sequences for easy looping
#DESCRIPTION
# This function returns \samp{1:nb} when \samp{nb > 0} and
#         \samp{numeric(0)} otherwise.
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
# nb is rounded before use
#ALIAS looping
#KEYWORDS iteration helpful
#INPUTS
#{nb}    <<length of the loop>>
#[INPUTS]
#VALUE
# \samp{1:nb} if \samp{nb > 0}
# else \samp{numeric(0)}.
#EXAMPLE
# bc(0);
# bc(5);
# bc(pi);
# bc(4*pi);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# bc for BouCle (loop in French)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 07_09_18
#REVISED 09_04_05
#--------------------------------------------
{
if (is.null(nb)) {return(numeric(0));}
if (length(nb)!=1) {
    erreur(nb,"bc deals only with scalar nb");
}
if (nb > 0) {return(1:max(1,round(nb)));}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bd <- function(n1,n2)
#TITLE  sequence for insertions
#DESCRIPTION
# This function returns \samp{n1:n2} when \samp{n1<=n2} and
#         \samp{numeric(0)} otherwise.
# Quite useful when some insertion must be done within
# a sequence
#DETAILS
#ALIAS
#KEYWORDS iteration helpful
#INPUTS
#{n1}    <<first element>>
#{n2}    <<second element>>
#[INPUTS]
#VALUE
# \samp{n1:n2} if \samp{n1<n2}
# else \samp{numeric(0)}.
#EXAMPLE
# xx <- 1:5;
# for (ii in 1:6) { print(c(xx[bd(1,ii-1)],10,xx[bd(ii,5)]));}
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# bc for BouCle (loop in French)
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 11_01_12
#REVISED 11_01_12
#--------------------------------------------
{
if (n1 <= n2) {return(n1:n2);}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bf <- function(x)
#TITLE  sequence for looping over an object
#DESCRIPTION
# This function returns \samp{1:length(x)} when \samp{length(x) > 0} and
#         \samp{numeric(0)} otherwise.
# Quite useful to prevent starting
# a loop of length nought
#DETAILS
#KEYWORDS iteration helpful
#ALIAS
#INPUTS
#{x}    <<vector>>
#[INPUTS]
#VALUE
# \samp{1:length(x)} if \samp{length(x) > 0}
# else \samp{numeric(0)}.
#EXAMPLE
# bf(0);
# bf(5);
# bf(character(0));
# bf(letters);
#REFERENCE
#SEE ALSO 
#CALLING
#COMMENT
# bf for Boucle For the elements of an object
#FUTURE
#AUTHOR J.-B. Denis
#CREATED 08_09_05
#REVISED 08_09_05
#--------------------------------------------
{
if (length(x) > 0) { return(1:length(x));}
numeric(0);
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
