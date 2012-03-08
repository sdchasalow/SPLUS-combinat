"dmnom.quick"<-
function(x, size = sum(x), prob = stop("no prob arg"))
{
#       DATE WRITTEN: 22 May 1995           LAST REVISED:  22 May 1995
#       AUTHOR:  Scott Chasalow
#
	if(sum(x) != size) 0 else exp(logfact(size) + sum(x * log(prob) - 
			logfact(x)))
}
