# $Id$

"nchoosem"<-
function(n, m)
{
	if(trunc(n) != n || trunc(m) != m)
		stop("n and/or m not an integer")
	if(n < 0 || m < 0)
		stop("n < 0 and/or m < 0")
	if(n < m)
		stop("n < m")
	if(m == 0)
		return(1)
	else return(round(exp(sum(log(seq(n - m + 1, n))))/fact(m)))
}
