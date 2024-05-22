# $Id$

"nCm.scalar"<-
function(n, m)
{
	if(trunc(m) != m)
		stop("m is not an integer")
	if(m == 0)
		1
	else if(m < 0 || n == 0)
		0
	else if(n < 0) {
		if(n == -1)
			(-1)^m
		else if(m %% 2)
			-1 * nCm(m - n - 1, m)
		else nCm(m - n - 1, m)
	}
	else if(n < m) {
		if(trunc(n) == n)
			0
		else {
			iseq <- seq(n - m + 1, n)
			negs <- sum(iseq < 0)
			((-1)^negs) * exp(sum(log(abs(iseq))) - lgamma(m + 1))
		}
	}
	else exp(lgamma(n + 1) - lgamma(m + 1) - lgamma(n - m + 1))
}
