# $Id$

"allwosamples"<-
function(n, k)
{
	if((n < 1) || (k < 0))
		stop("n must be positive and k must be non-negative")
	if((n != round(n)) || (k != round(k)))
		stop("n and k must be integers")
	if(n < k)
		stop("n must be greater than or equal to k")
	if(n == k)
		A <- matrix(1:n, 1, n)
	else {
		if(k == 1)
			A <- matrix(1:n)
		else {
			n <- n - 1
			B <- allwosamples(n, k)
			C <- allwosamples(n, (k - 1))
			L <- dim(C)[1]
			D <- cbind(C, rep(n + 1, L))
			A <- rbind(B, D)
		}
	}
	A
}
