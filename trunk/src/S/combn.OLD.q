"combn.OLD"<-
function(x, m = 2, sort.it = T)
{
#   DATE WRITTEN:  14 April 1994           LAST REVISED:  22 March 1995
#   AUTHOR:  Scott D. Chasalow
#
#   DESCRIPTION:
#         Generate all combinations of the elements of x taken m at a time. 
#         If x is a numeric object of length 1,  returns all combinations of
#         the elements of seq(x) taken m at a time.
#
	if(length(m) > 1) {
		warning(paste("Argument m has", length(m), 
			"elements: only the first used"))
		m <- m[1]
	}
	if(m < 0)
		stop("m < 0")
	if(m == 0)
		return(vector(mode(x), 0))
	if(is.numeric(x) && length(x) == 1)
		x <- seq(x)
	if(m == 1)
		return(as.matrix(x))
	n <- length(x)
	if(n < m)
		stop("n < m")
	if(n == 0)
		return(NULL)
	alist <- vector("list", m)
	ilist <- alist
	a <- array(seq(length = n), rep(n, m))
	alist[[1]] <- a
	p <- seq(m)
	lower.t <- rep(T, length(a))
	for(i in seq(2, m)) {
		p[i] <- 1
		p[1] <- i
		alist[[i]] <- aperm(a, p)
		p[i] <- i
		lower.t <- lower.t & (alist[[i - 1]] < alist[[i]])
	}
	ilist[[1]] <- alist[[1]][lower.t]
	for(i in seq(2, m))
		ilist[[i]] <- alist[[i]][lower.t]
	out <- sapply(ilist, function(x, y)
	x[y], x = x)
	if(sort.it)
		sortmat(out)
	else out
}
