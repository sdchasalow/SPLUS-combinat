# $Id$

"logfact.BAK"<-
function(x)
{
#       DATE WRITTEN: 22 May 1995           LAST REVISED:  22 May 1995
#       AUTHOR:  Scott Chasalow
#
	nna <- !is.na(x)
	y <- x[nna]
	if(length(y) == 0)
		return(x)
	if(any(y < 0)) {
		y[y < 0] <- NA
		warning("NAs generated")
		x[nna] <- y
		nna <- !is.na(x)
		y <- x[nna]
		if(length(y) == 0)
			return(x)
	}
	y[y == 0] <- 1
	which <- (y > 1)
	y <- as.list(y)
	y[which] <- lapply(y[which], seq, to = 2)
	y <- unlist(lapply(y, function(a)
	sum(log(a))))
	x[nna] <- y
	x
}
