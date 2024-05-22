# $Id$

"enummulti"<-
function(k, n)
{
# k = number of categories
# n = total count
	if(n == 0) return(matrix(0, 1, k))
	if(k == 1)
		return(n)
	E <- NULL
	for(i in 0:n)
		E <- rbind(E, cbind(i, Recall(k - 1, n - i)))
	E
}
