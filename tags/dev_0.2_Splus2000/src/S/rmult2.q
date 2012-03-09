# $Id$

"rmult2"<-
function(n, p)
{
# 14 Feb 1997 (from 13 Feb 1997 S-news)
# Random sample from different multinomial distributions, among which n
# varies (n can be a vector), but p is constant.
#
	x <- sample(seq(along = p), sum(n), T, p)
	indices <- rep(1:length(n), n)
	assign("k", length(p), frame = "0")
	lis <- tapply(x, indices, function(x)
	table(factor(x, 1:k)))
	matrix(unlist(lis), nrow = k)
}
