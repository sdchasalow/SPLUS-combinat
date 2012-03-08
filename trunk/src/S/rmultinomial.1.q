"rmultinomial.1"<-
function(n, p)
{
# 14 Feb 1997
# Generate a single sample from a Multinomial(n, p) distrn
#
	k <- length(p)
	tabulate(sample(k, n, replace = T, prob = p), k)
}
