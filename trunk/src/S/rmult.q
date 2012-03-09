# $Id$

"rmult"<-
function(reps, n, p)
{
# 14 Feb 1997 (S-news of 13 Feb 1997, Franz-Josef Mueter, mueter@ims.alaska.edu)
# Generate reps samples from a Multinomial(n, p) distribution, where n is a 
# scalar and p is a vector of probabilities.
#
	x <- matrix(sample(seq(along = p), reps * n, T, p), nrow = reps)
	assign("k", length(p), frame = 0)
	apply(x, 1, function(x)
	tabulate(x, k))
}
