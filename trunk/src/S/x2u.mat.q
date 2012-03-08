"x2u.mat"<-
function(x)
{
# DATE WRITTEN:  09 Jun 2003 		 LAST REVISED:  09 Jun 2003
# AUTHOR:  Scott D. Chasalow  (Scott.Chasalow@monsanto.com)
#
# DESCRIPTION:
#       Converts a matrix of x-encoded simplex-lattice points to a
#       matrix of u-encoded simplex-lattice points (equivalently, 
#       "untabulates" bin counts).
#
# REQUIRED ARGUMENTS:
# x     a matrix of non-negative integers, giving x-encoded points on
#       a simplex lattice.  Element (i, j) gives the number of units
#       in class i for point j.  That is, the points are stored as
#       COLUMNS of x.  The column sums of x must be the same; that
#       is, length(unique(colSums(x))) must be 1.
#
# VALUE:
#       a matrix of positive integers, giving u-encoded points on a
#       simplex lattice.  Column j gives the u-encoding for point
#       x[, j].  Each column gives the bin numbers for a point, in
#       lexicographic order.
#
# DETAILS:
#       Simplex lattice points are also known as p-part compositions
#       of n.  The support space of a multinomial distribution is a
#       simplex lattice.
#
# SEE ALSO:
#       xsimplex, rmultz2, x2u
# 
# EXAMPLES:
#       # 5 random samples from a multinomial(4, (1/3, 1/3, 1/3)) distn:
#       x <- rmultz2(n = 4,  p = rep(1/3, 3), draws = 5)
#       x2u.mat(x)
#
#       # Complete enumeration of the support space of a multinomial
#       # distribution with n = 5 and p = 3 classes:
#       x <- xsimplex(3, 5)
#       x2u.mat(x)
#
	d <- dim(x)
	nclasses <- d[1]
	draws <- d[2]
	n <- colSums(x)
	if(length(unique(n)) > 1)
		stop("What the...?")
	else n <- n[1]
	labs <- rep(1:nclasses, draws)
	u <- rep(labs, x)
	array(u, c(n, draws))
}
