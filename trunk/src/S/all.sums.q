# $Id$

"all.sums"<-
function(total, parts)
{
#
#  From Bill Venables,  end Dec 1993 s-news (see digests)
#
#  Returns all parts part compositions of total (i.e. a {part,total}
#  simplex lattice
#
	if(parts == 1) {
		return(as.matrix(total))
	}
	else {
		p <- NULL
		for(i in total:0)
			p <- rbind(p, cbind(i, Recall(total - i, parts - 1)))
	}
	p
}
