"combn.bill"<-
function(n, k, set = 1:n)
{
# From Bill Venables, email of 12 April 1996
	if(k <= 0) NULL else if(k >= n)
		set
	else rbind(cbind(set[1], Recall(n - 1, k - 1, set[-1])), Recall(n - 1, 
			k, set[-1]))
}
