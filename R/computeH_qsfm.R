############################################################
### Helper function: computes H values, given the dataset, 
### the units for which H must be computed and the indices 
### of the column in the dataset to compute H for
###
### authors:        Roberto Sichera, Pietro Pizzuto
### last modified:  13/10/2016

computeH_qsfm <- function(hht,id,yearVar){
    ### X:          matrix or data.frame with data
    ### id:         row indices of regions for which H is to be computed
    ### yearVar:    vector containing the indices of the variable columns
    ###
    ### returns a vector with H quantities
    
    # h <- apply(X[id,yearVar], 2, function(x) x/mean(x))
    # H <- apply(h, 1, function(h) mean((h-1)^2) )

    t		<- length(yearVar)
    p       <- round(t/3)
    
    trd		<- log(seq(from = 1, by = 1, length.out = t))	# construct log(t)
    trd		<- trd[(p + 1):t]						# also starts at p+1
    
    hht <- hht[id,yearVar]
    hht1	<- apply(hht, 2, function(x) x/mean(x))	# estimate transition coefficients	(h_it)
    hht.temp1	<- (hht1 - 1)^2						# construct cross sectional variance ratio, first (hit-1)^2
    hht.temp2	<- colMeans(hht.temp1)		# second: cross section average 
    lhht	<- log(hht.temp2[1]/hht.temp2)			# taking log(H0/Ht)
    rht		<- lhht[(p + 1):t]					# data begin at t=rT=p+1
    rht		<- rht - 2*log(trd)					# construct log(H0/Ht)-2*log(t)
    
    # print(list( hht1=hht1, rht=rht))
    return(rht)
}
