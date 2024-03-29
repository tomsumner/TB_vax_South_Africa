## FUNCTION ABC_mcmc: ABC coupled to MCMC (Marjoram et al. 2003, Wegmann et al. 2009)
##############################################################################
ABC_mcmc <-function(method,model,prior,summary_stat_target,n_rec=100,n_between_sampling=10,n_cluster=1,use_seed=FALSE,verbose=FALSE,...){
    ## checking errors in the inputs
    if(missing(method)) stop("'method' is missing")
    if(missing(model)) stop("'model' is missing")
    if(missing(prior)) stop("'prior' is missing")
    if(!is.list(prior)) stop("'prior' has to be a list")
    l=length(prior)
    for (i in 1:l){
    	if(!any(prior[[i]][1] == c("unif", "normal", "lognormal", "exponential"))) {
        	stop("Prior distribution type must be unif, normal, lognormal or exponential")
    	}
	if (prior[[i]][1]=="exponential"){
		if (length(prior[[i]])<2){
			stop(paste("Incomplete prior information for parameter ",i,sep=""))
		}
	}
	else{
		if (length(prior[[i]])<3){
			stop(paste("Incomplete prior information for parameter ",i,sep=""))
		}
	}
    }
    if(missing(summary_stat_target)) stop("'summary_stat_target' is missing")
    if(!any(method == c("Marjoram_original", "Marjoram", "Wegmann"))){
        stop("Method must be Marjoram_original, Marjoram or wegmann")
    }
    if(!is.vector(summary_stat_target)) stop("'summary_stat_target' has to be a vector.")
    if(!is.vector(n_cluster)) stop("'n_cluster' has to be a number.")
    if(length(n_cluster)>1) stop("'n_cluster' has to be a number.")
    if (n_cluster<1) stop ("'n_cluster' has to be a positive number.")
    n_cluster=floor(n_cluster)
    if(!is.logical(use_seed)) stop("'use_seed' has to be boolean")
    if(!is.logical(verbose)) stop("'verbose' has to be boolean")
 
    	mcmc=NULL
	if (n_cluster==1){
		mcmc = .ABC_mcmc_internal(method,model,prior,n_rec,n_between_sampling,summary_stat_target,use_seed,verbose,...)
	}
	else{
		mcmc = .ABC_mcmc_cluster(method,model,prior,n_rec,n_between_sampling,summary_stat_target,n_cluster,use_seed,verbose,...)
	}
mcmc
}

