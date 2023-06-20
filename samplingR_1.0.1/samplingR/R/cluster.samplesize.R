#' @title Sample size estimation on cluster sampling
#' @description
#' Calculates the required sample size in order to achieve an
#' absolute sampling error  less or equal to the specified for an specific
#' estimator and an optional confidence interval in cluster sampling.
#'
#' @param N Number of clusters in the population.
#' @param data Dataset.
#' @param error Sampling error.
#' @param alpha Significance level to obtain confidence intervals.
#' @param estimator The estimator to be estimated. Default is "total".
#' @param replace Whether the samples to be taken can have repeated instances or not.
#'
#' @return Number of clusters to be taken.
#'
#' @details This function admits both grouped and non-grouped by cluster data.\cr
#' Non-grouped data must have interest variable data in the first column and cluster
#' name each individual belongs to in the last column.\cr
#' Grouped by cluster data must have interest variable data in the first column,
#' cluster size in the second and the cluster name in the last column. Interest
#' values of grouped data must reflect the total value of each cluster.
#'
#' @export
#'
#' @examples
#' d<-cbind(rnorm(500, 50, 20), rep(c(1:50),10)) #Non-grouped data
#' sample<-cluster.sample(d, n=10) #Non-grouped sample
#' sampleg<-aggregate(sample[,1], by=list(Category=sample[,2]), FUN=sum)
#' sampleg<-cbind(sampleg[,2], rep(10,10), sampleg[,1]) #Sample sample with grouped data
#'
#' #Cluster size to be taken for estimation
#' cluster.samplesize(N=50, data=sample, error=500, estimator="total", replace=TRUE)
#'
#' newsample<-cluster.sample(d, n=26) #New sample for estimation
#' sum(d[,1])
#' cluster.estimator(N=50, data=newsample, estimator="total", alpha=0.05, replace=TRUE)
#' cluster.estimator(N=50, data=sampleg, estimator="total", alpha=0.05)

cluster.samplesize<-function(N, data, error, alpha, estimator=c("total", "mean", "proportion", "class total"), replace=FALSE){
  estimator = match.arg(estimator)

  #Aceptance conditions
  if(estimator != "total" && estimator != "proportion" && estimator!="mean" && estimator!="class total") stop('Estimator must be one of c("total", "proportion", "mean", "class total").')
  #if( (estimator == "proportion" || estimator == "class total") && !all(data==0 | data==1)) stop('Data must be of values 0, 1 for proportion and class total estimation.')
  if(!missing(alpha) && (alpha<0 || alpha>1)) stop("Alpha value must range between 0 and 1.")


  grouped<-ifelse(ncol(data)==3, TRUE, FALSE)

  #Transforms data into manageable list structure
  if(!is(data, "list")){
    data<-as.data.frame(data)
    data[,1]<-as.numeric(data[,1])

    clase<-levels(as.factor(data[,ncol(data)])) #strata names

    domaindata<-list()  #separated strata
    for(i in clase){
      domaindata[[i]]<-data[which(data[,ncol(data)]==i),]
    }
    data<-domaindata
  }

  if(!grouped)  if( (estimator == "proportion" || estimator == "class total") && !all(sapply(data, all01list)))
    stop('Data must be of values 0, 1 for proportion and class total estimation.')


  #Size of the sample
  n<-length(clase)
  M<-ifelse(grouped, sum(sapply(data, function(data){return(data[,2])}))/n, sum(sapply(data, nrow)/n) )
  f<-n/N

  data<-lapply(data, function(data){return(data[,1])}) #data without strata column

  if(estimator=="total" || estimator=="class total"){
    return( ifelse(replace,
                   ifelse(missing(alpha) , N^2*M*varb(data, M, n)/error^2, qnorm(1-alpha/2)^2*N^2*M*varb(data, M, n)/error^2) ,
                   ifelse(missing(alpha) , N^2*M*(1-f)*varb(data, M, n)/error^2, qnorm(1-alpha/2)^2*N^2*M*(1-f)*varb(data, M, n)/error^2) ) )
  }
  else{
    return( ifelse(replace,
                   ifelse(missing(alpha) , varb(data, M, n)/(error^2*M), qnorm(1-alpha/2)^2*varb(data, M, n)/(error^2*M) ),
                   ifelse(missing(alpha) , (1-f)*varb(data, M, n)/(error^2*M), qnorm(1-alpha/2)^2*(1-f)*varb(data, M, n)/(error^2*M) ))  )
  }
}


#
# d<-cbind(rnorm(500, 50, 20), rep(c(1:50),10)) #datos sin agrupar
# sample<-cluster.sample(d, n=10) #muestra sin agrupar
# sampleg<-aggregate(sample[,1], by=list(Category=sample[,2]), FUN=sum)
# sampleg<-cbind(sampleg[,2], rep(10,10), sampleg[,1]) #misma muestra pero agrupada
#
#
# cluster.samplesize(N=50, data=sample, error=500, estimator="total", replace=TRUE)
#
#
# newsample<-cluster.sample(d, n=26) #muestra sin agrupar
# sum(d[,1])
# cluster.estimator(N=50, data=newsample, estimator="total", alpha=0.05, replace=TRUE)
# cluster.estimator(N=50, data=sampleg, estimator="total", alpha=0.05)
#
#
# cluster.samplesize(N=50, data=sample, error=1, estimator="mean")
# newsample<-cluster.sample(d, n=27) #muestra sin agrupar
# mean(d[,1])
# cluster.estimator(N=50, data=newsample, estimator="mean", alpha=0.05)
# cluster.estimator(N=50, data=sampleg, estimator="mean", alpha=0.05)
