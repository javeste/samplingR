#' @title Parameter estimation for cluster samples
#' @description Estimates parameters with optional confidence interval for clustered data
#' of similar cluster size.
#' @param N Number of clusters for the population
#' @param data Cluster sample
#' @param estimator Estimator to compute. Can be one of "total", "mean", "proportion", "class total". Default is "total".
#' @param replace Whether the sample to be taken can have repeated instances or not.
#' @param alpha Optional value to calculate estimation error and build 1-alpha
#'
#' @return A list containing different interest values:
#'  \itemize{
#'  \item estimator
#'  \item variance
#'  \item sampling.error
#'  \item estimation.error
#'  \item confint
#' }
#' @details This function admits both grouped and non-grouped by cluster data.\cr
#' Non-grouped data must have interest variable data in the first column and cluster
#' name each individual belongs to in the last column.\cr
#' Grouped by cluster data must have interest variable data in the first column,
#' cluster size in the second and the cluster name in the last column. Interest
#' values of grouped data must reflect the total value of each cluster.

#' @export
#'
#' @examples
#' d<-cbind(rnorm(500, 50, 20), rep(c(1:50),10)) #Non-grouped data
#' sample<-cluster.sample(d, n=10) #Non-grouped sample
#' sampleg<-aggregate(sample[,1], by=list(Category=sample[,2]), FUN=sum)
#' sampleg<-cbind(sampleg[,2], rep(10,10), sampleg[,1]) #Same sample but with grouped data
#' sum(d[,1])
#' cluster.estimator(N=50, data=sample, estimator="total", alpha=0.05)
#' cluster.estimator(N=50, data=sampleg, estimator="total", alpha=0.05)

cluster.estimator<-function(N, data, estimator=c("total", "mean", "proportion", "class total"), replace=FALSE, alpha){

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
  #no replacement estimation
  if(!replace){
    if (estimator == "total" || estimator=="class total"){
      estimator<-N/n*sum(sapply(data, sum))
      var<-N^2*M^2*(1-f)*varb(data, M, n)/(n*M)
      serror<-sqrt(var)
    }
    else if(estimator == "mean" || estimator == "proportion"){
      estimator<-1/n*sum(sapply(data,sum)/M)
      var<-(1-f)*varb(data, M, n)/(n*M)
      serror<-sqrt(var)
    }
  }
  else{
    if (estimator == "total" || estimator=="class total"){
      estimator<-N/n*sum(sapply(data, sum))
      var<-N^2*M^2*varb(data, M, n)/(n*M)
      serror<-sqrt(var)

    }
    else if(estimator == "mean" || estimator == "proportion"){
      estimator<-1/n*sum(sapply(data,sum)/M)
      var<-varb(data, M, n)/(n*M)
      serror<-sqrt(var)
    }
  }
  if(!missing(alpha)){
    esterror<-qnorm(1-alpha/2)*serror
    confint<-c(estimator-esterror, estimator+esterror)
    return(list("estimator" = estimator, "variance" = var, "sampling.error" = serror,
                "estimation.error" = esterror, "confint" = confint))
  }

  return(list("estimator" = estimator, "variance" = var, "sampling.error" = serror))
}




# d<-cbind(rnorm(500, 50, 20), rep(c(1:50),10)) #datos sin agrupar
# sample<-cluster.sample(d, n=10) #muestra sin agrupar
# sampleg<-aggregate(sample[,1], by=list(Category=sample[,2]), FUN=sum)
# sampleg<-cbind(sampleg[,2], rep(10,10), sampleg[,1]) #misma muestra pero agrupada
#
# sum(d[,1])
# cluster.estimator(N=50, data=sample, estimator="total", alpha=0.05)
# cluster.estimator(N=50, data=sampleg, estimator="total", alpha=0.05)
#
# mean(d[,1])
# cluster.estimator(N=50, data=sample, estimator="mean", alpha=0.05)
# cluster.estimator(N=50, data=sampleg, estimator="mean", alpha=0.05)
#
# #proportion
# d01<-cbind(sample(c(0,1), size=500, replace=T), rep(c(1:50),10))
# sample01<-cluster.sample(d01, n=10);sample01
# sample01g<-aggregate(sample01[,1], by=list(Category=sample01[,2]), FUN=sum)
# sample01g<-cbind(sample01g[,2], rep(10,10), sample01g[,1]) #misma muestra pero agrupada
#
# mean(d01[,1])
# cluster.estimator(N=50, data=sample01, estimator="proportion", alpha=0.05)
# cluster.estimator(N=50, data=sample01g, estimator="proportion", alpha=0.05)
# #class total
# sum(d01[,1])
# cluster.estimator(N=50, data=sample01, estimator="class total", alpha=0.05)
# cluster.estimator(N=50, data=sample01g, estimator="class total", alpha=0.05)
