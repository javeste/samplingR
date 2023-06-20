#' @title Cluster sample for non-grouped data.
#' @description Retrieves a sample of clusters for data which interest variable values
#' are not grouped by cluster.\cr
#'
#' @param data Matrix or data.frame containing the population data in first column and the cluster it belongs to in the last column.
#' @param n Number of clusters of the returning sample.
#' @param replace Whether the sample to be taken can have repeated clusters or not.
#'
#' @return Data frame of a clustered sample
#' @export
#'
#' @details #' If your data is grouped by cluster use \code{\link{srs.sample}} function to retrieve
#' your sample. Remember grouped by cluster data must have interest variable data in the first column,
#' cluster size in the second and the cluster name in the last column. Interest
#' values of grouped data must reflect the total value of each cluster.
#' @examples
#' data<-cbind(rnorm(500, 50, 20), rep(c(1:50),10))
#' sample<-cluster.sample(data, 10);sample
#'

cluster.sample<-function(data, n, replace=FALSE){
  data<-as.data.frame(data)
  data[,1]<-as.numeric(data[,1])

  clase<-levels(as.factor(data[,ncol(data)])) #strata names
  N<-length(clase)

  if(N<n)stop("Sample size must be smaller than population size")

  domaindata<-list()  #separated strata
  for(i in clase){
    domaindata[[i]]<-data[which(data[,ncol(data)]==i),]
  }
  clusters<-srs.sample(N, n, replace, clase)
  return(dplyr::bind_rows(domaindata[clusters]))
}



#data<-cbind(rnorm(500, 50, 20), rep(c(1:50),10))
#sample<-cluster.sample(data, 10);sample
