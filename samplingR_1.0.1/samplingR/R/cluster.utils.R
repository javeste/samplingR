#' Title
#'
#' @param data List of data of a clustered sample
#' @param M Estimated cluster size
#' @param n Number of clusters in the sample
#' @details
#' Each entrance of the list only contains the interest values for the cluster.
#' The cluster name is the name of the list entrance
#'
#' @return Estimated between clusters variance
#' @noRd
#'

varb<-function(data, M, n){
  estmean<-1/n*sum(1/M*sapply(data,sum))
  return(1/(n-1)*sum(M*(sapply(data, sum)/M - estmean)^2 ) )
}
