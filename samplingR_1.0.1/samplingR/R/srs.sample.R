#'@title Simple Random Sample
#'@description With this function you receive a simple random sample consisting
#'on a list of the instances index
#'
#'@param N Number of instances of the data set.
#'@param n Number of instances of the returning sample.
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@param data Optional matrix or data.frame containing the population data.
#'If specified an object of same class as data will be returned with sample instances.
#'@return List of size n with numbers from 1 to N indicating the index of the
#'data set's instances to be taken.
#'
#'@importFrom stats runif
#'@examples
#' srs.sample(10,3)
#'
#'
#' data<-matrix(data=c(1:24), nrow=8)
#' N<-dim(data)[1]
#' sample<-srs.sample(N, 3, data = data)
#' sample
#'
#'@export


srs.sample <- function(N, n, replace=FALSE, data){
  if(N<n) stop("Sample size must be bigger than population size")
  if(!missing(data) && is(data, "vector")) data<-as.data.frame(data)
  if(!missing(data) && !is.data.frame(data) && !is.matrix(data)) stop("Data must be of class matrix or data.frame")

  if(replace){
    list<-floor(runif(n, 1, N+1))
    if(missing(data)){return(sort(list))}
    else{ return(data[sort(list),])}
  }
  else{
    index<-c(1:N)
    list<-runif(N, 0, 1)
    if(missing(data)){ return(sort(index[order(list)][1:n]))}
    else{ return(data[sort(index[order(list)][1:n]),])}
  }

}





# datos<-c(0,1,2,3,4,5,6)
# N<-length(datos)
# srs.sample(N,3)
# muestra<-datos[srs.sample(N, 3, replace=F)]
# muestra
#
# muestraR<-datos[srs.sample(N, 3, replace=T)]
# muestraR
#
# datos2<-data.frame(rbind(c(1,2,3),
#                          c(4,5,6),
#                          c(6,7,8),
#                          c(9,10,11)))
# names(datos2)=c("A","B","C")
# datos2
# muestra2<-datos2[srs.sample(dim(datos2)[1], 3),]
# muestra2
#
# datos2<-matrix(data=c(1:24), nrow=8)
# datos2
# srs.sample(dim(datos2)[1], 2, data=datos2)
