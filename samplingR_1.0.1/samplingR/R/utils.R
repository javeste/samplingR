#'Checks all data is of 0s and 1s one the first column.
#'@param data Data with multiple columns.
#'@noRd
all01list<-function(data){
  return(all(data[,1]==0 | data[,1]==1))
}

#'Returns the variance estimator of the total for simple random sample data
#'@param N Population size
#'@param n sample size
#'@param data Sample
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@importFrom stats var
#'@noRd
totalvarestimator<-function(N, n, data, replace=FALSE){
  f<-n/N
  if(!replace)return(N*N*(1-f)*var(data)/n)
  else return(N*N*var(data)/n)
}

#'Returns the variance estimator of the mean for simple random sample data
#'@param N Population size
#'@param n sample size
#'@param data Sample
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@importFrom stats var
#'@noRd
meanvarestimator<-function(N, n, data, replace=FALSE){
  f<-n/N
  if(!replace)return((1-f)*var(data)/n)
  else return(var(data)/n)
}

#'Returns the variance estimator of the proportion for simple random sample data
#'@param N Population size
#'@param n sample size
#'@param estimator Estimated proportion
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@noRd
pvarestimator<-function(N, n, estimator, replace=FALSE){
  f<-n/N
  if(!replace)return((1-f)*estimator*(1-estimator)/(n-1))
  else return(estimator*(1-estimator)/(n-1))
}

#'Returns the variance estimator of the class total for simple random sample data
#'@param N Population size
#'@param n sample size
#'@param estimator Estimated proportion
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@noRd
ctvarestimator<-function(N, n, estimator, replace=FALSE){
  f<-n/N
  if(!replace)return(N*N*(1-f)*estimator*(1-estimator)/(n-1))
  else return(N*N*estimator*(1-estimator)/(n-1))
}

