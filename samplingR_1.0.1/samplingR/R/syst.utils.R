#' @title Intraclass correlation coefficient
#'
#' @param N Population size
#' @param n Sample size
#' @param data Population data
#'
#' @return Intraclass correlation
#' @details
#' This value serves as a comparison between systematic and simple random sampling precision.\cr
#' At value=1 the systematic precision is minimum. At value=0 both sampling methods precision
#' are equal. At value= \eqn{\frac{-1}{n-1}} systematic precision is maximum.\cr
#' Summarising at values between 1 and 0 simple random sampling estimation has more precision
#' than systematic, so method="srs" should be set at \code{\link{syst.estimator}}.
#' The other way method="syst" of interpenetrating samples method is better.
#'
#' @export
#'
#' @examples
#' data<-c(1,3,5,2,4,6,2,7,3)
#' syst.intracorr(9, 3, data)  #0.34375 example 1

syst.intracorr<-function(N, n, data){
  k<-N/n
  m<-mean(data)
  s<-0
  for(j in 1:k){
    #cat("\ncolumna", j,"\n")
    for(z in 1:(n-1)){
      #cat("\nz",z)
      for(i in 0:(z-1)){
        #cat("\n",data[j+i*k])
        #cat("\n",data[j+z*k])
        s<-s+sum((data[j+i*k]-m)*(data[j+z*k]-m))
      }
    }
  }
  return(2*s/((N-1)*(n-1)*var(data)))

}





#' @title Mean of a given row
#'
#' @param N Population size
#' @param n Sample size
#' @param data Population data
#' @param row Row for the mean calculation
#'
#' @return The desired mean for the data row
#' @details The data is supposed to be arranged as the as.matrix(byrow=T) function return.
#' The row is 0 indexed, meaning row can take values between 0 and n-1.
#' @noRd
#'
#' @examples
#' data<-c(1,3,5,2,4,6,2,7,3) #1,3,5; 2,4,6; 2,7,3
#' rowmean(9,3, data, 1) #seccond row mean = 4

rowmean<-function(N, n, data, row){
  k<-N/n
  #print(data[1+row*k+c(0:(k-1))])
  return(mean(data[1+row*k+c(0:(k-1))])) #row mean
  #print(m)
}





#' @title Intraclass quasivariance
#'
#' @param N Population size
#' @param n Sample size
#' @param data Population data
#'
#' @return Intraclass quasivariance
#' @noRd
#'
#' @examples
#' data<-c(1,3,5,2,4,6,2,7,3)
#' swst(9, 3, data)  #22.16667

swst<-function(N, n, data){
  k<-N/n
  s<-0
  for(i in 0:(n-1)){
    #print(data[1+i*k+c(0:(k-1))])
    m<-rowmean(N, n, data, i)
    #print(m)
    for(j in 1:k){
      s<-s+sum(data[j+i*k]-m)^2
    }
  }
  return(s/(N-n))
}





#' @title Stratified correlation coefficient
#'
#' @param N Population size
#' @param n Sample size
#' @param data Population data
#'
#' @return Correlation coefficient
#' @details
#' This value serves as a comparison between systematic and stratified sampling precision.\cr
#' At value=1 the systematic precision is minimum. At value=0 both sampling methods precision
#' are equal. At value= \eqn{\frac{-1}{n-1}} systematic precision is maximum.\cr
#' Summarising at values between 1 and 0 stratified sampling estimation has more precision
#' than systematic, so method="strata" should be set at \code{\link{syst.estimator}}.
#' The other way method="syst" of interpenetrating samples method is better.
#' @export
#'
#' @examples
#' data<-c(1,3,5,2,4,6,2,7,3)
#' syst.intercorr(9,3,data)  #0.09022556

syst.intercorr<-function(N, n, data){
  s<-0
  k<-N/n
  for(j in 1:k){
    #cat("\ncolumna", j,"\n")
    for(z in 1:(n-1)){
      #cat("\nz",z)
      for(i in 0:(z-1)){
        #cat("\n",data[j+i*k])
        #cat("\n",data[j+z*k])
        s<-s+sum((data[j+i*k]-rowmean(N,n,data,i))*(data[j+z*k]-rowmean(N,n,data,z)))
      }
    }
  }
  return(2*s/(n*(n-1)*(k-1)*swst(N,n,data)))
}


#' @title Systematic samples
#' @description Returns all possible systematic samples of size n
#' @param data Population data
#' @param n Sample size
#'
#' @return List with a sample per entrance
#' @export
#'
#' @examples
#' data<-c(1,3,5,2,4,6,2,7,3)
#' syst.all.samples(data, 3)

syst.all.samples<-function(data, n){
  N<-length(data)
  k<-N/n
  if(is(data, "vector")) data<-as.data.frame(data)
  samples<-list()
  for(i in 1:k){
    samples[[i]]<-data[i+c(0:(n-1))*k, ]
  }
  return(samples)
}



#' Analysis of variance of population data
#'
#' @param data Population data
#' @param n Sample size
#'
#' @return Summary
#' @export
#'
#' @examples
#' data<-c(1,3,5,2,4,6,2,7,3)
#' syst.anova(data,3)

syst.anova<-function(data, n){
  N<-length(data)
  k<-N/n
  samples<-syst.all.samples(data, n)
  gl<-c(k-1, N-k, N-1)
  ss<-c(k*sum((sapply(samples,mean)-mean(data))^2), sum((data-sapply(samples,mean))^2), sum((data-mean(data))^2))
  sbs<-ss[1]/gl[1]
  sws<-ss[2]/gl[2]
  sm<-c(sbs, sws, (sbs*gl[1]+sws*gl[2])/gl[3])
  table<-cbind(gl, ss, sm)
  colnames(table)<-c("Liberty degrees", "Sum of squares", "Sum of means")
  rownames(table)<-c("Between samples", "Intra samples", "Total")
  return(table)
}
