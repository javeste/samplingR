#'@title Simple Random Sample size.
#'@description Calculates the required sample size in order to achieve a relative
#'or absolute sampling error  less or equal to the specified for an specific
#'estimator and an optional confidence interval in simple random sampling.
#'
#'@param N Number of instances of the data set.
#'@param var Estimated quasivariance.
#'@param error Sampling error
#'@param alpha Significance level to obtain confidence intervals.
#'@param estimator One of "total", "proportion", "mean", "class total". Default is "total"
#'@param p Estimated proportion. If estimator is not "proportion" or "class total" it will be ignored.
#'@param mean Estimated mean. If relative=FALSE it will be ignored.
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@param relative Whether the specified error is relative or not.
#'@return Number of instances of the sample to be taken.
#'
#'@details If the sample size result is not a whole number the number returned is
#'the next whole number so srs.samplesize>=n is satisfied.\cr
#'To estimate sample size of estimators "total" and "mean" estimated quasivariance
#'must be provided. If the error is relative then estimated mean must also be provided.\cr
#'To estimate sample size of estimator "proportion" and "class total" estimated
#'proportion must be provided. If p is not specified sample size will be estimated
#' based on worst-case scenario of p=0.5.\cr
#'N must be always be provided for calculations.
#'
#'@examples
#' data<-rnorm(200, 100, 20)
#' n<-srs.samplesize(200, var(data), estimator="total", error=400, alpha=0.05);n
#' sample<-data[srs.sample(200, n)]
#' srs.estimator(200, sample, "total", alpha=0.05)$sampling.error
#'
#'
#'@export

srs.samplesize<-function(N, var, error, alpha, estimator=c("total", "mean", "proportion", "class total"), p, mean, replace=FALSE, relative=FALSE){

  estimator=match.arg(estimator)

  #Aceptance conditions
  if(!missing(alpha) && (alpha<0 || alpha>1)) stop("Alpha value must range between 0 and 1.")
  if(estimator != "total" && estimator != "proportion" && estimator!="mean" && estimator!="class total") stop('Estimator must be one of c("total", "proportion", "mean", "class total").')
  #p parameter control
  if((estimator=="proportion" || estimator=="class total") && missing(p)){warning("Necessary p argument missing, will be set to worst case scenario value of 0.5"); p<-0.5}
  if(!missing(p) && (p<0 || p>1)) stop("p value must range between 0 and 1.")
  if(relative && (error<0 || error>1))stop("Relative error must range between 0 and 1")
  if(relative && (estimator=="total" || estimator=="mean") && missing(mean)) stop("For relative error estimated mean must be declared.")

  #SRS without replacement
  if(!replace){
    #Absolute error
    if(!relative){
      #n estimation with sampling error only
      if(missing(alpha)){
        if(estimator=="total"){
          return(ceiling(N*N*var/(error*error+N*var)))
        }
        else if(estimator=="proportion"){
          return(ceiling(N*p*(1-p)/(error*error*(N-1)+p*(1-p))))
        }
        else if(estimator=="mean"){
          return(ceiling(N*var/(N*error*error+var)))
        }
        else if(estimator=="class total"){
          return(ceiling(N*N*N*p*(1-p)/(error*error*(N-1)+N*N*p*(1-p))))
        }
      }
      #n estimation with sampling error and alpha
      else{
        k<-qnorm(1-alpha/2)

        if(estimator=="total"){
          n0<-k*k*var/(error*error)
          return(ceiling(N*N*n0/(1+N*n0)))
        }
        else if(estimator=="proportion"){
          n0<-k*k*p*(1-p)*(N/(N-1))/(error*error)
          return(ceiling((n0*N)/(N+n0)))
        }
        else if(estimator=="mean"){
          n0<-k*k*var/(error*error)
          return(ceiling(n0*N/(N+n0)))
        }
        else if(estimator=="class total"){
          return(ceiling(k*k*N*N*N*p*(1-p)/(error*error*(N-1)+k*k*N*N*p*(1-p))))
        }
      }
    }
    #Relative error
    else{

      if(missing(alpha)){
        if(estimator=="total" || estimator=="mean"){  #conclusiones pg 178
          c2<-var/(mean*mean)
          return(ceiling(N*c2/(N*error*error+c2)))
        }
        else{
          c2<-N*(1-p)/(p*(N-1))
          return(ceiling(N*c2/(N*error*error+c2)))
        }
      }
      else{
        k<-qnorm(1-alpha/2)
        if(estimator=="total" || estimator=="mean"){
          c2<-var/(mean*mean)
          return(ceiling(N*k*k*c2/(N*error*error+k*k*c2)))
        }
        else{
          c2<-N*(1-p)/(p*(N-1))
          return(ceiling(k*k*c2/(error*error+k*k*c2/N)))
        }
      }
    }
  }
  #SRS with replacement
  else{

    if(!relative){
      #n estimation with sampling error only
      if(missing(alpha)){
        if(estimator=="total"){
          var<-var*(N-1)/N
          return(ceiling(var*N*N/(error*error)))
        }
        else if(estimator=="proportion"){
          return(ceiling(p*(1-p)/(error*error)))
        }
        else if(estimator=="mean"){
          var<-var*(N-1)/N
          return(ceiling(var/(error*error)))
        }
        else if(estimator=="class total"){
          return(ceiling(N*N*p*(1-p)/(error*error)))
        }
      }
      #n estimation with sampling error and alpha
      else{
        k<-qnorm(1-alpha/2)           #lambda_alpha

        if(estimator=="total"){
          var<-var*(N-1)/N
          return(ceiling(k*k*var*N*N/(error*error)))
        }
        else if(estimator=="proportion"){
          return(ceiling(k*k*p*(1-p)/(error*error)))
        }
        else if(estimator=="mean"){
          var<-var*(N-1)/N
          return(ceiling(k*k*var/(error*error)))
        }
        else if(estimator=="class total"){
          return(ceiling(N*N*k*k*p*(1-p)/(error*error)))
        }
      }
    }
    else{

      #n estimation with sampling error only
      if(missing(alpha)){
        if(estimator=="total" || estimator=="mean"){
          c2<-var/(mean*mean)
          return(ceiling(c2/(error*error)))
        }
        else{
          return(ceiling((1-p)/(p*error*error)))
        }

      }
      #n estimation with sampling error and alpha
      else{
        k<-qnorm(1-alpha/2)           #lambda_alpha

        if(estimator=="total" || estimator=="mean"){
          c2<-var/(mean*mean)
          return(ceiling(k*k*c2/(error*error)))
        }
        else{
          return(ceiling(k*k*(1-p)/(p*error*error)))
        }
      }
    }
  }
}

# data<-rnorm(200, 100, 20)
# tau<-sum(data);tau
# mu<-mean(data);mu
# #alpha not declared
# n<-srs.samplesize(200, var(data), estimator="total", error=400);n
# sample<-data[srs.sample(200, n)]
# srs.estimator(200, sample, "total", alpha=0.05)
#
# #Alpha declared (wrong)
# n<-srs.samplesize(200, var(data), estimator="total", error=400, alpha=0.05);n
# sample<-data[srs.sample(200, n)]
# srs.estimator(200, sample, "total", alpha=0.05)
