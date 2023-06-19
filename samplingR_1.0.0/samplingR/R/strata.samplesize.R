#' @title Sample size estimation on stratified sampling
#' @description Calculates the required sample size in order to achieve an
#' absolute sampling error  less or equal to the specified for an specific
#' estimator and an optional confidence interval in stratified sampling.
#'
#' @param Nh Vector of population strata sizes.
#' @param var Vector of estimated strata variances.
#' @param error Sampling error.
#' @param alpha Significance level to obtain confidence intervals.
#' @param estimator The estimator to be estimated. Default is "total".
#' @param alloc The allocation to be used when taking samples. Default is "prop".
#' @param ch Vector of cost per strata to select an individual for the sample.
#' @param p Estimated population proportion. If estimator is not "proportion" or "class total" it will be ignored.
#' @param mean Estimated population mean. If relative=FALSE it will be ignored.
#' @param replace Whether the samples to be taken can have repeated instances or not.
#' @param relative Whether the specified error is relative or not.
#'
#' @return Number of instances of the sample to be taken.
#'
#' @details With "proportion" and "class total" estimators variance vector must
#' contain \code{\link{var}} return values equal to \eqn{\frac{Nh}{(Nh-1)}p*(1-p)} values.
#' @export
#'
#' @examples
#' strata.samplesize(c(120,100,110,50), c(458, 313,407,364), error=5, alpha=0.05, "mean", "prop")
#'
strata.samplesize<-function(Nh, var, error, alpha, estimator=c("total", "mean", "proportion", "class total"), alloc=c("prop", "min", "optim"), ch, p, mean, replace=FALSE, relative=FALSE){
  estimator=match.arg(estimator)
  alloc=match.arg(alloc)

  #Aceptance conditions
  if(!missing(alpha) && (alpha<0 || alpha>1)) stop("Alpha value must range between 0 and 1.")
  if(estimator != "total" && estimator != "proportion" && estimator!="mean" &&
     estimator!="class total") stop('Estimator must be one of c("none", "total", "proportion", "mean", "class total").')
  if(alloc != "prop" && alloc != "min" && alloc != "optim") stop('Alloc must be one of c("prop", "min", "optim")')

  if(length(var) != length(Nh)) stop("Strata size lenght must be equal to strata variance length")
  if(alloc=="optim" && missing(ch)) stop('Strata cost values must be given for alloc="optim"')
  if(alloc=="optim" && (length(ch) != length(Nh))) stop("Strata costs lenght must be equal to strata size length")

  if((estimator=="proportion" || estimator=="class total") && missing(var)){
    warning("\nNecessary var argument missing, will be set to worst case scenario value for each strata.\n")
    var<-c(Nh/(Nh-1)*0.25)
  }
  if(relative && (estimator=="proportion" || estimator=="class total") && missing(p)) {warning("\nNecessary p argument missing, will be set to worst case scenario value of 0.5\n"); p<-0.5}
  if(!missing(p) && (p<0 || p>1) )stop("Proportion value must range between 0 and 1")

  if(relative && (error<0 || error>1))stop("Relative error must range between 0 and 1")
  if(relative && missing(mean))stop("Mean must be provided to estimate sample size with relative error.")

  var<-as.array(var)
  #Population strata sizes
  Nh<-as.array(Nh)
  #Population size
  N<-sum(Nh)
  #Relative strata size coeficients
  Wh<-Nh/N


  #stratified without replacement
  if(!replace){
    #absolute error
    if(!relative){
      if(missing(alpha)){

        if(alloc == "prop"){
          if(estimator == "mean" || estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/(error*error+a/N)) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a) / (error*error+a)) )
          }
        }

        else if(alloc == "min"){
          if(estimator == "mean" || estimator == "proportion"){
            b<-sum(Wh*sqrt(var))^2
            a<-sum(Wh*var)
            return( ceiling(b/(error^2+a/N)))
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            a<-sum(Nh*var)
            return( ceiling(b/(error^2+a)))
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / (error^2+sum(Wh*var)/N) )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / (error^2+sum(Nh*var)) )
          }
        }
      }
      #Confidence interval
      else{
        k<-qnorm(1-alpha/2)
        if(alloc == "prop"){
          if(estimator == "mean" || estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/((error^2/k^2)+a/N)) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a) / ((error^2/k^2)+a) ) )
          }
        }
        #minimum variance allocation
        else if(alloc=="min"){
          if(estimator == "mean" || estimator == "proportion"){
            b<-sum(Wh*sqrt(var))^2
            a<-sum(Wh*var)
            return( ceiling(b/( (error^2/k^2)+a/N)) )
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            a<-sum(Nh*var)
            return( ceiling(b/ ((error^2/k^2)+a)) )
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / ((error^2/k^2)+(sum(Wh*var)/N)) )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / ((error^2/k^2)+sum(Wh*var)) )
          }
        }
      }
    }
    #relative error
    else{
      if(missing(alpha)){

        if(alloc == "prop"){
          if(estimator == "mean"){
            a<-sum(Wh*var)
            return( ceiling(a/(error^2*mean^2+a/N)) )
          }
          else if(estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/(p^2*error^2+a/N)) )
          }
          else if(estimator == "total"){
            a<-sum(Nh*var)
            return( ceiling( (N*a) / (N^2*mean^2*error^2+a)) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a) / (N^2*p^2*error^2+a)) )
          }
        }

        else if(alloc == "min"){
          if(estimator == "mean"){
            b<-sum(Wh*sqrt(var))^2
            a<-sum(Wh*var)
            return( ceiling(b/(mean^2*error^2+a/N)))
          }
          else if(estimator=="proportion"){
            b<-sum(Wh*sqrt(var))^2
            a<-sum(Wh*var)
            return( ceiling(b/(p^2*error^2+a/N)))
          }
          else if(estimator=="total"){
            b<-sum(Nh*sqrt(var))^2
            a<-sum(Nh*var)
            return( ceiling(b/(N^2*mean^2*error^2+a)))
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            a<-sum(Nh*var)
            return( ceiling(b/(N^2*p^2*error^2+a)))
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / (mean^2*error^2+sum(Wh*var)/N) )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / (mean^2*error^2+sum(Nh*var)) )
          }
        }
      }
      #Confidence interval
      else{
        k<-qnorm(1-alpha/2)
        if(alloc == "prop"){
          if(estimator == "mean"){
            a<-sum(Wh*var)
            return( ceiling(a/((mean^2*error^2/k^2)+a/N)) )
          }
          else if(estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/((p^2*error^2/k^2)+a/N)) )
          }
          else if(estimator == "total"){
            a<-sum(Nh*var)
            return( ceiling( (N*a) / ((N^2*mean^2*error^2/k^2) +a) ) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a) / ((N^2*p^2*error^2/k^2)+a) ) )
          }
        }
        #minimum variance allocation
        else if(alloc=="min"){
          if(estimator == "mean"){
            b<-sum(Wh*sqrt(var))^2
            a<-sum(Wh*var)
            return( ceiling(b/( (mean^2*error^2/k^2)+a/N)) )
          }
          else if(estimator=="proportion"){
            b<-sum(Wh*sqrt(var))^2
            a<-sum(Wh*var)
            return( ceiling(b/( (p^2*error^2/k^2)+a/N)) )
          }
          else if(estimator=="total"){
            b<-sum(Nh*sqrt(var))^2
            a<-sum(Nh*var)
            return( ceiling(b/ ((N^2*mean^2*error^2/k^2)+a)) )
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            a<-sum(Nh*var)
            return( ceiling(b/ ((N^2*p^2*error^2/k^2)+a)) )
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / ((mean^2*error^2/k^2)+(sum(Wh*var)/N)) )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / ((mean^2*error^2/k^2)+sum(Nh*var)) )
          }
        }
      }
    }
  }


  #stratified with replacement
  else{
    var<-(Nh-1)/Nh*var #var<-p*q
    #absolute error
    if(!relative){
      if(missing(alpha)){
        if(alloc == "prop"){
          if(estimator == "mean" || estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/(error^2)) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a)/(error^2)) )
          }
        }
        #minimum variance allocation
        else if(alloc=="min"){
          if(estimator == "mean" || estimator == "proportion"){
            b<-sum(Wh*sqrt(var))^2
            return( ceiling(b/(error^2)) )
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            return( ceiling(b/(error^2) ) )
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / error^2 )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / error^2 )
          }
        }
      }
      #Confidence interval
      else{
        k<-qnorm(1-alpha/2)
        if(alloc=="prop"){
          if(estimator == "mean" || estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/(error^2/k^2)) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a) / (error^2/k^2)) )
          }
        }
        else if(alloc=="min"){

          if(estimator == "mean" || estimator == "proportion"){
            b<-sum(Wh*sqrt(var))^2
            return( ceiling(b/(error^2/k^2)) )
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            return( ceiling(b/(error^2/k^2) ) )
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / (error^2/k^2) )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / (error^2/k^2) )
          }
        }

      }
    }
    #relative error
    else{
      if(missing(alpha)){
        if(alloc == "prop"){
          if(estimator == "mean"){
            a<-sum(Wh*var)
            return( ceiling(a/(mean^2*error^2)) )
          }
          else if(estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/(p^2*error^2)) )
          }
          else if(estimator == "total"){
            a<-sum(Nh*var)
            return( ceiling( (N*a) / (N*mean^2*error^2)) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a)/(N*p^2*error^2)) )
          }
        }
        #minimum variance allocation
        else if(alloc=="min"){
          if(estimator == "mean"){
            b<-sum(Wh*sqrt(var))^2
            return( ceiling(b/(mean^2*error^2)) )
          }
          else if(estimator=="proportion"){
            b<-sum(Wh*sqrt(var))^2
            return( ceiling(b/(p^2*error^2)) )
          }
          else if(estimator=="total"){
            b<-sum(Nh*sqrt(var))^2
            return( ceiling(b/(N^2*mean^2*error^2) ) )
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            return( ceiling(b/(N^2*p^2*error^2) ) )
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / (mean^2*error^2) )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / (mean^2*error^2) )
          }
        }
      }
      else{
        k<-qnorm(1-alpha/2)
        if(alloc=="prop"){
          if(estimator == "mean"){
            a<-sum(Wh*var)
            return( ceiling(a/(mean^2*error^2/k^2)) )
          }
          else if(estimator == "proportion"){
            a<-sum(Wh*var)
            return( ceiling(a/(p^2*error^2/k^2)) )
          }
          else if(estimator == "total"){
            a<-sum(Nh*var)
            return( ceiling( (N*a) / (N*mean^2*error^2/k^2)) )
          }
          else{
            a<-sum(Nh*var)
            return( ceiling( (N*a) / (N*p^2*error^2/k^2)) )
          }
        }
        else if(alloc=="min"){

          if(estimator == "mean"){
            b<-sum(Wh*sqrt(var))^2
            return( ceiling(b/(mean^2*error^2/k^2)) )
          }
          else if(estimator=="proportion"){
            b<-sum(Wh*sqrt(var))^2
            return( ceiling(b/(p^2*error^2/k^2)) )
          }
          else if(estimator=="total"){
            b<-sum(Nh*sqrt(var))^2
            return( ceiling(b/(N^2*mean^2*error^2/k^2) ) )
          }
          else{
            b<-sum(Nh*sqrt(var))^2
            return( ceiling(b/(N^2*p^2*error^2/k^2) ) )
          }
        }
        else{
          if(estimator == "mean" || estimator == "proportion"){
            return( (sum(Wh*sqrt(var)/sqrt(ch))*sum(Wh*sqrt(var)*ch) ) / (mean^2*error^2/k^2) )
          }
          else{
            return( (sum(Nh*sqrt(var)/sqrt(ch))*sum(Nh*sqrt(var)*ch) ) / (mean^2*error^2/k^2) )
          }
        }

      }
    }
  }


}

# Nh=rep(125,4)
# var=c(458.3932,313.914,407.8838,364.1714)
# strata.samplesize(Nh, var, 5, estimator="mean", alloc="prop", alpha=0.05)




