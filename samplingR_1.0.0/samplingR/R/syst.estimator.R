#' @title Parameter estimation on a systematic sample
#'
#' @param N Population size
#' @param sample Vector containing the systematic sample
#' @param estimator Estimator to compute. Can be one of "total", "mean", "proportion", "class total". Default is "total".
#' @param method Method of variance estimation. Can be one of "srs", "strata", "syst".
#' @param alpha Optional value to calculate estimation error and build 1-alpha confidence interval.
#' @param t Number of systematic samples to take with interpenetrating samples method.
#' @param data Population data.
#'
#' @details Variance estimation has no direct formula in systematic sampling, thus estimation method must be done. Refer to \code{\link{syst.intracorr}}
#' and \code{\link{syst.intercorr}} functions details for more information. \cr
#' "syst" method uses interpenetrating samples method in which t systematic samples of size=\eqn{\frac{n}{t}} are taken to estimate. \eqn{\frac{n}{t}} must be even.\cr
#' By choosing the start at random for all the samples they can be considered random taken. With this method population data and t must be given.
#'
#'
#' @return A list containing different interest values:
#'  \itemize{
#'    \item estimator
#'    \item variance
#'    \item sampling.error
#'    \item estimation.error
#'    \item confint
#'  }
#' @export
#'
#' @examples
#' data<-c(1,3,5,2,4,6,2,7,3)
#' sample<-syst.sample(9, 3, data)
#' syst.estimator(N=9, sample, "mean", "srs", 0.05)

syst.estimator<-function(N, sample, estimator=c("total", "mean", "proportion", "class total"), method=c("srs", "strata", "syst"), alpha, data, t){
  estimator<-match.arg(estimator)
  if(estimator != "total" && estimator!="mean" && estimator!="proportion" && estimator!="class total")
    stop('Estimator must be one of c("total", "mean", "proportion", "class total").')
  method=match.arg(method)
  if(method=="syst" && missing(t))stop("t must be provided for interpenetrating samples method estimation")

  n<-length(sample)

  if(method=="srs"){
    return(srs.estimator(N=N, data=sample, estimator=estimator, alpha=alpha))
  }
  else if(method=="strata"){
    #solve odd sample size problem
    if(n%%2!=0){
      sample[n+1]<-sample[srs.sample(n,1)]
      n<-n+1
    }

    k<-N/n
    sample<-data.frame(data=sample, strata=rep(c(1:(n/2)), each=2))
    return(strata.estimator(N=N, Nh=rep(2*k, n/2), data=sample, estimator=estimator, alpha=alpha))
  }
  else{
    if(n%%t!=0)stop("t value does not provide an even sample size")
    #Retrieve list of subsamples
    samples<-list()
    for(i in 1:t){
      aux<-syst.sample(N, n/t, data=data)
      samples[[i]]<-aux
    }
    if(estimator=="total" || estimator=="class total"){
      estimator<-sum(N*sapply(samples, mean))/t
      var<-sum((N*sapply(samples, mean))^2-estimator^2)/(t*(t-1))
      serror<-sqrt(var)
    }
    else if(estimator=="mean" || estimator=="proportion"){
      estimator<-sum(sapply(samples, mean))/t
      var<-sum(sapply(samples, mean)^2-estimator^2)/(t*(t-1))
      serror<-sqrt(var)
    }
    if(!missing(alpha)){
      esterror<-qnorm(1-alpha/2)*serror
      confint<-c(estimator-esterror, estimator+esterror)
      return(list("estimator" = estimator, "variance" = var, "sampling.error" = serror,
                  "estimation.error" = esterror, "confint" = confint))
    }

    return(list("estimator" = estimator, "variance" = var, "sampling.error" = serror))
  }

}

#data<-rnorm(1000,5,2)
#sum(data)
#sample<-syst.sample(1000, 100, data)
#syst.estimator(N=1000, sample, "total", "syst", 0.05, data=data, t=20)


#Ejemplo 1
#Repetir para cada posible muestra
#data<-c(1,3,5,2,4,6,2,7,3)
#syst.anova(data,3)
#sample<-syst.sample(9, 3, data);sample
#syst.intracorr(9,3,data) #0.34375
#syst.intercorr(9,3,data)   #0.4
#syst.estimator(N=9, sample, "mean", "srs", 0.05) #$variance c(1,2,2)=0.07407407 c(3,4,7)=0.962963 c(5,6,3)=0.5185185

#Ejemplo 2
#data<-c(1,3,5,2,4,6,2,7)
#sample<-syst.sample(8, 4, data);sample
#syst.intracorr(8,4,data) #-0.1428571
#syst.intercorr(8,4,data)   #-0.04761905
#syst.estimator(N=8, sample, "mean", "srs", 0.05) #$variance c(1,5,4,2)=0.4166667 c(3,2,6,7)=0.7083333
#syst.estimator(N=8, sample, "mean", "strata", 0.05) #$variance c(1,5,4,2)=0.625 c(3,2,6,7)=0.0625
