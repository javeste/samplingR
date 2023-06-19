#'@title Simple Ramdom Sampling parameter estimation.
#'@description Function to make estimations of diferent parameters based on
#'a Simple Random Sample.
#'
#'
#'@param N Number of instances of the data set.
#'@param data Sample of the data. It must only contain a single column of the
#'data to estimate.
#'@param estimator Estimator to compute. Can be one of "total", "mean", "proportion", "class total". Default is "total".
#'@param replace Whether the sample has been taken with replacement or not.
#'@param alpha Optional value to calculate estimation error and build 1-alpha
#'confidence interval.
#'
#'
#'@return A list containing different interest values:
#' \itemize{
#' \item estimator
#' \item variance
#' \item sampling.error
#' \item estimation.error
#' \item confint
#'}

#'@importFrom stats qnorm
#'
#'@examples
#'data<-rnorm(200, 100, 20)
#'sample<-data[srs.sample(200, 50)]
#'tau<-sum(data);tau
#'srs.estimator(200, sample, "total", alpha=0.05)
#'
#'
#'mu<-mean(data);mu
#'srs.estimator(200, sample, "mean", alpha=0.05)
#'@export


srs.estimator<-function(N, data, estimator=c("total", "mean", "proportion", "class total"), replace=FALSE, alpha){

  estimator = match.arg(estimator)

  #Aceptance conditions
  if(estimator != "total" && estimator != "proportion" && estimator!="mean" && estimator!="class total") stop('Estimator must be one of c("total", "proportion", "mean", "class total").')
  if( (estimator == "proportion" || estimator == "class total") && !all(data==0 | data==1)) stop('Data must be of values 0, 1 for proportion and class total estimation.')
  if(!missing(alpha) && (alpha<0 || alpha>1)) stop("Alpha value must range between 0 and 1.")

  #Size of the sample
  n<-length(data)

  #SRS estimation
  if(!replace){
    f<-n/N
    if (estimator == "total"){
      estimator<-N*sum(data/n)
      var<-N*N*(1-f)*var(data)/n
      serror<-sqrt(var)
    }
    else if(estimator == "mean"){
      estimator<-sum(data/n)
      var<-(1-f)*var(data)/n
      serror<-sqrt(var)
    }
    else if(estimator == "proportion"){
      estimator<-sum(data/n)
      var<-(1-f)*estimator*(1-estimator)/(n-1)
      serror<-sqrt(var)
    }
    else{
      estimator<-N*sum(data/n)
      var<-N*N*(1-f)*estimator*(1-estimator)/(n-1)
      serror<-sqrt(var)
    }
  }

  #SRS with replacement estimation
  else{
    if(estimator == "total"){
      estimator<-N*sum(data/n)
      var<-N*N*var(data)/n
      serror<-sqrt(var)
    }
    else if(estimator == "mean"){
      estimator<-sum(data/n)
      var<-var(data)/n
      serror<-sqrt(var)
    }
    else if(estimator=="proportion"){
      estimator<-sum(data/n)
      var<-estimator*(1-estimator)/(n-1)
      serror<-sqrt(var)
    }
    else{
      estimator<-N*sum(data/n)
      var<-N*N*estimator*(1-estimator)/(n-1)
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


##Ejemplos total
# data<-rnorm(200, 100, 20)
# sample<-data[srs.sample(200, 50)]
# tau<-sum(data);tau
# srs.estimator(200, sample, "total", alpha=0.05)
#
# sampleR<-data[srs.sample(200,50, replace=TRUE)]
# srs.estimator(200, sampleR, "total",replace=TRUE, alpha=0.05)

##Ejemplos mean
# mu<-mean(data);mu
# srs.estimator(200, sample, "mean", alpha=0.05)


# #Ejemplos proportion
# dataP<-sample(c(0,1), replace=TRUE, size=200)
# dataP
# p<-mean(dataP);p
# sampleP<-dataP[srs.sample(200, 50)]
# sampleP
# srs.estimator(200, sampleP, "proportion", alpha=0.05)


# #Ejemplos class total
# sum(dataP)
# srs.estimator(200, sampleP, "class total", alpha=0.05)
