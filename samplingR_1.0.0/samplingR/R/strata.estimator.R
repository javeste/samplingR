#'@title Parameter estimation of stratified data
#'
#'@description Function to make estimations of diferent parameters based on
#'a stratified sample.
#'
#'@param N Population size.
#'@param Nh Size of each population strata.
#'@param data Stratified sample.
#'@param estimator Estimator to compute. Can be one of "total", "mean", "proportion", "class total". Default is "total".
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@param alpha Optional value to calculate estimation error and build 1-alpha
#'
#'@return A list containing different interest values:
#' \itemize{
#' \item estimator
#' \item variance
#' \item sampling.error
#' \item estimation.error
#' \item confint
#'}
#'
#'@details Nh length must be equal to number of strata in data.\cr
#'data is meant to be a returned object of \code{\link{strata.sample}} function.
#'@importFrom methods is
#'@export

strata.estimator<-function(N, Nh, data, estimator=c("total", "mean", "proportion", "class total"), replace=FALSE, alpha){
  estimator<-match.arg(estimator)
  if(estimator != "total" && estimator!="mean" && estimator!="proportion" && estimator!="class total")
    stop('Estimator must be one of c("total", "mean", "proportion", "class total").')


  #Transforms data into manageable list structure
  if(!is(data, "list")){
    data<-as.data.frame(data)
    data[,1]<-as.numeric(data[,1])

    clase<-levels(as.factor(data[,ncol(data)])) #strata names

    if(length(clase) != length(Nh)) stop("Strata sizes length (Nh) must be equal to number of strata.")
    domaindata<-list()  #separated strata
    for(i in clase){
      domaindata[[i]]<-data[which(data[,ncol(data)]==i),]
    }
    data<-domaindata
  }

  nh<-sapply(data, nrow) #Sample strata size
  Nh<-as.array(Nh) #Strata real size
  Wh<-Nh/N  #Strata relative size

  if( (estimator == "proportion" || estimator == "class total") && !all(sapply(data, all01list)))
    stop('Data must be of values 0, 1 for proportion and class total estimation.')
  if(sum(Nh)!=N) stop("Sum of strata sizes is not equal to population size.")

  data<-lapply(data, function(data){return(data[,1])}) #data without strata column

  #Estimators are the same with and without replacement, only variance estimation differs.
  if (estimator == "total") {
    estimator<-sum(sapply(data, mean) * Nh)
    var<-sum(mapply(totalvarestimator, Nh, nh, data, replace = replace))
    serror<-sqrt(var)
  }
  else if (estimator == "mean") {
    estimator<-sum(sapply(data, mean) * Wh)
    var<-sum(mapply(meanvarestimator, Nh, nh, data, replace = replace) * Wh * Wh)
    serror<-sqrt(var)
  }
  else if (estimator == "proportion") {
    estimator<-sum(sapply(data, mean) * Wh)
    var<-sum(mapply(pvarestimator, Nh, nh, sapply(data, mean), replace = replace) *Wh * Wh)
    serror<-sqrt(var)
  }
  else{
    estimator<-sum(sapply(data, mean) * Nh)
    var<-sum(mapply(ctvarestimator, Nh, nh, sapply(data, mean), replace = replace))
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



# ######EXAMPLES#########
# #No replacement
#
# #Total and mean
# data<-cbind(rnorm(500, 50, 20), rep(c("class 1", "class 2","class 3","class 4"),125))
# N<-500;Nh<-rep(125,4);colnames(data)<-c("X", "Strata")
# sample<-strata.sample(data=data, n=c(10,20,30,40));sample
# tau<-sum(as.numeric(data[,1]));tau
# strata.estimator(500, Nh, data=sample, estimator="total", alpha=0.05)
# mu<-mean(as.numeric(data[,1]));mu
# strata.estimator(500, Nh, data=sample, estimator="mean", alpha=0.05)
#
# #Proportion and class total
# data01<-cbind(sample(c(0,1), size=500, replace=T), rep(c("clase 1", "clase 2","clase 3","clase4"),125))
# N<-500;Nh<-rep(125,4);colnames(data01)<-c("X", "Strata")
# sample01<-strata.sample(data=data01, n=c(10,20,30,40));sample01
#
# p<-mean(as.numeric(data01[,1]));p
# strata.estimator(500, Nh, data=sample01, estimator="proportion", alpha=0.05)
#
# a<-sum(as.numeric(data01[,1]));a
# strata.estimator(500, Nh, data=sample01, estimator="class total", alpha=0.05)
#
#
# #Replacement
# #Total and mean
# sampleR<-strata.sample(data=data, n=c(10,20,30,40), replace=TRUE);sampleR
# tau
# strata.estimator(500, Nh, data=sampleR, estimator="total", alpha=0.05, replace=TRUE)
# mu
# strata.estimator(500, Nh, data=sampleR, estimator="mean", alpha=0.05, replace=TRUE)
#
# #Proportion and class total
# sample01R<-strata.sample(data=data01, n=c(10,20,30,40), replace=TRUE);sample01R
#
# p<-mean(as.numeric(data01[,1]));p
# strata.estimator(500, Nh, data=sample01R, estimator="proportion", alpha=0.05, replace=TRUE)
#
# a<-sum(as.numeric(data01[,1]));a
# strata.estimator(500, Nh, data=sample01R, estimator="class total", alpha=0.05, replace=TRUE)
