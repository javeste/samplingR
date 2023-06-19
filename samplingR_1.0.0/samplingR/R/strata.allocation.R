#' @title Strata allocation given a sample size
#' @description Function to allocate the number of samples to be taken for each
#' strata given the total sample size and the strata.allocation method. The number of allocations
#'  returned will be equal to the length of the parameters.
#' @param Nh Vector of population strata sizes.
#' @param n Sample size
#' @param var Vector of strata variances.
#' @param alloc The allocation method to be used. Default is "unif".
#' @param C Total study cost.
#' @param cini Overhead study cost.
#' @param ch Vector of costs to take an individual from a strata for the sample.
#'
#'
#' @return Vector of strata sample sizes.
#' @export
#'
#' @details alloc="optim" is the only that requires cost function data.
#' Total study and overhead study costs are optional. If given allocation will be done
#' so total study cost is not surpassed.s
#' @examples
#' strata.allocation(Nh=rep(125,4), n=100, alloc="unif") #25, 25, 25, 25
#' strata.allocation(Nh=c(100, 50, 25), n=100, alloc="prop")


strata.allocation<-function(Nh, n, var, alloc=c("unif", "prop", "min", "optim"), C, cini, ch){
  alloc=match.arg(alloc)
  if(!missing(var)) var<-as.array(var)
  if(!missing(ch)) ch<-as.array(ch)

  #Acceptance conditions
  if(alloc != "unif" && alloc != "prop" && alloc != "min" && alloc != "optim")
    stop('Alloc must be one of c("unif", "prop", "min", "optim")')

  if((alloc=="min" || alloc=="optim") && missing(var)){
    warning("\nNecessary var argument missing, will be set to worst case scenario value for each strata.\n");
    var<-c(Nh/(Nh-1)*0.25)
  }
  if((alloc=="min" || alloc=="optim") && missing(var))stop('Strata variance values must be given for alloc=c("min","optim")')
  if( (alloc=="min" || alloc=="optim") && length(Nh) != length(var)) stop("Strata variance length must be equal to strata sizes length")
  if(alloc=="optim" && missing(ch))stop('Strata cost values must be given for alloc="optim"')
  if(alloc=="optim" && length(Nh) != length(ch)) stop("Strata costs length must be equal to strata sizes length")
  if(missing(cini)) cini<-0



  #Population strata sizes
  Nh<-as.array(Nh)
  #Population size
  N<-sum(Nh)
  Wh<-Nh/N

  if(alloc=="unif"){
    return(rep(n/length(Nh), length(Nh)))
  }
  else if(alloc=="prop"){
    k<-n/N
    return(Nh*k)
  }
  else if(alloc=="min"){
    return( n*(Nh*sqrt(var))/(sum(Nh*sqrt(var))) )
  }
  else{
    if(missing(C)){
      a<-Nh*sqrt(var)/sqrt(ch)
      return(n*a/sum(a))
    }
    else{
      return( (C-cini)*(Wh*sqrt(var)/sqrt(ch))/sum(Wh*sqrt(var)*sqrt(ch)) )
    }

  }
}




