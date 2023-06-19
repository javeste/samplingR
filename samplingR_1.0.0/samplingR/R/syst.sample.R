
#' @title Systematic sampling sample
#' @description Retrieves a \eqn{\frac{N}{n}} systematic sample
#'
#' @param N Population size.
#' @param n Sample size
#' @param data Optional data of the population.
#'
#' @return Vector of size n with numbers from 1 to N indicating the index samples to be taken.
#' If data is provided then the instances will be returned.
#'
#' @details If \eqn{\frac{N}{n}} is not an even number a 1 in floor(\eqn{\frac{N}{n}}) sample will be taken.
#' @export
#'
#' @examples
#' data<-runif(40)
#' syst.sample(40,8, data)
#'
syst.sample<-function(N, n, data){

  k<-floor(N/n)
  if(missing(data))return(srs.sample(k, 1)+ c(0:(n-1))*k)
  else {
    if(is(data, "vector")) data<-as.data.frame(data)
    return(data[srs.sample(k, 1)+c(0:(n-1))*k, ])
  }
}


