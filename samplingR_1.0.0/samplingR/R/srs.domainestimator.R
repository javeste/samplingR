#'@title Simple Random Sample parameter estimation of domains.
#'@description Function to make estimations of diferent parameters on a given domain
#'based on a Simple Random Sample.
#'
#'@param Nh Number of instances of the data set domain.
#'@param data Sample of the data. It must constain a column with the data to estimate
#'and a second column with the domain of each instance.
#'@param estimator One of "total", "mean". Default is "total".
#'@param domain Domain of the sample from which parameter estimation will be done.
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@param alpha Optional value to calculate estimation error and build 1-alpha
#'confidence interval.
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
#'@details Data columns must be arranged with interest values on the first column
#'and domain values on the last column.\cr
#'Domain parameter can be either numeric or character
#'and must be equal to one of the values of the domain column of data.
#'
#'@references Pérez, C. (1999) Técnicas de muestreo estadístico. Teoría, práctica y aplicaciones informáticas. 193-195
#'
#'@examples
#'data<-cbind(rnorm(500, 50, 20), rep(c(1:2),250))
#'sample<-data[srs.sample(500, 100),]
#'sum(data[which(data[,-1]==1),1])
#'srs.domainestimator(Nh = 250, data = sample, estimator="total", domain=1)
#'
#'@export


srs.domainestimator<-function(Nh, data, estimator=c("total", "mean", "proportion", "class total"), domain, replace=FALSE, alpha){ #data debe ser la muestra por lo que domain no es necesario
  estimator=match.arg(estimator)

  if(missing(domain)) stop("domain must be declared.")
  if(estimator != "total" && estimator!="mean" && estimator!="proportion" && estimator!="class total") stop('Estimator must be one of c("total", "mean", "proportion", "class total").')
  if(missing(Nh)) stop("Nh must be declared.")
  if(!any(data[,ncol(data)]==domain)) stop("Sample data does not contain instances of domain given.")

  #data format
  data<-as.data.frame(data)
  data[,1]<-as.numeric(data[,1])

  #data with domain selected instances only
  domaindata<-data[which(data[,ncol(data)]==domain),1]

  return(srs.estimator(Nh, domaindata, estimator, replace, alpha))    #TODO como obtener N ya que se si no se conoce Nj se debe conocer N
}


#  data<-cbind(rnorm(500, 50, 20), rep(c(1,2),250))
#  sample<-data[srs.sample(500, 100),]
#  sum(data[which(data[,-1]==2),1])
#  srsdomainestimator(Nj = 250, data = sample, estimator="total", domain=2)
#
#
# # #data.frame si permite tipos char en la clase
#  data<-data.frame(x=rnorm(500, 50, 20), domain=rep(c("clase 1","clase 2"),250))
#  sample<-data[srs.sample(500, 100),]
#  sum(data[which(data[,-1]=="clase 1"),1])
#  srsdomainestimator(Nj = 250, data = sample, estimator="total", domain="clase 1")
