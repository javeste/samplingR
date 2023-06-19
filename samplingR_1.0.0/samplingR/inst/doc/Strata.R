## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(samplingR)

## -----------------------------------------------------------------------------
N1<-585479
pen<-rnorm(N1, 750, 100)

## -----------------------------------------------------------------------------
N2<-932992
ass<-rnorm(N2, 1500, 500)

## -----------------------------------------------------------------------------
datos<-cbind(c(pen, ass), c(rep("pensionista", N1), rep("asalariado", N2)))
N<-N1+N2

## -----------------------------------------------------------------------------
Nh<-c(N1, N2)
n<-800
strata.allocation(Nh=Nh, n=n, alloc="unif")

## -----------------------------------------------------------------------------
C<-12000
Cini<-5000
ch<-c(45, 20)
size<-strata.samplesize.cost(Nh=Nh, C=C, cini=Cini, ch=ch, alloc="unif")
paste("Tamaño de muestra", size)
nh.unif<-strata.allocation(Nh=Nh, n=size, alloc="unif")

paste(c("Estrato 1:", "Estrato 2"), nh.unif)
paste("Coste:", Cini+sum(ch*nh.unif))

## -----------------------------------------------------------------------------
nh.unif<-floor(nh.unif)
paste(c("Estrato 1:", "Estrato 2"), nh.unif)
paste("Coste:", Cini+sum(ch*nh.unif))

## -----------------------------------------------------------------------------
strata.allocation(Nh=Nh, n=n, alloc="prop")

## -----------------------------------------------------------------------------
size<-strata.samplesize.cost(Nh=Nh, C=C, cini=Cini, ch=ch, alloc="prop")
paste("Tamaño de muestra", size)
nh.unif<-floor(strata.allocation(Nh=Nh, n=size, alloc="prop"))
paste(c("Estrato 1:", "Estrato 2"), nh.unif)
paste("Coste:", Cini+sum(ch*nh.unif))

## -----------------------------------------------------------------------------
vart<-c(var(pen), var(ass))
strata.allocation(Nh=Nh, n=n, var=vart, alloc="min")

## -----------------------------------------------------------------------------
sample<-strata.sample(data=datos, n=c(20, 20))
var<-c(var(sample[which(sample[,2]=="asalariado"),1]), var(sample[which(sample[,2]=="pensionista"),1]))
strata.allocation(Nh=Nh, n=n, var=var, alloc="min")


## -----------------------------------------------------------------------------
var<-c(Nh/(Nh-1)*0.5*(1-0.5))
strata.allocation(Nh=Nh, n=n, var=var, alloc="min")

## -----------------------------------------------------------------------------
strata.allocation(Nh=Nh, n=n, alloc="min")

## -----------------------------------------------------------------------------
size<-strata.samplesize.cost(Nh=Nh, var=vart, C=C, cini=Cini, ch=ch, alloc="optim")
paste("Tamaño de muestra", size)
nh.optim<-floor(strata.allocation(Nh=Nh, n=size, var=vart, alloc="optim", C=C, cini=Cini, ch=ch))
paste(c("Estrato 1:", "Estrato 2"), nh.optim)
paste("Coste:", Cini+sum(ch*nh.optim))

