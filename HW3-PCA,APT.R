library(xts)
library(lubridate)

#48 Industry portfolio returns
data = read.csv("48_Industry_Portfolios.CSV",skip=11,header=TRUE,stringsAsFactors = FALSE,nrows = 1085)
date = data[,1]
date_1=paste0(floor(as.numeric(date)/100),"-",as.numeric(date)%%100)
date_2 = as.yearmon(date_1)
date_3 = as.Date(date_2)+months(1)-days(1)
portfolio = as.xts(data[,-1],order.by = date_3)
portfolio1 = portfolio["1960-01-01/2016-01-01",]
portfolio2 = portfolio1[,apply(portfolio1,2,function(x) (!any(x==-99.99)))]

#FF factor returns for 
FF_3factors = read.csv("F-F_Research_Data_Factors.csv",skip=3,header=TRUE,stringsAsFactors = FALSE,nrows = 1085)
dateFF = FF_3factors[,1]
date_FF=paste0(floor(as.numeric(dateFF)/100),"-",as.numeric(dateFF)%%100)
date_2FF = as.yearmon(date_FF)
date_3FF = as.Date(date_2FF)+months(1)-days(1)
FFfactors = as.xts(FF_3factors[,-1],order.by = date_3FF)
FFfactors_1960 = FFfactors["1960-01/2015-12",]
all(index(port_exret)==index(FFfactors_1960))

port_exret = portfolio2 - FFfactors_1960[,4,drop=T]
market_exret = FFfactors_1960[,1]

port_exret_demean = as.matrix(apply(port_exret,2,function(x) (x-mean(x))))
var.cov_matrix = cov(port_exret_demean)

#1
eig_values = eigen(var.cov_matrix)
eig_values_prncomp = princomp(var.cov_matrix)


#2a
pc = eig_values$vectors[,1:3]
#plot the fractional variance explained by eigenvalues

#2b
factor3_portret = port_exret_demean%*%pc
factor_3 = as.matrix(apply(factor3_portret,2,function(x) x/sd(x)))
statret_factor3 = apply(factor_3,2,function(x) c("mean"=mean(x),"sd" = sd(x)))
cor_factor3 = cor(factor_3)

#c
realavgret = apply(port_exret,2,mean)
predret_demean = port_exret_demean%*%pc%*%t(pc)
predret = port_exret
coredata(predret) = as.matrix(apply(matrix(1:43,nrow=43),1,function(x) predret_demean[,x]+mean(port_exret[,x])))
predret_mean = as.vector(apply(predret,2,mean))
plot(realavgret,predret_mean)

R_Squared = 1 - (var(realavgret-predret_mean))/var(realavgret)
