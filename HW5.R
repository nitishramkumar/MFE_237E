#statistical properties of assests
#problem1
ret_markete = 0.05
samplemeane_a1 = 0.01+0.9*ret_markete
samplemeane_a2 = -0.015+1.2*ret_markete
samplemeane_a3 = 0.005+1*ret_markete

sd_market = 0.15
sd_a1 = sqrt(0.9*0.9*sd_market^2+0.1^2)
sd_a2 = sqrt(1.2*1.2*sd_market^2+0.15^2)
sd_a3 = sqrt(1*1*sd_market^2+0.05^2)

sharpe_market = 0.33
sharpe_a1 = samplemeane_a1/sd_a1
sharpe_a2 = samplemeane_a2/sd_a2
sharpe_a3 = samplemeane_a3/sd_a3


#statistical properties of unsystematic properties of assets 
#problem2
alpha_a1_meane = 0.01
alpha_a2_meane = -0.015
alpha_a3_meane = 0.005

alpha_a1_sde = 0.1
alpha_a2_sde = 0.15
alpha_a3_sde = 0.05

sharpe_a1_hedged = alpha_a1_meane/alpha_a1_sde
sharpe_a2_hedged = alpha_a2_meane/alpha_a2_sde
sharpe_a3_hedged = alpha_a3_meane/alpha_a3_sde

#portfolio of the assets 
#problem3
covar_matrix = matrix(c(0.1^2,0,0,0,0.15^2,0,0,0,0.05^2),nrow=3)
Re = c(alpha_a1_meane,alpha_a2_meane,alpha_a3_meane)
max_sharperatio_3 = sqrt(t(Re)%*%solve(covar_matrix)%*%Re)

#portfolio including market portfolio
#problem4
covar_matrix_4 = matrix(c(sd_a1^2,0.9*1.2*0.15^2,0.9*1*0.15^2,0.9*sd_a1*0.15,1.2*0.9*0.15^2,sd_a2^2,1.2*1*0.15^2,1.2*sd_a2*0.15,1*0.9*0.15^2,1*1.2*0.15^2,sd_a3^2,1*sd_a3*0.15,0.9*sd_a1*0.15,1.2*sd_a2*0.15,1*sd_a3*0.15,0.15^2),nrow=4)
Re_4 = c(samplemeane_a1,samplemeane_a2,samplemeane_a3,ret_markete)
max_sharperatio_4 = sqrt(t(Re_4)%*%solve(covar_matrix_4)%*%Re_4)

#problem5
#a
k=0.15/max_sharperatio_4
wts = as.numeric(k)*(solve(covar_matrix_4)%*%Re_4)


#b
Expret_portfolio = t(wts)%*%Re_4
sd_portfolio = sqrt(k^2*(t(Re_4)%*%solve(covar_matrix_4)%*%Re_4))
sharpe_portfolio=Expret_portfolio/sd_portfolio


#problem6
ret_stocks=c(samplemeane_a1,samplemeane_a2,samplemeane_a3)
covar_matrix_stocks = matrix(c(sd_a1^2,0.9*1.2*0.15^2,0.9*1*0.15^2,1.2*0.9*0.15^2,sd_a2^2,1.2*1*0.15^2,1*0.9*0.15^2,1*1.2*0.15^2,sd_a3^2),nrow=3)
betas=c(0.9,1.2,1)
#a
weights_mimick = (1/3)/var(betas)*(betas-mean(betas))
meanreturn_mimick = weights_mimick%*%ret_stocks
sd_mimick = sqrt(t(weights)%*%covar_matrix_stocks%*%weights)
sharpe_mimick = meanreturn_mimick/sd_mimick

#b
cor_mimick_market = (weights_mimick%*%c(0.9*0.15/sd_a1,1.2*0.15/sd_a2,1*0.15/sd_a3))

#c
eig = eigen(covar_matrix_stocks)
var_pcas = eig$values/sum(eig$values)

#d
portfolioweights_pc = eig$vectors

#