data <- read.csv('SB_LAW.csv', header = TRUE)

source('functions.R')

hist(data$logR1, probability = TRUE)

library(ggplot2)

library(R2jags)
library(SimTools)
library(mcmcplots)
library(mcmcse)

k <- 4

radii <- c(data$logR1, data$logR2)

ggplot( mapping = aes(x = radii))+
  geom_histogram(mapping = aes(y = ..density..), color = 'black',
                 fill = 'white') + geom_density(color = 'blue')


dat <- list(y = radii,
            N = length(radii),
            K = k)

mix_model <- "
model{
p ~ ddirich(rep(1,K))

mu[1] ~ dnorm(0, 1e-3)
lam[1] ~ dgamma(0.5,0.5)

for(j in 2:K)
{
  mu[j] ~ dnorm(0, 1e-3) T(mu[j-1],)
  
  lam[j] ~ dgamma(0.5,0.5)
}


for(i in 1:N)
{
  z[i] ~ dcat(p)
  y[i] ~ dnorm(mu[z[i]], lam[z[i]])
}
}
"

inits <- function () {
  list(mu = 1:k,
       lam = rep(1, k))
}

params0<-c("mu", "lam", "p") 


mix_fit = jags(textConnection(mix_model), 
               data = dat, 
               parameters = params0,
               inits=inits,
               n.thin = 1,
               n.chains = 1,
               n.burnin = 0,
               n.iter= 1e5,
               jags.seed = 528)

res <- as.mcmc(mix_fit)

df <- as.data.frame(res)

apply(res, 2, ess)

acf(res[,6:9])


apply(res, 2, summary)

par(mfrow = c(2,2))
hist_ci(df$`p[1]`, name = bquote(p[1]))
hist_ci(df$`p[2]`, name = bquote(p[2]))
hist_ci(df$`p[3]`, name = bquote(p[3]))
hist_ci(df$`p[4]`, name = bquote(p[4]))
par(mfrow = c(1,1))

