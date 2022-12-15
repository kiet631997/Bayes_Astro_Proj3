data <- read.csv('SB_LAW.csv', header = TRUE)

source('functions.R')

hist(data$logR1, probability = TRUE)

library(ggplot2)

library(R2jags)
library(SimTools)
library(mcmcplots)
library(mcmcse)

k <- 2

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

acf(res[,4:5])


apply(res, 2, summary)

par(mfrow = c(1,2))
hist_ci(df$`p[1]`)
hist_ci(df$`p[2]`)
par(mfrow = c(1,1))
n_samp <- length(radii)
data_mat <- matrix(NA_real_, ncol = n_samp, nrow = nrow(res))
for(j in 1:nrow(df))
{
  z <- rbinom(n_samp, size = 1, prob = df$`p[1]`[j])
  y1 <- rnorm(n_samp,
              mean = df$`mu[1]`[j],
              sd = 1/df$`lam[1]`[j])
  y2 <- rnorm(n_samp,
            mean = df$`mu[2]`[j],
              sd = 1/df$`lam[2]`[j])
  y <- ifelse(z == 1, y1, y2)
  
  data_mat[j,] <- y
  
}

ggplot( mapping = aes(x = data_list[[2]]))+
  geom_histogram(mapping = aes(y = ..density..), color = 'black',
                 fill = 'white') + geom_density(color = 'blue')

# prior predictive plot:
library(bayesplot)
pp_check(object = radii, yrep = data_mat[1:50,],
         ppc_dens_overlay)
