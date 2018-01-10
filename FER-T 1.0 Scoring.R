#### Environment setting ####

rm(list=ls())

library(BEST)
library(rstan)

#### Data Loading ####

numIter=10000

set.seed(108)

# choose as input the .csv file outputted by Psychopy
rawdata <- read.csv(file.choose())

data <- cbind(substr(rawdata$Stimulus, 9, 14), rawdata$key_resp_2.keys)
data <- data.frame(data[-1,])
colnames(data) <- c("Item", "Answer")
data$Answer <- as.character(data$Answer)
data$Answer[data$Answer == 1] <- "ANG"
data$Answer[data$Answer == 2] <- "DIS"
data$Answer[data$Answer == 3] <- "FEA"
data$Answer[data$Answer == 4] <- "HAP"
data$Answer[data$Answer == 5] <- "SAD"
data$Answer[data$Answer == 6] <- "SUR"
data$Correct <- substr(data$Item, 3, 5) == data$Answer

Item <- c("ARANG2", "ARDIS3", "ARFEA2", "ARHAP2", "ARSAD3", "ARSUR2", 
"EBANG1", "EBDIS2", "EBFEA3", "EBHAP3", "EBSAD3", "EBSUR3", 
"FFANG3", "FFDIS2", "FFFEA2", "FFHAP3", "FFSAD1", "FFSUR2", 
"FGANG2", "FGDIS2", "FGFEA2", "FGHAP3", "FGSAD1", "FGSUR3", 
"LDANG3", "LDDIS3", "LDFEA3", "LDHAP2", "LDSAD3", "LDSUR2", 
"MGANG1", "MGDIS2", "MGFEA3", "MGHAP3", "MGSAD2", "MGSUR1")

Alpha <- c(0.4651, 0.3005, 0.7169, 0.8780, 0.5474, 0.6123, 0.6064,
 0.2136, 0.7794, 1.4335, 0.5459, 0.4452, 1.6947, 0.4810, 0.5903,
 0.8826, 0.5491, 0.8933, 0.8345, 0.8804, 0.2824, 0.6313, 0.6104,
 0.4571, 0.7794, 0.5261, 0.8495, 0.6296, 0.4887, 0.6038, 1.4067,
 0.5214, 1.0957, 0.7295, 0.6006, 0.3117)

Beta <- c(-3.3289, -3.9263, -3.8320, -4.1036, -1.4685, -3.7313, -1.1982,
 0.3763, 0.3876, -4.7480, -4.7722, -6.2939, -3.3452, -6.4326, 0.4887,
 -5.8279, -1.7127, -3.8558, -2.8657, -4.3872, 0.1057, -6.1967, -3.4558,
 -3.0521, -2.8014, -4.6856, -0.3597, -3.4142, 0.2852, -3.6963, -0.9332,
 -3.5789, -1.4791, -5.4119, 0.1957, -6.5352)

Itempars <- data.frame(Item, Alpha, Beta)
data <- merge(data, Itempars)


#### Stan Model ####

# Data preparation

rstan_options(auto_write = TRUE)

options(mc.cores = parallel::detectCores())

stan_data <- list(Correct=data$Correct, Alpha=data$Alpha, Beta=data$Beta)

model <- "data {
vector[36] Alpha;
vector[36] Beta;
int<lower=0, upper=1> Correct[36];
}
parameters {
vector[1] theta;         
}

model {
vector[36] eta;
theta ~ normal(0,1);

for (n in 1:36)
eta[n] = Alpha[n] * (theta[1] - Beta[n]);

Correct ~ bernoulli_logit(eta);
}
"
twopl.fit <- stan(model_code=model, data = stan_data, iter = numIter, chains = 4)

stanfit <- extract(twopl.fit, pars=c("theta"))

plotPost(stanfit$theta)
print(paste("Estimated ability: ", round(mean(stanfit$theta), 2)))
