# The code below is to fit the multievent CR model. 
# The dataset used in this script includes only family units of females captured as 
# independnt bears at least twice between 1992 and 2019.


library(jagsUI)
library(tidyverse)
load(file = "data/CRlocalbears_revision2MEE.Rdata")
load(file = "data/agefactlocalbears_revision2MEE.Rdata")
load(file = "data/agereallocalbears_revision2MEE.Rdata")
load(file = "data/dataweaning_revision2MEE.Rdata")
load(file = "data/daylocalbears_revision2MEE.Rdata")
load(file = "data/initstatelocalbears_revision2MEE.Rdata")

CRfamily <- CRlb # Matrix of capture histories
daycapt <- daylb # Matrix of days of capture 
init_statematrix <- initmatlb 

# Define the age groups
age_mat <- agelbfull # Matrix of ages
age_mat[agelbfull < 10] <- 1                          # Young
age_mat[(agelbfull >= 10) & (agelbfull <= 15)] <- 2   # Prime-aged
age_mat[agelbfull > 15] <- 3                          # Old


# check order of family units
rownames(CRfamily) == rownames(daycapt)
rownames(CRfamily) == rownames(age_mat)
rownames(CRfamily) == rownames(init_statematrix)

##
data <- data.matrix(CRfamily)
alive1 <- data.matrix(init_statematrix)

head(data)
N <- dim(data)[1] ; N
Years <- dim(data)[2] ; Years


# Compute vector with occasion of first capture
get.first <- function(x) min(which(x!=0))
First <- apply(data, 1, get.first)

# Model the departure probability of two-year-old cubs as a function of the day ~~~~~~~~~~~~
nty <- dim(dataweaning)[[1]]
status <- dataweaning$status            # Response variable (1 alone, 0 with mother)
day_number <- dataweaning$daysinseason  # Covariate
modeld <- glm(status ~ day_number,
              family = "binomial")
# Predict departure probability function of date of capture
daycapt <- replace(daycapt,is.na(daycapt),80)
alpha = matrix(0,nrow=N,ncol=Years)
for(i in 1:N){
  for(j in (First[i]):Years){
    alpha[i,j] <- predict(modeld,newdata=list(doy=daycapt[i,j]),type="response")
  }
}


# Model with age except for beta & gamma ------------------------------------

my_data <- list(N = N, 
                First = First, 
                Years = Years,
                age = age_mat,
                mydata = data.matrix(data + 1),
                alpha = data.matrix(alpha)) 

# Specify the parameters to monitor
params <- c("phi", "s", "l02", "l12", "kappa", "beta", "gamma", "p", "prop")


# generate a list with the initial values drawn in normal distributions
set.seed(42)
init1 <- list(theta = rnorm(10, mean = 0, sd = 1), alive = alive1)
init2 <- list(theta = rnorm(10, mean = 0, sd = 1), alive = alive1)

inits <- list(init1, init2) # concatenate list of initial values 


#Z Run the model
fit_CR_model <- jags(data = my_data,
                     inits = inits,  
                     parameters.to.save = params, 
                     model.file = "CR_model_age_dep_except_beta_gamma.txt",
                     n.chains = 2, n.iter = 20000, n.burnin = 9000, n.thin = 5,
                     parallel = TRUE) 

save(fit_CR_model, file = "data/fit_CR_model_age_except_beta_gamma.RData")