library(tidyverse)
library(brms)
######## Import real data ##############
setwd(".")
real_data <- read_csv("data_p.csv")


### Separate train and test set #######
real_data %>% 
  group_by(Language) %>% 
  reframe(length(unique(Pair)))
# So 39 DK 38 NO
# 4, 4 to test
set.seed(1)

D_dansk <- real_data %>% 
  filter(Language == "Dansk")
unique(D_dansk$Pair)

D_no <- real_data %>% 
  filter(Language == "Norsk")
unique(D_no$Pair)

test_pairs <- c(sample(unique(D_dansk$Pair),4),
                sample(unique(D_no$Pair),4))
# test_pairs <- c(4,6,16,26,44,45,69,70)
test_set <- real_data %>% 
  filter(Pair %in% test_pairs)

train_set <- real_data %>% 
  filter(!Pair %in% test_pairs)

########## model 0 #####
formula_0 <- bf(latency ~ 0 + Language:Task + (1|Pair) ,
                sigma ~ 0 + Language:Task + (1|Pair),
                #sigma ~ 1, # in the simulations sigma was not varied to keep it simple
                beta ~ 0 + Language:Task + (1|Pair)
                #beta ~  (1|Pair)# beta was also not dependent on condition
)

#priors
get_prior(formula = formula_0,
          data = train_set,
          family = exgaussian())

priors_0<- c(
  #loosened/constricted a bit by trail and error so the model would initiate properly
  prior(normal(.45, .2), class = b), #mu
  prior(normal(.1, .1), class = sd), #mu_sd
  prior(normal(log(1.5), .1), class = b, dpar = beta),# beta
  prior(normal(.1,.1), class = sd, dpar = beta), #beta_sd
  prior(normal(log(.1),.1), class = b, dpar = sigma),# sigma
  prior(normal(.1,.1), class = sd, dpar = sigma) #sigma_sd
)

#pp_check 
mod_r_0 <- brm(
  formula = formula_0,
  data= train_set,
  family = exgaussian(),
  prior = priors_1,
  #stanvars = stanvars,
  sample_prior = "only",
  #backend = "cmdstanr",
  chains = 4,
  init = 0,
  cores = 32,
  iter = 1500,
  warmup = 500
  
)


pp_check(mod_r_0, ndraws = 100)
print(mod_r_0)

#fit 
mod_r_0_T <- brm(
  formula = formula_0,
  data= train_set,
  family = exgaussian(),
  prior = priors_0,
  #stanvars = stanvars,
  sample_prior = T,
  #backend = "cmdstanr",
  chains = 4,
  init = 0,
  cores = 32,
  iter = 1500,
  warmup = 500
  
)

pp_check(mod_r_0_T, ndraws = 100)
print(mod_r_0_T)


saveRDS(mod_r_0_T,"mod_0_T.rds")

mod <- read_rds("mod_0_T.rds")

print(mod)
