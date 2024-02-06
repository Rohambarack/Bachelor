library(tidyverse)
library(brms)
###### Simulations 
#n pairs, lang
n <- 30
lang <- c("DK","NO")

DF_sim <- tibble(
  Pair = seq(1:(length(lang)*n)),
  Lang = rep(lang, each = n)
  
)
#bases
# exgaussians have 3 parameters, mu, sigma, and beta. + sds for multilevels
DF_sim <- DF_sim %>% 
  mutate(lang_mu = ifelse(Lang=="NO",.25,.45), 
         lang_T_mu = .2, #task mu, will be added to change mu in the task condition
         lang_mu_sd = .1, 
         #lang_sigma = .1,
         #lang_sigma_sd = .033,
         lang_beta = 1.5,
         lang_beta_sd =.1)

#individual values for multilevel models for pairs
DF_sim <- DF_sim %>% 
  rowwise() %>% 
  mutate(ind_mu_T = rnorm(1,lang_mu + lang_T_mu,lang_mu_sd),
         ind_mu_S = rnorm(1,lang_mu, lang_mu_sd),
         
         #ind_sd = rnorm(1,lang_sigma, lang_sigma_sd),
         #ind_sd = ifelse(ind_sd < 0,0.00001,ind_sd),
         ind_sd = .1,
         ind_beta = rnorm(1,lang_beta, lang_beta_sd)
  )

#extend to sessions
s <- 4

DF_sim <-  DF_sim %>% expand(nesting(Pair,
                                     Lang,
                                     ind_mu_T,
                                     ind_mu_S,
                                     ind_sd,
                                     ind_beta),Session = 1:s)

#add trial tyoes
# Trial type
trial <- c("Spont", "Task","Task","Spont")

DF_sim$Trial <- rep(trial, times = length(lang)*n)


#add 200 turns
DF_sim <- DF_sim %>% 
  expand(nesting(Pair,
                 Lang,
                 Session,
                 Trial,
                 ind_mu_T,
                 ind_mu_S,
                 ind_sd,
                 ind_beta),Turn = 1:200)
# add individual turn latencies based on both language and trial type
DF_sim <- DF_sim %>% 
  rowwise() %>% 
  mutate(lat = ifelse(Trial == "Spont",
                      rexgaussian(1,
                                  mu = ind_mu_S,
                                  sigma = ind_sd,
                                  beta = ind_beta),
                      rexgaussian(1,
                                  mu = ind_mu_T,
                                  sigma = ind_sd,
                                  beta = ind_beta))
         
  )

##check plots
DF_sim %>% 
  ggplot(aes(x = lat, fill = Trial, alpha =.4)) +
  geom_density() +
  facet_wrap(~Lang)

###model

# mod 4 with pair

#formula
formula_4 <- bf(lat ~ 0 + Lang:Trial + (1|Pair) ,
                #sigma ~ 0 + Lang:Trial + (1|Pair),
                sigma ~ 1, # in the simulations sigma was not varied to keep it simple
                #beta ~ 0 + Lang:Trial + (1|Pair),
                beta ~  (1|Pair)# beta was also not dependent on condition
)

#priors
get_prior(formula = formula_4,
          data = DF_sim,
          family = exgaussian())

sim_priors_4 <- c(
  #loosened/constricted a bit by trail and error so the model would initiate properly
  prior(normal(.45, .2), class = b), #mu
  prior(normal(.1, .1), class = sd), #mu_sd
  prior(normal(log(1.5), .1), class = Intercept, dpar = beta),# beta
  prior(normal(.1,.1), class = sd, dpar = beta), #beta_sd
  prior(normal(log(.1),.1), class = Intercept, dpar = sigma)# sigma
)

#prior predictive check 
mod_sim_4 <- brm(
  formula = formula_4,
  data= DF_sim,
  family = exgaussian(),
  prior = sim_priors_4,
  #stanvars = stanvars,
  sample_prior = "only",
  #backend = "cmdstanr",
  chains = 4,
  init = 0,
  cores = 16,
  iter = 1500,
  warmup = 500
  
)


pp_check(mod_sim_4, ndraws = 100)

##fit
mod_sim_4_T <- brm(
  formula = formula_4,
  data= DF_sim,
  family = exgaussian(),
  prior = sim_priors_4,
  #stanvars = stanvars,
  sample_prior = T,
  #backend = "cmdstanr",
  chains = 4,
  init = 0,
  cores = 32,
  iter = 1500,
  warmup = 500
  
)


pp_check(mod_sim_4_T, ndraws = 100)

print(mod_sim_4_T)
