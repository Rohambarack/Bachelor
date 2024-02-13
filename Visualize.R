#####
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(see)
#### diferences in distributions
vis <- tibble(
  type = rep(c("normal","lognormal","exp_mod_normal"), each = 1000),
  value = c(rnorm(1000,mean = .45,sd = 1),
            rlnorm(1000, meanlog = 0, sdlog = 1),
            rexgaussian(1000,mu=.45, sigma = .1, beta = 1.5))
  
  
)

vis %>% 
  mutate(across(type, ~factor(.,
                              levels=c("normal",
                                       "lognormal",
                                       "exp_mod_normal")))) %>% 
  ggplot(aes(x=value, fill= type))+
  geom_density(alpha=.2)+
  xlim(c(-3,10)) +
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  scale_fill_manual(values = c("#7071E8","#EE7214","#527853"))+
  theme_default() +
  theme(legend.position = "none") +
  facet_wrap(~type)

########### sim plots
grid.arrange(
  DF_sim %>% 
    ggplot(aes(x=lat,fill= Lang))+
    geom_density(alpha=.3)+
    xlim(c(-3,10)) +
    xlab("")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    scale_fill_manual(values = c("#EE7214","#527853"))+
    theme_default(),
DF_sim %>% 
  ggplot(aes(x=lat, fill= Trial))+
  geom_density(alpha=.3)+
  xlim(c(-3,10)) +
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  scale_fill_manual(values = c("#EE7214","#527853"))+
  theme_default() +
  #theme(legend.position = "none") +
  facet_wrap(~Lang)


)

###### estimates
mod <- read_rds("mod_0_T.rds")

print(mod)

mod_draws<-as_draws_df(mod)

####prior posterior update checks
Sigma_s <- grid.arrange(
mod_draws %>% 
  ggplot()+
  geom_density(aes(x = `b_sigma_LanguageDansk:TaskSpontaneous`),
               alpha = .3,
               fill ="#527853")+
  geom_density(aes(x = prior_b_sigma),
               alpha= .3,
               fill ="#EE7214")+
  xlab("Sigma DK:Spont")+
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  theme_default(),

mod_draws %>% 
  ggplot()+
  geom_density(aes(x = `b_sigma_LanguageDansk:TaskTask`),
               alpha = .3,
               fill ="#527853")+
  geom_density(aes(x = prior_b_sigma),
               alpha= .3,
               fill ="#EE7214")+
  xlab("Sigma DK:Task")+
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  theme_default(),

mod_draws %>% 
  ggplot()+
  geom_density(aes(x = `b_sigma_LanguageNorsk:TaskSpontaneous`),
               alpha = .3,
               fill ="#527853")+
  geom_density(aes(x = prior_b_sigma),
               alpha= .3,
               fill ="#EE7214")+
  xlab("Sigma NO:Spont")+
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  theme_default(),

mod_draws %>% 
  ggplot()+
  geom_density(aes(x = `b_sigma_LanguageNorsk:TaskTask`),
               alpha = .3,
               fill ="#527853")+
  geom_density(aes(x = prior_b_sigma),
               alpha= .3,
               fill ="#EE7214")+
  xlab("Sigma NO:Task")+
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  theme_default(), nrow = 1

)


##### mu_s
Mu_s <- grid.arrange(
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageDansk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu DK:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageDansk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu DK:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageNorsk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu NO:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageNorsk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu NO:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(), nrow = 1
  
)

##### beta_s
Beta_s <- grid.arrange(
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageDansk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta DK:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageDansk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta DK:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageNorsk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta NO:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageNorsk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta NO:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(), nrow = 1
  
)



##### ind_sd
Ind_s <- grid.arrange(
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `sd_Pair__Intercept`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_sd_Pair),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu Pair Variance")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `sd_Pair__sigma_Intercept`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_sd_Pair__1),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Sigma Pair Variance")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_draws %>% 
    ggplot()+
    geom_density(aes(x = `sd_Pair__beta_Intercept`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_sd_Pair__2),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta Pair Variance")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(), nrow=1
  
)
grid.arrange(Mu_s, Sigma_s, Beta_s, Ind_s, nrow =4)

######## priors

mod_1 <- read_rds("mod_prior_T.rds")

curve(dgamma(x,2,2), from = -5, to = 5) # ind_sd
curve(dnorm(x,0.4,.5), add = T)
curve(dnorm(x,0, 1), add = T)
mod_1_draws<-as_draws_df(mod_1)
print(mod_1)
###### sigma s_1
Sigma_s_1 <- grid.arrange(
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_sigma_LanguageDansk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_sigma),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Sigma DK:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_sigma_LanguageDansk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_sigma),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Sigma DK:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_sigma_LanguageNorsk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_sigma),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Sigma NO:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_sigma_LanguageNorsk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_sigma),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Sigma NO:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(), nrow = 1
  
)
##### mu_s_1
Mu_s_1 <- grid.arrange(
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageDansk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu DK:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageDansk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu DK:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageNorsk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu NO:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_LanguageNorsk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu NO:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(), nrow = 1
  
)

##### beta_s_1
Beta_s_1 <- grid.arrange(
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageDansk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta DK:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageDansk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta DK:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageNorsk:TaskSpontaneous`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta NO:Spont")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `b_beta_LanguageNorsk:TaskTask`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_b_beta),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta NO:Task")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(), nrow = 1
  
)



##### ind_sd
Ind_s_1 <- grid.arrange(
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `sd_Pair__Intercept`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_sd_Pair),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Mu Pair Variance")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `sd_Pair__sigma_Intercept`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_sd_Pair__1),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Sigma Pair Variance")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(),
  
  mod_1_draws %>% 
    ggplot()+
    geom_density(aes(x = `sd_Pair__beta_Intercept`),
                 alpha = .3,
                 fill ="#527853")+
    geom_density(aes(x = prior_sd_Pair__2),
                 alpha= .3,
                 fill ="#EE7214")+
    xlab("Beta Pair Variance")+
    ylab("")+
    scale_y_continuous(breaks = NULL)+
    theme_default(), nrow=1
  
)
grid.arrange(Mu_s_1, Sigma_s_1, Beta_s_1, Ind_s_1, nrow =4)

grid.arrange(Sigma_s,Sigma_s_1, nrow=2)
##### viualize betas


draws <- mod_draws[,1:12]
#draws <- mod_1_draws[,1:12]

mod_b <- draws[,1:4] %>% 
  pivot_longer(cols = starts_with("b"),
               names_to = "beta",
               values_to =   "value")
mod_b <- mod_b %>% 
  mutate(name = gsub("b_Language","",beta)) %>% 
  mutate(name = gsub(".Task","",name)) %>% 
  mutate(Language = gsub("Task|Spontaneous","",name)) %>% 
  mutate(name = gsub("[[:lower:]]","",name))

mod_int <- mod_b %>% 
ggplot(aes(y = value, x = name, fill =Language)) +
  geom_violinhalf(alpha=.3)+
  ggtitle("Intercept betas") +
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c("#EE7214","#527853"))+
  theme_default() +
  theme(legend.position = "none") 

##################

mod_b <- draws[,5:8] %>% 
  pivot_longer(cols = starts_with("b"),
               names_to = "beta",
               values_to =   "value")
mod_b <- mod_b %>% 
  mutate(name = gsub("b_sigma_Language","",beta)) %>% 
  mutate(name = gsub(".Task","",name)) %>% 
  mutate(Language = gsub("Task|Spontaneous","",name)) %>% 
  mutate(name = gsub("[[:lower:]]","",name))

mod_sigma <- mod_b %>% 
  ggplot(aes(y = value, x = name, fill =Language)) +
  geom_violinhalf(alpha=.3)+
  ggtitle("Sigma betas") +
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c("#EE7214","#527853"))+
  theme_default() +
  theme(legend.position = "none") 

######## betas


mod_b <- draws[,9:12] %>% 
  pivot_longer(cols = starts_with("b"),
               names_to = "beta",
               values_to =   "value")
mod_b <- mod_b %>% 
  mutate(name = gsub("b_beta_Language","",beta)) %>% 
  mutate(name = gsub(".Task","",name)) %>% 
  mutate(Language = gsub("Task|Spontaneous","",name)) %>% 
  mutate(name = gsub("[[:lower:]]","",name))
 

mod_betas <- mod_b %>% 
  ggplot(aes(y = value, x = name, fill =Language)) +
  geom_violinhalf(alpha=.3)+
  ggtitle("Beta betas") +
  xlab("")+
  ylab("")+
  scale_fill_manual(values = c("#EE7214","#527853"))+
  theme_default() +
  theme(legend.position = "none") 
hypothesis(mod,"LanguageDansk:TaskTask > LanguageDansk:TaskSpontaneous ")


#### m
lay <- rbind(c(1,1,1),
             c(2,3,4),
             c(2,3,4),
             c(2,3,4),
             c(2,3,4))
m_b_1 <- grid.arrange(main=textGrob("Narrow Priors model",
                                    gp=gpar(fontsize=20,font=6)),
                      mod_int,mod_sigma,mod_betas,
                      layout_matrix = lay
                      )
m_b_2 <- grid.arrange(main=textGrob("Broad Priors model",
                                    gp=gpar(fontsize=20,font=6)),
                      mod_int,mod_sigma,mod_betas,
                      layout_matrix = lay
)

grid.arrange(m_b_1,m_b_2)
?grep

print(mod)
print(mod_1)

###predictions

d <- read_csv("data_p.csv")
test_pairs <- c(4,6,16,26,44,45,69,70)
test_set <- d %>% 
  filter(Pair %in% test_pairs)

train_set <- d %>% 
  filter(!Pair %in% test_pairs)


pred_train_set <- train_set
pred_train_set$mod_0 <- predict(mod, newdata = train_set)
pred_train_set$mod_1 <- predict(mod_1, newdata = train_set)

pred_test_set <- test_set
pred_test_set$mod_0 <- predict(mod, newdata = test_set,
                               allow_new_levels =T)
pred_test_set$mod_1 <- predict(mod_1, newdata = test_set,
                               allow_new_levels =T)

####### pred_plot

pred_train_set %>% 
  ggplot()+
  geom_density(aes(x=latency), color = "black") +
  geom_density(aes(x=mod_0[,1]), color = "red") +
  geom_density(aes(x=mod_1[,1]), color = "blue")
  

pp <- grid.arrange(
  pp_check(mod,ndraws = 100) +
    ggtitle("Narrow Priors") +
    xlim(c(-3,9)) +
    theme(legend.position = "none"),
  pp_check(mod_1,ndraws = 100) +
    ggtitle("Broad Priors") +
    xlim(c(-3,9)) +
    theme(legend.position = "none")
  ,nrow=1)


hm <- (pred_train_set$latency - pred_train_set$mod_0[,1])^2
hm_2 <- (pred_train_set$latency - pred_train_set$mod_1[,1])^2

mean(hm)
mean(hm_2)

hm_3 <- (pred_test_set$latency - pred_test_set$mod_0[,1])^2
hm_4 <- (pred_test_set$latency - pred_test_set$mod_1[,1])^2

mean(hm_3)
mean(hm_4)

vis_MSE <- tibble(
  values = c(mean(hm),mean(hm_2),mean(hm_3),mean(hm_4)),
  model = c("Np","Bp","Np","Bp"),
  set = c("Train","Train","Test","Test")
)

ggplot()
#### MSE

MSE <- vis_MSE %>% 
  ggplot(aes(x = set, y = values, group = model, color= model)) +
  geom_point(alpha=.5)+
  geom_line(alpha=.5)+
  ggtitle("Model MSE") +
  xlab("")+
  ylab("")+
  scale_color_manual(values = c("#EE7214","#527853"))+
  theme_default() +
  theme(legend.position = "none") +
  facet_wrap(~model)

grid.arrange(pp,MSE)
###mu
hypothesis(mod_1,"LanguageDansk:TaskTask > LanguageDansk:TaskSpontaneous")
hypothesis(mod_1,"LanguageNorsk:TaskTask > LanguageNorsk:TaskSpontaneous")
hypothesis(mod_1,"LanguageDansk:TaskSpontaneous > LanguageNorsk:TaskSpontaneous")
hypothesis(mod_1,"LanguageDansk:TaskTask > LanguageNorsk:TaskTask")

###sigma
hypothesis(mod_1,"sigma_LanguageDansk:TaskSpontaneous > sigma_LanguageDansk:TaskTask")
hypothesis(mod_1,"sigma_LanguageNorsk:TaskSpontaneous > sigma_LanguageNorsk:TaskTask")
hypothesis(mod_1,"sigma_LanguageDansk:TaskSpontaneous < sigma_LanguageNorsk:TaskSpontaneous")
hypothesis(mod_1,"sigma_LanguageDansk:TaskTask < sigma_LanguageNorsk:TaskTask")

###beta
hypothesis(mod_1,"beta_LanguageDansk:TaskSpontaneous < beta_LanguageDansk:TaskTask")
hypothesis(mod_1,"beta_LanguageNorsk:TaskSpontaneous < beta_LanguageNorsk:TaskTask")
hypothesis(mod_1,"beta_LanguageDansk:TaskSpontaneous > beta_LanguageNorsk:TaskSpontaneous")
hypothesis(mod_1,"beta_LanguageDansk:TaskTask > beta_LanguageNorsk:TaskTask")

##playground 
sim <- read_rds("sim_mod.rds")

print(sim)
print(mod_1)


curve(dexgaussian(x,mu = .23,
                  sigma = exp(-0.49),
                  beta = exp(-0.37)),
      from = -2, to =4)
curve(dexgaussian(x,mu = .24,
                  sigma = exp(-0.45),
                  beta = exp(-0.39)), col ="red", add = T)

curve(dexgaussian(x,mu = 0,
                  sigma = 1,
                  beta = 1),
      from = -4, to =4)


hmh <- train_set %>% 
  group_by(Language, Task) %>% 
  summarize(n(), sum(Backchannel)/n(), sum(Open_repair)/n())


curve(dexgaussian(x,mu = 0.25,
                  sigma = 0.1,
                  beta = 0.1),
      from = -2, to =2)

d_2 <- d %>% 
  mutate(lat_2 = ifelse(Backchannel == 1,NA,latency))
d_2 %>% 
  ggplot(aes(x=latency))+
  geom_density() +
  geom_density(aes(x=lat_2),color = "red")

citation("brms")
####bac no back

d_2 %>% 
  ggplot(aes(x=latency))+
  geom_density() +
  geom_density(aes(x=lat_2), color = "#EE7214", alpha = .3)+
  ggtitle("Removing Backchannels") +
  xlab("")+
  ylab("")+
  theme_default() +
  theme(legend.position = "none")


##### sim noise
d_3 <- d_2 %>% 
  select(latency)
d_3$label <- rep("data",length(d_3))

d_3 <- d_3 %>% 
  rename(values = latency)
noise <- tibble( values = 
                   c(rexgaussian(7000,mu = .2, sigma = .2, beta =.3),
                     runif(2000, -2,0),
                     runif(1000,0,8)),
                 label = rep("simulation",10000)
                 )

noisesim <- rbind(d_3,noise)

noisesim %>% 
  ggplot(aes(x=values))+
  geom_density() +
  ggtitle("Illustrate possible noise") +
  xlab("")+
  ylab("")+
  theme_default() +
  theme(legend.position = "none")+
  facet_wrap(~label)

