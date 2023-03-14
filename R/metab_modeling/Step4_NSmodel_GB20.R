#### Metabolism Modeling Workflow
#### February 21, 2023
#### Script created by: Noah Lottig, Facundo Scordo
#### Script edited by: Kelly Loria, Heili Lowman

# This script is designed to estimate metabolism in the nearshore.

# Load packages
library(tidyverse)
library(rstan)
library(loo)
library(patchwork)
library(lubridate)
library(shinystan)
library(here)

## source
source("R/metab_modeling/stan_utility.R")
# Contains helpful functions written by N. Lottig.

lake <- "GB20" 

year <- c(2022)

# Stan settings
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Read data in
data <- read_rdump("data_working/GB20_2022_sonde_list.R")

# Set reference temperature
# mean(data$temp) # 14.04175
data$temp_ref <- 14.04175

# Add parameters that can be modified
data$sig_b0 <- 0.01 #pmax smoothing parameter
data$sig_r <- 0.01  #respiration smoothing parameter
data$sig_i0 <- 0.2  #light saturation smoothing parameter

# Define model location
model <- "o2_model_inhibition.stan" # Steele 2 param inhibition
model_path <- paste0("R/metab_modeling/",model)

# Set specifications for stan
chains <- 3 # was 3
iter <- 10000 # was 10000
warmup <- 5000 # was 5000
adapt_delta <- 0.99
max_treedepth <- 15
thin <- 1

# Takes a minute, so just be patient.
sm <- stan_model(file = model_path)

#### Fit model ####
# Note, Kelly approximates this takes 6 hours for 4 months 
# of data on the server.
# That was for 10,000 iterations.
# I found 5,000 iterations with 4-6 months takes 2-4 hours.
stanfit <- sampling(sm, 
                    data = data, 
                    chains = chains, 
                    cores = chains, 
                    iter = iter, 
                    warmup = min(iter*0.5, warmup),
                    control = list(adapt_delta = adapt_delta, 
                                   max_treedepth = max_treedepth), 
                    seed = 194838, 
                    thin = thin, 
                    save_warmup=FALSE) # helps make output less large

#### Examine output ####

# Warning: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low.

# Check mixing of chains and divergences in shinystan.
launch_shinystan(stanfit)

# Create summary table with median and credible interval values.
fit_summary <- summary(stanfit, probs=c(0.025,0.5,0.975))$summary %>% 
  {as_tibble(.) %>%
      mutate(var = rownames(summary(stanfit)$summary))}

# Additional model convergence diagnostics.
check_n_eff(stanfit)
check_rhat(stanfit)
check_div(stanfit)
check_treedepth(stanfit,max_treedepth)
check_energy(stanfit)

# Save model full output
# saveRDS(stanfit, paste0("data_model_outputs/",lake,"_fit.rds"))

# Edit summary dataframe
fit_clean <- fit_summary %>%
  rename(lower = '2.5%', middle = `50%`,upper = '97.5%')  %>%
  mutate(name = strsplit(var, "\\[|\\]|,") %>% map_chr(~.x[1]),
         index = strsplit(var, "\\[|\\]|,") %>% map_int(~as.integer(.x[2])),
         day = ifelse(name %in% c("GPP","ER","NEP","AIR","Flux","GPP_m2","ER_m2","NEP_m2","b0","r","i0","b"), 
                      index, 
                      data$map_days[index])) %>%
  select(name, index, day, middle,lower,upper)

# Read in input data
sonde_data <- read_csv("data_working/sonde_prep_GB20_2022.csv")

# Join model fit summary with input data.
out <- fit_clean %>%
  filter(name %in% c("GPP","ER","NEP")) %>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP","ER","NEP"))) %>% 
  mutate(middle = ifelse(name=="ER",-middle,middle),
         lower = ifelse(name=="ER",-lower,lower),
         upper = ifelse(name=="ER",-upper,upper),
         name = factor(name, levels=c("GPP","NEP","ER")))

out2 <- fit_clean %>%
  filter(name %in% c("GPP_m2","ER_m2","NEP_m2"))%>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP_m2","ER_m2","NEP_m2"))) %>% 
  mutate(middle = ifelse(name=="ER_m2",-middle,middle),
         lower = ifelse(name=="ER_m2",-lower,lower),
         upper = ifelse(name=="ER_m2",-upper,upper),
         name = factor(name, levels=c("GPP_m2","NEP_m2","ER_m2")))

out3 <- rbind(out,out2)

# Export model summaries.
write_csv(out3, "data_model_outputs/GB20_daily_full_031423.csv")
write_csv(fit_clean, "data_model_outputs/GB20_summary_clean_031423.csv")

# Plot primary parameters
(p1 <- fit_clean %>%  
  filter(name=="b0" | name == "r" | name == "i0" ) %>% 
  rename(unique_day = index) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>%
  ggplot(aes(x=yday,y=middle,color=factor(year))) + 
  geom_point(size=0.5) +
  geom_line(alpha=0.5) +
  facet_wrap(vars(name),ncol=1,scales="free_y") +
  theme_bw() +
  labs(y="Mean Estimated Value",color="year",x="Day of Year"))

# Export figure.
ggsave(plot = p1,filename = "figures/GB20fit_2022_031423.png",
       width=8, height=4.5, dpi=300)

# Plot time series of estimates
(p2 <- ggplot(data = out %>% drop_na(year),aes(yday, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+
  #geom_point(data = out %>% left_join(c14),aes(x=yday,y=(p80/12.011),color="C14")) +
  scale_color_manual(values = c("green","black","dodgerblue","firebrick")) +
  scale_fill_manual(values = c("dodgerblue","firebrick","black")) +
  theme_bw() +
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) +
  facet_wrap(vars(year)))

# Export figure.
ggsave(plot = p2,filename = "figures/GB20metab_2022_031423.png",
       width=8, height=4.5, dpi=300)

# End of script.
