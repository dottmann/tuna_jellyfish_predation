
#--------------------------------------------
#-- Predation of metaephyrae on tuna larvae--
#--------------------------------------------

# Author: Daniel Ottmann
# Created on: February 2021
# Last update: May 2021

###################################################################################################
#       Readme

# Paper title: Spawning site selection of a migratory tuna reduces jellyfish predation on early life stages
# This script is designed to:
# 1.- Simulate tuna egg and larval survival to metaephyrae in relation to temperature-dependent development time
# 2.- Evaluate tuna egg and larval survival based on field data of water temperature and ephyrae and larval densities

###################################################################################################


##################################################################
# Clear environment:
rm(list = ls())

##########################
# Load packagges:
library(tidyverse)
library(cowplot)


##################################################################################################
# Load field data:
df <- read.delim('data/txt/data_metaephyrae_tuna.txt', sep = '\t', header = T, stringsAsFactors = F)
save(df, file = "data/data_metaephyrae_tuna.RData")


##################################################################################################

# Proportion of metaephyrae >4mm:
metaephyrae_proportion <- 0.6340426

# Proportion of tuna larvae <4.5mm:
preflexion_proportion <- 0.8216647

# Edit data:
df <- df %>%
  mutate(year = as.factor(year),
    metaephyrae_dens = pnoctiluca_dens * metaephyrae_proportion,
         tthynnus_preflex_dens = tthynnus_dens * preflexion_proportion,
         overlap_index = metaephyrae_dens * tthynnus_preflex_dens)


# Plot abundance histograms of preflexion larvae and metaephyrae:
# Preflexion larvae:
p1 <- ggplot() + 
  geom_histogram(data = subset(df, tthynnus_preflex_dens > 0), aes(tthynnus_preflex_dens), color = "black", fill = "white", bins = 60, boundary = 0) +
  labs(x = expression(italic("T. thynnus "), "m"^-3*""), y = "Frequency (zeros excluded)") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Metaephyrae:
p2 <- ggplot() + 
  geom_histogram(data = subset(df, metaephyrae_dens > 0), aes(metaephyrae_dens), color = "black", fill = "white",  bins = 60, boundary = 0) +
  labs(x = expression(italic("P. noctiluca"), "m"^-3*""), y = "Frequency (zeros excluded)") +
  theme_bw()  +
  theme(panel.grid = element_blank())

# Plot it out:
png(filename = "plots/frequency_plots.png", width = 12, height = 8, units = "cm", res = 300)
plot_grid(p1, p2,labels = "auto")
dev.off()

####################################################################################################################
# Simulating functional response of survival as a function of temperature-dependent egg and larval development time:

# Hours in a day:
hr <- 24

# Clearance rate (Gordoa et al 2013):
cr_23 <- .00414 # m^3 ind^-1 h^-1 at 23?C
q10_cr <- 2.8 # Hansen Hansen et al 1997

# Set range of temperatures:
min_temperature <- 20
max_temperature <- 27
temperature <- rep(seq(from = min_temperature, to = max_temperature, by = .1), 5)

# Now use Reglero et al 2018 equation to calculate egg develoment time in days:
dt <- (8787.5 * temperature^-1.701)

# Calculate the yolk-sack duration:
q10 <- 2      # Q10 coeficient
tq10 <- 25    # Temperature of experiment
dq10 <- 2.5   # Development time in experiment
yst <- dq10 * q10^((tq10 - temperature)/10) * hr  # Duration of yolk-sack stage

# Speciffic larval growth rate
sgr <- 0.0418 * temperature - 0.8355

# Add the time from yolk-sac to flexion Using SGR from Reglero et al 2018 and f0 dry weigth from Blancoety al 2019:
wf <- .1
w0 <- .018

# Preflexion time:
pft <- log(wf/w0)/(0.0418 * temperature - 0.8355) * hr


# Put it in a table:
df2 <- data.frame(temperature, dt, yst, pft)
df2$cumult <- df2$dt + df2$yst + df2$pft


# Edit table: 
df2 <- df2 %>%
  
  mutate(metaephyrae = c(rep(.5, length(seq(from = min_temperature, to = max_temperature, by = .1))),    # Set metaephyrae densities
                         rep(1.1, length(seq(from = min_temperature, to = max_temperature, by = .1))),   
                         rep(4, length(seq(from = min_temperature, to = max_temperature, by = .1))),
                         rep(10, length(seq(from = min_temperature, to = max_temperature, by = .1))),
                         rep(40, length(seq(from = min_temperature, to = max_temperature, by = .1)))),
         
         cr = cr_23 * q10_cr^((temperature - 23)/10),                       # Calculate clearance rate
         er = metaephyrae * cr,                                      # Calculate encounter rate (i.e. volume of water cleared by all metaephyrae within 1 m^3)
         egg_survival = exp(-er * dt),                               # Calculate egg survival:
         larval_survival = exp(-er * (yst + pft)),                   # Calculate larval survival:
         cumul_survival = exp(-er * cumult))                         # Calculate survival to flexion:



# What is the range of egg survival when ephyrae density is 1.1?
df2 %>%
  filter(metaephyrae == 1.1) %>%
  dplyr::select(egg_survival) %>%
  range()


# What is the range survival to flexion when ephyrae density is 1.1, 10 and 40?
df2 %>%
  filter(metaephyrae == 1.1) %>%
  dplyr::select(cumul_survival) %>%
  range()

df2 %>%
  filter(metaephyrae == 10) %>%
  dplyr::select(cumul_survival) %>%
  range()

df2 %>%
  filter(metaephyrae == 40) %>%
  dplyr::select(cumul_survival) %>%
  range()


# Plot egg survival out:
p <- ggplot() + 
  geom_histogram(data = df, aes(x = temperature), binwidth = .5, alpha = .5) +
  geom_line(data = df2, aes(x = temperature, y = egg_survival * 80, colour = as.factor(metaephyrae))) +
  labs(x = "Temperature ?C", y = "Temperature frequency") +
  scale_color_discrete(name = expression("Metaephyrae m"^-3*"")) +
  scale_x_continuous(breaks = c(21, 24, 27, 30)) +
  scale_y_continuous(sec.axis = sec_axis(~./80, name = "Probability of egg survival")) +
  theme_bw()  +
  theme(panel.grid = element_blank())

png(filename = "plots/egg_survival_vs_temperature.png", width = 10, height = 9, units = "cm", res = 300)
p
dev.off()


# Plot survival to flexion out:
p <- ggplot() + 
  geom_histogram(data = df, aes(x = temperature), binwidth = .5, alpha = .5) +
  geom_line(aes(x = temperature, y = cumul_survival * 80, colour = as.factor(metaephyrae)), data = df2) +
  labs(x = "Temperature ?C", y = "Temperature frequency") +
  scale_color_discrete(name = expression("Metaephyrae m"^-3*"")) +
  scale_x_continuous(breaks = c(21, 24, 27, 30)) +
  scale_y_continuous(sec.axis = sec_axis(~./80, name = "Probability of survival to flexion")) +
  theme_bw()  +
  theme(panel.grid = element_blank())

png(filename = "plots/larval_survival_vs_temperature.png", width = 10, height = 9, units = "cm", res = 300)
p
dev.off()


###############################################################
# Field data:

# Add encounter rate per hour and per day for 1 prey m^-3 to df:
# Edit table:
df <- df %>%
  mutate(cr = cr_23 * q10_cr^((temperature - 23)/10),                       # Calculate clearance rate
    er = metaephyrae_dens *  cr,                                     # Calculate clearance rate
    erd = metaephyrae_dens *  cr * hr,                               # Calculate clearance rate per day
    ierd =  metaephyrae_dens * tthynnus_preflex_dens * cr * hr,      # Instantaneous encounter rate per day for observed prey densities to df
    dt = (8787.5 * df$temperature^-1.701)/hr,                               # Add development time
    egg_surv = exp(-erd * dt),                                       # Egg survival
    egg_death = 1 - egg_surv,                                        # Death
    yst = dq10 * q10^((tq10 - temperature)/10),                             # Yolk-sack duration
    pft = log(wf/w0)/(0.0418 * temperature - 0.8355),                       # Pre-flexion duration
    cumult = dt + yst + pft,                                         # Acumuulated yolk-sack + preflexion + flexion times
    cumul_surv = exp(-erd * cumult),                                 # Survival of the combined development time
    cumul_death = 1 - egg_surv)                                      # Death of the combined development time



# What is the mean egg survival?
mean(df$egg_surv)

# What is the mean survival of egg + larvae?
mean(df$cumul_surv)

# Mean and standard deviation of encounter rate:
mean(df$erd)
sd(df$erd)

# Greatest encounter rate:
max(df$erd)

# By year:
df %>% 
  group_by(year) %>%
  summarise(mean_erd_year = mean(erd),
            sd_erd_year = sd(erd))

# Probability of surviving and dying in 1 day:
df$field_survival_24h <- exp(-df$er * hr)
df$field_mortality_24h <- 1 - df$field_survival_24h

# Range of probability of mortality:
range(df$field_mortality_24h)

# Number of stations with instantaneous predation rate > 0.1:
nrow(subset(df, ierd > .1))

# Now do the same by year and compare with predation if all metaephyrae were homogeneously distributed:
df %>%
  group_by(year) %>%
  summarise(sum_ierd = sum(ierd),
            sum_tthynnus_preflex_dens = sum(tthynnus_preflex_dens),
            mean_metaephyrae_dens = mean(metaephyrae_dens),
            mean_tthynnus_preflex_dens = mean(tthynnus_preflex_dens),
            observed_predation_rate = sum_ierd / sum_tthynnus_preflex_dens,
            homogeneous_predation_rate = mean_tthynnus_preflex_dens * mean_metaephyrae_dens * cr_23 * hr / mean_tthynnus_preflex_dens,
            comparison = homogeneous_predation_rate / observed_predation_rate)


# Compare ephyrae densities in sites where there is zero fish (no recent spawning) vs sites with fish (spawning site):
df <- df %>%
  mutate(spawning_site = case_when(tthynnus_preflex_dens > 0 ~ 1,
                                   T ~ 0))

t.test(log(subset(df, spawning_site == 1)$metaephyrae_dens + 1), 
       log(subset(df, spawning_site == 0)$metaephyrae_dens + 1))


# Plot the density distribution of tuna larvae relative to predatory pressure:
# Egg development time (h) at 24?C
dt24 <- (8787.5 * 24^-1.701)

# Probability of surviving the egg development time:
df <- df %>%
  mutate(p_survival24_C = exp(-er * dt24)) 


# Plot it year by year and including back-calculated egg densities:
# back calculate egg densities:
df <- df %>%
  mutate(prey_dens_t0 = tthynnus_preflex_dens / p_survival24_C) 

# Plot it out:
p <- ggplot() +
  geom_point(data = df, aes(y = log10(tthynnus_preflex_dens), x = log10(erd)), alpha = .7) +
  geom_point(data = df, aes(y = log10(prey_dens_t0), x = log10(erd)), alpha = .7, shape = 1) +

  geom_line(data = df, aes(x = log10(erd), y = (p_survival24_C - 6/8) * 4), color = "red", linetype = "dashed") +
  
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1), 
                     label = (c("0.001", "0.01", "0.1", "1", "10")),
                     sec.axis = sec_axis(~ .* 1/4 + 6/8, name = "Probability of egg survival at 24?C", breaks = c(0,.5, 1))) +
  
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1), 
                     label = (c("0.0001", "0.001", "0.01", "0.1", "1", "10"))) +
  
  annotation_logticks() +
  
  labs(x = expression("Encounter rate (E) with increasing metaephyrae density (encounters d"^-1*")"), y = expression(paste(italic("T. thynnus "), "density (larvae m"^-3*")"))) +
  
  theme_bw() +
  
  theme(panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 0.8),
        legend.position = "none") +
  
  facet_wrap(~year)

png(filename = "plots/prey_abundance_vs_enconter_rate.png", width = 15, height = 10, units = "cm", res = 400)
p
dev.off()


# # Plot second x-axis:
# p <- ggplot() +
#   geom_point(data = df, aes(y = log10(tthynnus_preflex_dens), x = log10(metaephyrae_dens)), alpha = 0) +
# 
#     geom_line(data = df, aes(x = log10(erd), y = (p_survival24_C - 6/8) * 4), color = "red", linetype = "dashed", alpha = 0) +
#   
#   scale_y_continuous(breaks = c(1), 
#                      label = (c("1")),
#                      sec.axis = sec_axis(~ .* 1/4 + 6/8, name = "Probability of egg survival at 24?C", breaks = c(0,.5, 1))) +
#   
#   scale_x_continuous(breaks = c(-3, -2, -1, -0, 1), 
#                      label = (c("0.001", "0.01", "0.1", "1", "10"))) +
#   
#   annotation_logticks(sides = "b") +
#   
#   labs(x = expression(paste(italic("P. noctiluca "), "density (metaephyrae m"^-3*")")), y = expression(paste(italic("T. thynnus "), "density (larvae m"^-3*")"))) +
#   
#   theme_bw() +
#   
#   theme(axis.title.y.right = element_text(color = "red"),
#         axis.text.y.right = element_text(color = "red"),
#         axis.text.x = element_text(angle = 30, vjust = 1, hjust = 0.8),
#         legend.position = "none",
#         panel.grid = element_blank()) +
#   
#   facet_wrap(~year) 
# 
# png(filename = "plots/prey_abundance_vs_metaephyrae_density.png", width = 15, height = 10, units = "cm", res = 400)
# p
# dev.off()

#           END OF SCRIPT
#########################################