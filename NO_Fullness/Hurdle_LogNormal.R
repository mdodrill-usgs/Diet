###############################################################################
#                                                                      April 19
#                 Gut Fullness - Log-Normal Hurdle Models 
#  
#  Notes:
#  *
#
#  To Do:
#  * 
#
###############################################################################
library(rstan)
library(brms)
# library(ggplot2)
library(dplyr)
# library(foodbase)
# library(ggthemes)
library(tidybayes)
library(modelr)
# library(suncalc)
# library(arm)

rstan_options(auto_write = TRUE)
options (mc.cores=parallel::detectCores()) # Run on multiple cores

#-----------------------------------------------------------------------------#
# fit some models 


fit.1 = brm(bf(est.mass.g ~ density.2 + (1|no.trip.site), hu ~ density.2 + (1|no.trip.site)), 
            data = dat.in, 
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)

fit.2 = brm(bf(est.mass.g ~ ts.mean.2 + (1|no.trip.site), hu ~ ts.mean.2 + (1|no.trip.site)), 
            data = dat.in, 
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)


waic(fit.1, fit.2)
# loo(fit)

#-----------------------------------------------------------------------------#
# fit a model set...

# fit.1 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
#                hu ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + drift.mass.2 + (1|no.trip.site)), 
#             data = dat.in, 
#             family = "hurdle_lognormal",
#             chains = 3, iter = 1000)

# this is the best model from the prior work
fit.1 = brm(bf(est.mass.g ~ ts.mean.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
               hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)


#-----------------------------------------------------------------------------#
# fit a model set...

fit.a.1 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ 1 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.1 = loo(fit.a.1)

fit.a.2 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ ts.mean.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.2 = loo(fit.a.2)

fit.a.3 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ density.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.3 = loo(fit.a.3)

fit.a.4 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ mean.temp.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.4 = loo(fit.a.4)

fit.a.5 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ tss.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.5 = loo(fit.a.5)

fit.a.6 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ drift.mass.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.6 = loo(fit.a.6)



# 2 variable models for hu
fit.a.7 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ ts.mean.2 + density.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.7 = loo(fit.a.7)

fit.a.8 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ ts.mean.2 + mean.temp.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.8 = loo(fit.a.8)

fit.a.9 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ ts.mean.2 + tss.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.9 = loo(fit.a.9)

fit.a.10 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
               hu ~ ts.mean.2 + drift.mass.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.10 = loo(fit.a.10)

fit.a.11 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ density.2 + mean.temp.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.11 = loo(fit.a.11)

fit.a.12 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ density.2 + tss.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.12 = loo(fit.a.12)

fit.a.13 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ density.2 + drift.mass.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.13 = loo(fit.a.13)

fit.a.14 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ mean.temp.2 + tss.2  + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.14 = loo(fit.a.14)

fit.a.15 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ mean.temp.2 + drift.mass.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.15 = loo(fit.a.15)

fit.a.16 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ tss.2 + drift.mass.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.16 = loo(fit.a.16)



# 3 variable models 
fit.a.17 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + mean.temp.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.17 = loo(fit.a.17)

fit.a.18 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + tss.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.18 = loo(fit.a.18)

fit.a.19 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.19 = loo(fit.a.19)

fit.a.20 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + mean.temp.2 + tss.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.20 = loo(fit.a.20)

fit.a.21 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + mean.temp.2  + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.21 = loo(fit.a.21)

fit.a.22 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + tss.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.22 = loo(fit.a.22)

fit.a.23 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ density.2 + mean.temp.2 + tss.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.23 = loo(fit.a.23)

fit.a.24 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ density.2 + mean.temp.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.24 = loo(fit.a.24)

fit.a.25 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ density.2 + tss.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.25 = loo(fit.a.25)

fit.a.26 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ mean.temp.2 + tss.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.26 = loo(fit.a.26)



# hu ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + drift.mass.2 + (1|no.trip.site)),
# 4 variable models 

fit.a.27 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ density.2 + mean.temp.2 + tss.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.27 = loo(fit.a.27)

fit.a.28 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + mean.temp.2 + tss.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.28 = loo(fit.a.28)

fit.a.29 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + tss.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.29 = loo(fit.a.29)

fit.a.30 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + mean.temp.2 + drift.mass.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.30 = loo(fit.a.30)

fit.a.31 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + (1|no.trip.site)), 
            data = dat.in,
            family = "hurdle_lognormal",
            chains = 3, iter = 1000)
loo.a.31 = loo(fit.a.31)


# 5 variable models
fit.a.32 = brm(bf(est.mass.g ~ 1 + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + drift.mass.2 + (1|no.trip.site)),
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)
loo.a.32 = loo(fit.a.32)


waic(fit.a.1, fit.a.2, fit.a.3, fit.a.4, fit.a.5, fit.a.6, fit.a.7, fit.a.8,
     fit.a.9, fit.a.10, fit.a.11, fit.a.12, fit.a.13, fit.a.14, fit.a.15,
     fit.a.16, fit.a.17, fit.a.18, fit.a.19, fit.a.20, fit.a.21,fit.a.22,
     fit.a.23, fit.a.24, fit.a.25, fit.a.26, fit.a.27, fit.a.28, fit.a.29,
     fit.a.30, fit.a.31, fit.a.32)


loo_compare(loo.a.1, loo.a.2, loo.a.3, loo.a.4, loo.a.5, loo.a.6, loo.a.7, loo.a.8,
            loo.a.9, loo.a.10, loo.a.11, loo.a.12, loo.a.13, loo.a.14, loo.a.15,
            loo.a.16, loo.a.17, loo.a.18, loo.a.19, loo.a.20, loo.a.21,loo.a.22,
            loo.a.23, loo.a.24, loo.a.25, loo.a.26, loo.a.27, loo.a.28, loo.a.29,
            loo.a.30, loo.a.31, loo.a.32)


# old.loo = list(loo.a.1, loo.a.2, loo.a.3, loo.a.4, loo.a.5, loo.a.6, loo.a.7, loo.a.8,
#             loo.a.9, loo.a.10, loo.a.11, loo.a.12, loo.a.13, loo.a.14, loo.a.15,
#             loo.a.16, loo.a.17, loo.a.18, loo.a.19, loo.a.20, loo.a.21,loo.a.22,
#             loo.a.23, loo.a.24, loo.a.25, loo.a.26, loo.a.27, loo.a.28, loo.a.29,
#             loo.a.30, loo.a.31, loo.a.32)




loo(fit.a.12, reloo = TRUE)

dat.in[c(850,1147,1148),]

out = lapply(fit.list, loo())

#-----------------------------------------------------------------------------#
# fit a model set... log-normal

fit.b.1 = brm(bf(est.mass.g ~ 1 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.2 = brm(bf(est.mass.g ~ ts.mean.2 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.3 = brm(bf(est.mass.g ~ density.2 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.4 = brm(bf(est.mass.g ~ mean.temp.2  + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.5 = brm(bf(est.mass.g ~ tss.2 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.6 = brm(bf(est.mass.g ~ drift.mass.2 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)



# 2 variable models for hu
fit.b.7 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.8 = brm(bf(est.mass.g ~ ts.mean.2 + mean.temp.2 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.9 = brm(bf(est.mass.g ~ ts.mean.2 + tss.2 + log(Weight) + (1|no.trip.site),
                 hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
              data = dat.in,
              family = "hurdle_lognormal",
              chains = 3, iter = 1000)

fit.b.10 = brm(bf(est.mass.g ~ ts.mean.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.11 = brm(bf(est.mass.g ~ density.2 + mean.temp.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.12 = brm(bf(est.mass.g ~ density.2 + tss.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.13 = brm(bf(est.mass.g ~ density.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.14 = brm(bf(est.mass.g ~ mean.temp.2 + tss.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.15 = brm(bf(est.mass.g ~ mean.temp.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.16 = brm(bf(est.mass.g ~ tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)



# 3 variable models 
fit.b.17 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + mean.temp.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.18 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + tss.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.19 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.20 = brm(bf(est.mass.g ~ ts.mean.2 + mean.temp.2 + tss.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.21 = brm(bf(est.mass.g ~ ts.mean.2 + mean.temp.2  + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.22 = brm(bf(est.mass.g ~ ts.mean.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.23 = brm(bf(est.mass.g ~ density.2 + mean.temp.2 + tss.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.24 = brm(bf(est.mass.g ~ density.2 + mean.temp.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.25 = brm(bf(est.mass.g ~ density.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.26 = brm(bf(est.mass.g ~ mean.temp.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)



# hu ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + drift.mass.2 + (1|no.trip.site)),
# 4 variable models 

fit.b.27 = brm(bf(est.mass.g ~ density.2 + mean.temp.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.28 = brm(bf(est.mass.g ~ ts.mean.2 + mean.temp.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.29 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.30 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + mean.temp.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)

fit.b.31 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)


# 5 variable models
fit.b.32 = brm(bf(est.mass.g ~ ts.mean.2 + density.2 + mean.temp.2 + tss.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
                  hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)), 
               data = dat.in,
               family = "hurdle_lognormal",
               chains = 3, iter = 1000)


waic(fit.b.1, fit.b.2, fit.b.3, fit.b.4, fit.b.5, fit.b.6, fit.b.7, fit.b.8,
     fit.b.9, fit.b.10, fit.b.11, fit.b.12, fit.b.13, fit.b.14, fit.b.15,
     fit.b.16, fit.b.17, fit.b.18, fit.b.19, fit.b.20, fit.b.21,fit.b.22,
     fit.b.23, fit.b.24, fit.b.25, fit.b.26, fit.b.27, fit.b.28, fit.b.29,
     fit.b.30, fit.b.31, fit.b.32)


#-----------------------------------------------------------------------------#

