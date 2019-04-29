###############################################################################
#                                                                      April 19
#                     Gastric Lavage - Gut Fullness
#   
#
#  Notes:
#  * No fish mass was collected for the Lavage samples, could estimate this from
#  L-W
#
#  To Do:
#  *  There are a bunch of mass estimates that are comming out as NA -- > Track
#  this down
#
###############################################################################
# library(rstan)
# library(brms)
library(ggplot2)
# library(reshape2)
library(dplyr)
library(foodbase)
library(ggthemes)

# rstan_options(auto_write = TRUE)
# options (mc.cores=parallel::detectCores()) # Run on multiple cores

d.tmp = readDB(gear = "FishGut", type = "Sample", updater = TRUE)

d.1 = d.tmp[d.tmp$FishSpeciesID %in% c("RBT", "BNT"),]

#-----------------------------------------------------------------------------#
dat = sampspec(samp = d.1)

# cut down some of the columns in samples (below is the full set)
keep.col = c("PITTagID",
             "TripID",
             # "Region",
             # "Reach",
             "Date",
             "Time",
             "RiverMile",
             "Site",
             "SpeciesID",
             "ForkLength",
             "Weight")#,
# "FlagStrange",
# "FlagDelete",
# "EntererSample",
# "Processor",
# "ProcessDate",
# "ProcessTime",
# "QAQC",
# "Checker",
# "EntererSpecimen",
# "Notes"

ltl.samps = dat$Samples[,names(dat$Samples) %in% keep.col]

#--------------------------------------
# get mass
spec = dat$Biomass

spec$TotalMass = rowSums(spec[,3:23])

spec.ltl = spec[,c(1:2,24)]

diet = left_join(ltl.samps, spec.ltl, by = "PITTagID")

# Deal with the NAs !!!!!!!!!!!!!!

#-----------------------------------------------------------------------------#
# only the gastric lavage samples, start with 'G'
diet2 = diet[which(substr(diet$PITTagID, 1, 1) == "G"),]

diet2 = droplevels(diet2)

diet2$SpeciesID = as.character(diet2$SpeciesID)

tmp = group_by(diet2, PITTagID) %>%
      summarise(tot.mass = sum(TotalMass, na.rm = TRUE)/1000)

d.ltl = select(diet2, PITTagID, RiverMile, Date, ForkLength) %>%
        filter(!duplicated(PITTagID))

dat.in = left_join(d.ltl, tmp, by = "PITTagID")

# drop the fish without fork length
dat.in = dat.in[!is.na(dat.in$ForkLength),]

# code in the days of the week
dow = weekdays(dat.in$Date)

dat.in$dow = factor(dow, levels = c("Friday", "Saturday", "Sunday", "Monday"),
                   ordered = TRUE)
#-----------------------------------------------------------------------------#
# predict weight, given length
# email from Eldo, where Korman provides the a & b parms, don't handle the
# sigma, these are from a log ~ log regression
a = -11.1584485245053
b = 2.9485978095191
# sigma = 0.153933682398055

# tl = seq(10,400,10)

# pred.wt = a + b*log(tl)

dat.in$pred.wt = exp(a + b*log(dat.in$ForkLength))
#-----------------------------------------------------------------------------#
# fit some log normal models

dat.in$pred.wt.2 = rescale(log(dat.in$pred.wt))

fit.1 = lm(log(tot.mass) ~ pred.wt.2 + dow, data = dat.in)

new.dat = data.frame(dow = unique(dat.in$dow),
                     pred.wt.2 = 0)

pred = predict(fit.1, newdata = new.dat, se.fit = TRUE, type = "response")

new.dat$mean = exp(pred$fit)
new.dat$lower = exp(pred$fit - (1.96 * pred$se.fit))
new.dat$upper = exp(pred$fit + (1.96 * pred$se.fit))


#-----------------------------------------------------------------------------#
# plots


p = ggplot(new.dat, aes(x = dow, y = mean)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 1),
                  width = 0, size = .75) +
    # geom_jitter(data = dat.in, aes(x = dow, y = tot.mass), shape = 1, width = .25) +
    # scale_y_continuous(limits = c(0,2)) +
    labs(x = "", y = "Gut Fullness for Average Size Fish (g) +- 95% CI",
         title = "Lees Ferry - Rainbow Trout Diet Lavage \n (preliminary data - do not cite)") +
    theme_base() +
    theme(axis.text.x = element_text(size = 14))

p


#-----------------------------------------------------------------------------#
# end