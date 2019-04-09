###############################################################################
#                                                                      Feb 2019 
#           Logistic Regression of Fish Prey in BNT & RBT Diets
#
# Notes: 
# *
#  
###############################################################################
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')
library(dplyr)
library(foodbase)
library(lme4)
library(arm)
library(ggplot2)
library(bbmle)
library(gridExtra)

d.tmp = readDB(gear = "FishGut", type = "Sample", updater = FALSE)

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
             "FishSpeciesID",
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

# cut specimens down to only fish
fish.list = c("FEGG", "CYP", "SUC",	"FISH",	"BHS",	"FMS", "HBC",	"GSF", "SMB",
              "STB",	"RBT",	"FHM",	"SPD",	"BNT")

spec.1 = dat$Specimens[dat$Specimens$SpeciesID %in% fish.list,]

# get total counts
spec.1$TotalCount = rowSums(spec.1[,3:23])

spec.ltl = spec.1[,c(1:2,24)]

diet = left_join(ltl.samps, spec.ltl, by = "PITTagID")

#-----------------------------------------------------------------------------#

dat.1 = group_by(diet, PITTagID) %>%
        summarise(FishPrey = sum(TotalCount))

dat.in = left_join(ltl.samps, dat.1, by = "PITTagID")

dat.in$FishPrey.2 = ifelse(dat.in$FishPrey >= 1, 1, 0)

# drop the NA for forklength
dat.in = dat.in[!is.na(dat.in$ForkLength),]

# dat.in = dat.in[!is.na(dat.in$RiverMile),]

# drop bad RBT value
dat.in = dat.in[!dat.in$ForkLength > 1000,]

#-----------------------------------------------------------------------------#
# fit a little model set
fit.1 = glm(FishPrey.2 ~ ForkLength + FishSpeciesID, data = dat.in, family = binomial)
fit.2 = glm(FishPrey.2 ~ ForkLength, data = dat.in, family = binomial)
fit.3 = glm(FishPrey.2 ~ FishSpeciesID, data = dat.in, family = binomial)
fit.4 = glm(FishPrey.2 ~ 1, data = dat.in, family = binomial)

# River mile doesn't improve anything
# fit.5 = glm(FishPrey.2 ~ ForkLength + FishSpeciesID + RiverMile, data = dat.in, family = binomial)
# fit.6 = glm(FishPrey.2 ~ FishSpeciesID + RiverMile, data = dat.in, family = binomial)
# fit.7 = glm(FishPrey.2 ~ ForkLength  + RiverMile, data = dat.in, family = binomial)
# fit.8 = glm(FishPrey.2 ~ RiverMile, data = dat.in, family = binomial)

AIC(fit.1, fit.2, fit.3, fit.4)
BIC(fit.1, fit.2, fit.3, fit.4)
ICtab(fit.1, fit.2, fit.3, fit.4, type = c("AICc"))

#-----------------------------------------------------------------------------#
# mixed models? - No, don't think it's worth the complications
# fit.1 = glmer(FishPrey.2 ~ ForkLength + FishSpeciesID + (1|trip), data = dat.in, family = binomial)
# fit.2 = glmer(FishPrey.2 ~ ForkLength + (1|trip), data = dat.in, family = binomial)
# fit.3 = glmer(FishPrey.2 ~ FishSpeciesID + (1|trip), data = dat.in, family = binomial)
# fit.4 = glmer(FishPrey.2 ~  (1|trip), data = dat.in, family = binomial)
# ICtab(fit.1, fit.2, fit.3,  type = c("AICc"))

#-----------------------------------------------------------------------------#
# get predictions for both species + fork length

rbt.min = min(dat.in[dat.in$FishSpeciesID == "RBT",]$ForkLength)
rbt.max = max(dat.in[dat.in$FishSpeciesID == "RBT",]$ForkLength)
rbt.mean = mean(dat.in[dat.in$FishSpeciesID == "RBT",]$ForkLength)

bnt.min = min(dat.in[dat.in$FishSpeciesID == "BNT",]$ForkLength)
bnt.max = max(dat.in[dat.in$FishSpeciesID == "BNT",]$ForkLength)
bnt.mean = mean(dat.in[dat.in$FishSpeciesID == "BNT",]$ForkLength)

#--------------------------------------
# pred for the mean size
my.mean = data.frame(FishSpeciesID = c("RBT", "BNT"),
                     ForkLength = c(rbt.mean, bnt.mean))
mu.pred = predict(fit.1, newdata = my.mean, type = "link", se.fit = TRUE)

mu.pred.1 = invlogit(mu.pred$fit)
#--------------------------------------

new.dat = rbind(data.frame(FishSpeciesID = c("RBT"),
                           ForkLength = seq(rbt.min, rbt.max, 1)),
                data.frame(FishSpeciesID = c("BNT"),
                           ForkLength = seq(bnt.min, bnt.max, 1)))

tmp = predict(fit.1, newdata = new.dat, type = "link", se.fit = TRUE)

y = tmp$fit
y.l = y - (1.96 * tmp$se.fit)
y.u = y + (1.96 * tmp$se.fit)

new.dat$yhat = invlogit(y)
new.dat$y.l = invlogit(y.l)
new.dat$y.u = invlogit(y.u)

# Probability of 50% piscivory - BNT
new.dat[which(round(new.dat$yhat,2) == .5),]

# Probabiliy of piscivory at max size - BNT
new.dat[which(new.dat$ForkLength == bnt.max),]

# Probabiliy of piscivory at max size - BNT
new.dat[which(new.dat$ForkLength == rbt.max),]

#-----------------------------------------------------------------------------#
# plots of both species 

windows(record = T, xpos = 25, width = 8*1.5, height = 7*1.5)

new.dat$lab = ifelse(new.dat$FishSpeciesID == "RBT", "Rainbow Trout", "Brown Trout")

p1 = ggplot(new.dat, aes(x = ForkLength, y = yhat)) +
  geom_line(aes(color = lab), size = 1.5) +
  geom_ribbon(aes(ymin = y.l, ymax = y.u, fill = lab), alpha = 0.2) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(labels = c(0, 100, 200, 300, 400, 500, 600),
                     breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  labs(y = "Probability of Piscivory", x = "", title = "A") +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

g1 = p1 + theme(plot.margin = unit(c(1,1,0,1), "cm"),
                panel.spacing = unit(1, "lines"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # axis.title.x = element_text(size = 18, vjust = -.1),
                axis.title.y = element_text(size = 18, vjust = 4),
                axis.text.x = element_text(size = 16, colour = "black"),
                axis.text.y = element_text(size = 16, colour = "black"),
                axis.ticks = element_line(color = "black"),
                title = element_text(size = 18),
                legend.text = element_text(size = 16),
                legend.title = element_blank(),
                legend.position = c(.1,.9))
g1


#-----------------------------------------------------------------------------#
# length freq
dat.in$lab = ifelse(dat.in$FishSpeciesID == "RBT", "Rainbow Trout", "Brown Trout")

p2 = ggplot(dat.in, aes(x = ForkLength, fill = lab))+
  geom_histogram(aes(y = 100 *..density..), color = "gray", position = "dodge") +
  scale_x_continuous(labels = c(0, 100, 200, 300, 400, 500, 600),
                     breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  labs(y = "Relative Frequency", x = "Fork Length (mm)", title = "B") +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) 

g2 = p2 + theme(plot.margin = unit(c(0,1,1,1), "cm"),
              panel.spacing = unit(1, "lines"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.x = element_text(size = 18, vjust = -2),
              axis.title.y = element_text(size = 18, vjust = 4),
              axis.text.x = element_text(size = 16, colour = "black"),
              axis.text.y = element_text(size = 16, colour = "black"),
              axis.ticks = element_line(color = "black"),
              title = element_text(size = 18),
              legend.text = element_text(size = 16),
              legend.title = element_blank(),
              legend.position = c(.1,.8))
g2


grid.arrange(g1, g2, ncol = 1, heights = c(2/3, 1/3))



# old plotting code
#-----------------------------------------------------------------------------#
# Brown trout section
# bnt = dat.in[dat.in$SpeciesID == "BNT",]
# bnt$UpDown = ifelse(bnt$RiverMile <= 0, "Up", "Down")
# 
# bnt = bnt[which(!is.na(bnt$UpDown)),]
# 
# fit.1 = glm(FishPrey.2 ~ ForkLength, data = bnt, family = binomial)
# fit.2 = glm(FishPrey.2 ~ ForkLength + UpDown, data = bnt, family = binomial)
# AIC(fit.1, fit.2)
# BIC(fit.1, fit.2)
# ICtab(fit.1, fit.2, type = c("AICc"))
# 
# new.dat = data.frame(ForkLength = seq(1, 630, 1))
# # new.dat.2 = data.frame(expand.grid(ForkLength = seq(1, 630, 1),
# #                                    UpDown = c("Up", "Down")))
# 
# tmp = predict(fit.1, newdata = new.dat, type = "link", se.fit = TRUE)
# # tmp = predict(fit.2, newdata = new.dat.2, type = "link", se.fit = TRUE)
# 
# y = tmp$fit
# y.l = y - (1.96 * tmp$se.fit)
# y.u = y + (1.96 * tmp$se.fit)
# 
# 
# new.dat$yhat = invlogit(y)
# new.dat$y.l = invlogit(y.l)
# new.dat$y.u = invlogit(y.u)
# 
# # new.dat.2$yhat = invlogit(y)
# # new.dat.2$y.l = invlogit(y.l)
# # new.dat.2$y.u = invlogit(y.u)
# 
# # plot(y = new.dat$yhat, x = new.dat$ForkLength, ylim = c(0,1), type = 'l')
# # lines(y = new.dat$y.l, x = new.dat$ForkLength, col = 'red')
# # lines(y = new.dat$y.u, x = new.dat$ForkLength, col = 'red')
# # 
# # up = new.dat.2[new.dat.2$UpDown == "Up",]
# # 
# # plot(y = up$yhat, x = up$ForkLength, ylim = c(0,1), type = 'l')
# # lines(y = up$y.l, x = up$ForkLength, col = 'red')
# # lines(y = up$y.u, x = up$ForkLength, col = 'red')
# # 
# # down = new.dat.2[new.dat.2$UpDown == "Down",]
# # 
# # lines(y = down$yhat, x = down$ForkLength, ylim = c(0,1), type = 'l')
# # lines(y = down$y.l, x = down$ForkLength, col = 'blue')
# # lines(y = down$y.u, x = down$ForkLength, col = 'blue')
# 
# bnt.dat = new.dat
# bnt.dat$Species = "Brown Trout"
# #-----------------------------------------------------------------------------#
# 
# windows(record = T, xpos = 25, width = 8*1.5, height = 7*1.5)
# 
# # bnt.dat$UpDown.2 = ifelse(bnt.dat$UpDown == "Up", "Upstream of Lees", "Downstream of Lees")
# 
# p = ggplot(bnt.dat, aes(x = ForkLength, y = yhat)) +
#   # geom_line(aes(color = UpDown.2), size = 1.5) +
#   geom_line(size = 1.5) +
#   # geom_ribbon(aes(ymin = y.l, ymax = y.u, fill = UpDown.2), alpha = 0.2) +
#   geom_ribbon(aes(ymin = y.l, ymax = y.u), alpha = 0.2) +
#   scale_y_continuous(limits = c(0,1)) +
#   scale_x_continuous(labels = c(0, 100, 200, 300, 400, 500, 600),
#                      breaks = c(0, 100, 200, 300, 400, 500, 600)) +
#   labs(y = "Probability of Piscivory", x = "Fork Length (mm)", title = "Brown Trout (n = 271)")
# 
# p
# 
# G = p + theme(axis.title.x = element_text(size = 16, vjust = -.1),
#               axis.title.y = element_text(size = 16, vjust = 1),
#               axis.text.x = element_text(size = 14, colour = "black"),
#               axis.text.y = element_text(size = 14, colour = "black"),
#               panel.background = element_rect(fill = "white"),
#               panel.grid.minor = element_line(colour = "white"),
#               panel.grid.major = element_line(colour = "white"),
#               panel.border = element_rect(colour = "black", fill = NA),
#               panel.spacing = unit(.8, "lines"),
#               strip.background = element_blank(),
#               strip.text = element_text(size = 18, vjust = 1),
#               title = element_text(size = 16),
#               legend.text = element_text(size = 14),
#               legend.title.align = .5,
#               legend.position = c(.15,.9),
#               legend.title = element_blank())
# G
# 
# #-----------------------------------------------------------------------------#
# # Rainbow trout section
# rbt = dat.in[dat.in$SpeciesID == "RBT",]
# rbt = rbt[rbt$ForkLength != 2287,] # bad, remove
# 
# fit.1 = glm(FishPrey.2 ~ ForkLength, data = rbt, family = binomial)
# 
# new.dat = data.frame(ForkLength = seq(1, max(rbt$ForkLength, na.rm = TRUE), 1))
# 
# tmp = predict(fit.1, newdata = new.dat, type = "link", se.fit = TRUE)
# 
# y = tmp$fit
# y.l = y - (1.96 * tmp$se.fit)
# y.u = y + (1.96 * tmp$se.fit)
# 
# new.dat$yhat = invlogit(y)
# new.dat$y.l = invlogit(y.l)
# new.dat$y.u = invlogit(y.u)
# 
# rbt.dat = new.dat
# rbt.dat$Species = "Rainbow Trout"
# #-----------------------------------------------------------------------------#
# # Plot both together
# 
# all.dat = rbind(rbt.dat, bnt.dat)
# 
# all.dat$Sp.lab = ifelse(all.dat$Species == "Brown Trout", "Brown Trout (n = 271)", "Rainbow Trout (n = 1248)")
# 
# 
# p2 = ggplot(all.dat, aes(x = ForkLength, y = yhat)) +
#   geom_line(aes(color = Species), size = 1.5) +
#   geom_ribbon(aes(ymin = y.l, ymax = y.u, fill = Species), alpha = 0.2) +
#   scale_y_continuous(limits = c(0,1)) +
#   scale_x_continuous(labels = c(0, 100, 200, 300, 400, 500, 600),
#                      breaks = c(0, 100, 200, 300, 400, 500, 600)) +
#   # labs(y = "Probability of Piscivory", x = "Fork Length (mm)", title = "") +
#   labs(y = "Probability of Piscivory", x = "", title = "") +
#   facet_wrap(~ Sp.lab, scales = "free_x")
# 
# G2 = p2 + theme(axis.title.x = element_text(size = 16, vjust = -.1),
#                 axis.title.y = element_text(size = 16, vjust = 1),
#                 axis.text.x = element_text(size = 14, colour = "black"),
#                 axis.text.y = element_text(size = 14, colour = "black"),
#                 panel.background = element_rect(fill = "white"),
#                 panel.grid.minor = element_line(colour = "white"),
#                 panel.grid.major = element_line(colour = "white"),
#                 panel.border = element_rect(colour = "black", fill = NA),
#                 panel.spacing = unit(.8, "lines"),
#                 strip.background = element_blank(),
#                 strip.text = element_text(size = 18, vjust = 1),
#                 title = element_text(size = 16),
#                 legend.text = element_text(size = 14),
#                 legend.title = element_text(size = 14),
#                 legend.title.align = .5,
#                 legend.position = "none")
# G2
# 
# 
# #-----------------------------------------------------------------------------#
# 
# all.dat.2 = rbind(rbt, bnt[,-12])
# all.dat.3 = all.dat.2[which(!is.na(all.dat.2$ForkLength)),]
# 
# 
# 
# p3 = ggplot(all.dat.3, aes(x = ForkLength))+
#      geom_histogram(aes(fill = SpeciesID), color = "gray") +
#      labs(y = "Count", x = "Fork Length (mm)") +
#      facet_wrap(~ SpeciesID, scales = "free")
# 
# G3 = p3 + theme(axis.title.x = element_text(size = 16, vjust = -.1),
#                 axis.title.y = element_text(size = 16, vjust = 1),
#                 axis.text.x = element_text(size = 14, colour = "black"),
#                 axis.text.y = element_text(size = 14, colour = "black"),
#                 panel.background = element_rect(fill = "white"),
#                 panel.grid.minor = element_line(colour = "white"),
#                 panel.grid.major = element_line(colour = "white"),
#                 panel.border = element_rect(colour = "black", fill = NA),
#                 panel.spacing = unit(.8, "lines"),
#                 strip.background = element_blank(),
#                 # strip.text = element_text(size = 18, vjust = 1),
#                 strip.text = element_blank(),
#                 title = element_text(size = 16),
#                 legend.text = element_text(size = 14),
#                 legend.title = element_text(size = 14),
#                 legend.title.align = .5,
#                 legend.position = "none")
# G3
# 
# # grid.arrange(G2, arrangeGrob(G3, G4, heights = c(1/2, 1/2), ncol = 1), ncol = 2, widths = c(2/3, 1/3))
# grid.arrange(G2, G3, ncol = 1, heights = c(2/3, 1/3))


#-----------------------------------------------------------------------------#
#End
