###############################################################################
#                                                                        May 19
#                 Gut Fullness - Plotting
#  
#  Notes:
#  * Run Data_Prep first...
#                     
#
#  To Do:
#  * 
#
###############################################################################
library(ggplot2)
library(gridExtra)
library(grid)
windows(record = T, xpos = 25)

#-----------------------------------------------------------------------------#
my_theme =  theme(axis.title.x = element_text(size = 18, vjust = -2),
                  axis.title.y = element_text(size = 18, vjust = 4),
                  axis.text.x = element_text(size = 16, colour = "black"),
                  axis.text.y = element_text(size = 16, colour = "black"),
                  # panel.background = element_rect(fill = "white"),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  # panel.border = element_rect(colour = "black", fill = NA),
                  # panel.spacing = unit(1, "lines"),
                  plot.margin = unit(c(.75,.75,.75,.75), "cm"),
                  # strip.background = element_blank(),
                  # strip.text = element_text(size = 14, vjust = 1),
                  title = element_text(size = 18),
                  legend.text = element_text(size = 16),
                  legend.key = element_rect(fill = "white"),
                  legend.title = element_text(size = 16))

#-----------------------------------------------------------------------------#
# this is the best model from the prior work
# dat.in$Weight.2 = log(dat.in$Weight)

fit.1 = brm(bf(est.mass.g ~ ts.mean.2 + drift.mass.2 + log(Weight) + (1|no.trip.site),
               hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)),
            data = dat.in,
            family = "hurdle_lognormal")

# this is just using turbidity, not log turbidity... fit.1 is better
# fit.2 = brm(bf(est.mass.g ~ ts.mean.3 + drift.mass.2 + log(Weight) + (1|no.trip.site),
#                hu ~ ts.mean.2 + density.2 + drift.mass.2 + (1|no.trip.site)),
#             data = dat.in,
#             family = "hurdle_lognormal")
# 
# loo(fit.1, fit.2)
# waic(fit.1, fit.2)


windows(record = TRUE, xpos = 25)

plot(marginal_effects(fit.1, method = "fitted"), ask = FALSE)

list_of_data <- marginal_effects(fit.1)


#-----------------------------------------------------------------------------#
# Turbidity
dat.ts.mean = list_of_data$ts.mean.2

p = ggplot(dat.ts.mean, aes(y = estimate__, x = ts.mean.2)) +
  geom_ribbon(aes(ymax = upper__, ymin = lower__), fill = "gray50", alpha = .2) +
  geom_line(color = "black", size = 1.5) +
  labs(y = "Gut Fullness - log(g)", x = "Turbidity - log(FNU)") +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
p

text = textGrob("b", gp = gpar(fontsize = 18))

g1 = p + my_theme + annotation_custom(grob = text, xmin = 1.6, xmax = 1.6, ymin = .335, ymax = .335)
g1 = g1 + theme(plot.margin = unit(c(.75,0,.75,0), "cm"))

#--------------------------------------
# drift
dat.drift.mass = list_of_data$drift.mass.2

p = ggplot(dat.drift.mass, aes(y = estimate__, x = drift.mass.2)) +
  geom_ribbon(aes(ymax = upper__, ymin = lower__), fill = "gray50", alpha = .2) +
  geom_line(color = "black", size = 1.5) +
  # labs(y = "Gut Fullness - log(g)", x = expression(paste('Drift Mass (g / m'^'3',')'))) +
  labs(y = "", x = expression(paste('Drift Mass (g / m'^'3',')'))) +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
p

text = textGrob("c", gp = gpar(fontsize = 18))

g2 = p + my_theme + annotation_custom(grob = text, xmin = 1.95, xmax = 1.95, ymin = 1.75, ymax = 1.75)
g2 = g2 + theme(plot.margin = unit(c(.75,.5,.75,0), "cm"))


#--------------------------------------
# RBT Density
dat.density = list_of_data$density.2

p = ggplot(dat.density, aes(y = estimate__, x = density.2)) +
  geom_ribbon(aes(ymax = upper__, ymin = lower__), fill = "gray50", alpha = .2) +
  geom_line(color = "black", size = 1.5) +
  labs(y = "Gut Fullness - log(g)", x = "Rainbow Trout Density (Fish / m)") +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
p

text = textGrob("d", gp = gpar(fontsize = 18))

g3 = p + my_theme + annotation_custom(grob = text, xmin = 1.35, xmax = 1.35, ymin = .1125, ymax = .1125)
g3 = g3 + theme(plot.margin = unit(c(.75,0,.75,0), "cm"))


#--------------------------------------
# Weight
dat.weight = list_of_data$Weight

p = ggplot(dat.weight, aes(y = estimate__, x = Weight)) +
  geom_ribbon(aes(ymax = upper__, ymin = lower__), fill = "gray50", alpha = .2) +
  geom_line(color = "black", size = 1.5) +
  # labs(y = "Gut Fullness - log(g)", x = "Fish Weight (g)") +
  labs(y = "", x = "Fish Weight (g)") +
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
p

text = textGrob("e", gp = gpar(fontsize = 18))

g4 = p + my_theme + annotation_custom(grob = text, xmin = 2000, xmax = 2000, ymin = .29, ymax = .29)
g4 = g4 + theme(plot.margin = unit(c(.75,.5,.75,0), "cm")) #t,r,b,l


#-----------------------------------------------------------------------------#
fix = as.data.frame(fixef(fit.1))

fix$Parm = row.names(fix)

fix.2 = fix[which(!fix$Parm %in% c("Intercept", "hu_Intercept")),]

fix.2$tmp = c("Turbidity", "Drift \n Mass", "Fish \n Weight", "Hurdle \n Turbidity",
                "Hurdle \n Rainbow \n Density", "Hurdle \n Drift Mass") 
fix.2$names = factor(fix.2$tmp,
                     levels = c("Turbidity", "Drift \n Mass", "Fish \n Weight", "Hurdle \n Turbidity",
                                "Hurdle \n Rainbow \n Density", "Hurdle \n Drift Mass"),
                     ordered = T)


p = ggplot(fix.2, aes(x = names, y = Estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_point() +
    geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5)) +
    labs(x = "", y = "Effect Size (+- 95% CRI)") +
    coord_flip() +
    theme_minimal() +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
p

text = textGrob("a", gp = gpar(fontsize = 18))

g.tmp = p + my_theme + annotation_custom(grob = text, xmin = 6.5, xmax = 6.5, ymin = 4, ymax = 4)

g = g.tmp + theme(plot.margin = unit(c(.75,0,.75,0), "cm"))
#-----------------------------------------------------------------------------#
# CJFAS: a two-column illustration is 18.2 cm × 23.7 cm (7.2 in. × 9.3 in.)
# windows(record = T, xpos = 25, height = 7.2*1.5, width = 7.2*2)


# grid.arrange(g1, g2, g3, g4, ncol = 2)

grid.arrange(g, arrangeGrob(g1, g2, g3, g4, ncol = 2), ncol = 2, widths = c(1/3, 2/3))



#-----------------------------------------------------------------------------#
# End