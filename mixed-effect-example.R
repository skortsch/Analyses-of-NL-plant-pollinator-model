#libraries
library(tidyverse)
library(glmmTMB)
library(ggplot2)
library(DHARMa)

#wd

setwd("~/Dropbox/RProjects/NLmodelAnalyses/Data_output")

#dataframe
vis_data

#models
vis_mod1 <- glmmTMB(number_visits~seed_percent*conn+(1|run),
        family="nbinom1",
        data=vis_data)

#dharma residual checks
vis_res <- simulateResiduals(vis_mod1)
plotResiduals(vis_res)
testResiduals(vis_res)
testZeroInflation(vis_res)

vis_mod2 <- glmmTMB(number_visits~seed_percent*conn+(1|run),
                    family="nbinom1",
                    ziformula=~1,
                    data=vis_data)

AIC(vis_mod1,vis_mod2)

vis_res2 <- simulateResiduals(vis_mod2)
plotResiduals(vis_res2)
testResiduals(vis_res2)
testZeroInflation(vis_res2) #some indications of zero-inflation

vis_mod3 <- glmmTMB(number_visits~seed_percent*conn+(1|run),
                    family="nbinom1",
                    ziformula=~1,
                    data=vis_data)

#perhaps pollinator species as a random effect
#but could also be a fixed effect?

vis_mod4 <- glmmTMB(number_visits~seed_percent*conn+(1|run)+(1|pollinator_species),
                    family="nbinom1",
                    ziformula=~1,
                    data=vis_data)

#dharma residual checks
vis_res4 <- simulateResiduals(vis_mod4)
plotResiduals(vis_res4)
testResiduals(vis_res4)
testZeroInflation(vis_res4)

#you have a lot of data and Dharma checks are sensitive to large data sets i think

AIC(vis_mod4,
    vis_mod3,
    vis_mod2,
    vis_mod1)

###idea for plotting interaction between seed percent and connectance

###
prepplot <- as.data.frame(matrix(ncol = 3, nrow = 1000))
colnames(prepplot) <- c("seed_percent", "conn", "pred")
#colnames(prepplot) <- c("cent_v1", "cent_sd", "pred")
prepplot$seed_percent <- rep(seq(0,0.99, by=0.01),10)
prepplot <- prepplot[order(prepplot$seed_percent),]

#set REs to NA
prepplot$run <- NA
prepplot$pollinator_species <- NA
prepplot$conn <- rep(seq(0.1,1, by=0.1),100)

#pedictions extract
prepplot$pred <- predict(vis_mod4,
                           newdata = prepplot,
                           re.formula = NA,
                         type="response")

#plot
raster.surface.1 <- ggplot(prepplot,
                           aes(x = seed_percent, 
                               y = conn,
                               z = pred,
                               fill = pred)) + 
  geom_raster(interpolate = TRUE)  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0,face="italic",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=10),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0.25,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
       # strip.text = element_text(face = "italic",size=14,family="Helvetica"),
        panel.spacing = unit(0.5,"lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.4))+
  ylab("Connectance") + 
  xlab("Seed percentage") +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  labs(fill="Number of visits") +  scale_fill_viridis_c(option=c("magma"))

raster.surface.1
