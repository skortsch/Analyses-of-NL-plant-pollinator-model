#libraries
library(tidyverse)
library(glmmTMB)
library(ggplot2)
library(DHARMa)
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions

#wd
setwd("C:/LocalData/susakort/pollinatorsNL-main_18/R_scripts")

#data 
vis_data<-read.csv("Data/NLdata_ex_2_prefs.csv", header=TRUE) #import data

#vis_data<- vis_data %>% rowwise() %>% mutate(pref.diff = abs(plant.pref -  prefs_after_vis))
vis_data<- vis_data %>% rowwise() %>% mutate(pref.diff =  prefs_after_vis - plant.pref)

#number of links per pollinator and plant
vis_data<- vis_data %>%  arrange (run, pollinator_species) %>%  group_by(run, seed_percent, pollinator_species) %>% mutate(pol.links = n())
vis_data<- vis_data %>% arrange (run, pollinator_species) %>%  group_by(run, seed_percent, plant_species) %>% mutate(pl.links = n())

#remove NAs
rm.na<-which(is.na(vis_data$pl.dens))
vis_data<-vis_data[-rm.na,]

df.vis.sum.poll<-vis_data %>% group_by(run, seed_percent, pollinator_species, 
                pol.soc, pol.bm, plant_species, pl.dens,plant.pref, prefs_after_vis, pref.diff, pol.links, pl.links) %>% 
                summarise(pl.pref=mean(plant.pref), number_visits = sum(number_visits), cons= sum(cons), 
                pvis=sum(pvis), connectance=mean(conn), tot.plant.dens=mean(plant.density)) 



#add number of links per pollinator and plant to the dataframe
#df.vis.sum.poll<-cbind(df.vis.sum.poll, pol.links=poll.links$pol.links, pl.links=plant.links$pl.links)



niche.max <- df.vis.sum.poll %>% group_by(run, pollinator_species) %>% filter(plant.pref == max(plant.pref)) 
  
niche.min <- df.vis.sum.poll %>% group_by(run, pollinator_species) %>% filter(plant.pref == min(plant.pref)) 

niche.max1<-subset(niche.max, !pref.diff==0 & !pref.diff==-1)#remove rows with zero diff


ggplot(niche.max, aes(x=plant.pref, y=pref.diff)) + geom_point(alpha=0.5)+
  geom_smooth(col="red", method=lm)+stat_cor() 


ggplot(niche.max, aes(x=log(seed_percent), y=pref.diff)) + geom_point(alpha=0.5)+
  geom_smooth(col="red", method=lm)+
  stat_cor() 


#obligate  specialists no difference
niche_0<-subset(niche.max, pref.diff==0)

plot_niche0<-ggplot(niche_0, aes(x=seed_percent, y=pref.diff)) + geom_point(alpha=0.5)+
  labs(x = "Seed percentage", y = "Difference in preference") +
  theme_classic() + 
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12))+
        ggtitle("Obligate specialists 11%") 


#obligate  specialists no flowers encountered
niche_neg1<-subset(niche.max, pref.diff==-1)
plot_niche.neg1<-ggplot(niche_neg1, aes(x=seed_percent, y=pref.diff)) + geom_point(alpha=0.5)+
  labs(x = "Seed percentage", y = "Difference in preference") +
  theme_classic() + 
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),
        axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12))+
  ggtitle("Obligate specialists 0.2%") 


Fig_SI_obligate_specialists<-ggarrange(plot_niche0, plot_niche.neg1 + rremove("ylab"), labels = c("a", "b"), ncol = 2,
                            font.label = list(size = 15, color = "black", family = NULL, position = "top"))
ggsave(paste0(dirF, "Fig_SI_obligate_specialists.png"),width=10, height = 6, units="in", dpi=600 ) 


############################


mod_niche_1 <- glmmTMB(pref.diff~seed_percent*pl.dens+(1|run),
                        family="gaussian",
                        ziformula=~1,
                        data=niche.max1)

mod_niche_2 <- glmmTMB(pref.diff~seed_percent*plant.pref*pl.dens+(1|run),
                        family="gaussian",
                        ziformula=~1,
                        data=niche.max1)


mod_niche_3<- glmmTMB(pref.diff~seed_percent*pol.links*pl.dens+(1|run),
                        family="gaussian",
                        ziformula=~1,
                        data=niche.max1)

anova(mod_niche_1, mod_niche_2, mod_niche_3)
AIC(mod_niche_1, mod_niche_2, mod_niche_3)

hist(residuals(mod_niche_4, type = c("response")))

summary(mod_niche_1b)
summary(mod_niche_2b)
summary(mod_niche_3)

#dharma residual checks
#goodness of fit
res_4<- simulateResiduals(mod_niche_4, 200)
#plot(simulationOutput)

plotSimulatedResiduals(res_4)
testUniformity(res_4)
plotResiduals(res_4)
testResiduals(res_4)
#testZeroInflation(res_4)

#WHY does the K.S test look significant, is it a problem?


#PLOT
pred <- expand.grid(seed_percent=seq(0.00001,1,0.0001),  plant.pref=c(0.2, 0.4, 0.8), connectance=c(0.25, 0.5, 0.75,  1), pl.dens=c(0.1, 0.5, 0.9))
pred$run<-NA
#pred$pollinator_species<-NA
pred$y <- predict(mod_niche_4, pred, type="response",allow.new.levels=TRUE)
plot_niche_change<-ggplot(pred,aes(x=seed_percent,y=y,color=factor(connectance),group=connectance))+geom_line()+ 
  geom_hline(yintercept=0, linetype="dotted", color = "black", size=1)+
  facet_grid(cols=vars(plant.pref), rows=vars(pl.dens), labeller ="label_both", scales = "fixed") +
  theme_light()+  ylab("Preference change")+xlab("Seed percentage") +labs(col = "connectance") +theme(strip.text.x = element_text(size = 12))
plot_niche_change



pred <- expand.grid(seed_percent=seq(0.00001,1,0.0001),  plant.pref=c(0.2, 0.4, 0.8), pol.links=c(2, 4, 6), pl.dens=c(0.1, 0.5, 0.9))
pred$run<-NA
#pred$pollinator_species<-NA
pred$y <- predict(mod_niche_3, pred, type="response",allow.new.levels=TRUE)
plot_niche_change<-ggplot(pred,aes(x=seed_percent,y=y,color=factor(pol.links),group=pol.links))+geom_line()+ 
  geom_hline(yintercept=0, linetype="dotted", color = "black", size=1)+
  facet_grid(cols=vars(plant.pref), rows=vars(pl.dens), labeller ="label_both", scales = "fixed") +
  theme_light()+  ylab("Preference change")+xlab("Habitat plant clumpiness-mixing") +labs(col = "pollinator degree") +theme(strip.text.x = element_text(size = 12))
plot_niche_change


ggsave(paste0(dirF, "plot_niche_change.png"),width=10, height = 6, units="in", dpi=600 ) 



#RASTER PLOT
###
prepplot <- as.data.frame(matrix(ncol = 3, nrow = 100000))
#colnames(prepplot) <- c("seed_percent", "connectance", "pred")
colnames(prepplot) <- c("seed_percent", "pol.links", "pred")
#colnames(prepplot) <- c("seed_percent", "plant.pref", "pred")
#colnames(prepplot) <- c("cent_v1", "cent_sd", "pred")
prepplot$seed_percent <- rep(seq(0.00001,0.99999, by=0.0001),10)
prepplot <- prepplot[order(prepplot$seed_percent),]

#set REs to NA
prepplot$run <- NA
prepplot$pollinator_species <- NA
#prepplot$pl.dens <- NA
prepplot$pl.pref <- NA
#prepplot$pl.dens <- rep(seq(0.1,1, by=0.1),10000)
#rep(seq(0.1,1, by=0.1),10000)
prepplot$pl.dens <-0.5
prepplot$plant.pref <- rep(seq(0.1,1, by=0.1),10000)
#prepplot$connectance  <- rep(seq(0.1,1, by=0.1),10000)
prepplot$pol.links <- rep(seq(1, 5, by=1),10000)

#prepplot<-expand.grid(seed_percent=seq(0.00001,1,0.0001),  connectance=c(0.25, 0.5, 1), pl.dens=c(0.15, 0.45, 0.85))
#prepplot$run <- NA
#prepplot$pollinator_species <- NA

#pedictions extract
prepplot$pred <- predict(mod_niche_3,
                         newdata = prepplot,
                         re.formula = NA,
                         type="response",
                         allow.new.levels=TRUE)

#plot
raster.surface<- ggplot(prepplot,
                           aes(x = seed_percent, 
                               y = pol.links,
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
  ylab("Pol.links") + 
  xlab("Seed percentage") +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  labs(title = "Plant density 0.1")+
  labs(fill="Pref change") + scale_fill_viridis_c(option=c("viridis"), limits = c(-0.5, 0.5))
  
  
  #scale_fill_gradientn(
   # colours = hcl.colors(10, "YlGnBu"),trans = "reverse",
  #  breaks =c(-0.5, -0.3, -0.1, 0.1 ,0.3), limits = c(-0.5, 0.3))

#coloring
#https://stackoverflow.com/questions/43772017/continuous-gradient-color-fixed-scale-heatmap-ggplot2
  
  
raster.surface

raster.surface.0.1
ggsave(paste0(dirF, "niche_change_0.1_max.png"),width=10, height = 6, units="in", dpi=600 ) 

raster.surface
ggsave(paste0(dirF, "niche_change_0.5_max.png"),width=10, height = 6, units="in", dpi=600 ) 

raster.surface
ggsave(paste0(dirF, "niche_change_09_max.png"),width=10, height = 6, units="in", dpi=600 ) 


#niche.figs<-ggarrange(raster.surface.0.1, raster.surface.0.4, raster.surface.0.8, labels = c("a", "b", "c"), ncol = 3)
#niche.figs




############################

