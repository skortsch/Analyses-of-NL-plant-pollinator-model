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
#setwd("~/Dropbox/RProjects/NLmodelAnalyses/Data_output")

#dataframe
vis_data


vis_data<-readRDS("../Data_output/vis_data.rds") #change to your own local path


df.vis.sum.poll<-vis_data %>% group_by(run, seed_percent, pollinator_species, 
                                       pol.soc, pol.bm, plant_species, pl.dens,plant.pref, prefs_after_vis, pref.diff, pol.links, pl.links) %>% 
  summarise(pl.pref=mean(plant.pref), number_visits = sum(number_visits), cons= sum(cons), 
            pvis=sum(pvis), connectance=mean(conn), tot.plant.dens=mean(plant.density)) 


df.vis.sum.poll.mean<-vis_data %>% group_by(run, seed_percent, pollinator_species, pl.dens) %>%  summarise(pl.pref=mean(plant.pref), 
    number_visits = sum(number_visits), cons= sum(cons), pvis=sum(pvis), connectance=mean(conn),  mean.diff=mean(pref.diff), pol.links=mean(pol.links)) 


df.vis.sum.poll.pos<-df.vis.sum.poll.mean %>% group_by(run, pollinator_species) %>% filter(mean.diff>0) 
df.vis.sum.poll.neg<-df.vis.sum.poll.mean %>% group_by(run, pollinator_species) %>% filter(mean.diff<0) 



###############################

mod_niche_1 <- glmmTMB(mean.diff~seed_percent*pol.links+(1|run),
                    family="gaussian",
                    ziformula=~1,
                    data=df.vis.sum.poll.pos)


#dharma residual checks
res <- simulateResiduals(mod_niche_1)
plotResiduals(res_1)
testResiduals(res_1)
testZeroInflation(res_1)

mod_niche_2 <- glmmTMB(mean.diff~seed_percent*pol.links+(1|run),
                       family="gaussian",
                       ziformula=~1,
                       data=df.vis.sum.poll.neg)



summary(mod_niche_6)

#dharma residual checks
res_6<- simulateResiduals(mod_niche_6)
plotResiduals(res_6)
testResiduals(res_6)
testZeroInflation(res_6)


df.vis.sum.poll_1<-df.vis.sum.poll[-c(which(df.vis.sum.poll$mean.diff=="1")),]
df.vis.sum.poll_2<-df.vis.sum.poll_1[-c(which(df.vis.sum.poll_1$mean.diff=="0")),]
mod_niche_7 <- glmmTMB(mean.diff~seed_percent*connectance+(1|pl.pref)+(1|pl.dens)+(1|run)+(1|pollinator_species),
                       family=beta_family(link = "logit"),
                       ziformula=~1,
                       data=df.vis.sum.poll_2)


mod_niche_8<- glmmTMB(abs(pref.diff)~seed_percent*pl.dens+(1|run)+(1|pollinator_species),
                      family="gaussian",
                      ziformula=~1,
                      data=df.vis.sum.poll)


AIC(mod_niche_8,
    mod_niche_7,
    mod_niche_6,
    mod_niche_5,
    mod_niche_4,
    mod_niche_3,
    mod_niche_2,
    mod_niche_1)


##########################################

mod_niche_1 <- glmmTMB(abs(pref.diff)~seed_percent*connectance*pl.dens+(1|run),
                       family="gaussian",
                       ziformula=~1,
                       data=df.vis.sum.poll)

mod_niche_2 <- glmmTMB(abs(pref.diff)~seed_percent*connectance*pl.dens+(1|run)+(1|pollinator_species),
                       family="gaussian",
                       ziformula=~1,
                       data=df.vis.sum.poll)

mod_niche_3 <- glmmTMB(abs(pref.diff)~seed_percent*connectance*pl.dens+(1|run:pollinator_species),
                       family="gaussian",
                       ziformula=~1,
                       data=df.vis.sum.poll)

AIC(mod_niche_1, mod_niche_2, mod_niche_3)

summary(mod_niche_1)

#dharma residual checks
res_3<- simulateResiduals(mod_niche_3, 200)
#plot(simulationOutput)
plotResiduals(res_3)
testResiduals(res_3)
testZeroInflation(res_3)



###
prepplot <- as.data.frame(matrix(ncol = 3, nrow = 100000))
#colnames(prepplot) <- c("seed_percent", "connectance", "pred")
colnames(prepplot) <- c("seed_percent", "pol.links", "pred")
#colnames(prepplot) <- c("cent_v1", "cent_sd", "pred")
prepplot$seed_percent <- rep(seq(0.00001,0.99999, by=0.0001),10)
prepplot <- prepplot[order(prepplot$seed_percent),]

#set REs to NA
prepplot$run <- NA
#prepplot$pollinator_species <- NA
#prepplot$pl.dens <- NA
#prepplot$pl.pref <- NA
prepplot$pl.dens <- 0.5
#prepplot$pl.dens <- rep(seq(0.1,1, by=0.1),10000)
#rep(seq(0.1,1, by=0.1),10000)
#prepplot$pl.dens <-0.1
#prepplot$pl.pref <- rep(seq(0.1,1, by=0.1),10000)
#prepplot$connectance  <- rep(seq(0.1,1, by=0.1),10000)
prepplot$pol.links <- rep(seq(1, 5, by=1),10000)
#prepplot<-expand.grid(seed_percent=seq(0.00001,1,0.0001),  connectance=c(0.25, 0.5, 1), pl.dens=c(0.15, 0.45, 0.85))
#prepplot$run <- NA
#prepplot$pollinator_species <- NA

#pedictions extract
prepplot$pred <- predict(mod_niche_1,
                         newdata = prepplot,
                         re.formula = NA,
                         type="response",
                         allow.new.levels=TRUE)

#plot
raster.surface.0.5.pos <- ggplot(prepplot,
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
  ylab("Pol links") + 
  xlab("Seed percentage") +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  labs(title = "Mean plant density 0.5")+
  labs(fill="Pos niche diff") +  scale_fill_viridis_c(option=c("viridis"), direction = 1)


#scale_colour_viridis_d(...,alpha = 1, begin = 0, end = 1,direction = 1,option = "D", aesthetics = "colour")

raster.surface.0.5.neg
raster.surface.0.5.pos

Fig_glmm_pol<-ggarrange(raster.surface.0.5.neg, raster.surface.0.5.pos, widths = c( 6, 6), labels = c("a", "b"), font.label = list(size = 16, color = "black"), ncol = 2)
annotate_figure(Fig_glmm_pol,bottom = text_grob("plant intermixing", size=16))
ggsave(paste0(dirF, "Fig4_geo_based_perferences.png"),width=11, height = 7, units="in", dpi=600 ) 

raster.surface.0.5 
ggsave(paste0(dirF, "niche_change_pos.png"),width=10, height = 6, units="in", dpi=600 ) 


#niche.figs<-ggarrange(raster.surface.0.1, raster.surface.0.4, raster.surface.0.8, labels = c("a", "b", "c"), ncol = 3)
#niche.figs


prepplot$pred <- predict(mod_niche_3,
                         newdata = prepplot,
                         re.formula = NA,
                         type="response",
                         allow.new.levels=TRUE)

pred <- expand.grid(seed_percent=seq(0.00001,1,0.0001),  connectance=c(0.25, 0.5, 1), pl.dens=c(0.15, 0.45, 0.85))
pred$run<-NA
pred$pollinator_species<-NA
pred$y <- predict(mod_niche_3, pred, type="response",allow.new.levels=TRUE)
plot_mod3<-ggplot(prepplot,aes(x=seed_percent,y=pred,color=factor(connectance),group=connectance))+geom_line()+ 
  facet_grid(~pl.dens,labeller ="label_both") +
  theme_light()+  ylab("niche change")+xlab("seed") +labs(col = "connectance") +theme(strip.text.x = element_text(size = 12))
ggsave(paste0(dirF, "plot_mod3.png"),width=10, height = 6, units="in", dpi=600 ) 


############################

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

vis_mod4 <- glmmTMB(round(pvis)~seed_percent*conn+(1|run)+(1|pollinator_species),
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


