######################################################################################################################
# R Code used for constructing figures 5 and 6 in:
# Landscape composition and plant-pollinator network structure interact to influence pollination success in an individual-based mode ###
# Susanne Kortsch, Leonardo Saravia, Alyssa Cirtwill, Thomas Timberlake, Jane Memmott, Liam Kendall, Tomas Roslin, and Giovanni Strona

#load packages
library("tidyverse") #needed to easily summarise, analyse, and visualise data 
library("ggpubr")    #arrange plot
library("sjPlot")    #visualizing results, making tables with model output
library("ggeffects")
library("MASS")      #nb.glm function
library("DHARMa")    #check residuals
library("glmmTMB")   #glmm 
library("jtools")
library("fmsb")
library("performance") #check residuals, calculates pseudo R-squared values

#Figure dir 
dirF<-"../Figures/"

#Working Dir
#setwd("C:/LocalData/susakort/abm pollination/Scripts_1st_submission/R_scripts") #setwd to your own directory

#data 
#vis_data<-read.csv("../Data/NLdata.csv", header=TRUE) #import data

##########################################################################################################################
#Process vis_data

#add number of plant links
plant.l<- vis_data %>% arrange (run, plant_species) %>%  group_by(run, seed_percent, plant_species) %>% mutate(pl.links = n())
#add number of pollinator links
poll.l<- vis_data %>%  arrange (run, plant_species) %>%  group_by(run, seed_percent, pollinator_species) %>% mutate(pol.links = n())
plant.l_2<- plant.l %>% inner_join(poll.l)

#range(as.vector(plant.l_2[which(plant.l_2$pol.links==6),5]))

vis.per.plant<-plant.l_2 %>% group_by(run, seed_percent, plant_species, connectance=conn, pl.dens) %>% 
  summarize(number_visits = sum(number_visits), cons = sum(cons), pvis = sum(pvis),  pl.no=mean(plant.density), plant.links=mean(pl.links),pol.links=mean(pol.links))

vis.per.poll<-plant.l_2 %>% group_by(run, seed_percent, plant_species,  pollinator_species, pol.soc, connectance=conn, pl.dens, pol.bm) %>% 
  summarize(number_visits = sum(number_visits), cons = sum(cons), pvis = sum(pvis), pl.no=mean(plant.density), plant.links=mean(pl.links),pol.links=mean(pol.links))



### GLMMM ###

##### Vis Per Pollinator Data, predictor Pollinator body size

#+offset(pl.dens)
mod.vis2<-glmmTMB(round(number_visits)~log(seed_percent)*pol.bm*pol.soc+(1|run), 
                  family="nbinom2",
                  data=vis.per.poll)

#checks model fit
#check_model(mod.vis2)

#dharma residual checks
#res_vis<- simulateResiduals(mod.vis2, 200)
#plot(res_vis)#tests if the overall distribution conforms to expectations
#testDispersion(res_vis) #tests if the simulated dispersion is equal to the observed dispersion
#testResiduals(res_vis)
#testUniformity(res_vis)
#testZeroInflation(res_vis) #tests if there are more zeros in the data than expected from the simulations

#+offset(pl.dens)
#consecutive visits
mod.cons2<-glmmTMB(round(cons)~log(seed_percent)*pol.bm*pol.soc+(1|run), 
                   family="nbinom2",
                   data=vis.per.poll)


#checks model fit
check_model(mod.cons2)

#dharma residual checks
#res_vis<- simulateResiduals(mod.cons2, 200)
#plot(res_vis)#tests if the overall distribution conforms to expectations
#testDispersion(res_vis) #tests if the simulated dispersion is equal to the observed dispersion
#testResiduals(res_vis)
#testUniformity(res_vis)
#testZeroInflation(res_vis) #tests if there are more zeros in the data than expected from the simulations

#offset(pl.dens)+offset(pl.dens)
#expected pollination visits
mod.pvis2<-glmmTMB(round(pvis)~log(seed_percent)*pol.bm*pol.soc+(1|run), 
                   family="nbinom2",
                   data=vis.per.poll)


#checks model fit
#check_model(mod.pvis2)

#dharma residual checks
#res_vis<- simulateResiduals(mod.pvis2, 200)
#plot(res_vis)#tests if the overall distribution conforms to expectations
#testDispersion(res_vis) #tests if the simulated dispersion is equal to the observed dispersion
#testResiduals(res_vis)
#testUniformity(res_vis)
#testZeroInflation(res_vis) #tests if there are more zeros in the data than expected from the simulations


#visitation rate

pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=mean(vis.per.poll$pl.dens), pol.bm=c(2,4,6,8), pol.soc=c(1,2,3,4))
pred$run<-NA
pred$y <- predict(mod.vis2, pred, type="response",allow.new.levels=TRUE)
pred$se <- predict(mod.vis2, pred, type="response", se.fit = TRUE)$se
pred$lower <- pred$y - 1.96 * pred$se
pred$upper <- pred$y + 1.96 * pred$se

                                
plot.vis2<-ggplot(pred,aes(x=log(seed_percent),y=y, color=factor(pol.bm),linetype=factor(pol.bm)))+
  theme_bw()+ facet_grid(~pol.soc,labeller ="label_both", scales = "free")+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred)+
  scale_x_continuous(labels=c("0.00001", "0.0001", "0.001", 0.1, "1"))+
  scale_color_manual(values = c("#0072B2","#E69F00", "#009E73", "#CC79A7"))+
  theme(panel.margin.x=unit(0.8, "lines") , panel.margin.y=unit(0.8,"lines"))+
  geom_line(size=1)+ylab("Visitation rate")+xlab("") +labs(col ="ITD",linetype="ITD") +
  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14))  + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", size=0.5, linetype="solid"))+
  theme(legend.position="top")+
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
  #    strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.text = element_text(size=14)) +
  theme(legend.title = element_text(size=14))+
  theme(legend.key.width= unit(2, 'cm'))
plot.vis2



#consecutive visits
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=mean(vis.per.poll$pl.dens), pol.bm=c(2,4,6,8), pol.soc=c(1,2,3,4))
pred$run<-NA
pred$y <- predict(mod.cons2, pred, type="response",allow.new.levels=TRUE)
pred$se <- predict(mod.cons2, pred, type="response", se.fit = TRUE)$se
pred$lower <- pred$y - 1.96 * pred$se
pred$upper <- pred$y + 1.96 * pred$se

plot.cons2<-ggplot(pred,aes(x=log(seed_percent),y=y, color=factor(pol.bm),linetype=factor(pol.bm)))+
  theme_bw()+ facet_grid(~pol.soc,labeller ="label_both", scales = "free")+
  theme(panel.margin.x=unit(0.8, "lines") , panel.margin.y=unit(0.8,"lines"))+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred)+
  scale_x_continuous(labels=c("0.00001", "0.0001", "0.001", 0.1, "1"))+
  scale_color_manual(values = c("#0072B2","#E69F00", "#009E73", "#CC79A7"))+
  geom_line(size=1)+ylab("Consecutive visits")+xlab("") +labs(col ="ITD",linetype="ITD") +
  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14))  + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", size=0.5, linetype="solid"))+
  theme(legend.position="top")+
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
  #    strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.text = element_text(size=14)) +
  theme(legend.title = element_text(size=14))+
  theme(legend.key.width= unit(2, 'cm'))
plot.cons2


#expected pollination visits
pred3 <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=mean(vis.per.poll$pl.dens), pol.bm=c(2,4,6,8), pol.soc=c(1,2,3,4))
pred3$run<-NA
pred3$y <- predict(mod.pvis2, pred3, type="response",allow.new.levels=TRUE)
pred3$se <- predict(mod.pvis2, pred3, type="response", se.fit = TRUE)$se
pred3$lower <- pred3$y - 1.96 * pred3$se
pred3$upper <- pred3$y + 1.96 * pred3$se

plot.pvis2<-ggplot(pred3,aes(x=log(seed_percent),y=y, color=factor(pol.bm),linetype=factor(pol.bm)))+
  theme_bw()+ facet_grid(~pol.soc,labeller ="label_both", scales = "free")+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred3)+
  scale_x_continuous(labels=c("0.00001", "0.0001", "0.001", 0.1, "1"))+
  scale_color_manual(values = c("#0072B2","#E69F00", "#009E73", "#CC79A7"))+
  geom_line(size=1)+ylab("Expected no. of plants pollinated")+xlab("") +labs(col ="ITD",linetype="ITD") +
  theme(panel.margin.x=unit(0.8, "lines") , panel.margin.y=unit(0.8,"lines"))+
  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14))  + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", size=0.5, linetype="solid"))+
  theme(legend.position="top")+
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
    #    strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.text = element_text(size=14)) +
  theme(legend.title = element_text(size=14))+
  theme(legend.key.width= unit(2, 'cm'))
plot.pvis2

Fig_glmm_pol_dens<-ggarrange(plot.vis2, plot.cons2, plot.pvis2, widths = c(6, 6, 6), labels = c("a", "b", "c"), nrow = 3, common.legend = TRUE)
annotate_figure(Fig_glmm_pol_dens, bottom = text_grob("plant intermixing [log]"))
ggsave(paste0(dirF, "Fig_glmm_pol_bm_ex_2.png"),width=8, height = 12, units="in", dpi=600 )
ggsave(paste0(dirF, "Fig_glmm_bodysizes_all_log.png"),width=8, height = 10, units="in", dpi=600 ) 

#Fig_glmm_pol_bm<-ggarrange(plot.vis2, plot.cons2, plot.pvis2, widths = c(6, 6, 6), labels = c("a", "b", "c"), nrow = 3, common.legend = TRUE, hjust=-4,heights=c(1,1,1.2))
#annotate_figure(Fig_glmm_pol_bm, bottom = text_grob("plant intermixing [log]", size=14))
ggsave(paste0(dirF, "Fig_glmm_pol_bm_ex_2.png"),width=8, height = 12, units="in", dpi=600 )


tab_model(mod.vis2)
tab_model(mod.cons2)
tab_model(mod.pvis2)

library("webshot")
# first save table to html file
tab_model(lme1, file = "plot.html")

############################################################################################################




