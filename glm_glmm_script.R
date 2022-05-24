#Title: Habitat and plant-pollinator network structure drive pollination

#load packages
library("tidyverse") #Needed to easily summarise and analyse data 
library("bipartite")
library("ggpubr")   #arrange plot
library("sjPlot")  #visualisaing results, making tables with model output
library("ggeffects")
library("MASS")
library("DHARMa")
library("glmmTMB")
library("MuMIn")
library("jtools")
library("fmsb")
library("performance")
library("jtools")
library("fmsb")


#Figure dir 
dirF<-"../Figures/"

#Working Dir
setwd("C:/LocalData/susakort/pollinatorsNL-main_18/R_scripts") #setwd to your own directory

#data 
vis_data<-read.csv("../Data_output/NLdata.csv", header=TRUE) #import data

#visit per plant used for GLMM
#calculate and add number of plant 
plant.l<- vis_data %>% arrange (run, plant_species) %>%  group_by(run, seed_percent, plant_species) %>% mutate(pl.links = n())
#add number of pollinator links
poll.l<- vis_data %>%  arrange (run, plant_species) %>%  group_by(run, seed_percent, pollinator_species) %>% mutate(pol.links = n())
plant.l_2<- plant.l %>% inner_join(poll.l)

vis.per.plant<-plant.l_2 %>% group_by(run, seed_percent, plant_species, free.dist, connectance=conn, nestedness=nest, pl.dens) %>% 
  summarize(number_visits = sum(number_visits), cons = sum(cons), pvis = sum(pvis), pl.no=mean(plant.density), 
  plant.links=mean(pl.links),pol.links=mean(pol.links))


#total and mean visits, data used for GLM figure
tot.vis.plant<-vis.per.plant %>% group_by(run, seed_percent,connectance) %>% 
  summarize(sum.vis = sum(number_visits), sum.cons = sum(cons), sum.pvis = sum(pvis), pl.no=sum(pl.no), 
            hab.size=mean(free.dist))


mean.vis.plant<-vis.per.plant %>% group_by(run, seed_percent,connectance) %>% 
  summarize(number_visits = mean(number_visits), cons = mean(cons), pvis = mean(pvis), mean.plant.dens=mean(pl.dens), mean.pl.no=mean(mean.plant.dens), area.size=mean(free.dist))

#NOTE!!! for some strange reason one run is duplicated in the mean and tot vis per plant. 
#I have not been able to detect the reason yet. the input variables do not contain duplicates, I have checked.
which(duplicated(mean.vis.plant$run))
mean.vis.plant[1564,]
mean.vis.plant[1565,] #due to time constraints, I delete this line in the data until we have figured out why it is duplicated.
mean.vis.plant<-mean.vis.plant[-1565,]
which(duplicated(mean.vis.plant$run))

### Figures Main Text

#**Figure 1:** Relationship between total pollination visits and habitat heterogeneity for different levels of network connectance 

#head(cbind(mean.vis.plant$seed_percent, log(mean.vis.plant$seed_percent+1), log(mean.vis.plant$seed_percent)))


nb.seed.conn_1<-glm.nb(round(number_visits) ~ log(seed_percent)*connectance+offset(log(mean.plant.dens)), data=mean.vis.plant,  link = log) 
summary(nb.seed.conn_1)

#peudo R2
r2_nagelkerke(nb.seed.conn_1)

#check residuals
plot(nb.seed.conn_1)

#predict
pred.1 <- expand.grid(seed_percent=seq(0.00001, 1, 0.001), connectance=c(0.25, 0.5, 0.75, 1), mean.plant.dens= mean(mean.vis.plant$mean.plant.dens))
pred.1
pred.1$y <- predict(nb.seed.conn_1, pred.1, type="response")
pred.1$se <- predict(nb.seed.conn_1, pred.1, type="response", se.fit = TRUE)$se

#calculate confidence intervals
pred.1$lower <- pred.1$y - 1.96 * pred.1$se
pred.1$upper <- pred.1$y + 1.96 * pred.1$se
pred.1$type<-"A) visitation rate"

#make plot
plot_nb.1<-ggplot(pred.1, aes(x=log(seed_percent),y=y,color=factor(connectance),group=connectance))+ 
  theme_classic()+ theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.border = element_rect(fill=NA, color="black", size=0.5, linetype="solid"))+
  geom_line(size=1) +
  ylab("Visitation rate")+ xlab("") +labs(col = "Connectance") +theme(legend.position = "none")+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred.1) +
  scale_color_manual(values =  c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top", legend.title=element_text(size=12), legend.text = element_text(size=12))
plot_nb.1

#Check deviance explained
#dev.null <- nb.seed.conn_1$null.deviance
#dev.resid <- nb.seed.conn_1$deviance
#dev.explained <- (dev.null - dev.resid)/dev.null*100
#dev.explained

#consecutive visits
nb.seed.conn_2<-glm.nb(round(cons) ~ log(seed_percent)*connectance+offset(log(mean.plant.dens)), data=mean.vis.plant,  link = log)
summary(nb.seed.conn_2)

#check residuals
#plot(nb.seed.conn_2)

#predict
pred.2 <- expand.grid(seed_percent=seq(0.00001, 1, 0.001), connectance=c(0.25, 0.5, 0.75, 1),  mean.plant.dens= mean(mean.vis.plant$mean.plant.dens))
pred.2$y <- predict(nb.seed.conn_2, pred.2, type="response")
pred.2$se <- predict(nb.seed.conn_2, pred.2, type="response", se.fit = TRUE)$se

pred.2$lower <- pred.2$y - 1.96 * pred.2$se
pred.2$upper <- pred.2$y + 1.96 * pred.2$se
pred.2$type<-"B) consecutive visits"

#plot 
plot_nb.2<-ggplot(pred.2, aes(x=log(seed_percent),y=y,color=factor(connectance),group=connectance))+geom_line(size=1) + 
  theme_classic()+ theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  ylab("Consecutive visits")+ xlab("") +labs(col = "Connectance") +theme(legend.position = "none")+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred.2) +
  theme(panel.border = element_rect(fill=NA, color="black", size=0.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top", legend.title=element_text(size=12), legend.text = element_text(size=12))    
plot_nb.2

#dev.null <- nb.seed.conn_2$null.deviance
#dev.resid <- nb.seed.conn_2$deviance
#dev.explained <- (dev.null - dev.resid)/dev.null*100
#dev.explained

### expected pollination visits
nb.seed.conn_3<-glm.nb(round(pvis) ~ log(seed_percent)*connectance+offset(log(mean.plant.dens)), data=mean.vis.plant,  link = log)
summary(nb.seed.conn_3)

#check residuals
#plot(nb.seed.conn_3)

#predict
pred.3 <- expand.grid(seed_percent=seq(0.00001, 1, 0.001), connectance=c(0.25, 0.5, 0.75, 1), mean.plant.dens= mean(mean.vis.plant$mean.plant.dens))
pred.3$y <- predict(nb.seed.conn_3, pred.3, type="response")
pred.3$se <- predict(nb.seed.conn_3, pred.3, type="response", se.fit = TRUE)$se

#calculate confidence intervals
pred.3$lower <- pred.3$y - 1.96 * pred.3$se
pred.3$upper <- pred.3$y + 1.96 * pred.3$se
pred.3$type<-"C) pollination success"

#plot
plot_nb.3<-ggplot(pred.3, aes(x=log(seed_percent),y=y,color=factor(connectance),group=connectance))+geom_line(size=1) + 
  theme_classic()+ theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  ylab("Expected pollination success")+ xlab("") +labs(col = "Connectance") +theme(legend.position = "none")+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred.3) +
  theme(panel.border = element_rect(fill=NA, color="black", size=0.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top", legend.title=element_text(size=12), legend.text = element_text(size=12)) 
plot_nb.3

#dev.null <- nb.seed.conn_3$null.deviance
#dev.resid <- nb.seed.conn_3$deviance
#dev.explained <- (dev.null - dev.resid)/dev.null*100


Fig1_mean<-ggarrange(plot_nb.1, plot_nb.2, plot_nb.3, labels = c("a", "b", "c"), ncol = 3, common.legend = TRUE)
annotate_figure(Fig1_mean, bottom = text_grob("plant intermixing", size=14))

ggsave(paste0(dirF, "Fig1_mean_logged.png"),width=10, height = 6, units="in", dpi=600 ) 

#GLM output table
mod_tab_1<-tab_model(nb.seed.conn_1, nb.seed.conn_2, nb.seed.conn_3)
mod_tab_1

tab_model(nb.seed.conn_1)
tab_model(nb.seed.conn_2)
tab_model(nb.seed.conn_3)


#####################
### GLMMM ###

##### Vis Per Plant Data, predictor Pollinator links 


mod.vis2<-glmmTMB(round(number_visits)~log(seed_percent)*pol.links*pl.dens+(1|run), 
                   family="nbinom2",
                   ziformula=~1,
                   data=vis.per.plant)

#checks model fit
check_model(mod.vis2)

#dharma residual checks
res_vis<- simulateResiduals(mod.vis2, 200)
plot(res_vis)#tests if the overall distribution conforms to expectations
testDispersion(res_vis) #tests if the simulated dispersion is equal to the observed dispersion
testResiduals(res_vis)
testUniformity(res_vis)
testZeroInflation(res_vis) #tests if there are more zeros in the data than expected from the simulations


#consecutive visits
mod.cons2<-glmmTMB(round(cons)~log(seed_percent)*pol.links*pl.dens+(1|run), 
                   family="nbinom2",
                   ziformula=~1,
                   data=vis.per.plant)


#checks model fit
check_model(mod.cons2)

#dharma residual checks
res_vis<- simulateResiduals(mod.cons2, 200)
plot(res_vis)#tests if the overall distribution conforms to expectations
testDispersion(res_vis) #tests if the simulated dispersion is equal to the observed dispersion
testResiduals(res_vis)
testUniformity(res_vis)
testZeroInflation(res_vis) #tests if there are more zeros in the data than expected from the simulations


#expected pollination visits
mod.pvis2<-glmmTMB(round(pvis)~log(seed_percent)*pol.links*pl.dens+(1|run), 
                   family="nbinom2",
                   ziformula=~1,
                   data=vis.per.plant)


#checks model fit
check_model(mod.pvis2)

#dharma residual checks
res_vis<- simulateResiduals(mod.pvis2, 200)
plot(res_vis)#tests if the overall distribution conforms to expectations
testDispersion(res_vis) #tests if the simulated dispersion is equal to the observed dispersion
testResiduals(res_vis)
testUniformity(res_vis)
testZeroInflation(res_vis) #tests if there are more zeros in the data than expected from the simulations


#visitation rate

pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.vis2, pred, type="response",allow.new.levels=TRUE)
pred$se <- predict(mod.vis2, pred, type="response", se.fit = TRUE)$se
pred$lower <- pred$y - 1.96 * pred$se
pred$upper <- pred$y + 1.96 * pred$se

#plot1<-ggplot(pred,aes(x=seed_percent,y=y,color=factor(pl.dens),group=pl.dens))+
plot.vis2<-ggplot(pred,aes(x=log(seed_percent),y=y, color=factor(pol.links)),group=pol.links)+
  theme_bw()+facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  geom_line()+ylab("Visitation rate")+xlab("") +labs(col ="poll links") +
  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  #theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", size=0.5, linetype="solid"))+
  theme(legend.position="top")
plot.vis2


#consecutive visits
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.cons2, pred, type="response",allow.new.levels=TRUE)
pred$se <- predict(mod.cons2, pred, type="response", se.fit = TRUE)$se
pred$lower <- pred$y - 1.96 * pred$se
pred$upper <- pred$y + 1.96 * pred$se

plot.cons2<-ggplot(pred,aes(x=log(seed_percent),y=y, color=factor(pol.links)),group=pol.links)+
  theme_bw()+facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  geom_line()+ylab("Consecutive visits")+xlab("") +labs(col ="poll links") +
  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", size=0.5, linetype="solid"))+
  theme(legend.position="top")
plot.cons2


#expected pollination visits
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.pvis2, pred, type="response",allow.new.levels=TRUE)
pred$se <- predict(mod.pvis2, pred, type="response", se.fit = TRUE)$se
pred$lower <- pred$y - 1.96 * pred$se
pred$upper <- pred$y + 1.96 * pred$se

plot.pvis2<-ggplot(pred,aes(x=log(seed_percent),y=y, color=factor(pol.links)),group=pol.links)+
  theme_bw()+facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  geom_line()+ylab("Expected pollination success")+xlab("") +labs(col ="poll links") +
  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", size=0.5, linetype="solid"))+
  theme(legend.position="top")
plot.pvis2

Fig_glmm_pol_dens<-ggarrange(plot.vis2, plot.cons2, plot.pvis2, widths = c(6, 6, 6), labels = c("a", "b", "c"), nrow = 3, common.legend = TRUE)
annotate_figure(Fig_glmm_pol_dens, bottom = text_grob("plant intermixing (log)"))
ggsave(paste0(dirF, "Fig_glmm_pol_dens_all_log.png"),width=8, height = 12, units="in", dpi=600 ) 


tab_model(mod.vis2)
tab_model(mod.cons2)
tab_model(mod.pvis2)

library("webshot")
# first save table to html file
tab_model(lme1, file = "plot.html")

############################################################################################################

