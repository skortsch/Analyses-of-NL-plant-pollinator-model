#Title: Habitat and plant-pollinator network structure drive pollination

#load packages
library("tidyverse") #Needed to easily summarise and analyse data 
#library("lubridate") #Getting R to agree that your data contains the dates and times
library("bipartite")
library("ggpubr")   #arrange plot
#library("sjPlot") #visualisaing results
#library("sjmisc")
#library("sjlabelled")
#library("moments") #for calculating kurtosis
library("ggeffects")
#library("showtext")#https://albert-rapp.de/post/2022-02-19-ggplot2-color-tips-from-datawrapper/?s=09
#library("thematic")
library("MASS")
library("DHARMa")
library("glmmTMB")
library("MuMIn")


#Figure dir and pixels
#ppi<-600 #pixels per inches
dirF<-"../Figures/"


#Working Dir
setwd("C:/LocalData/susakort/pollinatorsNL-main_18/R_scripts") #setwd to your own directory

#data 
vis_data<-read.csv("../Data_output/NLdata.csv", header=TRUE) #import data


#calculate preferences after the simulation
vis_data<- vis_data %>% rowwise() %>% mutate(pref.diff =  prefs_after_vis - plant.pref)

#calculate number of links per pollinator and plant
poll.links<- vis_data %>%  arrange (run, pollinator_species) %>%  group_by(run, seed_percent, pollinator_species) %>% mutate(pol.links = n())
plant.links<- vis_data %>% arrange (run, pollinator_species) %>%  group_by(run, seed_percent, plant_species) %>% mutate(pl.links = n())

#pairwise plant pollinator data for each simulation (run)
df.vis<-vis_data %>% group_by(run, seed_percent, pollinator_species, 
                              pol.soc, pol.bm, plant_species, pl.dens,plant.pref, prefs_after_vis, pref.diff) %>% 
  summarise(pl.pref=mean(plant.pref), number_visits = sum(number_visits), cons= sum(cons), 
            pvis=sum(pvis), connectance=mean(conn), nestedness= mean(nest), tot.plant.dens=mean(plant.density))

#add number of links per pollinator and plant to the df.vis dataframe
df.vis<-cbind(df.vis, pol.links=poll.links$pol.links, pl.links=plant.links$pl.links)


#calculate difference in preference for the most preferred plant
niche.max <- df.vis %>% group_by(run, pollinator_species) %>% filter(plant.pref == max(plant.pref)) 
niche.min <- df.vis %>% group_by(run, pollinator_species) %>% filter(plant.pref == min(plant.pref)) 
niche.max1<-subset(niche.max, !pref.diff==0 & !pref.diff==-1)#removes rows with zero diff


#visits per pollinator
df.vis.poll<-df.vis %>% group_by(run, seed_percent, pollinator_species, pl.dens, connectance, plant.pref) %>%  
  summarize(number_visits = sum(number_visits), cons = sum(cons), pvis = sum(pvis), pol.links=mean(pol.links))

#per pollinator
df.vis.poll.sum<-df.vis %>% group_by(run, seed_percent, pollinator_species, connectance, pol.bm, pol.soc) %>%  
  summarize(number_visits = sum(number_visits), cons = sum(cons), pvis = sum(pvis), pol.links=mean(pol.links),
            m.pl.dens=mean (pl.dens), m.plant.pref=mean(plant.pref), m.tot.plants=mean(tot.plant.dens))


#mean visits per pollinator
df.vis.poll.mean<-df.vis.poll %>% group_by(run, seed_percent, connectance) %>%  
  summarize(number_visits = mean(number_visits), cons = mean(cons), pvis = mean(pvis))

#visit per plant
#calculate and add number of plant links
plant.l<- vis_data %>% arrange (run, plant_species) %>%  group_by(run, seed_percent, plant_species) %>% mutate(pl.links = n())
poll.l<- vis_data %>%  arrange (run, plant_species) %>%  group_by(run, seed_percent, pollinator_species) %>% mutate(pol.links = n())
plant.l_2<- plant.l %>% inner_join(poll.l)

vis.per.plant<-plant.l_2 %>% group_by(run, seed_percent, plant_species, free.dist, connectance=conn, nestedness=nest, pl.dens) %>% 
  summarize(number_visits = sum(number_visits), cons = sum(cons), pvis = sum(pvis), pl.no=mean(plant.density), 
            plant.links=mean(pl.links),pol.links=mean(pol.links))

#vis.per.plant2<- vis.per.plant %>% filter(pol.links<2)

#total and mean visits, data used for GLM figure
tot.vis.plant<-vis.per.plant %>% group_by(run, seed_percent,connectance) %>% 
  summarize(sum.vis = sum(number_visits), sum.cons = sum(cons), sum.pvis = sum(pvis), pl.no=sum(pl.no), 
            hab.size=mean(free.dist))

mean.vis.plant<-vis.per.plant %>% group_by(run, seed_percent, connectance) %>% 
  summarize(number_visits = mean(number_visits), cons = mean(cons), pvis = mean(pvis), mean.plant.dens=mean(pl.dens))


mean.links<-df.vis %>% group_by(run, seed_percent, connectance) %>% summarize(m.pol.links = mean(pol.links), m.pl.links=mean(pl.links))
mean.vis.plant2<-mean.vis.plant%>% inner_join(mean.links)

########## GLMM ########################################################################################################

# Pollination visits as a function of connectance and plant pollinator density

#Overdispersion was detected in the data therefore a negative binomial distribution was used

#visitation rate

mod.vis1<-glmmTMB(round(number_visits)~seed_percent*pol.links*pl.dens+, 
                 family="nbinom1",
                 ziformula=~1,
                 data=vis.per.plant)

mod.vis2<-glmmTMB(round(number_visits)~seed_percent*pol.links*pl.dens+(1|run), 
                       family="nbinom1",
                       ziformula=~1,
                       data=vis.per.plant)



AIC(mod.vis1, mod.vis2)

#df.vis pairwise interaction data
mod.vis3<-glmmTMB(round(number_visits)~seed_percent*pol.links*pl.dens, 
                   family="nbinom1",
                   ziformula=~1,
                   data=df.vis)

mod.vis4<-glmmTMB(round(number_visits)~seed_percent*pol.links*pl.dens+(1|run), 
                   family="nbinom1",
                   ziformula=~1,
                   data=df.vis)

AIC(mod.vis3, mod.vis4)


### consecutive visits ###

mod.cons1<-glmmTMB(round(cons)~seed_percent*pol.links*pl.dens, 
                        family="nbinom1",
                        ziformula=~1,
                        data=vis.per.plant)

mod.cons2<-glmmTMB(round(cons)~seed_percent*pol.links*pl.dens+(1|run), 
                        family="nbinom1",
                        ziformula=~1,
                        data=vis.per.plant)

AIC(mod.cons1, mod.cons2)


#df.vis pairwise interaction data
mod.cons3<-glmmTMB(round(cons)~seed_percent*pol.links*pl.dens, 
                   family="nbinom1",
                   ziformula=~1,
                   data=df.vis)

mod.cons4<-glmmTMB(round(cons)~seed_percent*pol.links*pl.dens+(1|run), 
                   family="nbinom1",
                   ziformula=~1,
                   data=df.vis)

AIC(mod.cons3, mod.cons4)


### expected pollination visits ###
#vis.per.plant data
mod.pvis1<-glmmTMB(round(pvis)~seed_percent*pol.links*pl.dens, 
                  family="nbinom1",
                  ziformula=~1,
                  data=vis.per.plant)

mod.pvis2<-glmmTMB(round(pvis)~seed_percent*pol.links*pl.dens+(1|run), 
                       family="nbinom1",
                       ziformula=~1,
                       data=vis.per.plant)


AIC(mod.pvis1, mod.pvis2)

#df.vis pairwise interaction data
mod.pvis3<-glmmTMB(round(pvis)~seed_percent*pol.links*pl.dens, 
                   family="nbinom1",
                   ziformula=~1,
                   data=df.vis)

mod.pvis4<-glmmTMB(round(pvis)~seed_percent*pol.links*pl.dens+(1|run), 
                    family="nbinom1",
                    ziformula=~1,
                    data=df.vis)

AIC(mod.pvis3, mod.pvis4)


#dharma residual checks
res_vis<- simulateResiduals(mod.vis.conn, 200)
plot(res_vis)#tests if the overall distribution conforms to expectations
testDispersion(res_vis) #tests if the simulated dispersion is equal to the observed dispersion
testResiduals(res_vis)
testUniformity(res_vis)
testZeroInflation(res_vis) #tests if there are more zeros in the data than expected from the simulations

res_cons<- simulateResiduals(mod.cons.conn, 200)
testResiduals(res_cons)
testZeroInflation(res_cons) #tests if there are more zeros in the data than expected from the simulations

res_pvis<- simulateResiduals(mod.vis.conn, 200)
testResiduals(res_pvis)
testZeroInflation(res_pvis) #tests if there are more zeros in the data than expected from the simulations
checkModel(mod.vis.conn, stop = F)



##### Vis Per Plant Data, predictor Pollinator links 

#visitation rate
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.vis2, pred, type="response",allow.new.levels=TRUE)
pred$lower <- pred$y - 1.96 * pred$se
pred$upper <- pred$y + 1.96 * pred$se

#plot1<-ggplot(pred,aes(x=seed_percent,y=y,color=factor(pl.dens),group=pl.dens))+
plot.vis2<-ggplot(pred,aes(x=seed_percent,y=y, color=factor(pol.links)),group=pol.links)+
  #geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred)+
  geom_line()+ 
  #facet_grid(cols=vars(plant.links), rows=vars(pol.links),labeller ="label_both", scales = "free") +
  facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  theme_light()+  ylab("Visitation rate")+xlab("") +labs(col ="poll links") +
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  #theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top")
plot.vis2


#consecutive visits
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.cons2, pred, type="response",allow.new.levels=TRUE)

plot.cons2<-ggplot(pred ,aes(x=seed_percent,y=y, color=factor(pol.links)),group=pol.links)+
  geom_line()+ 
  #facet_grid(cols=vars(plant.links), rows=vars(pol.links),labeller ="label_both", scales = "free") +
  facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  theme_light()+  ylab("Consecutive visits")+xlab("") +labs(col ="poll links") +
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top")
plot.cons2



#expected pollination visits
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.pvis2, pred, type="response",allow.new.levels=TRUE)

plot.pvis2<-ggplot(pred ,aes(x=seed_percent,y=y, color=factor(pol.links)),group=pol.links)+
  geom_line()+ 
  #facet_grid(cols=vars(plant.links), rows=vars(pol.links),labeller ="label_both", scales = "free") +
  facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  theme_light()+  ylab("Pollination success")+xlab("") +labs(col ="poll links") +
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top")
plot.pvis2

Fig_glmm_pol_dens<-ggarrange(plot.vis2, plot.cons2, plot.pvis2, widths = c(6, 6, 6), labels = c("a", "b", "c"), nrow = 3, common.legend = TRUE)
annotate_figure(Fig_glmm_pol_dens, bottom = text_grob("Habitat plant clumpiness/mixing"))
ggsave(paste0(dirF, "Fig_glmm_pol_dens.png"),width=8, height = 10, units="in", dpi=600 ) 


##### df.vis Data , predictor Pollinator links 

#visitation rate
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.vis4, pred, type="response",allow.new.levels=TRUE)
pred$lower <- pred$y - 1.96 * pred$se
pred$upper <- pred$y + 1.96 * pred$se

#plot1<-ggplot(pred,aes(x=seed_percent,y=y,color=factor(pl.dens),group=pl.dens))+
plot.vis4<-ggplot(pred,aes(x=seed_percent,y=y, color=factor(pol.links)),group=pol.links)+
  #geom_ribbon(aes(ymin=lower, ymax=upper), linetype = 0, alpha=0.1, data=pred)+
  geom_line()+ 
  #facet_grid(cols=vars(plant.links), rows=vars(pol.links),labeller ="label_both", scales = "free") +
  facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  theme_light()+  ylab("Visitation rate")+xlab("") +labs(col ="poll links") +
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  #theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top")
plot.vis4


#consecutive visits
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.cons4, pred, type="response",allow.new.levels=TRUE)

plot.cons3_df.vis<-ggplot(pred ,aes(x=seed_percent,y=y, color=factor(pol.links)),group=pol.links)+
  geom_line()+ 
  #facet_grid(cols=vars(plant.links), rows=vars(pol.links),labeller ="label_both", scales = "free") +
  facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  theme_light()+  ylab("Consecutive visits")+xlab("") +labs(col ="poll links") +
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top")
plot.cons3_df.vis



#expected pollination visits
pred <- expand.grid(seed_percent=seq(0.00001,1,0.001), pl.dens=c(0.1, 0.5,0.9), pol.links=c(2,4,6))
pred$run<-NA
pred$y <- predict(mod.pvis.conn3, pred, type="response",allow.new.levels=TRUE)

plot.pvis3<-ggplot(pred ,aes(x=seed_percent,y=y, color=factor(pol.links)),group=pol.links)+
  geom_line()+ 
  #facet_grid(cols=vars(plant.links), rows=vars(pol.links),labeller ="label_both", scales = "free") +
  facet_grid(~pl.dens,labeller ="label_both", scales = "free") +
  theme_light()+  ylab("Pollination success")+xlab("") +labs(col ="poll links") +
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "grey20"))+
  theme(legend.position="top")
plot.pvis3

Fig_df.vis<-ggarrange(plot.vis4, plot.cons3_df.vis, plot.pvis3, widths = c(6, 6, 6), labels = c("a", "b", "c"), nrow = 3, common.legend = TRUE)
annotate_figure(Fig_df.vis, bottom = text_grob("Habitat plant clumpiness/mixing"))
ggsave(paste0(dirF, "Fig_df.vis.png"),width=8, height = 10, units="in", dpi=600 ) 


