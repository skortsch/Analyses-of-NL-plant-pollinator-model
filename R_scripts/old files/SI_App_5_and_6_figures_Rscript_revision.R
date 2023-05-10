##############################################################################################
# R Code used for figures in Appendices 6 and 10 in the SI for paper:
#Landscape composition and pollinator traits interact to influence pollination success in an individual-based mode ###
#Susanne Kortsch, Leonardo Saravia, Alyssa Cirtwill, Thomas Timberlake, Jane Memmott, Liam Kendall, Tomas Roslin, and Giovanni Strona

#load packages
library("tidyverse") #needed to easily summarise, analyse, and visualise data s
library("ggpubr") 

#Working Dir, e.g.,
#setwd("C:/LocalData/susakort/pollinatorsNL/R_scripts") #setwd to your own directory

#Figure dir 
dirF<-"../Figures/"

###############################################################################################

#Appendix 6, Fig. S4
#Relationship between habitat size and plant intermixing (seed percentage)
#load file to read distance file (path.dist) from the centre of a "habitat" to the edge
path.dist<-read.csv("../Data/path.dist.csv")
path.dist$seed_logged<-log(path.dist$seed_percent)
path.dist$free_dist_logged<-log(path.dist$free.dist)

hab1<-ggplot(path.dist, aes(x= seed_logged , y = free_dist_logged)) + xlab("plant lintermixing [log]") + 
  geom_point(alpha=0.5) +  guides(x =  guide_axis(angle = 90))+
  theme_light() + ylab("mean distance from centre of a habitat to the edge [log]")+
  geom_smooth(method = "lm", se=FALSE, color="red")+
  stat_regline_equation(label.y = 4.5, aes(label = ..eq.label..), size=6)+
  theme(axis.text = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 14)))+
  theme(strip.text.x = element_text(size = 14, color = "black"))
hab1

ggsave(paste0(dirF, "App5_hab_size.png"),width=10, height = 6, units="in", dpi=600 ) 

################################################################################################

###load data for Appendices 5 and 6
#vis_data<-read.csv("../Data/NLdata.csv", header=TRUE) #import data

### Process data

#calculate and add number of plant 
plant.l<- vis_data %>% arrange (run, plant_species) %>%  group_by(run, seed_percent, plant_species) %>% mutate(pl.links = n())
#add number of pollinator links
poll.l<- vis_data %>%  arrange (run, plant_species) %>%  group_by(run, seed_percent, pollinator_species) %>% mutate(pol.links = n())
plant.l_2<- plant.l %>% inner_join(poll.l)

vis.per.plant<-plant.l_2 %>% group_by(run, seed_percent, plant_species, free.dist, connectance=conn, nestedness=nest, pl.dens) %>% 
  summarize(number_visits = sum(number_visits), cons = sum(cons), pvis = sum(pvis), pl.no=mean(plant.density), plant.links=mean(pl.links),pol.links=mean(pol.links))

#total and mean visits, data used for GLM figure
tot.vis.plant<-vis.per.plant %>% group_by(run, seed_percent,connectance) %>% 
  summarize(sum.vis = sum(number_visits), sum.cons = sum(cons), sum.pvis = sum(pvis), pl.no=sum(pl.no), hab.size=mean(free.dist))

#seed_percent,connectance
mean.vis.plant<-vis.per.plant %>% group_by(run, seed_percent, connectance) %>% 
  summarize(number_visits = mean(number_visits), cons = mean(cons), pvis = mean(pvis), mean.plant.dens=mean(pl.dens), mean.pl.no=mean(mean.plant.dens), area.size=mean(free.dist))


#Appendix 10, Figs. 1-3

#Plot visits as a function of input connectance 

#mean.vis.plant$seed_logged<-log(mean.vis.plant$seed_percent)

# set up cut-off values 
breaks <- c( 0.00005,  0.001, 0.005, 0.01, 0.05, 0.00, 0.2, 1)

# specify interval/bin labels
tags <- c( "[0.00005-0.001)", "[0.001-0.005)", "[0.005-0.01)", 
           "[0.01-0.05)","[0.05-0.00)", "[0.0-0.2)", "[0.2-1)")


# set up cut-off values 
#breaks <- c( -12,  -9, -6, -3, 0)

# specify interval/bin labels
#tags <- c( "[(-12)-(-9))", "[(-9)-(-6))", "[(-6)-(-3))", 
#           "[(-3)-0)")


# set up cut-off values 
#breaks <- c( -12, -10, -8, -5, -3, 0)

# specify interval/bin labels
#tags <- c( "[(-12)-(-10))", "[(-10)-(-8))", "[(-8)-(-5))", 
#           "[(-5)-(-3))","[(-3)-0)")


# bucketing values into bins
vis.per.plant$bins_seed <- cut(vis.per.plant$seed_percent, breaks=breaks, include.lowest=TRUE, right=FALSE, labels=tags)

#mean.vis.plant$bins_seed <- cut(mean.vis.plant$seed_logged, breaks=breaks, include.lowest=TRUE, right=FALSE, labels=tags)
library(hrbrthemes)

#Visitation rate
bs1 <- vis.per.plant[-12001,]%>% mutate(cut_deg = cut(pol.links,breaks=c(0, 1, 2, 3, 4, 5, 6))) %>% 
  ggplot( aes(x = factor(bins_seed), y = number_visits)) + xlab("Plant intermixing") + 
  ylab("Visitation rate") + theme_light() +  geom_violin()+ geom_jitter(height = 0, width = 0.2, alpha=0.05)+
  #ggbeeswarm::geom_quasirandom() +
  #geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  #geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  #scale_y_continuous(labels=c("0","5000", "10000"))+
  theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14)) +
  stat_summary(geom = "point", fun = "median",col = "black",size = 5,shape = 21,fill = "red") +
  #guides(x =  guide_axis(angle = 90)) + 
  facet_wrap( ~cut_deg, scales="free")+ theme(axis.text.x = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  stat_compare_means(aes(label = paste0("p=", after_stat(p.format))))
  #stat_compare_means(method = "anova", label.y = 12000)
bs1


ggsave(paste0(dirF, "SI_App6_Fig1_vis_rate.png"),width=8, height = 10, units="in", dpi=600 ) 

#Consecutive visits
bs2 <- vis.per.plant[-12001,]%>% mutate(cut_deg = cut(pol.links,breaks=c(0, 1, 2, 3, 4, 5, 6))) %>% 
  ggplot( aes(x = factor(bins_seed), y = cons)) + xlab("Plant intermixing") + 
  ylab("consecutive visits") + theme_light() +  geom_violin()+ geom_jitter(height = 0, width = 0.2, alpha=0.05)+
  #ggbeeswarm::geom_quasirandom() +
  #geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  #geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14)) +
  stat_summary(geom = "point", fun = "median",col = "black",size = 5,shape = 21,fill = "red") +
  guides(x =  guide_axis(angle = 90)) + facet_wrap( ~cut_deg, scales="free")+ theme(axis.text.x = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  stat_compare_means(aes(label = paste0("p=", after_stat(p.format))))
#stat_compare_means(method = "anova", label.y = 12000)
bs2

ggsave(paste0(dirF, "SI_App6_Fig2_cons.png"),width=8, height = 10, units="in", dpi=600 ) 


#Expected number of plants pollinated
bs3 <-vis.per.plant[-12001,]%>% mutate(cut_deg = cut(pol.links,breaks=c(0, 1, 2, 3, 4, 5, 6))) %>% 
  ggplot( aes(x = factor(bins_seed), y = pvis)) + xlab("Plant intermixing") + 
  ylab("expected number of plants pollinated") + theme_light() +  
  geom_violin()+ geom_jitter(height = 0, width = 0.2, alpha=0.05)+
  #ggbeeswarm::geom_quasirandom() +
  #geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  #geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14)) +
  stat_summary(geom = "point", fun = "median",col = "black",size = 5,shape = 21,fill = "red") +
  guides(x =  guide_axis(angle = 90)) + facet_wrap( ~cut_deg, scales="free")+ theme(axis.text.x = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  stat_compare_means(aes(label = paste0("p=", after_stat(p.format))))
bs3

ggsave(paste0(dirF, "SI_App6_Fig3_pvis.png"),width=8, height = 10, units="in", dpi=600 ) 


