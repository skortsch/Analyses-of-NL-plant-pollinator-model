##############################################################################################
# R Code used for figures in Appendices 4, 5 and 6 in the SI for paper:
#Landscape composition and plant-pollinator network structure interact to influence pollination success in an individual-based mode ###
#Susanne Kortsch, Leonardo Saravia, Alyssa Cirtwill, Thomas Timberlake, Jane Memmott, Liam Kendall, Tomas Roslin, and Giovanni Strona

#load packages
library("tidyverse") #needed to easily summarise, analyse, and visualise data s
library("ggpubr") 

#Working Dir, e.g.,
setwd("C:/LocalData/susakort/pollinatorsNL/R_scripts") #setwd to your own directory

#Figure dir 
dirF<-"../Figures/"

###############################################################################################

#Appendix 5, Fig. 1
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
vis_data<-read.csv("../Data/NLdata.csv", header=TRUE) #import data

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

#one simulation (run=7973243067) got accidentally divided into two, while doing the above procedures and appears as duplicated 
#however, the simulation is not duplicated in the input files. there is only one input file
#to solve this I took the mean of the duplicated rows to arrive at an overall mean
which(duplicated(mean.vis.plant$run))
mean.vis.plant[1564,]
mean.vis.plant[1565,] #due to time constraints, I delete this line in the data until we have figured out why it is duplicated.
mean.dup<-as.numeric(apply(rbind(mean.vis.plant[1565,][1,], mean.vis.plant[1564,]), 2, mean))
mean.vis.plant[1564,]<-as.list(mean.dup)
mean.vis.plant<-mean.vis.plant[-1565,]
which(duplicated(mean.vis.plant$run))


#Appendix 6, Figs. 1-3

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
mean.vis.plant$bins_seed <- cut(mean.vis.plant$seed_percent, breaks=breaks, include.lowest=TRUE, right=FALSE, labels=tags)

#mean.vis.plant$bins_seed <- cut(mean.vis.plant$seed_logged, breaks=breaks, include.lowest=TRUE, right=FALSE, labels=tags)

#Visitation rate
bs1 <- mean.vis.plant%>% mutate(cut_conn = cut(connectance,breaks=c(0.24, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95, 1))) %>% 
  ggplot( aes(x = factor(bins_seed), y = number_visits)) + xlab("Plant intermixing") + 
  ylab("Visitation rate") + theme_light() + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  #geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  stat_summary(geom = "point", fun = "mean",col = "black",size = 2,shape = 21,fill = "red") +
  guides(x =  guide_axis(angle = 90)) + facet_wrap( ~cut_conn)+ theme(axis.text.x = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  stat_compare_means(method = "anova", label.y = 200)
bs1


ggsave(paste0(dirF, "SI_App6_Fig1_vis_rate.png"),width=8, height = 10, units="in", dpi=600 ) 

#Consecutive visits
bs2 <- mean.vis.plant%>% mutate(cut_conn = cut(connectance,breaks=c(0.24, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95, 1))) %>% 
  ggplot( aes(x = factor(bins_seed), y = cons)) + xlab("plant intermixing") + 
  ylab("Consecutive visits") + theme_light() + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  #geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  stat_summary(geom = "point",fun = "mean",col = "black",size = 3,shape = 21,fill = "red") +
  guides(x =  guide_axis(angle = 90)) + facet_wrap( ~cut_conn)+ theme(axis.text.x = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  stat_compare_means(method = "anova", label.y = 200)
bs2

ggsave(paste0(dirF, "SI_App6_Fig2_cons.png"),width=8, height = 10, units="in", dpi=600 ) 


#Expected number of plants pollinated
bs3 <-mean.vis.plant%>% mutate(cut_conn = cut(connectance,breaks=c(0.24, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95, 1))) %>% 
  ggplot( aes(x = factor(bins_seed), y = pvis)) + xlab("plant intermixing") + 
  ylab("Expected no. of plants pollinated") + theme_light() + geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  #geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  stat_summary(geom = "point",fun = "mean",col = "black",size = 2,shape = 21,fill = "red") +
  guides(x =  guide_axis(angle = 90)) + facet_wrap( ~cut_conn)+ theme(axis.text.x = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey90", fill="grey90", size=1.5, linetype="solid"))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  stat_compare_means(method = "anova", label.y = 200)
bs3
ggsave(paste0(dirF, "SI_App6_Fig3_pvis.png"),width=8, height = 10, units="in", dpi=600 ) 


