#load packages
#library("viridis")
library("RColorBrewer") #colors for figures
library("igraph")       #needed for network analyses #https://www.oberlo.com/blog/color-combinations-cheat-sheet
library("forcats")
library("ggpubr")
library("tidyverse") #Needed to easily summarise and analyse data 
library("lubridate") #Getting R to agree that your data contains the dates and times
library("bipartite")

#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"../Figures/"

setwd("C:/LocalData/susakort/GitHub/NLmodel_GIT/NLmodelAnalyses/R_scripts")

#set wd 
#original place of script
#setwd("Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_18/R_scripts")

#facet-wrapped geometric

#merge files to make face_wrap
dat.geo.SI<-read.csv("../Data/dat.geo.SI.csv")
dat.geo.SI$vis.type[dat.geo.SI$vis.type=="geo decay 0.1"]<-"c) geo decay 0.1"
dat.geo.SI$vis.type[dat.geo.SI$vis.type=="geo decay 0.2"]<-"d) geo decay 0.2"
dat.geo.SI$vis.type[dat.geo.SI$vis.type=="geo decay 0.3"]<-"e) geo decay 0.3"
dat.geo.SI$vis.type[dat.geo.SI$vis.type=="geo decay 0.4"]<-"f) geo decay 0.4"
dat.geo.SI$vis.type[dat.geo.SI$vis.type=="geo decay 0.5"]<-"g) geo decay 0.5"
dat.geo.SI$vis.type[dat.geo.SI$vis.type=="geo decay 0.7"]<-"h) geo decay 0.7"
dat.geo.SI$vis.type[dat.geo.SI$vis.type=="geo decay 0.9"]<-"i) geo decay 0.9"

ggplot(dat.geo.SI, aes(seed_percent, number_visits, colour=vis.type, fill=vis.type)) +
  geom_smooth(method="lm") + geom_point() + theme_bw() +
  facet_wrap(~ vis.type, scales="free")+
  theme(strip.text.x = element_text(size = 11))+
  ylab("Pollination visits") + xlab("plant intermixing")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"), 
        panel.border = element_rect(fill = NA, colour = "white"), 
        axis.line = element_line(),
        strip.background = element_blank(),
        panel.margin = unit(1, "lines"))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size = 11), axis.title = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 11), axis.title = element_text(size = 12))+
  stat_cor(label.y=c(8000,3500,5000,5000,5000,4000,3000,1000,300)) #LEO CAN YOU Customise the stat cor function!!!
#also we don't need to show text on x axes for the two upper panels

#   geom_text(data=r_df, aes(label=paste("rsq=", rsq)), 
#          x=-Inf, y=Inf, hjust=-0.2, vjust=1.2)+
#  label.y=0.025

#strip.text.x = element_blank(), removes title text
#ggsave(paste0(dirF, "geometric_SI.png"),width=8, height = 6, units="in", dpi=600 ) 
ggsave(paste0(dirF, "SI_Fig_S6.png"),width=8, height = 6, units="in", dpi=600 ) 

