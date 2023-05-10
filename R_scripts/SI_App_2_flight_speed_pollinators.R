#Flight speed

#Analyses in this document explore the size of the flight speeds with which pollinators move.
#In any given NetLogo model, individuals travel a certain amount of grid cells per time-step, their movement speed.

#set working directory
setwd("C:/LocalData/susakort/pollinatorsNL-main_18/R_scripts")
setwd("C:/LocalData/susakort/abm pollination/pollinatorsNL/Data_processing_ex_v2/Data_output")

library("tidyverse") #Needed to easily summarise and analyse data 

#Figure dir 
dirF<-"../Figures/"

load("NL_model_data_v2.Rdata")
load("../Data_output/NL_model_data.Rdata") #to get the file.list data
#step_distance<-read.csv("../Data_output/step_distance.csv")
path.dist<-read.csv("../Data_output/path.dist.csv")

#calculate foraging distances
res <- mdl %>% group_by(seed_percent,run,pollinator_species, pollinator_agent) %>% 
  dplyr::select(seed_percent,run,day,plant_species, pollinator_species, plant_patch,pollinator_agent,foraging_distance) %>% 
  mutate(plant_foraging_distance = foraging_distance - lag(foraging_distance), step_distance = plant_foraging_distance / ceiling((day - lag(day))* 480)) %>% 
  mutate(plant_foraging_distance=if_else(is.na(plant_foraging_distance) | plant_foraging_distance<0, foraging_distance, plant_foraging_distance ),
  step_distance=if_else(is.na(step_distance) | step_distance<0, 0, step_distance ))

saveRDS(res,"../Data_output/step_distance.rds")
step_distance<-readRDS("../Data_output/step_distance.rds")

#group by pollinators
step_distance<-res
step_distance %>% group_by(pollinator_species) %>% summarise_at(vars(step_distance),
    list(max=max ,Q1=~quantile(., probs = 0.25), median=median, Q3=~quantile(., probs = 0.75), mean=mean, sd=sd))

#mean flight speed
res.mean <- step_distance %>% group_by(pollinator_species) %>% summarise(Mean.SL = mean(step_distance))
res.median <- step_distance %>% group_by(pollinator_species) %>% summarise(Mean.SL = median(step_distance))

res.mean <- res %>% group_by(pollinator_species) %>% summarise(Mean.SL = mean(step_distance))
#res.median <- res %>% group_by(pollinator_species) %>% summarise(Mean.SL = median(step_distance))


ggplot(step_distance, aes(x=step_distance,color=factor(pollinator_species))) + geom_histogram(aes(y=..density..), fill="white", binwidth=2)+
  geom_density(alpha=.2, fill="#FF6666") + facet_wrap( ~pollinator_species)  +  scale_color_viridis_d(guide="none")+
  geom_vline(data = res.median, mapping = aes(xintercept = Mean.SL),col='red',size=0.5) +
  geom_text(data = res.median, aes(x = 20, y = 0.2, label = round(Mean.SL, digits=2)), col="red")+
  theme_light()+xlab("flight speed")+
  #theme(panel.border = element_rect(fill=NA, color="black", size=0.5, linetype="solid"))
  theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 12, color = "black"))+
  #theme(strip.text.y = element_text(size = 12, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", size=0.5, linetype="solid"))

ggsave(paste0(dirF, "flight_speeds.png"),width=8, height = 8, units="in", dpi=600 ) 

