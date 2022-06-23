#Calculates pollination probabilities according to a geometric decay assumption

#load data
#This is one example using the Visits_10.csv data file
vis<-read.csv("C:/LocalData/susakort/pollinatorsNL-main_18/Simulations_all_variables/Visits_10.csv", sep=";")

#load function to calculate pollination probability
source("prob_pol_function.R") 

#calculates "expeted number of plants pollinated"
res<-pol.prob(vis, 0.7)
#names(res)<- c("seed_percent", "run", "number_visits_sum", "number_visits_mean", "number_vis_per_plant_sum", "number_vis_per_plant_mean")

res

#I did this for all 2000 Visit files
#For each of these I save a list with estoimates per pollinator

name.res<-res[[1]]$run

#save file
saveRDS(res, paste0("../Data/rds_files_cluster_all_var/prob.pol.res_", name.res, ".rds"))


