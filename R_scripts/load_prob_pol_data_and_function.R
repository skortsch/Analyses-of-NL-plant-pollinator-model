#Calculates pollination probabilities according to a geometric decay assumption

#load data
#This is one example using the Visits_10.csv data file
vis<-read.csv("C:/LocalData/susakort/pollinatorsNL/NetLogo_model/Simulations/Visits_10.csv", sep=";")

#load function to calculate pollination probability
source("prob_pol_function.R") 

#calculates "expeted number of plants pollinated"
res<-pol.prob(vis, 0.7)
#names(res)<- c("seed_percent", "run", "number_visits_sum", "number_visits_mean", "number_vis_per_plant_sum", "number_vis_per_plant_mean")

#results
res

#This was done for all 2000 (Simulations) Visit files for the paper
#For each of these I save a list with pollination estimates per pollinator and plant

name.res<-res[[1]]$run #replication number

#save file
saveRDS(res, paste0("../Data/rds_files/prob.pol.res_", name.res, ".rds"))


