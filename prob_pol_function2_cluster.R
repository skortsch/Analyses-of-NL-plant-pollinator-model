#Calculates pollination probability based on decay assumptions

pol.prob<-function(ex_run, a){
 
  ex_run<-as.data.frame(ex_run)
  
#---------DEFINING SOME VARIABLES---------------------------
  p_n<-length(unique(ex_run$plant_species))       #no. of plant species in the simulation
  pol_n<-length(unique(ex_run$pollinator_agent))  #no. of pollinator individuals
  f_n<-length(unique(ex_run$plant_patch))         #no. of flower individuals
  v_n<-nrow(ex_run) 
  pol_sp<-unique(ex_run$pollinator_species)
  
  #tot_number of visits

  #match plant id (order of plant id) with patch number
  u.p<-unique(ex_run$plant_patch)
  patch.plant.name<-c() 
  
  #plant id dictionary
  for (i in 1:length(u.p)){
      idd<-which(ex_run$plant_patch%in%u.p[i])
      patch.plant.name[i]<-unique(ex_run[idd, 6])
    }
  f_sp_dict<-as.list(patch.plant.name)          #plant id dictionary

  #dataset
  vis_data<-data.frame('f_id'=ex_run$plant_patch,'f_sp'=ex_run$plant_species,'p_id'=ex_run$pollinator_agent, 'pol_sp'=ex_run$pollinator_species)
  
  #store, for each flower INDIVIDUAL, all the pollinators' visits with their respective pollination probability based on the decay assumption

#per pollinator species   
prob_poll_sp<-list()  
for (sp in 1:length(pol_sp)){
  
  spp<-sort(unique(ex_run$pollinator_species))
  pid<-ex_run[ex_run$pollinator_species==spp[sp],]
  agid<-unique(pid$pollinator_agent)
  
#---------Pollination probability---------------------------  
  p_all<-list() 
  for (i in 1:f_n){p_all[[i]]<-'empty'}
  for (p_id_ in 1:length(agid)){
    #p_id_ <-5
    #ags.id<-sort(unique(ex_run$pollinator_agent))
    visits<-vis_data[vis_data$p_id==agid[p_id_],]
    v_n_<-nrow(visits)
    if (v_n_>1){
    for (i in 1:(v_n_-1)){
      f_sp_<-visits[i,2]
      p<-1
      j<-i+1
      while(p>0.001 && j<=v_n_){
        p<-p*a #**i 
          if (! is.na(visits[j,2])){
          if (visits[j,2]==f_sp_){
          id.l<-which(unique(ex_run$plant_patch)==visits[j,1]) # match plant patch with prob_all list
          if (p_all[[id.l]][1]=='empty'){p_all[[id.l]]<-c(p)} else {p_all[[id.l]]<-c(p_all[[id.l]],p)}}
        j<-j+1
         }
      }
      }
      }
    }

  ###now compute the actual probability that a given flower INDIVIDUAL has been pollinated at the end of the visitation sequence
  prob_poll_all<-c()
  prob_poll_spp<-list()
  for (i in 1:p_n){prob_poll_spp[[i]]<-c(0)}
  for (i in 1:f_n){
    target_sp<-f_sp_dict[[i]]
    if (p_all[[i]][1]!='empty'){
    p_none<-1 #probability that none of the pollinator's visits is successful.
      for (j in 1:length(p_all[[i]])){p_none<-p_none*(1-p_all[[i]][j])}
       p_poll<-1-p_none #probability that at least one pollination visit is successful
      prob_poll_all<-c(prob_poll_all,p_poll)
      prob_poll_spp[[target_sp]]<-c(prob_poll_spp[[target_sp]],p_poll)}
        else {
      prob_poll_all<-c(prob_poll_all,0)
      prob_poll_spp[[target_sp]]<-c(prob_poll_spp[[target_sp]],0)}
  }

#--------------Output-------------------------------------------------------------------------------------------  
#seed_percent and run number of data.frame ex.run  
#seed_percent
#seed_percent<-unique(ex_run$seed_percent)
#run
run<-unique(ex_run$run)

poll_sp<-spp[sp]
  
###prob_poll_spp list for each plant species, the individual flower probabilities of having been pollinated
### is the list of probabilities for each visited flower 
number_visits_sum<-sum(prob_poll_all)
#return(number_visits_sum)

number_visits_mean<-mean(prob_poll_all)
#return(number_visits_mean)

number_vis_per_plant_sum<-t(unlist(lapply(prob_poll_spp, sum)))
#return(number_vis_per_plant_sum)

number_vis_per_plant_mean<-t(unlist(lapply(prob_poll_spp, mean)))
#return(number_vis_per_plant_mean)

res<-list(run, poll_sp, number_visits_sum, number_visits_mean, number_vis_per_plant_sum, number_vis_per_plant_mean) #seed_percent, 
names(res)<- c("run", "poll_sp","number_visits_sum" , "number_visits_mean", "number_vis_per_plant_sum", "number_vis_per_plant_mean") #"seed_percent",
#return(res)
prob_poll_sp[[sp]]<- res
#return(prob_poll_sp)
}
return(prob_poll_sp)
}

