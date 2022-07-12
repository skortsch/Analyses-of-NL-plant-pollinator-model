#Calculates pollination probability based on decay assumptions
p_n<-3
f_n<-9
f_id<-c(1:9)
f_sp<-c(1,2,3,2,1,1,3,1,1)
p_id<-1
vis_data<-data.frame('f_id'=f_id,'f_sp'=f_sp,'p_id'=p_id) #dummy dataset

f_sp_dict<-as.list(f_sp)

p_all<-list() #list to store, for each flower INDIVIDUAL, all the pollinators' visits with their respective pollination probability based on the decay assumption
for (i in 1:f_n){p_all[[i]]<-'empty'}
#decay_p<-c(runif(pol_n)) # this line allows varying the decay probability among pollinators
for (p_id_ in 1:pol_n){ 
  visits<-vis_data[vis_data$p_id==p_id_,] #visitation sequence for each pollinator
  v_n_<-nrow(visits) 
  for (i in 1:(v_n_-1)){
      f_sp_<-visits[i,2]
      p<-1
      j<-i+1
      #t<-c(1:(v_n_-1))
      while(p>0.001 && j<=v_n_){
        #p<-p*decay_p[p_id_] #if you want to vary the decay probability among pollinators
        p<-p*0.7 #for geometric decay p*0.9, or p<-p*0.9**i for exponential decay; the value is arbitrary 
        # if statement chaceks in a sequences which flower visits are to the same plant species and assigns probabilities
        if (visits[j,2]==f_sp_){ 
          #id.l<-which(unique(f_id)==visits[j,1])
          #id.l<-visits[j,1]
          if (p_all[[visits[j,1]]][1]=='empty'){p_all[[visits[j,1]]]<-c(p)} else {p_all[[visits[j,1]]]<-c(p_all[[visits[j,1]]],p)}}
          #if (p_all[[id.l]]=='empty'){p_all[[id.l]]<-c(p)} else {p_all[[id.l]]<-c(p_all[[id.l]],p)}}
        j<-j+1
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


###prob_poll_spp list for each plant species, the individual flower probabilities of having been pollinated
### is the list of probabilities for each visited flower 
sum(prob_poll_all)
for (i in 1:p_n){print(sum(prob_poll_spp[[i]]))}
#for (i in 1:p_n){print(mean(prob_poll_spp[[i]]))}
