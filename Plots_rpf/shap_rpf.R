shap_rpf <- function(res, X,cores){
  
  force(res)
  
  force(X)
  
  values <- individuals <- list()
  
  # Function to get all subsets of set
  subsets <- function(x) {
    do.call(c, lapply(0:length(x), combn, x = x, simplify = FALSE))
  }
  
  res_shap <- mclapply(1:ncol(res), function(s){
    
    ### lim_list is a list giving for each variable all interval end-points
    
    lim_list <- lapply(1:ncol(X),  ### go through all variables of the component
                       function(x){
                         my.var.tree.index2 <- (1:length(res[,s][[3]]))[ sapply(1:length(res[,s][[3]]), function(i) is.element(x,as.numeric(res[,s][[3]][[i]])))]  ## all trees that have x as variable
                         
                         if (sum(x==res[,s][[6]])==0){ ## not categorical variable
                           
                           bounds <- sort(unique( na.omit( as.numeric(unlist(sapply(1:length(my.var.tree.index2), ### go through all relevant trees
                                                                                    
                                                                                    function(i){ 
                                                                                      as.numeric(sapply(1:length( res[,s][[1]][[my.var.tree.index2[i]]]),
                                                                                                        function(j){ ### go through all leaves and get interval ends of variable
                                                                                                          res[,s][[1]][[my.var.tree.index2[i]]][[j]][,x]
                                                                                                        }))
                                                                                    }))))))
                           # bounds[length(bounds)] <- bounds[length(bounds)] +0.001 ### may need fix
                           return(bounds)
                         }else{ ## categorical variable
                           
                           temp <- lapply(my.var.tree.index2, ### go through all relevant trees
                                          
                                          function(i){ 
                                            sapply(1:length( res[,s][[1]][[i]]),
                                                   function(j){ ### go through all leaves and get their sets 
                                                     res[,s][[1]][[i]][[j]][,x]
                                                   })
                                          })
                           
                           
                           for(i in 1:length(temp )) { 
                             if (i==1) temp2 <- temp[[1]] else temp2 <- cbind(temp2,temp[[i]])
                           }
                           
                           temp <- temp2
                           
                           if (!is.null(ncol(temp2))){
                             rm(temp)
                             for (j in 1:ncol(temp2)){
                               if (j==1) temp <- matrix(temp2[,1]) else{
                                 intercepts <-  sapply(1:ncol(temp), function(jj){
                                   sapply(1:nrow(temp2), function(k){
                                     any(temp2[k,j]==temp[,jj], na.rm=TRUE)
                                   })
                                 })
                                 
                                 for(jj in 1:ncol(as.matrix(intercepts))){
                                   if(any(as.matrix(intercepts)[,jj])){
                                     #                             if(sum(as.matrix(intercepts)[,jj])!=length(unique(temp2[as.matrix(intercepts)[,jj],j]))){
                                     temp3 <- temp2[as.matrix(intercepts)[,jj],j]
                                     temp <- cbind(temp,c(temp3, rep(NA, nrow(temp2)-sum(as.matrix(intercepts)[,jj])) ))
                                     temp[t(sapply(1:length(temp3), function(jjj) which(temp3[jjj]==temp[,jj]))),jj]<-NA
                                     #}
                                   }
                                 }
                                 intercepts <- apply(intercepts, 1 , function(j) !any(j))
                                 temp<-cbind(temp,c(temp2[intercepts,j],rep(NA,sum(!intercepts))))
                                 temp<-as.matrix(temp[, colSums(is.na(temp)) != nrow(temp)])
                               }
                             }
                           }
                           
                           return(temp)
                           
                         }})
    
    my.functions <- unique(res[,s][[3]])  ### all function components
    
    
    
    ####
    ######### Get values and indiviudals
    
    for (l in 1:length( my.functions)){  ### go through all  function components
      
      my.var.tree.index <- (1:length(res[,s][[3]]))[ sapply(1:length(res[,s][[3]]), function(i) setequal(as.numeric(res[,s][[3]][[i]]),my.functions[[l]]))]  ### all trees equal function component
      
      
      
      
      
      ### Leaves are defined through lim_list
      #### valaues is a list giving for each leaf a value. 
      #### individuals is a list giving for each leaf the number of individuals. 
      
      if (l==1){values <- individuals<-list()}
      values[[l]] <- individuals[[l]] <- array(0, dim=sapply(1:length(my.functions[[l]]), function(m) {
        
        if (!any(my.functions[[l]][m]==res[,s][[6]])) {length(lim_list[[my.functions[[l]][m]]] )-1} else{
          ncol(as.matrix(lim_list[[my.functions[[l]][m]]]))
        }
        
      }
      
      ))
      
      
      
      
      ## x is index through all leaves
      x<-expand.grid(   lapply(1:length(my.functions[[l]]), function(j) 
        
        if (!any(my.functions[[l]][j]==res[,s][[6]])) {
          1:(length(lim_list[[my.functions[[l]][j]]])-1)} else{
            1:ncol(as.matrix(lim_list[[my.functions[[l]][j]]]))
          }
      )     
      ) 
      
      # if (is.null(dim(x))){
      # 
      #   for (a in 1:(length(x)-1))
      #   {
      #     
      #     for (i in my.var.tree.index){
      #       for(j in 1:length(res[,s][[2]][[i]])){ ### go through all leaves
      #       
      #         in_leave <- all(sapply(1:length(my.functions[[l]]),  function(k) {  
      #           res[,s][[1]][[i]][j][1,k]>=lim_list[[1]][a] & res[,s][[1]][[i]][j][2,k]<=lim_list[[1]][a+1] 
      #         }
      #         ))
      #         
      #     values[[s]][[l]][a] <- values[[s]][[l]][a] + res[,s][[2]][[i]][j] * in_leave
      #     individuals[[s]][[l]][a] <- individuals[[s]][[l]][a] + length(res[,s][[7]][[i]][j]) * in_leave
      #       
      #        
      #       } 
      #      } 
      #   }
      #   
      #   
      # }else
      {
        
        for (a in 1:(nrow(x)))
        {
          for (i in my.var.tree.index){
            for(j in 1:length(res[,s][[2]][[i]])){
              
              
              in_leave <- all(sapply(1:length(my.functions[[l]]),  function(k) {
                kk<-my.functions[[l]][k]
                if (!any(my.functions[[l]][k]==res[,s][[6]])){
                  res[,s][[1]][[i]][[j]][1,my.functions[[l]][k]]<=lim_list[[kk]][x[a,k]] & res[,s][[1]][[i]][[j]][2,my.functions[[l]][k]]>=lim_list[[kk]][[x[a,k]+1]]
                }else{
                  all(is.element(sort(lim_list[[kk]][,x[a,k]]),res[,s][[1]][[i]][[j]][,my.functions[[l]][k]])) ### any may be enough
                }
              }
              ))
              values[[l]][as.matrix((x[a,]))] <- values[[l]][as.matrix((x[a,]))] + res[,s][[2]][[i]][j] * in_leave 
              #individuals[[s]][[l]][as.matrix((x[a,]))] <- individuals[[s]][[l]][as.matrix((x[a,]))] + length(res[,s][[7]][[i]][j]) * in_leave
            } 
          }
          
          individuals[[l]][as.matrix((x[a,]))] <- sum(apply(
            sapply(1:length(my.functions[[l]]), function(k){
              kk <- my.functions[[l]][k]
              if (!any(my.functions[[l]][k]==res[,s][[6]])){
                #res[,s][[1]][[i]][j][1,k]>=lim_list[[k]][x[a,k]] & res[,s][[1]][[i]][j][2,k]<=lim_list[[k]][[x[a+1,k]]]
                up <- lim_list[[kk]][[x[a,k]+1]]
                #  if ((x[a,k]+1)==length(lim_list[[kk]])) up <- lim_list[[kk]][[x[a,k]+1]] + 0.01
                X[,my.functions[[l]][k]]>=lim_list[[kk]][x[a,k]] & X[,my.functions[[l]][k]]<up
              }else{
                is.element(X[,my.functions[[l]][k]],lim_list[[kk]][,x[a,k]]) 
              }
              
              
            }), 1, all))
        }
      }
    }
    
    #go through my.functions from top to bottom. 
    
    l_max = length(my.functions)  ### length of original components
    
    my.functions[[l_max +1]] <-  0   ### add constant function to the list of functions
    values[[l_max +1]] <-  array(0,dim=1)
    
    
    #### getnew components
    
    all_components_created <- 0
    while(  all_components_created == 0){
      
      for (l in length( my.functions):1){
        
        for (j in (1:length(dim(values[[l]])))){ ## j=which variable to integrate over
          
          #l2 is tree that has same variables as l minus j-variable
          
          if (length(my.functions[[l]])!= 1){ 
            l2 <- which(sapply(1:length( my.functions ), function (z) 
            {length(my.functions[[z]])==length(my.functions[[l]][-j])&
                all(is.element(my.functions[[z]],my.functions[[l]][-j]))}
            )
            )
          } else l2 <- l_max +1
          
          if (length(l2)==0)  {
            
            my.functions[[length(my.functions)+1]] <- my.functions[[l]][-j] 
            values[[length(my.functions)]] <-  array(0, dim=dim(values[[l]])[-j])
          }
        }
      }
      
      all_components_created <- 1
      for (l in length( my.functions):1){
        
        
        
        for (j in (1:length(dim(values[[l]])))){ ## j=which variable to integrate over
          
          
          
          #l2 is tree that has same variables as l minus j-variable
          
          if (length(my.functions[[l]])!= 1){ 
            l2 <- which(sapply(1:length( my.functions ), function (z) 
            {length(my.functions[[z]])==length(my.functions[[l]][-j])&
                all(is.element(my.functions[[z]],my.functions[[l]][-j]))}
            )
            )
          } else l2 <- l_max +1
          
          if (length(l2)==0)  {
            
            all_components_created <- 0
            break
            
          }
        }
      }
    }
    
    #### get individuals for leaves in new components 
    if ((l_max +2)<=length( my.functions)){
      for (l in  (l_max +2):length( my.functions)){
        
        
        x<-expand.grid( lapply(my.functions[[l]], function(j){
          if (!any(j==res[,s][[6]])) {
            1:(length(lim_list[[j]])-1)} else{
              1:ncol(as.matrix(lim_list[[j]]))
            }}
        )     
        )
        
        individuals[[l]] <- array(0, dim=sapply(1:length(my.functions[[l]]), function(m) {
          
          if (!any(my.functions[[l]][m]==res[,s][[6]])) {length(lim_list[[my.functions[[l]][m]]] )-1} else{
            ncol(as.matrix(lim_list[[my.functions[[l]][m]]]))
          }
          
        }
        ))
        
        for (a in 1:(nrow(x)))
        {
          # print(    dim(individuals[[l]]))
          individuals[[l]][as.matrix((x[a,]))] <- sum(apply(
            sapply(1:length(my.functions[[l]]), function(k){
              kk <- my.functions[[l]][k]
              if (!any(my.functions[[l]][k]==res[,s][[6]])){
                #res[,s][[1]][[i]][j][1,k]>=lim_list[[k]][x[a,k]] & res[,s][[1]][[i]][j][2,k]<=lim_list[[k]][[x[a+1,k]]]
                up <- lim_list[[kk]][[x[a,k]+1]]
                #  if ((x[a,k]+1)==length(lim_list[[kk]])) up <- lim_list[[kk]][[x[a,k]+1]] + 0.01
                X[,my.functions[[l]][k]]>=lim_list[[kk]][x[a,k]] & X[,my.functions[[l]][k]]<up
              }else{
                is.element(X[,my.functions[[l]][k]],lim_list[[kk]][,x[a,k]]) 
              }
              
              
            }), 1, all))
        }
        
      }
    }
    
    
    
    
    #################
    #######################         Purify
    
    
    ##### sort components
    values <- values[order(sapply(my.functions,function(x) length(x) ))]
    individuals <- individuals[order(sapply(my.functions,function(x) length(x) ))]
    
    my.functions <- my.functions[order(sapply(my.functions,function(x) length(x) ))]
    
    #create Vector for new values
    values_new = values
    
    
    ### where is the constant function
    l0 <- which(sapply(my.functions,"[[",1)==0)
    
    
    #### start purify
    for (l in (length( my.functions):1)[(length( my.functions):1)!=l0]   ){ ## go through all functions
      
      #print(c("start",s,l))
      x<-expand.grid( lapply(my.functions[[l]], function(j){ ## x= all leaves in function
        if (!any(j==res[,s][[6]])) {
          1:(length(lim_list[[j]])-1)} else{
            1:ncol(as.matrix(lim_list[[j]]))
          }
      }
      )     
      )
      
      variables_subsets = subsets(1:length(my.functions[[l]]))
      
      variables_subsets = variables_subsets[2:length(variables_subsets)]
      
      for (j in 1:length(variables_subsets)){ ## j=which variable to integrate over
        
        #l2 are the trees to be updated
        
        l2 <- which(sapply(1:l, function (z) 
        {all(is.element(setdiff(my.functions[[l]],my.functions[[z]]),my.functions[[l]][variables_subsets[[j]]]))&
            all(is.element(my.functions[[z]],my.functions[[l]]))}
        )
        )
        
        if (length(my.functions[[l]])!= length(variables_subsets[[j]])){
          
          xj       <- as.matrix(unique( x[ ,-variables_subsets[[j]] ]))
          
          n.values <-  nrow(xj)
          
          n.values.j=expand.grid( lapply(variables_subsets[[j]], function(v){ 1:dim(values[[l]])[v] }))
          
          for(v1 in 1:l){
            
            if(length(my.functions[[v1]]== length(variables_subsets[[j]]))){
              
              if(all(is.element(my.functions[[v1]], my.functions[[l]][variables_subsets[[j]]]))){
                
                ori=v1
              }
            }
          }
          
          for(k in 1:n.values){
            
            slice <- rep(xj[k,],dim(n.values.j)[1])
            dim(slice) = c(length(xj[k,]),dim(n.values.j)[1])
            slice = t(slice)
            
            for(v in 1:dim(n.values.j)[2]){
              
              if (variables_subsets[[j]][v]==1){ 
                
                slice <- cbind(n.values.j[,v],slice) 
              } else if(variables_subsets[[j]][v]>dim(slice)[2]){ 
                
                slice <- cbind(slice, n.values.j[,v]) 
              } else{
                
                slice <- cbind(slice[,1:(variables_subsets[[j]][v]-1)], n.values.j[,v],slice[,variables_subsets[[j]][v]:dim(slice)[2]])
              }  
            }
            
            if (is.null(dim(slice))) slice<-t(slice)
            
            if (sum(individuals[[l]][as.matrix(slice)])!=0){
              
              weight  <- individuals[[ori]]/ sum(individuals[[ori]])
              avg     <- sum(weight* values[[l]][as.matrix(slice)])
              
              for(v in l2){
                
                v_Indizees = c()
                
                for(w in my.functions[[v]]){
                  
                  v_Indizees = c(v_Indizees, which(w == my.functions[[l]]))
                  
                }
                values_new[[v]][as.matrix(unique(slice[,v_Indizees]))] <- values_new[[v]][as.matrix(unique(slice[,v_Indizees]))] + (-1)^{length(variables_subsets[[j]])+length(my.functions[[l]])+length(my.functions[[v]])}*as.numeric(avg)
              }
            }
          }
        } 
        
        if(length(my.functions[[l]])== length(variables_subsets[[j]])) {
          
          weight <- individuals[[l]] / sum(individuals[[l]])
          avg   <- sum(weight* values[[l]])
          values_new[[l0]] <- values_new[[l0]] + as.numeric(avg)
          
          for(v in l2){
            
            values_new[[v]] <- values_new[[v]] + (-1)^{length(my.functions[[v]])}*as.numeric(avg)
          }
        }
      }
      #  print(c("end",s,l))   
    }
    
    return(list(values=values_new, lim_list =lim_list ,individuals=individuals, my.functions= my.functions))
  }, mc.cores=cores)
}


pred_shap_rpf<-function(x,pur_res, interpret=TRUE, shap=FALSE,cores){
  #sapply(1:nrow(x), function(i){
  
  categorical_variables=pur_res$categorical_variables
  
  if (is.null(dim(x)))  x<-t(as.matrix(x))
  x <-as.matrix(x)
  
  if(!interpret){ 
    pred <- apply(sapply(1:length(pur_res),
                         function(s){
                           l0 <- which(sapply(pur_res[[s]]$my.functions,"[[",1)==0)
                           return(rowSums(as.matrix(sapply((1:length(pur_res[[s]]$values))[-l0], ### go through all non-zero compoennts
                                                           function(k){
                                                             my.var <-pur_res[[s]]$my.functions[[k]]  
                                                             
                                                             pos <- numeric(length(my.var))
                                                             y <- sapply(1:nrow(x),function(i){
                                                               for(kk in (1:length(pos))){
                                                                 
                                                                 if (!is.element(my.var[kk], categorical_variables)){
                                                                   bounds <-  unlist((pur_res[[s]]$lim_list)[[my.var[kk]]])
                                                                   bounds[length(bounds)]<- bounds[length(bounds)]
                                                                   pos[kk] <- which(x[i,my.var[kk]]<bounds)[1]-1
                                                                   
                                                                   if (is.na(pos[kk])) {print("warning: observation is out of training range; extrapolating") 
                                                                     pos[kk]<-length(bounds)-1} else if (pos[kk]==0) {print("warning: observation is out of training range; extrapolating") 
                                                                       pos[kk]<-1}
                                                                   
                                                                   
                                                                 } else {    leaves <-  pur_res[[s]]$lim_list[[my.var[kk]]]
                                                                 pos[kk] <- which(sapply(1:ncol(leaves), function(j) is.element(x[i,my.var[kk]],  leaves[,j])))
                                                                 if (length(pos[kk])==0) {print("warning: observation is out of training range; extrapolating") }
                                                                 }
                                                                 
                                                               }
                                                               return(pur_res[[s]]$values[[k]][t(as.matrix(pos))]) 
                                                             })
                                                             as.numeric(y)}
                           )),na.rm=TRUE)+as.numeric(pur_res[[s]]$values[[l0]]))
                           
                         }
                         
                         
    ),1,mean)
    return(pred)
    
    # }
    #)
  } 
  
  if(shap){
    
    my_components = NULL
    
    for(s in 1:length(pur_res)){
      
      my_components = c(my_components, pur_res[[s]]$my.functions[sapply(1:length(pur_res[[s]]$my.functions), function(z) sum(abs(pur_res[[s]]$values[[z]]))>0)])
      my_components = unique(my_components)    
    }
    
    # my_components <- my_components[order(sapply(my_components, function(x){ if (length(x)>1) {length(x)+10} else x   }      ))]
    
    #my_components <- my_components[order(sapply(my_components, function(x){ if (length(x)>1) {length(x)+10} else x   }      ))]
    
    pred <- mclapply(1:nrow(x),function(i){
      
      
      
      
      res1 <-  sapply(1:length(pur_res),
                      function(s){
                        l0 <- which(sapply(pur_res[[s]]$my.functions,"[[",1)==0)
                        return(sapply(1:length(my_components), ### go through all compoennts
                                      function(k){
                                        
                                        component <- which(sapply(1:length(pur_res[[s]]$my.functions), function(l) setequal(pur_res[[s]]$my.functions[[l]],my_components[[k]])))
                                        
                                        if (length(component)==0) return(0)
                                        
                                        my.var <-pur_res[[s]]$my.functions[[component]]  
                                        
                                        if(sum(my.var==0)) return(pur_res[[s]]$values[[component]])
                                        
                                        pos <- numeric(length(my.var))
                                        
                                        
                                        {
                                          for(kk in (1:length(pos))){
                                            
                                            if (!is.element(my.var[kk], categorical_variables)){
                                              bounds <-  unlist((pur_res[[s]]$lim_list)[[my.var[kk]]])
                                              
                                              pos[kk] <- which(x[i,my.var[kk]]<bounds)[1]-1
                                              
                                              if (is.na(pos[kk])) {print("warning: observation is out of training range; extrapolating") 
                                                pos[kk]<-length(bounds)-1} else if (pos[kk]==0) {print("warning: observation is out of training range; extrapolating") 
                                                  pos[kk]<-1}
                                              
                                              
                                            } else {    leaves <-  pur_res[[s]]$lim_list[[my.var[kk]]]
                                            pos[kk] <- which(sapply(1:ncol(leaves), function(j) is.element(x[i,my.var[kk]],  leaves[,j])))
                                            if (length(pos[kk])==0) {print("warning: observation is out of training range; extrapolating") }
                                            }
                                            
                                          }
                                          y <- pur_res[[s]]$values[[component]][t(as.matrix(pos))]
                                          return(y)
                                        }
                                      }
                        ))
                      }
      )
      
      res1 <- apply(  res1  ,1,mean)
      
      var_names <- sapply(1:length(my_components), function(k){
        paste(
          paste(colnames(x)[as.numeric(my_components[[k]])], collapse = ","),"(",
          paste(as.numeric(my_components[[k]]), collapse = ","),")",sep = "")
      }
      )
      
      if(shap){
        
        res_shap=rep(0,ncol(x))
        
        for(k in 1:length(my_components)){
          
          if(!is.element(0, my_components[[k]])){
            
            for(i in my_components[[k]]){
              
              res_shap[[i]]=res_shap[[i]]+res1[k]/length(my_components[[k]])
            }
          }
        } 
      }
      
      res1 <- c(sum(res1,na.rm=TRUE), res1)
      
      names(res1)=c("Y.hat", var_names)
      
      if(shap){
        
        res1=c(res1,res_shap)
      }
      
      return(res1)
    },mc.cores=cores)
    
    return(do.call(rbind, pred))
    # }
    #)
  }
  
  if (interpret){
    
    # my_components <- sapply(1:length(pur_res), function(s)
    #   pur_res[[s]]$my.functions[sapply(1:length(pur_res[[s]]$my.functions), function(z) sum(abs(pur_res[[s]]$values[[z]]))>0)]
    # )  
    
    my_components = NULL
    
    for(s in 1:length(pur_res)){
      
      my_components = c(my_components, pur_res[[s]]$my.functions[sapply(1:length(pur_res[[s]]$my.functions), function(z) sum(abs(pur_res[[s]]$values[[z]]))>0)])
      my_components = unique(my_components)    
    }
    
    # my_components <- my_components[order(sapply(my_components, function(x){ if (length(x)>1) {length(x)+10} else x   }      ))]
    
    #my_components <- my_components[order(sapply(my_components, function(x){ if (length(x)>1) {length(x)+10} else x   }      ))]
    
    pred <- sapply(1:nrow(x),function(i){
      
      
      
      
      res1 <-  sapply(1:length(pur_res),
                      function(s){
                        l0 <- which(sapply(pur_res[[s]]$my.functions,"[[",1)==0)
                        return(sapply(1:length(my_components), ### go through all compoennts
                                      function(k){
                                        
                                        component <- which(sapply(1:length(pur_res[[s]]$my.functions), function(l) setequal(pur_res[[s]]$my.functions[[l]],my_components[[k]])))
                                        
                                        if (length(component)==0) return(0)
                                        
                                        my.var <-pur_res[[s]]$my.functions[[component]]  
                                        
                                        if(sum(my.var==0)) return(pur_res[[s]]$values[[component]])
                                        
                                        pos <- numeric(length(my.var))
                                        
                                        
                                        {
                                          for(kk in (1:length(pos))){
                                            
                                            if (!is.element(my.var[kk], categorical_variables)){
                                              bounds <-  unlist((pur_res[[s]]$lim_list)[[my.var[kk]]])

                                              pos[kk] <- which(x[i,my.var[kk]]<bounds)[1]-1
                                              
                                              if (is.na(pos[kk])) {print("warning: observation is out of training range; extrapolating") 
                                                pos[kk]<-length(bounds)-1} else if (pos[kk]==0) {print("warning: observation is out of training range; extrapolating") 
                                                  pos[kk]<-1}
                                              
                                              
                                            } else {    leaves <-  pur_res[[s]]$lim_list[[my.var[kk]]]
                                            pos[kk] <- which(sapply(1:ncol(leaves), function(j) is.element(x[i,my.var[kk]],  leaves[,j])))
                                            if (length(pos[kk])==0) {print("warning: observation is out of training range; extrapolating") }
                                            }
                                            
                                          }
                                          y <- pur_res[[s]]$values[[component]][t(as.matrix(pos))]
                                          return(y)
                                        }
                                      }
                        ))
                      }
      )
      
      res1 <- apply(  res1  ,1,mean)
      
      var_names <- sapply(1:length(my_components), function(k){
        paste(
          paste(colnames(x)[as.numeric(my_components[[k]])], collapse = ","),"(",
          paste(as.numeric(my_components[[k]]), collapse = ","),")",sep = "")
      }
      )
      
      res1 <- c(sum(res1,na.rm=TRUE), res1)
      
      names(res1)=c("Y.hat", var_names)
      
      
      return(res1)
    })
    
    return(t(pred))
    # }
    #)
  } 
}
