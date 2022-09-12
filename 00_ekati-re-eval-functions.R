
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  
}

PCA <- function(dat) {
  
  cor.mat <- cor(dat, use='pairwise.complete.obs')
  
  eig <- eigen(x=cor.mat, symmetric=TRUE)
  
  Eigen.Val <- round(eig$val,4)
  Eigen.Val[which(Eigen.Val< 0)] <- 0
  
  Prop.Var.Explain <- Eigen.Val/sum(Eigen.Val)
  
  Cumulative.Var.Explain <- NULL 
  
  for (i in 1:length(Prop.Var.Explain)){	
    Cumulative.Var.Explain[i] <- sum(Prop.Var.Explain[1:i]) 
  }
  
  Eigen.Vec <- eig$vec
  Eigen.Vec[,1] <- -Eigen.Vec[,1]
  
  Eig.Val.Mat <- t(matrix(c(rep(Eigen.Val,dim(Eigen.Vec)[2])),nrow=dim(Eigen.Vec)[1]))
  Loadings <- round(Eigen.Vec*Eig.Val.Mat^0.5,4)
  rownames(Loadings) <- colnames(dat)
  Loadings.save <- round(Loadings[rev(order(Loadings[,1])),],4)
  
  propvar <- round(cbind(Prop.Var.Explain,Cumulative.Var.Explain),4)
  
  if(sum(is.na(dat))>0) { 
   # print('Warning: Missing values are replaced with a standardized mean of 0 for PC Score Calculations')
  }
  
  fill.zero<-function(num)
  {
    if (is.na(num)){
      num <- 0
    }
    num
  }
  
  dat.impute <- apply(dat,c(1,2), fill.zero)
  Scores.impute <- dat.impute%*%(Eigen.Vec)
  
  if(mean(Loadings[,1] < 0) > 0.4) {
    
    Loadings[,1] <- -1 *Loadings[,1]
    Scores.impute[,1] <- -1*Scores.impute[,1]
    
  }
  
  return(list(loadings=Loadings, 
              ordered.loadings=Loadings.save, 
              Scores=Scores.impute, 
              Eigen.Vec=Eigen.Vec, 
              Eigen.Val=Eigen.Val, 
              Prop.Var.Explain=Prop.Var.Explain,
              Cumulative.Var.Explain=Cumulative.Var.Explain))
  
}

wrangle_x_variables_biomass <- function(df, col_name){
  
  df %>% dplyr::select(Lake, Year, {{col_name}}) %>% group_by(Lake, Year) %>%
    summarise(new_col = mean({{col_name}}, na.rm = TRUE)) %>% 
    mutate(Watershed = case_when(Lake %in% koala_sites ~ "koala",
                                 Lake %in% kingcujo_sites ~ "kingcujo",
                                 Lake %in% horseshoe_sites ~ "horseshoe",
                                 Lake %in% ref_sites ~  "reference",
                                 TRUE ~ "Error")) %>% 
    filter(Watershed != "Error")
  
  
  
}


wrangle_x_variables_environmental <- function(df, col_name1, col_name2){
  
  
  df %>% dplyr::select(Lake, Year, {{col_name1}}, {{col_name2}}) %>% group_by(Lake, Year) %>%
    summarise(new_col1 = mean({{col_name1}}, na.rm = TRUE),
              new_col2 = mean({{col_name2}}, na.rm = TRUE))%>% 
    mutate(Watershed = case_when(Lake %in% koala_sites ~ "koala",
                                 Lake %in% kingcujo_sites ~ "kingcujo",
                                 Lake %in% horseshoe_sites ~ "horseshoe",
                                 Lake %in% ref_sites ~  "reference",
                                 TRUE ~ "Error")) %>% 
    filter(Watershed != "Error")
  
  
  
}




plot_relativeD_phyto <- function(df){
  
  df %>%  ggplot(aes(x= Year, y= density_percent, fill= class))+geom_bar(stat = "identity")+labs(title = unique(df$Watershed))+
    facet_wrap(~Site)
  
  
}


scatterplots_Rdensity_vs_pc <- function(df,x_vars, x_names,pc_num){
  
  df_mod <- df %>% dplyr::select(Year , Site  ,Bacillariophyta:Unidentified.Algae,   Watershed, Concentration.Factor,Waterbody, all_of(x_vars))
  
  df_mod_long <- df_mod %>% pivot_longer(cols = Bacillariophyta:Unidentified.Algae, names_to = "Class", values_to = "Rel_Density") %>% 
    pivot_longer(cols = x_vars, names_to =  x_names, values_to = "X") %>% filter(PC == pc_num)
  
   
  # df_mod_long %>% ggplot(aes(x=X, y=Rel_Density, col= Class, size= Year))+ geom_point()+labs(title = unique(df_mod_long$Watershed), x= pc_num)+
  #    facet_wrap(~Site)
  # 
  
  df_mod_long %>% ggplot(aes(x=X, y=Rel_Density, col=Site))+ geom_point()+labs(title = unique(df_mod_long$Watershed), x= pc_num)+
    facet_wrap(~Class)
  
}



scatterplots_Rdensity_vs_biomass <- function(df,x_vars, x_names,biomass_vars){
  
  df_mod <- df %>% dplyr::select(Year , Site  ,Bacillariophyta:Unidentified.Algae,   Watershed, Concentration.Factor,Waterbody, all_of(x_vars))
  
  df_mod_long <- df_mod %>% pivot_longer(cols = Bacillariophyta:Unidentified.Algae, names_to = "Class", values_to = "Rel_Density") %>% 
    pivot_longer(cols = x_vars, names_to =  x_names, values_to = "X") %>% filter(Bio_mass == biomass_vars)
  
  # df_mod_long %>% ggplot(aes(x=X, y=Rel_Density, col= Class, size= Year))+ geom_point()+labs(title = unique(df_mod_long$Watershed), x= biomass_vars)+
  #   facet_wrap(~Site)
  # 
  df_mod_long %>% ggplot(aes(x=X, y=Rel_Density, col=Site))+ geom_point()+labs(title = unique(df_mod_long$Watershed), x= biomass_vars)+
    facet_wrap(~Class)
  
  
}


scatterplots_Rdensity_vs_environmental <- function(df,x_vars, x_names,env_vars){
  
  df_mod <- df %>% dplyr::select(Year , Site  ,Bacillariophyta:Unidentified.Algae,   Watershed, Concentration.Factor,Waterbody, all_of(x_vars))
  
  df_mod_long <- df_mod %>% pivot_longer(cols = Bacillariophyta:Unidentified.Algae, names_to = "Class", values_to = "Rel_Density") %>% 
    pivot_longer(cols = x_vars, names_to =  x_names, values_to = "X") %>% filter(environmental == env_vars)
  
  #df_mod_long %>% ggplot(aes(x=X, y=Rel_Density, col= Class, size= Year))+ geom_point()+labs(title = unique(df_mod_long$Watershed), x= env_vars) + facet_wrap(~Site)

  df_mod_long %>% ggplot(aes(x=X, y=Rel_Density, col= Site))+ geom_point()+labs(title = unique(df_mod_long$Watershed), x= env_vars) + facet_wrap(~Class)
  
}

