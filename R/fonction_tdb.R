TDB<-function(tbl, var, sexe, c_age, nomvar){
  
  tbl_freq_s_a<-dcast(tbl, sexe + c_age ~ var)[,-c(1,2)]
  tbl_freq_a<-dcast(tbl,  c_age ~ var)[,-c(1)]
  tbl_freq_s_e<-rbind(dcast(tbl,  sexe ~ var)[,-c(1)], dcast(tbl,  . ~ var)[,-c(1)])
  
  tbl_pr_s_a<-round(tbl_freq_s_a/apply(tbl_freq_s_a,1,sum)*100,2)
  tbl_pr_a<-round(tbl_freq_a/apply(tbl_freq_a,1,sum)*100,2)
  tbl_pr_s_e<-round(tbl_freq_s_e/apply(tbl_freq_s_e,1,sum)*100,2)
  
  nClass<-length(levels(var))
  vect_c<-NULL
  for (i in 1:nClass)
  {vect_c<-append(vect_c,c(i,nClass+i)) 
   vect_c }
  
  tbl_M_F <-cbind(tbl_freq_s_a, tbl_pr_s_a)[, vect_c]
  tbl_E   <-cbind(tbl_freq_a, tbl_pr_a)[, vect_c]
  tbl_A   <-cbind(tbl_freq_s_e, tbl_pr_s_e)[, vect_c]
  
  nClass_age<-length(levels(c_age))
  h<-1:(3*nClass_age)
  vect_a <- c(rbind(matrix(h, nrow = nClass_age), (3*nClass_age+1):(3*nClass_age+3)))
  
  tdb<-rbind(tbl_M_F,tbl_E,tbl_A)[vect_a,]
  names(tdb)<-nomvar
  tdb
}

tbl_char<- function(x,entete,label){
  
  y=sapply(x, function(x) iconv(x,  "UTF-8", "latin1"))
  ny=nrow(y)
  cvide = rep('', length(label))
  rvide = rep('', ncol(x)+1)
  
  k_h  <- cbind(cvide, label, y[1:(ny/3),])
  k1_h <- rbind(c('Homme', rvide), k_h)
  
  k_f  <- cbind(cvide, label, y[((ny/3)+1):(2*(ny/3)),])
  k1_f <- rbind(c('Femme', rvide), k_f)
  
  k_e  <- cbind(cvide, label, y[(2*((ny/3))+1):(3*(ny/3)),])
  k1_e <- rbind(c('Ensemble', rvide), k_e)
  
  all_ch <- rbind(k1_h, k1_f, k1_e)
  colnames(all_ch) <- entete
  return(all_ch)
}



graph<-function(x,y){
  vec=seq(from = 2, to = 2*y, by = 2)
  lign=c(1:3,5:7,9:11)
  pl=x[lign , vec]
  
  datm <- melt(cbind(pl, ind = rownames(pl)), id.vars = c('ind'))
  
  Data <- group_by(datm,ind) %>% mutate(pos = cumsum(value) - (0.5 * value))
  
  Data$ind   <- factor(Data$ind , levels=c(12,9,8,7,11,6,5,4,10,3,2,1))
  
  
  ggplot(Data,aes(x = ind, y = value,fill = variable)) + 
    geom_bar(aes(fill = variable),stat="identity",width=0.4) + 
    geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 4,colour="#FFFFFF", family="Open Sans")+
    scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
    annotate("text", x = 9, y =-30, label = "Hommes") +
    annotate("text", x = 9, y =-10, label = "18-29 ans") +
    annotate("text", x = 8, y =-10, label = "30-59 ans") +
    annotate("text", x = 7, y =-10, label = "60 ans et plus") +
    annotate("text", x = 6, y =-30, label = "Femmes") +
    annotate("text", x = 6, y =-10, label = "18-29 ans") +
    annotate("text", x = 5, y =-10, label = "30-59 ans") +
    annotate("text", x = 4, y =-10, label = "60 ans et plus") +
    annotate("text", x = 3, y =-30, label = "Ensemble") +
    annotate("text", x = 3, y =-10, label = "18-29 ans") +
    annotate("text", x = 2, y =-10, label = "30-59 ans") +
    annotate("text", x = 1, y =-10, label = "60 ans et plus") +
    ylab("Pourcentage")+
    xlab(" ")+
    coord_flip() +
    theme(#axis.line=element_blank(),
      #axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      #axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="top",
      panel.background=element_blank(),
      #panel.border=element_blank(),
      #panel.grid.major=element_blank(),
      #panel.grid.minor=element_blank(),
      plot.background=element_blank())
  
}





knit_print.data.frame = function(x, ...) {
  res = paste(c("", "", kable(x)), collapse = "\n")
  asis_output(res)
}



