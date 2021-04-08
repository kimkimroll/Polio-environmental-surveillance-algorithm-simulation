setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/simulation")


library(tidyverse)
library(readr)
library(janitor)

envs<-function(rw){
  
  #number of simulations
  n.sims<-10
  
  #randomly assigning positive or negative results to each assay
  
  p1f1<-sample(c("positive", "negative"), n.sims, replace = TRUE, prob = c(0.3, 0.7))
  p1f2<-sample(c("positive", "negative"), n.sims, replace = TRUE, prob = c(0.3, 0.7))
  p1f3<-sample(c("positive", "negative"), n.sims, replace = TRUE, prob = c(0.3, 0.7))
  p1f4<-sample(c("positive", "negative"), n.sims, replace = TRUE, prob = c(0.3, 0.7))
  p1f5<-sample(c("positive", "negative"), n.sims, replace = TRUE, prob = c(0.3, 0.7))
  p1f6<-sample(c("positive", "negative"), n.sims, replace = TRUE, prob = c(0.7, 0.3)) 

  #bind all results together by column
  new1<-as.data.frame(cbind(p1f1, p1f2, p1f3, p1f4, p1f5, p1f6))
  print(new1)
  
  #conditioning based on pos/neg flasks from 1st passage
  #if NEG
  new1$p2f1[new1$p1f1 == "negative"]<-"negative"
  new1$p2f2[new1$p1f2 == "negative"]<-"negative"
  new1$p2f3[new1$p1f3 == "negative"]<-"negative"
  new1$p2f4[new1$p1f4 == "negative"]<-"negative"
  new1$p2f5[new1$p1f5 == "negative"]<-"negative"
  new1$p2f6[new1$p1f6 == "negative"]<-"negative"
  
  new1$p2f1[new1$p1f1 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new1$p2f2[new1$p1f2 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new1$p2f3[new1$p1f3 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new1$p2f4[new1$p1f4 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new1$p2f5[new1$p1f5 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new1$p2f6[new1$p1f6 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  
  new2<-as.data.frame(cbind(new1))
  print(new2)
  
  new2$p3f1[new2$p2f1 == "negative"]<-"negative"
  new2$p3f2[new2$p2f2 == "negative"]<-"negative"
  new2$p3f3[new2$p2f3 == "negative"]<-"negative"
  new2$p3f4[new2$p2f4 == "negative"]<-"negative"
  new2$p3f5[new2$p2f5 == "negative"]<-"negative"
  new2$p3f6[new2$p2f6 == "negative"]<-"negative"
  
  new2$p3f1[new2$p2f1 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new2$p3f2[new2$p2f2 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new2$p3f3[new2$p2f3 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new2$p3f4[new2$p2f4 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new2$p3f5[new2$p2f5 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  new2$p3f6[new2$p2f6 == "positive"]<-sample(c("positive", "negative"), 1, replace = TRUE, prob = c(0.7, 0.3)) 
  
  new3<-as.data.frame(cbind(new2))
  print(new3)
  
  #if ITD
  new3$itd1[new2$p3f1 == "positive"]<-"yes"
  new3$itd2[new2$p3f2 == "positive"]<-"yes"
  new3$itd3[new2$p3f3 == "positive"]<-"yes"
  new3$itd4[new2$p3f4 == "positive"]<-"yes"
  new3$itd5[new2$p3f5 == "positive"]<-"yes"
  new3$itd6[new2$p3f6 == "positive"]<-"yes"
  
  new3$itd1[new2$p3f1 == "negative"]<-"no"
  new3$itd2[new2$p3f2 == "negative"]<-"no"
  new3$itd3[new2$p3f3 == "negative"]<-"no"
  new3$itd4[new2$p3f4 == "negative"]<-"no"
  new3$itd5[new2$p3f5 == "negative"]<-"no"
  new3$itd6[new2$p3f6 == "negative"]<-"no"

  #1 is negative
  #2 is positive
  
  #add DASH column to data
  new4<-new3 %>% mutate(DASH = row_number(),
                        DASH = as.character(DASH)) 
  
  total_itd <- new4 %>% group_by(DASH) %>%
    mutate(itd1 = as.character(itd1),
                          itd2 = as.character(itd2),
                          itd3 = as.character(itd3),
                          itd4 = as.character(itd4),
                          itd5 = as.character(itd5),
                          itd6 = as.character(itd6)) %>%
    
    summarise("overall" = sum(itd1 == 'yes',
                              itd2 == 'yes',
                              itd3 == 'yes',
                              itd4 == 'yes',
                              itd5 == 'yes',
                              itd6 == 'yes', na.rm = TRUE),
              "flask1" = sum(itd1 == 'yes', na.rm = TRUE),
              "flask2" = sum(itd2 == 'yes', na.rm = TRUE),
              "flask3" = sum(itd3 == 'yes', na.rm = TRUE),
              "flask4" = sum(itd4 == 'yes', na.rm = TRUE),
              "flask5" = sum(itd5 == 'yes', na.rm = TRUE),
              "flask6" = sum(itd6 == 'yes', na.rm = TRUE),
    )
  
  new5<-left_join(new4, total_itd, by = "DASH")
  
  print(new5)
  
  output<-as.data.frame(cbind(new5))

  print(output)
  return(output)

}

envs(rw)
rw1<-envs(rw)

#save sim1
write.csv(rw1, "//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/simulation/rw1.csv", row.names = FALSE)




#SIM 2 for ITD ct values
#grab POS flask from cell culture

#sanity check totals
print(rw1)

total = sum(rw1$overall)

ct<-function(rw2){
  
  #number of simulations by positive flasks
    rw2 <- rw1 %>%
    group_by(DASH) %>%
    select(DASH, flask1, flask2, flask3, flask4, flask5, flask6) %>%
    gather(rw1, "flask", flask1:flask6) %>%
    mutate(flask = as.numeric(flask)) %>%
    filter(flask == '1')# %>%
    n.flasks<-nrow(rw2[rw2$flask == '1',])
    n.flasks<-as.numeric(n.flasks)
    rw2 <- rw2 %>% select(-flask)

  #randomly assigning positive or negative results to each assay
  
  rw2$sabin1<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.7, 0.3))
  
  rw2$sabin2<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.035, 0.965))
  
  rw2$sabin3<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.3, 0.3))
  
  rw2$WPV1<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.018, 0.982))
  
  rw2$Qb<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.999, 0.001))
  
  rw2$WPV3<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.0, 1)) 
  
  rw2$PVsec<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.7, 0.3))
  
  rw2$PV2sec<-sample(c("positive", "negative"), n.flasks, replace = TRUE, prob = c(0.0001, 0.9999))
  
  #bind all results together by column
  #new1<-as.data.frame(cbind(rw2, rw1, sabin1, sabin2, sabin3, WPV1, Qb, WPV3, PVsec, PV2sec))
  print(rw2)
  new1<-rw2
  
  #define ct values for positive results. randomly generated by assay
  new1$Sabin1_ct<-sample(17:37, size = n.flasks, replace = TRUE)
  new1$Sabin1_ct[new1$sabin1 == "negative"]<-"NA" 
  
  new1$Sabin2_ct<-sample(20:34, size = n.flasks, replace = TRUE)
  new1$Sabin2_ct[new1$sabin2 == "negative"]<-"NA" 
  
  new1$Sabin3_ct<-sample(16:35, size = n.flasks, replace = TRUE)
  new1$Sabin3_ct[new1$sabin3 == "negative"]<-"NA"
  
  new1$WPV1_ct<-sample(15:33, size = n.flasks, replace = TRUE)
  new1$WPV1_ct[new1$WPV1 == "negative"]<-"NA"
  
  new1$QbCt<-sample(16:32, size = n.flasks, replace = TRUE)
  new1$QbCt[new1$Qb == "negative"]<-"NA"
  
  new1$WPV3_ct<-sample(20:31, size = n.flasks, replace = TRUE)
  new1$WPV3_ct[new1$WPV3 == "negative"]<-"NA"
  
  
  #conditioning PanPV based on each assay's result
  
  new1$PV[new1$WPV1_ct > 32 | new1$Sabin1_ct > 32 | new1$Sabin2_ct > 32 | new1$Sabin3_ct > 32 | new1$WPV3_ct > 32]<-"negative"
  new1$PV[new1$WPV1_ct <= 32 | new1$Sabin1_ct <= 32 | new1$Sabin2_ct <= 32 | new1$Sabin3_ct <= 32 | new1$WPV3_ct <= 32]<-"positive"
  
  new1$PVthir[new1$PV == "negative" & new1$PVsec == "negative"]<-"negative"
  new1$PVthir[new1$PV == "negative" & new1$PVsec == "positive"]<-"positive"
  new1$PVthir[new1$PV == "positive" & new1$PVsec == "positive"]<-"positive"
  new1$PVthir[new1$PV == "positive" & new1$PVsec == "negative"]<-"positive"
  
  #1 is negative
  #2 is positive
  
  #if PanPV positive, then assign Ct value
  
  new1$panPV_ct<-sample(17:39, size = n.flasks, replace = TRUE)
  new1$panPV_ct[new1$PVthir == "negative"]<-"NA"
  
  #conditioning PV2 based on Sabin 2 Ct and result
  
  new1$PV2[new1$sabin2 == "negative"]<-"negative"
  new1$PV2[new1$sabin2 == "positive"]<-"positive"
  
  new1$PV2thir[new1$PV2sec == "negative" & new1$PV2 == "negative"]<-"negative"
  new1$PV2thir[new1$PV2sec == "positive" & new1$PV2 == "positive"]<-"positive"
  new1$PV2thir[new1$PV2sec == "positive" & new1$PV2 == "negative"]<-"positive"
  new1$PV2thir[new1$PV2sec == "negative" & new1$PV2 == "positive"]<-"positive"
  
  #if Sabin 2 positive, assign Ct value to PV2
  
  new1$PV2_ct<-sample(20:31, size = n.flasks, replace = TRUE)
  new1$PV2_ct[new1$PV2thir == "negative"]<-"NA"
  
  
  #add DASH column to data
  new2<-new1 #%>% mutate(DASH = row_number())
  print(new2)
  
  output<-as.data.frame(cbind(new2$DASH, new2$rw1, new2$QbCt, new2$Sabin1_ct, new2$Sabin2_ct, new2$Sabin3_ct, new2$WPV1_ct, new2$PV2_ct, new2$WPV3_ct, new2$PV, new2$PVsec,  new2$PVthir, new2$panPV_ct, new2$PV2, new2$PV2sec, new2$PV2thir, new2$PV2_ct))
  
  colnames(output)<-c("DASH", "Flask", "QbCt", "Sabin1_ct", "Sabin2_ct", "Sabin3_ct",  "WPV1_ct", "PV2_ct", "WPV3_ct", "PV1st", "PVsec", "PVthir", "panPV_ct", "PV2", "PV2sec", "PV2thir", "PV2_ct")  
  print(output)
  return(output)
}


ct(rw2)

rw2<-ct(rw2)

#save sim1
write.csv(rw2, "//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/simulation/rw2.csv", row.names = FALSE)



