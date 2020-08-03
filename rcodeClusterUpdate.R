#creating upload files for the clusters in R
#reading data from excel
rm(list = ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)

cluster <- read_excel("C:/Users/user/Desktop/worked/mwendaR.xlsx", sheet = "cluster", range = "a5:n7")
cluster$interview__id=cluster$clu00
head(cluster)
View(cluster)


str <- read_excel("C:/Users/user/Desktop/worked/mwendaR.xlsx",  sheet = "struct", range = "a4:k192")
str$interview__id=str$clu00
head(str)
View(str)

hh <- read_excel("C:/Users/user/Desktop/worked/mwendaR.xlsx", sheet = "hh", range = "a4:r258")
hh$interview__id=hh$clu00
head(hh)
View(hh)

##create the nassepv tab

#create the interview__id




###write the first nassep VI tab 
write.table(cluster, file="C:/Users/user/Desktop/worked/nassepv.TAB", na = "",
                    row.names = F, col.names = T,   quote = F,   sep = '\t')


########################################--------------end of nassep part

head(str)
names(str)
attach(str)

data_nas<-str5 %>% group_by(interview__id) %>% 
  mutate(clusterpart__id = ifelse(s02 %in% 1:40,1,
                                  ifelse(s02%in% 41:80,2,
                                         ifelse(s02%in% 81:120,3,
                                                ifelse(s02%in% 121:160,4,
                                                       ifelse(s02%in% 161:200,5,6)))))) %>%
           ungroup() %>%
  group_by(interview__id,clusterpart__id) %>% mutate(val=1:n()-1,nid = cur_group_id()) %>% ungroup() %>%
  select(-clusterpart__id) %>% mutate(interview__id=paste0(interview__id,'.',nid),val=paste0('s02','__',val)) %>% select(-nid) %>%
  pivot_wider(names_from = c(val),values_from = s02) %>%
  mutate(interview__id=gsub("\\..*","", interview__id)) %>% group_by(interview__id) %>%
  mutate(clusterpart__id=1:n()) #%>% select(order(colnames(.)))
head(data_nas)
#View(data_nas)

###write the cluster tab 
write.table(data_nas, file="C:/Users/user/Desktop/worked/cluster.TAB", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

############################### Creating the structure id based on cluster__id and interview__id from cluster tab 
d3<-data_nas %>% 
  pivot_longer(cols = starts_with('s02'), values_to = 's02') %>%    
  group_by(interview__id, clusterpart__id) %>% 
  mutate(structure__id = row_number())
head(d3)
d4<-d3 %>% filter(!is.na(s02)) %>% select(-name)
head(d4)


##manipulate to create the household tab
head(hh)
#check if there is any missing h07 household number and drop it
hh1<-hh %>% drop_na(h07)  #drop missing household numbers
hh2<-hh1 %>% group_by(clu00,h07) %>%filter(n() > 1)#no duplicated households in a cluster
hh3<-hh2 %>% group_by(clu00,s02, h06b) %>%filter(n() > 1)#no duplicated households in a structure
hh4<-subset(hh3, h13+h14+h15+h16+h17+h18 != h12) ##sum of members in different households age category sum to the total in the household
hh4  

#Exctract the interview id and cluster part id from the created cluster data frame
hh5<-hh1 %>% left_join(d4, by=c('s02','interview__id'))%>% arrange(interview__id, clusterpart__id,s02)

hh6<-hh5 %>% group_by(interview__id,structure__id) %>% mutate(household__id= row_number(structure__id))
head(hh6)

hhf<-hh6 %>% select(interview__id,	clusterpart__id,	structure__id,	household__id,s02,	h06b,	h07,	h08,	h09,	h10,	h20)
head(hhf) ##household data
View(hhf)

##############-----------------------------final household tab
write.table(hhf, file="C:/Users/user/Desktop/worked/household.TAB", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
##############-----------------------------final household tab


###########################------------------------------------creating structure sheet
hhg<-hhf %>% select(interview__id ,clusterpart__id,structure__id,s02, h06b,h07) 
str2<-hhg %>%
  mutate(h06b = h06b - 1) %>% 
  pivot_wider(names_from = h06b,values_from = h07,names_prefix = "h06b_")

##merge with some selected variables from str
str3<-str %>% select(interview__id,s02,s03,s04,s05,s06a,s06b,s06c)
#head(str3)
final.str<-str2 %>% left_join(str3, by=c("interview__id","s02"))
head(final.str)
write.table(final.str, file="C:/Users/user/Desktop/worked/structure.TAB", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
########################-------------------------------------final structure tab






#################################code for cleaning the data from survey solutions

#R codes for cluster update data cleaning 2020

##Ngugi Mwenda, standards and methods

##loading the packages
library(readstata13)  #for reading the stata files
library(dplyr)  #data manipulation
library(haven)##foreign
library(htmlTable)##html tables
library(magrittr)  #manipulate
library(loose.rock)  #for proper changing of the string cases

nassepvUpdate<-read.dta13("C:/Users/user/Desktop/kcshp Cluster update/dataStata/NassepVupdate.dta")
head(nassepvUpdate)



cleanCluster<-subset(nassepvUpdate, select = c(clu00,clu01, clu02 ,clu03, clu04, clu05,clu06 , clu07 , clu08,                
                                               clu09,clu10, clu11 ,clu12 ,clu13,date_int,approxdistance,travelhrsmins,
                                               townsleep,distfromclust,security,causeofinsecurity__1,causeofinsecurity__2,
                                               causeofinsecurity__3,causeofinsecurity__4,modetransport,terrain,terrainother,
                                               clustersize,clustsize,clustsizeother,settlementpattern,tribesincluster1,per1,
                                               tribesincluster2,per2,tribesincluster3,per3,othercomments,chief,chieftelphone,
                                               asschief,asschieftelphone,villageelder,villageeldertelephone))
head(cleanCluster)  #clean cluster part
cleanCluster %>% htmlTable()


##structure tab

structure<- read.dta13("C:/Users/user/Desktop/kcshp Cluster update/dataStata/structure.dta")
nassepStructure<-merge(nassepvUpdate,structure, by="interview__id")
clust2<-nassepStructure %>% select(clu00,structure__id,s02,s03,gps_new__Latitude, gps_new__Longitude ,s06a,s06b,s06c, s06d_1,s06d)
head(clust2)  ##clean structure
View(clust2)

##household tab

household<- read.dta13("C:/Users/user/Desktop/kcshp Cluster update/dataStata/household.dta")
nassepHouseHold<-merge(nassepvUpdate,household, by="interview__id")
cleanHouseUnsort<-subset(nassepHouseHold, select = c( clu00,clusterpart__id,structure__id, household__id,
                                                      h06b, h06e , h06f ,h07, h08 , h09 ,h10 , h12 , h13 ,
                                                      h14 , h15, h16 , h17 ,h18 , h19 ,h20 , h20b ))

house2<-cleanHouseUnsort %>%
  mutate(str_add=case_when(
    clusterpart__id==1 ~0,
    clusterpart__id==2 ~40,
    clusterpart__id==3 ~80,
    clusterpart__id==4 ~120,
    clusterpart__id==5 ~160,
    clusterpart__id==6 ~200
  )) %>%
  group_by(clu00) %>%
  mutate(s02=structure__id+str_add)

housfin<-subset(house2, select = c( clu00,s02,  h06b,h07, h08 , h09 ,h10 , h12 , h13 ,h14 , h15, h16 , h17 ,h18 , h19 ,h20 , h20b ))
View(housfin)  ##clean household