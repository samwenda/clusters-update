#################################code for cleaning the data from survey solutions

#R codes for cluster update data cleaning 2020

##Ngugi Mwenda, standards and methods

##loading the packages
#rm(list = ls)
library(readstata13)  #for reading the stata files
library(dplyr)  #data manipulation
library(haven)##foreign
library(htmlTable)##html tables
library(magrittr)  #manipulate
library(loose.rock)  #for proper changing of the string cases

setwd('C:/Users/user/Desktop/cluster')

nassepvUpdate<-read.dta13("raw/NassepVupdateMalaria.dta")
head(nassepvUpdate)



cleanCluster<-subset(nassepvUpdate, select = c(clu00,clu01, clu02 ,clu03, clu04, clu05,clu06 , clu07 , clu08,                
                                               clu09,clu10, clu11 ,clu12 ,clu13,date_int,approxdistance,travelhrsmins,
                                               townsleep,distfromclust,security,causeofinsecurity__1,causeofinsecurity__2,
                                               causeofinsecurity__3,causeofinsecurity__4,modetransport,terrain,terrainother,
                                               clustersize,clustsize,clustsizeother,settlementpattern,tribesincluster1,per1,
                                               tribesincluster2,per2,tribesincluster3,per3,othercomments,chief,chieftelphone,
                                               asschief,asschieftelphone,villageelder,villageeldertelephone))
#head(cleanCluster)  #clean cluster part
#cleanCluster %>% htmlTable()


##structure tab

structure<- read.dta13("raw/structure.dta")
View(structure)
nassepStructure<-merge(nassepvUpdate,structure, by="interview__id")
str2<-nassepStructure %>% select(clu00,structure__id,s02,s03,gps_new__Latitude, gps_new__Longitude,gps_new__Altitude ,s06a,s06b,s06c, s06d_1,s06d)
head(str2)  ##clean structure
View(str2)
str3<-str2 %>% select(clu00,s02,s03,gps_new__Latitude, gps_new__Longitude, gps_new__Altitude ,s06a,s06b,s06c, s06d_1,s06d)
str_final<-str3 %>% rename(Cluster_Number=clu00,Structure_Number=s02, Structure_Type=s03,
                           Latitude=gps_new__Latitude,Longitude=gps_new__Longitude, Altitude=gps_new__Altitude,
                           Structure_Residential=s06a,Name_of_structure=s06b,comments=s06c,Structure_Existing=s06d)
#head(str_final)

##household tab

household<- read.dta13("raw/household.dta")
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

housfin<-subset(house2, select = c( clu00,s02,  h06b,h07, h08 , h09 ,h10 , h12 , h13 ,h14 , h15, h16 , h17 ,h18 , h19 , h20b ))
#View(housfin)  ##clean household
#head(housfin)
house_final<-housfin %>% rename(Cluster_Number=clu00, Structure_Number=s02,Household_Serial_Number=h06b,
                                Household_Number=h07, Name_Household_Head=h08,Gender=h09, Occupation=h10,
                                Total_HH_Members=h12,male_0_4Y=h13,female_0_4Y=h14,male_5_17Y=h15,female_5_17Y=h16,
                                male_18_plus_Y=h17,female_18_plus_Y=h18,Operate_Holding=h19,comments=h20b)
#head(house_final)
#View(house_final)
write.csv(cleanCluster, file = "clean/cluster.tab", row.names=FALSE)
write.csv(str_final, file = "clean/structure.tab", row.names=FALSE)
write.csv(house_final, file = "clean/household.tab", row.names=FALSE)

