### Load Data
data = read.csv("Crime_Data_from_2010_to_Present.csv")

###Select Useful Columns Only
library(dplyr)
clean_data = data%>%
  select(Date.Reported,Date.Occurred,Time.Occurred,Crime.Code.Description,Victim.Age,Victim.Sex,Victim.Descent,Location)

# Make sure the time is in the same 4-digit format
library(stringr)
clean_data$Time.Occurred=str_pad(clean_data$Time.Occurred,width=4,side="left",pad="0")
clean_data$Time.Occurred= paste0(substr(clean_data$Time.Occurred,1,2),":",substr(clean_data$Time.Occurred,3,4))


### Format Date and Time
library(lubridate)

clean_data$Date.Reported=mdy(clean_data$Date.Reported)
clean_data$Date.Occurred=mdy(clean_data$Date.Occurred)
clean_data$Time.Occurred=hm(clean_data$Time.Occurred)

clean_data=clean_data%>%
  mutate(Hour = hour(Time.Occurred))%>%
  mutate(Weekday=wday(Date.Reported,label=T,abbr=F))


### Omit the 2018 data, since it doesn't cover the whole year
clean_data=clean_data%>%
  filter(year(Date.Occurred)!=2018)


### Format the location column into Lat & Lon
# remove the ()
library(stringi)
clean_data$Location=stri_sub(clean_data$Location,2,-2)
# seperate Lat and Lon into two seperate columns
library(tidyr)
clean_data=separate(clean_data,Location,into=c("Lat","Lon"), sep= "[^[:alnum:].]+",remove=TRUE)
# change the Lat and Lon from text to numerics
clean_data$Lat=as.numeric(clean_data$Lat)
clean_data$Lon=as.numeric(clean_data$Lon)
clean_data$Lon=-1*clean_data$Lon


#Adding Crime Category: Non Violent v.s. Violent Crimes
library(dplyr)
clean_data=clean_data%>%
  mutate(Crime.Category=ifelse(Crime.Code.Description%in% c("ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER",
                                                            "ASSAULT WITH DEADLY WEAPON,AGGRAVATED ASSAULT",
                                                            "ATTEMPTED ROBBERY",
                                                            "CHILD ABUSE(PHYSICAL)-AGGRAVATED ASSAULT",
                                                            "CHILD ABUSE(PHYSICAL)-SIMPLE ASSAULT",
                                                            "CRIMINAL HOMICIDE",
                                                            "MANSLAUGHTER,NEGLIGENT","OTHER ASSAULT",
                                                            "RAPE,ATTEMPTED","RAPE,FORCIBLE",
                                                            "ROBBERY","SEXUAL PENETRATION W/FOREIGN OBJECT",
                                                            "SEXUAL PENETRATION WITH A FOREIGN OBJECT"),"Violent","Non-Violent"))


### Crime around USC
library(ggmap)
library(dplyr)
library(ggplot2)

## Find crimes happend two blocks around the USC campus
USC_data=clean_data%>%
  filter(Lon>=-118.309088&Lon<=-118.263512,Lat>=34.003799&Lat<=34.040293)

## Total number of crimes happend each year from 2010 - 2015
USC_data%>%
  ggplot(aes(x=year(Date.Occurred),fill=Crime.Category))+
  geom_bar()+
  scale_x_continuous(breaks=seq(2010,2017,1))+
  scale_y_continuous(limits=c(0,10000))+
  ggtitle("Total Numbers of Crimes around USC")+
  xlab("Year")+
  ylab("")+
  scale_fill_manual(values=c("Violent"="red","Non-Violent"="light blue"))+
  theme(legend.title=element_blank())

## The percentage of violent v.s non-violent crimes
USC_data%>%
  ggplot(aes(x=year(Date.Occurred),fill=Crime.Category))+
  geom_bar(position="fill")+
  scale_x_continuous(breaks=seq(2010,2017,1))+
  ggtitle("Non-Violent v.s Violent Crimes around USC")+
  xlab("Year")+
  ylab("")+
  scale_fill_manual(values=c("Violent"="red","Non-Violent"="light blue"))+
  theme(legend.title=element_blank())

## Plotting crimes around USC
library(ggmap)
USC=qmap("University of Southern California",zoom=14,color="bw")
Los_Angeles=qmap("Los Angeles",zoom=12,color="bw")

Los_Angeles+
  geom_bin2d(data=clean_data,aes(x=Lon,y=Lat,fill=Crime.Category),
             color="black",size=0.01,bins=200,alpha=0.3)+
  scale_fill_manual(values=c("Violent"="red","Non-Violent"="black"))+
  theme(legend.title=element_blank())+
  ggtitle("Los Angeles Non-Violent v.s Violent Crime Map 2010-2017" )



### Trim down data to violent crime only
violent_data=clean_data%>%
  filter(Crime.Category=="Violent")%>%
  mutate(Crime.Type=ifelse(Crime.Code.Description%in%c("ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER",
                                                       "ASSAULT WITH DEADLY WEAPON,AGGRAVATED ASSAULT",
                                                       "CHILD ABUSE(PHYSICAL)-AGGRAVATED ASSAULT",
                                                       "CHILD ABUSE(PHYSICAL)-SIMPLE ASSAULT",
                                                       "OTHER ASSULT"),"Assult",
                           ifelse(Crime.Code.Description%in%c("ATTEMPTED ROBBERY","ROBBERY"),"Robbery",
                                  ifelse(Crime.Code.Description%in%c("CRIMINAL HOMICIDE","MANSLAUGHTER,NEGLIGENT"),"Murder and Manslaughter","Rape"))))

## Violent crime data around USC
violent_data_USC=violent_data%>%
  filter(Lon>=-118.309088&Lon<=-118.263512,Lat>=34.003799&Lat<=34.040293)


##Total number of violent crimes each year 2010 - 2017
violent_data%>%
  ggplot(aes(x=year(Date.Occurred),fill=Crime.Type))+
  geom_bar()+
  scale_x_continuous(breaks=seq(2010,2017,1))+
  ggtitle("Total Numbers of Violent Crimes in Los Angeles")+
  xlab("Year")+
  ylab("")+
  theme(legend.title=element_blank())

##Percentage of each category each year 2010 - 2018
violent_data%>%
  ggplot(aes(x=year(Date.Occurred),fill=Crime.Type))+
  geom_bar(position="fill")+
  scale_x_continuous(breaks=seq(2010,2017,1))+
  ggtitle("Percentage breakdown of Violent Crimes in Los Angeles")+
  xlab("Year")+
  ylab("")+
  theme(legend.title=element_blank())

##USC: Total number of violent crimes each year 2010 - 2017
violent_data_USC%>%
  ggplot(aes(x=year(Date.Occurred),fill=Crime.Type))+
  geom_bar()+
  scale_x_continuous(breaks=seq(2010,2017,1))+
  ggtitle("Total Numbers of Violent Crimes around USC")+
  xlab("Year")+
  ylab("")+
  theme(legend.title=element_blank())

##USC: Percentage of each category each year 2010 - 2018
violent_data_USC%>%
  ggplot(aes(x=year(Date.Occurred),fill=Crime.Type))+
  geom_bar(position="fill")+
  scale_x_continuous(breaks=seq(2010,2017,1))+
  ggtitle("Percentage breakdown of Violent Crimes around USC")+
  xlab("Year")+
  ylab("")+
  theme(legend.title=element_blank())

##Violent crime occurance times
violent_data_time=violent_data%>%
  group_by(Weekday,Hour)%>%
  summarize(count=n())

violent_data_time%>%
  ggplot(aes(x=Weekday,y=Hour,fill=count))+
  geom_tile()+
  scale_y_continuous(breaks=seq(0,23,1))+
  xlab("")+
  ylab("")+
  scale_fill_gradient(low="white", high="dark red")
#Violent crimes occur most often at night time around 20p.m. to 21p.m From Monday to Friday.
#On weekend, the peak violent crime occurs more often around 1a.m.
#In general, morning hour around 6a.m to 7a.m are the safest time.

# Victim Age (stacked bar chart, fill = gender)

violent_data%>%
  ggplot(aes(x = Victim.Age, fill = Victim.Sex))+
  geom_bar()


### Chris' Part
#Most dangerous USC neighborhoods by day of week
USC +
  stat_density2d(data=violent_data_USC,
                 aes(x=Lon,y=Lat,
                     fill=..level..,
                     alpha=..level..),
                 bins=5, geom="polygon")+
  scale_fill_gradient(low="black",high="red")+
  ggtitle("Most Dangerous USC Neighborhoods by Day of Week")+
  theme(legend.position="None")+
  facet_wrap(~Weekday, nrow = 2)
##The red disctricts to the south side of USC are the most dangerous neighborhoods from Sunday to Saturday.
##Wednesday seems particularly dangerous with an dense area of violent crimes. Dangerous areas seem to spread out on Sunday,
##, Monday and Friday, and got concentrated on Tuesday, Wednesday, Thursday and Saturday.


### Parin's Part
library(ggmap)

library(lubridate)

USC=qmap("University of Southern California",zoom=15,color="bw")

Los_Angeles=qmap("Los Angeles",zoom=12,color="bw")



USC_robbery = violent_data_USC %>%
  
  mutate(year = year(Date.Occurred)) %>%
  
  filter (Crime.Type == "Robbery")



USC_robbery_2017 = USC_robbery %>%
  
  mutate(year = year(Date.Occurred)) %>%
  
  filter (year == "2017")



USC_robbery_2013 = USC_robbery %>%
  
  mutate(year = year(Date.Occurred)) %>%
  
  filter (year == "2013")



### Robberies in USC Area in 2017 (After fence was built around campus)

USC+
  
  geom_bin2d(data=USC_robbery_2017,aes(x=Lon,y=Lat,fill=Crime.Category),
             
             color="red",size=0.01,bins=200,alpha=0.3)+
  
  scale_fill_manual(values=c("Robbery"="red"))+
  
  theme(legend.title=element_blank(),
        
        legend.position = "None")+
  
  ggtitle("Robberies around USC in 2017" )





###Robberies in USC Area in 2013 (Prior to fence being built around campus)

USC+
  
  geom_bin2d(data=USC_robbery_2013,aes(x=Lon,y=Lat,fill=Crime.Category),
             
             color="red",size=0.01,bins=200,alpha=0.3)+
  
  scale_fill_manual(values=c("Robbery"="red"))+
  
  theme(legend.title=element_blank(),
        
        legend.position = "None")+
  
  ggtitle("Robberies around USC in 2013")


###Justine's Parts
# We'd like to create a stacked bar chart showing a breakdown of Victim Age by Gender.
# To do this, we first need to clean the violent_data set to remove all Victim Age information that is 
# denoted by NA and Victim Sex information that is unknown or blank:

clean_data_VA = violent_data%>%
  filter(Victim.Age != "NA", Victim.Sex == c("M", "F"))

# Now we can plot the data:

clean_data_VA%>%
  ggplot(aes(x = Victim.Age, 
             fill = Victim.Sex))+
  geom_bar(position = "dodge")+
  scale_x_continuous(breaks = seq(0, 100, 5))+
  scale_fill_manual(values = c("M"="lightblue", "F" = "hotpink"))+
  ggtitle("Breakdown of Violent Crimes by Victim Age and Gender")+
  xlab("Victim Age")+
  ylab("")

## We can see from this chart that males between 20-25 are the biggest victims of violent crimes.
## However, Female and Male victims exhibit similar trends in terms of being victims of violent crimes
## (i.e. the crime rates appear to be pretty consistent across both genders), indicating that citizens
## age 18-25 were victims of violent crime in Los Angeles. 

###Hannah's Parts



