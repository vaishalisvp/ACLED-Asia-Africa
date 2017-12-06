## Prerequisites
library(readxl)
library(dplyr)
library(ggplot2)
library(ggmap)



## Importing datafiles
#For Vaishali
Africa <- read_excel("C:/Users/Windows-PC/Dropbox/Hertie School of Governance/Semester 3/Final R/ACLED-Africa.xlsx")
Asia2015 <- read_excel("C:/Users/Windows-PC/Dropbox/Hertie School of Governance/Semester 3/Final R/ACLED-Asia-2015.xlsx")
Asia2016 <- read_excel("C:/Users/Windows-PC/Dropbox/Hertie School of Governance/Semester 3/Final R/ACLED-Asia-2016.xlsx")

#For Shruti
Africa <- read_excel("C:/Users/Shruti Gogia/Dropbox/Final R/ACLED-Africa.xlsx")
Asia2015 <- read_excel("C:/Users/Shruti Gogia/Dropbox/Final R/ACLED-Asia-2015.xlsx")
Asia2016 <- read_excel("C:/Users/Shruti Gogia/Dropbox/Final R/ACLED-Asia-2016.xlsx")

## Subsetting Africa file
Africa2015 <- Africa %>% filter(YEAR=="2015")
Africa2016 <- Africa %>% filter(YEAR=="2016")

## Create variable "continent" in each dataset
Africa2015$continent <- "Africa"
Africa2016$continent <- "Africa"
Asia2015$continent <- "Asia"
Asia2016$continent <- "Asia"

## Creating the final database
ACLED_Final <- bind_rows(Africa2015,Africa2016,Asia2015,Asia2016)
ACLED_Final <- ACLED_Final %>% select ("GWNO", "continent", everything())

## Checking for Missing values in variables of interest
any(is.na(ACLED_Final$continent))
any(is.na(ACLED_Final$EVENT_DATE))
any(is.na(ACLED_Final$YEAR))
any(is.na(ACLED_Final$EVENT_TYPE))
any(is.na(ACLED_Final$INTER1))
any(is.na(ACLED_Final$INTER2))
any(is.na(ACLED_Final$INTERACTION))
any(is.na(ACLED_Final$COUNTRY))
any(is.na(ACLED_Final$LATITUDE)) ## has missing values
any(is.na(ACLED_Final$LONGITUDE))  ## has missing values
any(is.na(ACLED_Final$GEO_PRECISION))
any(is.na(ACLED_Final$FATALITIES))

## Dropping rows with missing values in latitude and longitude variable
ACLED_Final <- ACLED_Final[!is.na(ACLED_Final$LATITUDE),]
any(is.na(ACLED_Final$LATITUDE))
ACLED_Final <- ACLED_Final[!is.na(ACLED_Final$LONGITUDE),]
any(is.na(ACLED_Final$LONGITUDE))

## Recoding values within variables
table(ACLED_Final$EVENT_TYPE)
ACLED_Final$EVENT_TYPE[ACLED_Final$EVENT_TYPE=="RIots/Protests"] <- "Riots/Protests"
ACLED_Final$EVENT_TYPE[ACLED_Final$EVENT_TYPE=="Battle-Non-state actors overtake territory"] <- "Battle-Non-state actor overtakes territory"
ACLED_Final$EVENT_TYPE[ACLED_Final$EVENT_TYPE=="Violence Against Civilians"] <- "Violence against civilians"
ACLED_Final <- transform(ACLED_Final, YEAR = as.character(YEAR))

## Renaming variables
colnames(ACLED_Final)[colnames(ACLED_Final)=="INTER1"] <- "ACTOR_TYPE1"
colnames(ACLED_Final)[colnames(ACLED_Final)=="INTER2"] <- "ACTOR_TYPE2"

ACLED_Final$ACTOR_NAME1 <- NA
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==1] <- "Government or mutinous force"
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==2] <- "Rebel force"
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==3] <- "Political militia"
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==4] <- "Ethnic militia"
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==5] <- "Rioters"
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==6] <- "Protesters"
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==7] <- "Civilians"
ACLED_Final$ACTOR_NAME1[ACLED_Final$ACTOR_TYPE1==8] <- "Outside/external force"


## Plotting preliminary statistics

# Number of fatalities
# PLOT 1: Stacked bar chart
plot1 <- ggplot(ACLED_Final, mapping = aes(x=factor(continent), y=FATALITIES, fill=factor(YEAR))) + 
  geom_bar(stat="identity") + 
  xlab("Continent") + 
  ylab("Number of fatalities") + 
  labs(title = "Number of fatalities by continent, across two years", caption = "Source: ACLED, 2015")
plot1

# PLOT 2,3: Facetwrap -Barchart (Africa and Asia)
plot2 <- ACLED_Final %>% 
  filter(continent=="Africa") %>% 
  ggplot(mapping = aes(x=FATALITIES, fill=YEAR))
plot2 + geom_histogram(bins = 5) + facet_wrap(~COUNTRY) + 
  xlab("Number of Fatalities") + 
  ylab("Frequency") + 
  labs(title = "Number of fatalities by country in Africa", caption = "Source: ACLED, 2015")+
  theme(plot.title = element_text(hjust = 0.5))
#plot2 + geom_line(stat='summary', fun.y='mean')+
#geom_point(stat='summary', fun.y='mean')+
#theme(axis.text.x = element_text(angle = 30, hjust = 1))+
#facet_wrap(~COUNTRY) 
plot3 <- ACLED_Final %>% 
  filter(continent=="Asia") %>% 
  ggplot(mapping = aes(x=FATALITIES, fill=YEAR))
plot3 + geom_histogram(bins = 5) + facet_wrap(~COUNTRY)+ 
  xlab("Number of Fatalities") + 
  ylab("Frequency") + 
  labs(title = "Number of fatalities by country in Asia", caption = "Source: ACLED, 2015")+
  theme(plot.title = element_text(hjust = 0.5))
#plot3 + geom_line(stat='summary', fun.y='mean')+
#geom_point(stat='summary', fun.y='mean')+
#theme(axis.text.x = element_text(angle = 30, hjust = 1))+
#facet_wrap(~COUNTRY)

#Analysis by actor type
# PLOT 4: Line Plot1 (Overall)
Plot4 <- ggplot(data = ACLED_Final, mapping = aes(x=ACTOR_NAME1, 
                                              y=FATALITIES, 
                                              group=YEAR, color=YEAR))
Plot4 + geom_line(stat='summary', fun.y='mean')+
   geom_point(stat='summary', fun.y='mean')+theme(axis.text.x = element_text(angle = 30, hjust = 1))+ 
   xlab("Type of Actor") + 
   ylab("Average Number of fatalities") + 
   labs(title = "Fatalities by type of actor(combined Asia & Africa)", caption = "Source: ACLED, 2015")+
   theme(plot.title = element_text(hjust = 0.5))

# PLOT 5: Facetwrap Lineplot (Africa and Asia)
Plot4 + geom_line(stat='summary', fun.y='mean')+
  geom_point(stat='summary', fun.y='mean')+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  facet_wrap(~continent) +
  xlab("Type of Actor") + 
  ylab("Average Number of fatalities") + 
  labs(title = "Fatalities by type of actor", caption = "Source: ACLED, 2015")+
  theme(plot.title = element_text(hjust = 0.5))

# Line Plot (Africa)
#q + geom_line(stat='summary', fun.y='mean')+
  #geom_point(stat='summary', fun.y='mean')+
  #theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Line Plot (Asia)
#r + geom_line(stat='summary', fun.y='mean')+
   #geom_point(stat='summary', fun.y='mean')+
   #theme(axis.text.x = element_text(angle = 30, hjust = 1))


#Analysis by event type

# Plot 6: Vertical bar chart (overall)
ggplot(ACLED_Final,aes(x=factor(EVENT_TYPE))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#F8766D") + 
  coord_flip() + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", position=position_stack(vjust=0.5)) + 
  xlab("Type of event") + 
  ylab("Percentage of total") + 
  ggtitle("Type of event - proportions")

# Plot 7: Vertical bar chart (Africa)
ggplot(AfricaMaps,aes(x=factor(EVENT_TYPE))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#00BFC4") + 
  coord_flip() + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", position=position_stack(vjust=0.5)) + 
  xlab("Type of event") + 
  ylab("Percentage of total") + 
  ggtitle("Type of event - proportions")

# Plot 8: Vertical bar chart (Asia)
ggplot(AsiaMaps,aes(x=factor(EVENT_TYPE))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#00BFC4") + 
  coord_flip() + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", position=position_stack(vjust=0.5)) + 
  xlab("Type of event") + 
  ylab("Percentage of total") + 
  ggtitle("Type of event - proportions")

#Plot 9: Line Plot (Overall)
Plot9 <- ggplot(data = ACLED_Final, mapping = aes(x=EVENT_TYPE, 
                                              y=FATALITIES, 
                                              group=YEAR, color=YEAR))

Plot9 + geom_line(stat='summary', fun.y='mean')+
  geom_point(stat='summary', fun.y='mean')+theme(axis.text.x = element_text(angle = 40, hjust = 1))+ 
  xlab("Type of Event") + 
  ylab("Average Number of fatalities") + 
  labs(title = "Fatalities by type of event(combined Asia & Africa)", caption = "Source: ACLED, 2015")+
  theme(plot.title = element_text(hjust = 0.5))

# Plot 10: Facetwrap (Lineplot, Africa and Asia)

Plot9 + geom_line(stat='summary', fun.y='mean')+
  geom_point(stat='summary', fun.y='mean')+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  facet_wrap(~continent) +
  xlab("Type of Event") + 
  ylab("Average Number of fatalities") + 
  labs(title = "Fatalities by type of event", caption = "Source: ACLED, 2015")+
  theme(plot.title = element_text(hjust = 0.5))

## Maps
#Asia 
AsiaMaps <- ACLED_Final %>% filter(continent=="Asia")
AsiaMaps1 <- AsiaMaps %>% filter(EVENT_TYPE %in% c("Riots/Protests", "Battle-No change of territory", "Violence against civilians")) %>% filter(FATALITIES > 0)
Asiabbox <- make_bbox(LONGITUDE, LATITUDE, data=AsiaMaps1, f=0.05)
AsiaMaps2 <- get_map(Asiabbox)
# Asia (event type)
ggmap(AsiaMaps2) + 
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=EVENT_TYPE, shape = EVENT_TYPE),data=AsiaMaps1)+
  scale_size_manual(values = c(0.1,1,10))
  scale_color_manual(values = c("midnightblue", "chartreuse4", "red4"))
# Asia (actor type)
ggmap(AsiaMaps2) + 
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=ACTOR_NAME1),data=AsiaMaps) 

  
#Africa
AfricaMaps <- ACLED_Final %>% filter(continent=="Africa")
Africabbox <- make_bbox(LONGITUDE, LATITUDE, data=AfricaMaps, f=0.16)
AfricaMaps2 <- get_map(Africabbox)

#Africa (event type)
ggmap(AfricaMaps2) + 
  geom_point(aes(x=LONGITUDE, y=LATITUDE, size = FATALITIES, color=FATALITIES),data=AfricaMaps) + 
  scale_color_gradient(limits = c(0,90), low = "indianred1", high = "indianred4")
#Africa (actor type)
ggmap(AfricaMaps2) + 
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=ACTOR_NAME1),data=AfricaMaps) 



## Regression
#reg <- rpart(FATALITIES ~ continent + ACTOR_NAME1 +EVENT_TYPE, data=ACLED_Final, method = "anova")
#rpart.plot(reg, type=4,cex=0.6, digits = 3)

#reg1 <- lm(FATALITIES~ continent+COUNTRY+ACTOR_NAME1, ACLED_Final, na.action = na.exclude)
#ACLED_Final$resid <- resid(reg1)
#ACLED_Final$fitted <- fitted(reg1)
#summary(reg1)

#Overall regression
reg1 <- lm(FATALITIES~ continent, ACLED_Final, na.action = na.exclude)
summary(reg1)

reg2 <- lm(FATALITIES~ continent+ACTOR_NAME1, ACLED_Final, na.action = na.exclude)
summary(reg2)

reg3 <- lm(FATALITIES~ continent+ACTOR_NAME1+EVENT_TYPE, ACLED_Final, na.action = na.exclude)
summary(reg3)

#Africa regression
reg4 <- lm(FATALITIES~ ACTOR_NAME1, AfricaMaps, na.action = na.exclude)
summary(reg4)

reg5 <- lm(FATALITIES~ ACTOR_NAME1+EVENT_TYPE, AfricaMaps, na.action = na.exclude)
summary(reg5)

reg6 <- lm(FATALITIES~ ACTOR_NAME1+EVENT_TYPE+ COUNTRY, AfricaMaps, na.action = na.exclude)
summary(reg6)

#Asia regression
reg7 <- lm(FATALITIES~ ACTOR_NAME1, AsiaMaps, na.action = na.exclude)
summary(reg7)

reg8 <- lm(FATALITIES~ ACTOR_NAME1+EVENT_TYPE, AsiaMaps, na.action = na.exclude)
summary(reg8)

reg9 <- lm(FATALITIES~ ACTOR_NAME1+EVENT_TYPE+COUNTRY, AsiaMaps, na.action = na.exclude)
summary(reg9)
