rm(list = ls())
setwd("D:/Learning for Qian/Graduate Learning/Fall 2019/ISM6137-Stats Data Mining/Final Project")
#devtools::install_github("hrbrmstr/localgeo")
library(ggmap)
library(readxl)
library(tidyverse)
library(MASS)

#Load the data
boat <- read.csv("newboatdata.csv", header=TRUE,na.strings=c(""," ","NA"))
dim(boat)

#########Data Cleaning and Exploration-----------------------------------
#1. inclusion criteria --------------------------------------------------
# Remove duplicated URL
table(duplicated(boat$URL))
boat[duplicated(boat$URL),]  #identify the duplicates, confirm before delete
boat1 <- distinct(boat,URL, .keep_all= TRUE)
table(duplicated(boat1$URL))  #15466 unique records

# Remove missing price records
is.null(boat1$Price)  #check whether there are null values
boat2 <- boat1[!boat1$Price=="Request a Price",] #remove rows that does not have a price
dim(boat2)

#2. Univariable exploration-----------------------------------------------
# Price
boat2$Price <- gsub("\\$|,| ","",as.character(boat2$Price))
boat2$Price <- as.numeric(boat2$Price ) #convert to numeric values

hist(boat2$Price)  #raw value of price is highly right-skewed
hist(log(boat2$Price)) #log transformation exhibites nearly normal distribution
boat2$log_Price <- log(boat2$Price)  #create a new log transformed price variable


### Year  -- Age
# Create a new variable 'age' which captures the year difference
#   between current year and the model year of the boat. 
#   Notice: a 2020 model year boat can legally go on sale on January 1, 2019, this explains
#   why there are year values of '2020' in the data set. 

# set the current year as 2020 to include records with year value of 2020
current_year <- 2020
boat2$Age <- 2020 - boat2$Year

table(is.na(boat2$Age))
summary(boat2$Age)
hist(boat2$Age)

# Histogram of log transformation
boat2$log_Age <- log(boat2$Age)
hist(log(boat2$Age))
table(cut(boat2$Age,c(0,1,2,3,5,10,15,20,30,150),include.lowest = T))
table(cut(boat2$Age,c(0,1,2,3,5,10,15,20,30,150)))#the marjority age is below 10, will discuss this in later section

#Age cut 2

boxplot(boat2$log_Price~boat2$Age_cut1)
#New age cut
boat2$Age_cut2 <- ifelse(boat2$Age <= 1, "Low",
                         ifelse(boat2$Age <= 10, "Medium",'High'))
boat2$Age_cut2 <- factor(boat2$Age_cut2, levels=c("Low","Medium","High"))
table(boat2$Age_cut2, useNA = "always")
boxplot(boat2$log_Price~boat2$Age_cut2) 


### Length 
boat2$Length <- as.numeric(gsub("\\'","",as.character(boat2$Length)))
hist(boat2$Length)
hist(log(boat2$Length))
boat2$log_Length <- log(boat2$Length)

### Class
table(boat2$Class, useNA = "always")
barplot(table(boat2$Class)) #Most of the boats are power boats

### Make
nlevels(boat2$Make) #781 different make, it will be further discussed.

a### Material
table(boat2$Material, useNA = "always") #check levels and frequencies
barplot(table(boat2$Material))

### Fuel Type
table(boat2$Fuel.Type, useNA = "always") #check levels and frequencies
barplot(table(boat2$Fuel.Type))

### Location -- State
# Check if there is null values in Location
is.null(boat2$Location)
# check each value of Location column has a two-letter initials
boat2[str_detect(boat2$Location, ",",negate = T),]
dim(boat2[str_detect(boat2$Location, ",",negate = T),])

#Obtain state abbreviations
boat2$State_abb <- gsub(".*, ","",boat2$Location)
table(boat2$State_abb,useNA = 'always')

#Convert state abbreviations to full names
boat2$State <- state.name[match(boat2$State_abb, state.abb)]
table(boat2$State,useNA = 'always')
boat2$State <- ifelse(boat2$State %in% NA,"washington DC",paste(boat2$State))
table(boat2$State,useNA = 'always')

# Create Region Variable
# Five Region Categories: Pacific, GreateLake, Florida, MidAtlantic, inner land
# Pacific: AK, CA, HI, OP, WA
# GreatLake: IL, IN, MI, NY, OH, PA, WI
# FL
# MidAtlantic: MD, NJ, VA, DC, NC, SC

#Assign region value to each record
boat2$Region <- 
  ifelse(str_detect(boat2$State_abb, "FL"),"Florida",
         ifelse(str_detect(boat2$State_abb,"IL|IN|MI|NY|OH|PA|WI"), "GreateLake",
                ifelse(str_detect(boat2$State_abb,"AK|CA|HI|OR|WA"), "Pacific",
                       ifelse(str_detect(boat2$State_abb, "MD|NJ|VA|DC|NC|SC"), 
                              "Mid Atlantic","InnerLand"))))
table(boat2$Region, useNA = 'always')
barplot(table(boat2$Region))



### Engine Type
table(boat2$Engine.Type, useNA = "always") #check levels and frequencies
barplot(table(boat2$Engine.Type))


#3. Multivariable Exploration

### Price & Age
plot(boat2$log_Price~boat2$Age)  
plot(boat2$log_Price~boat2$log_Age)
#there is no obvious patterns betwwen price and age, 
#potientially because most of the boats are in the age range of (0,1)



### Price & Length
plot(boat2$log_Price, boat2$log_Length)
cor.test(boat2$log_Price, boat2$log_Length,method = "pearson") 
#plot exhibites there is a pattern between transformed price and length, pearon test also support this.

ggplot(boat2, aes(x=log_Length, y=log_Price)) +
  geom_point()+
  facet_wrap(.~Class)



### Price & Make
Temp_Make <- boat2 %>%
  group_by(Make) %>%
  summarise(medianPrice = median(Price), n=n()) %>%
  arrange(desc(medianPrice))
  
Temp_Make$Cut1 <- cut(Temp_Make$medianPrice, breaks=c(quantile(Temp_Make$medianPrice,
                                           probs = seq(0, 1, by = 0.20))),
                      labels = c("G1","G2","G3","G4","G5"),
                      include.lowest=TRUE)

boat2$Make_20 <- ifelse(boat2$Make %in% Temp_Make$Make[Temp_Make$Cut1 == "G1"],"G1",
                        ifelse(boat2$Make %in% Temp_Make$Make[Temp_Make$Cut1 == "G2"], "G2",
                               ifelse(boat2$Make %in% Temp_Make$Make[Temp_Make$Cut1 == "G3"], "G3",
                                      ifelse(boat2$Make %in% Temp_Make$Make[Temp_Make$Cut1 == "G4"], "G4",
                                             ifelse(boat2$Make %in% Temp_Make$Make[Temp_Make$Cut1 == "G5"], "G5", NA)))))

boxplot(boat2$log_Price~boat2$Make_20)

#Explore interaction: relationship between price and lenght in different make group
ggplot(boat2, aes(x=log_Length, y=log_Price)) +
  geom_point()+
  facet_wrap(.~Make_20)

### Price & Material
boxplot(boat2$log_Price ~ boat2$Material)
table(boat2$Material, useNA = "always")
boat2$Material_Cat <- ifelse(boat2$Material %in% "Aluminum", "Aluminum",
                             ifelse(boat2$Material %in% "Fiberglass", "Fiberglass",
                                    ifelse(boat2$Material %in% "Pvc", "Pvc", "Others")))
boat2$Material_Cat <- factor(boat2$Material_Cat, levels = c("Fiberglass","Aluminum","Pvc","Others"))
table(boat2$Material_Cat, useNA = "always")
boxplot(boat2$log_Price~boat2$Material_Cat)

#Interaction explore: relationship between price and length in different material group
ggplot(boat2, aes(x=log_Length, y=log_Price)) +
  geom_point()+
  facet_wrap(.~Material_Cat)


### Price & Fuel Type
boxplot(boat2$log_Price ~ boat2$Fuel.Type)
table(boat2$Fuel.Type, useNA = "always")

type(boat2$Fuel.Type)


boat2$NewFuelType <- ifelse(boat2$Fuel.Type %in% "Diesel", "Diesel",
                             ifelse(boat2$Fuel.Type %in% "Electric", "Electric",
                                    ifelse(boat2$Fuel.Type %in% "Gas", "Gas",
                                           ifelse(boat2$Fuel.Type %in% "Other","Other","Unknown"))))

table(boat2$NewFuelType, useNA = "always")
boxplot(boat2$log_Price ~ boat2$NewFuelType)


#Interaction explore: plot relationship between price and length in different fuel type
ggplot(boat2, aes(x=log_Length, y=log_Price)) +
  geom_point()+
  facet_wrap(.~NewFuelType)





# Price & State 
register_google(key = "xxxx", write = TRUE)
has_google_key()


boat2_State <- boat2 %>%
  group_by(State) %>%
  summarise(meanPrice = mean(Price), 
            medianPrice = median(Price),
            IQRPrice = IQR(Price),
            sdPrice = sd(Price),
            totalPrice = sum(Price),
            n=n())%>%
  filter(!State %in% c("Hawaii","Alaska"))


for (i in 1:nrow(boat2_State)) {
  latlon = geocode(boat2_State$State[i])
  boat2_State$lon[i] = as.numeric(latlon[1])
  boat2_State$lat[i] = as.numeric(latlon[2])
}

usa_center = as.numeric(geocode("United States"))

USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4))

#number of listed boats in each state
USAMap + 
  geom_point(aes(x=lon, y=lat), data=boat2_State, col="orange", alpha=0.6, 
             size=boat2_State$n*0.015) +  
  scale_size_continuous(range= range(boat2_State$n))

#Mean Price of each state
USAMap + 
  geom_point(aes(x=lon, y=lat), data=boat2_State, col="orange", alpha=0.6, 
             size=boat2_State$meanPrice*0.00008) +  
  scale_size_continuous(range= range(boat2_State$meanPrice))

#Median Price of each state
USAMap + 
  geom_point(aes(x=lon, y=lat), data=boat2_State, col="orange", alpha=0.6, 
             size=boat2_State$medianPrice*0.00015) 

#Total Price of each state
USAMap + 
  geom_point(aes(x=lon, y=lat), data=boat2_State, col="orange", alpha=0.6, 
             size=boat2_State$totalPrice*0.0000001) 



### Price & Region
boxplot(boat2$log_Price ~ boat2$Region)

boat2_Region <- boat2 %>%
  group_by(Region) %>%
  summarise(meanPrice = mean(Price), 
            medianPrice = median(Price),
            IQRPrice = IQR(Price),
            sdPrice = sd(Price),
            totalPrice = sum(Price),
            n=n()) 
boat2_Region$lon <- c(-122.4194, -87.6298, -100.8727, -78.8986, -81.3792)
boat2_Region$lat <- c(37.7749, 41.8781, 40, 35.9940, 28.5383)

#number of boats listed in each region
USAMap + 
  geom_point(aes(x=lon, y=lat), data=boat2_Region, col="orange", alpha=0.6, 
             size=boat2_Region$n*0.01) +  
  scale_size_continuous(range= range(boat2_Region$n)) 


#Median Price in each region
USAMap + 
  geom_point(aes(x=lon, y=lat), data=boat2_Region, col="orange", alpha=0.6, 
             size=boat2_Region$medianPrice*0.0003)


#Explore interaction: relationship between price and length in different region
ggplot(boat2, aes(x=log_Length, y=log_Price)) +
  geom_point()+
  facet_wrap(.~Region)

### Price & Engine Type
table(boat2$Engine.Type,useNA = "always")
boxplot(boat2$log_Price ~ boat2$Engine.Type)
#Further group engine type
boat2$NewEngineType <- ifelse(boat2$Engine.Type %in% 
                                c("Other", "Single Inboard", "Single Outboard"), 
                              "Single", "Multiple")

boat2$NewEngineType <- factor(boat2$NewEngineType, levels = c("Single","Multiple"))

table(boat2$NewEngineType, useNA = "always")
boxplot(boat2$log_Price ~ boat2$NewEngineType)

### Price & Class
table(boat2$Class,useNA = "always")
boxplot(boat2$log_Price ~ boat2$Class)





############Model Building-------------------------------------------------
# .	Length (continuous)
# .	Age (grouped, ordinal)
# .	Make (ordinal)
# .	Material (nominal)
# .	Fuel Type (nominal)
# .	Region (nominal)
# .	Engine Type (nominal)
# .	Class (nominal)


stepAIC(lm(boat2$log_Price~
             boat2$log_Length+
             boat2$Age_cut2+
             boat2$Make_20+
             boat2$Material_Cat+
             boat2$NewFuelType+
             boat2$Region+
             boat2$NewEngineType+
             boat2$Class+
             boat2$Age_cut2*boat2$NewEngineType),
        direction="both",trace = T)


Full_lm <- lm(boat2$log_Price~
                boat2$log_Length+
                boat2$Age_cut2+
                boat2$Make_20+
                boat2$Material_Cat+
                boat2$NewFuelType+
                boat2$Region+
                boat2$NewEngineType+
                boat2$Class+
                boat2$Age_cut2*boat2$NewEngineType)
summary(Full_lm) 

# comparing to single engine, when a boat has multiple engine 
# the price will drop faster as the boat ages.

##############Export the dataset
#select the desired the variables and create the final data
#write.csv(boat2, "BoatFinal.csv")

