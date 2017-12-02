library(htmltab)
url <- "https://www.massshootingtracker.org/data/all"
newShootings <- htmltab(doc=url, which=1)
head(shootings)

rownames(shootings) <- c(1:1881)
head(shootings)

print("hello")
rm(shootings)
sapply(newShootings, function(x) sum(is.na(x)))

setwd("C:\\Users\\Robbie\\Documents\\College\\DAD\\R-Project")
terror <- read.csv("globalterror.csv", stringsAsFactors = T) #will autoencode the text attributes to factors

summary(shootings)

str(terror$iyear)

#Subsetting the data over the last few years
rm(newTerror)
attach(terror)
newTerror <- terror[ which(terror$iyear > 2012),]
detach(terror)
rm(terror)
#Formatting the date
newShootings$Date <- format(as.Date(newShootings$Date, format="%d/%m/%Y"),"%Y")
print(newShootings$Date)

sum(is.na(newShootings$Date))

newShootings$Date = as.integer(newShootings$Date)

newShootings$Date = as.integer(newShootings$Date)

View(terror)

#Changing names
colnames(newShootings)[which(names(newShootings) == "#")] <- "id"
colnames(newShootings)[which(names(newShootings) == "Date")] <- "year"
colnames(newTerror)[which(names(newTerror) == "iyear")] <- "year"
colnames(newTerror)[which(names(newTerror) == "eventid")] <- "id"
colnames(newTerror)[which(names(newTerror) == "country_txt")] <- "Country"
colnames(newTerror)[which(names(newTerror) == "city")] <- "Location"
colnames(newTerror)[which(names(newTerror) == "success")] <- "Dead"


newShootings$id = as.numeric(newShootings$id)
newTerror$Country = as.character(newTerror$Country)
newTerror$Location = as.character(newTerror$Location)
newShootings$Dead = as.integer(newShootings$Dead)


#Deleting some rows for binding
newTerror[3]<- NULL
newTerror[3:6]<- NULL
newTerror[4]<- NULL
newTerror[4:5]<- NULL
newTerror[5:11]<- NULL
newTerror[5:11]<- NULL
newTerror[5:10]<- NULL
newTerror[5:11]<- NULL
newTerror[6:40]<- NULL
newTerror[6:30]<- NULL
newTerror[6:23]<- NULL
newTerror[6:10]<- NULL
newShootings[5]<- NULL


View(newTerror)
View(shootings)

rm(shootings)

#Adding country to shootings for binding
newShootings$Country <- c("United States")
View(newShootings)

#Merging shooting and terrorism together
terrorism <- rbind(newShootings, newTerror)
terrorism
View(terrorism)

boxplot(terrorism$Dead ~ terrorism$year)

aggregate(year ~ Dead, mean, data = terrorism)

attach(terrorism)
plot(year,Dead)

#Subsetting data for Analysis
USAttacks2016 <- terrorism[ which(terrorism$Country=='United States' & terrorism$year>=2016 & terrorism$year<2017 & terrorism$Dead>0 & terrorism$Dead<2),]
WorldAttacks2016 <- terrorism[ which(terrorism$Country!=c('United States','Iraq','Afghanistan','Syria') & terrorism$year>=2016 & terrorism$year<2017 & terrorism$Dead>0 & terrorism$Dead<2),]
WarAttacks2016 <- terrorism[ which(terrorism$year>=2016 & terrorism$year<2017 & terrorism$Dead>0 & terrorism$Dead<2),]
WarAttacks2016 <- WarAttacks2016[ WarAttacks2016$Country==c('Iraq','Afghanistan','Syria'),]
USWarAttacks2016 <- terrorism[ which(terrorism$Country==c('United States','Iraq','Afghanistan','Syria') & terrorism$year>=2016 & terrorism$year<2017 & terrorism$Dead>0 & terrorism$Dead<2),]

#Binding War and US 2016 numbers
IraqLength <- length(WarAttacks2016$Country=='Iraq')
usLength <- length(USAttacks2016$Country)

#Visualizing Data against each other..
boxplot(usLength,IraqLength)
barplot(table(USWarAttacks2016$Country), xlab = "Nationality", ylab="Count")

#Amount of Attacks in each data frame
#Finding how many attacks in america there are for ever attack in a war torn countries together
NumUSA <- length(USAttacks2016$id)
NumWar <- length(WarAttacks2016$id)

print(NumWar/NumUSA)
#For every 13.28704 Attacks in the a war torn country, there's an attack in America

#Finding the amount of towns there were Attacks in USA over past 4 years.
View(newShootings)
ViewUs <- aggregate(newShootings$Dead~newShootings$Location,FUN=toString)
View(ViewUs)

USCountPlot <- nrow(ViewUs)
#827 seperate destinations 

#Finding the amount of towns there were Attacks in the world over past 4 years.
ViewWorld <- aggregate(newTerror$Dead~newTerror$Location,FUN=toString)
View(ViewWorld)

WorldCountPlot <-  nrow(ViewWorld)
#14,881 seperate destinations

#plotting destination counts
plot(USCountPlot,WorldCountPlot, xlab = "US City Count", ylab="World City Count")

#finding what the highest death count is in one attack in USA
View(newShootings)
USAOne <- newShootings[ which(newShootings$Dead > 10),]
View(USAOne)

boxplot(USAOne$Dead ~ USAOne$Location)

#finding what the highest death count is in one attack in the World
View(newTerror)
WorldOne <- newTerror[ which(newTerror$Dead > 5),]
View(WorldOne)

boxplot(USAOne$Dead ~ USAOne$Location)
