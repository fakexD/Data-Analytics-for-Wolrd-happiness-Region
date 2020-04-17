




#library Dplyr for manipulation
library(dplyr)
#Reading the data
getwd()
whd<- read.csv("world-happiness-data.csv")
View(whd)
#Delete the coloumn
whd <- whd[,-5]
View(whd)
#Creating new coloumn based on Continents
whd$continents <- NA
#Australia
whd$continents[which(whd$Region %in% c("Australia and New Zealand"))] <- "Australia"
#North America
whd$continents[which(whd$Region %in% c("North America"))] <- "North America"
#Europe
whd$continents[which(whd$Region %in% c("Western Europe","Central and Eastern Europe"))] <- "Europe"
#Africa
whd$continents[which(whd$Region %in% c("Sub-Saharan Africa","Middle East and Northern Africa"))] <- "Africa"
#Asia
whd$continents[which(whd$Region %in% c("Eastern Asia","Southern Asia","Southeastern Asia"))] <- "Asia"
#South America
whd$continents[which(whd$Region %in% c("Latin America and Caribbean"))] <- "South America"

#Creating an average of all the numerical data based on continents wise
hp <- aggregate(whd[,4:11],list(whd$continents),mean)
View(hp)

# Including library for visulization
library(ggplot2)
library(corrgram)
library(corrplot)

#Graphs the mean data of all continents
ggplot(hp,aes(x=Group.1,y=Happiness.Score,fill=Group.1))+geom_bar(stat = "identity")+ggtitle("Happiness Score of each continents")+ylab("Happiness Score")+xlab("Continents")

#lets find the correlation in variables
col <- sapply(whd,is.numeric)
cor.data <- cor(whd[,col])
corrplot(cor.data,method = "square",type ="upper")
#To find the maximum correlation 
corrplot(cor.data,method = "number",type ="upper")

#To create Box plot based on regions
box <- ggplot(whd,aes(x=Region,y=Happiness.Score,color=Region))
box + geom_boxplot()+geom_jitter(aes(color=Country),size=1.0)+ggtitle("Happiness Score for Regions and Countries")+coord_flip()+theme(legend.position = "none")

#To create Box plot based on Continents
ggplot(whd,aes(x=continents,y=Happiness.Score,color=continents))+geom_boxplot()+ggtitle("Happiness Score for Continents")

#Regression on all continents scatterplot for health life expectancy
ggplot(whd,aes(x=Health..Life.Expectancy.,y=Happiness.Score))+geom_point(aes(color=continents),size=3,alpha=0.8)+geom_smooth(aes(color=continents,fill=continents),method="lm",fullrange=T)+facet_wrap(~continents)+theme_bw()+ggtitle("Scatter plot for life Expectancy")

#scatterplot for economy
ggplot(whd,aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))+geom_point(aes(color=continents),size=3,alpha=0.8)+geom_smooth(aes(color=continents,fill=continents),method="lm",fullrange=T)+facet_wrap(~continents)+theme_bw()+ggtitle("Scatter plot for Economy")

#With freedom
ggplot(whd,aes(x=Freedom,y=Happiness.Score))+geom_point(aes(color=continents),size=3,alpha=0.8)+geom_smooth(aes(color=continents,fill=continents),method="lm",fullrange=T)+facet_wrap(~continents)+theme_bw()+ggtitle("Scatter plot for Freedom")

#for family
ggplot(whd,aes(x=Family,y=Happiness.Score))+geom_point(aes(color=continents),size=3,alpha=0.8)+geom_smooth(aes(color=continents,fill=continents),method="lm",fullrange=T)+facet_wrap(~continents)+theme_bw()+ggtitle("Scatter plot for Family")

#for trust in the government
ggplot(whd,aes(x=Trust..Government.Corruption.,y=Happiness.Score))+geom_point(aes(color=continents),size=3,alpha=0.8)+geom_smooth(aes(color=continents,fill=continents),method="lm",fullrange=T)+facet_wrap(~continents)+theme_bw()+ggtitle("Scatter plot for Trust in Governmnet")

#plot for the most unhappiest places ,noticed in the box plot
box + geom_boxplot()+geom_jitter(aes(color=Country),size=1.0)+ggtitle("Happiness Score for Regions and Countries")+coord_flip()+theme(legend.position = "none")

#Sun-Saharan Africa is the most unhappiest place as per box plot .
#whereas WesternEurope,NorthAmerica,Australia ans NewZealand are the happiest places.

#Classify all the data based on happy,neutral,unhappy nation.
#Creating new coloumn happiness meters
whd$happinessmeter <- NA
whd$happinessmeter[which(whd$Region %in% c("Western Europe","North America","Australia and New Zealand"))] <- "Happiest nation"
whd$happinessmeter[which(whd$Region %in% c("Southern Asia","Southeastern Asia","Middle East and Northern Africa","Latin America and Caribbean","Eastern Asia","Central and Eastern Europe"))] <- "Neutral nation"
whd$happinessmeter[which(whd$Region %in% c("Sub-Saharan Africa"))] <- "Unhappiest nation"

#Plot Regression for all three meters  
ggplot(whd,aes(x=Health..Life.Expectancy.,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=.8)+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()
#for economy
ggplot(whd,aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=.8)+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()
#for family
ggplot(whd,aes(x=Family,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=.8)+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()
#for freedom
ggplot(whd,aes(x=Freedom,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=.8)+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()
#for Generosity
ggplot(whd,aes(x=Generosity,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=.8)+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()
#for Dystopia.Residual
ggplot(whd,aes(x=Dystopia.Residual,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=.8)+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()

#Plot the GDP and HealthExpectancy on worldmap
#Need to Install rworldmap package
library(rworldmap)
#FOR GDP
d <- data.frame(country=whd$Country,value=whd$Economy..GDP.per.Capita.)
n<- joinCountryData2Map(d,joinCode="NAME",nameJoinColumn = "country")
mapCountryData(n,nameColumnToPlot = "value",mapTitle = "WorldMap for GDP 2015",colourPalette="terrain")
#FOR HEALTH LIFE EXPECTANCY
d <- data.frame(country=whd$Country,value=whd$Health..Life.Expectancy.)
n<- joinCountryData2Map(d,joinCode="NAME",nameJoinColumn = "country")
mapCountryData(n,nameColumnToPlot = "value",mapTitle = "WorldMap for Health..Life..Expectancy..",colourPalette="terrain")



