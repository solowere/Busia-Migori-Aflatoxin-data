#Data comparison for aflatoxin label between Migori and Busia Counties.
#importing data as text.
#Setting my directory to the folder holding my files.
setwd("C:/Users/admin/Desktop/N. Mwenda")

#Loading packages that I will be use in the analysis on data manipulation and visualisation.
library(dplyr)
library(ggplot2)
library(plyr)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

#Reading my data from the folder.

df <- read.table("county data.csv",sep = ",",header = TRUE)
View(df)

#More about the imported data inclusing its structures.
str(df)
sink("Analysis2.doc", append = TRUE)

#Quick summary statistics
summary(df)

#Sample sizes from County,Store and Aflatoxin tested.
with(df, summary(County))
with(df, summary(Site))
with(df, summary(Aflatoxin))

with(df, summary(Concentration))

#Mean of Aftaxon detections.
AflatoxinMeans = with(df, by(Concentration, Aflatoxin, mean))
AflatoxinMeans

#StoreMeans
StoreMeans<-with(df, by(Concentration, Site, County, mean))
StoreMeans 

#CountyMeans
CountyMeans<-with(df, by(Concentration, County, mean))
CountyMeans 

#Mean per for Aflatoxin partition by Store type
SumStatics<-tapply(df$Concentration,
       list(df$County,df$Aflatoxin,df$Site), mean)
SumStatics

Chart1<-ggplot(df, aes(x=Aflatoxin, y=Concentration, 
                        fill=Aflatoxin)) + geom_boxplot() + facet_grid(County~Site, scales = "free") + scale_fill_brewer(palette = "Set1")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
                                                                                                                       
Chart1 + labs(title = "Conc.(ug/kg) per the different stores in Migori & Busia",x="Aflatoxin", y="Conc.(ug/kg)")+theme(plot.title = element_text(hjust = 0.5))   



write.


#Anova tests

#OneWay Anova to test if there is difference in mean on the concentration on function aflatoxin type.
#Null_Hypothesis: There is no difference in mean of concenyration of Aflatoxin detected.

Aflatoxin_one_way_Anova<- aov(Concentration~Aflatoxin,data = df)
summary(Aflatoxin_one_way_Anova)


#Two way Anova
Aflatoxin_two_way_Anova<-aov(Concentration~Aflatoxin+Site, data = df)
summary(Aflatoxin_two_way_Anova)


#Examining whether there is an interaction between Site and Aflatoxin.

Aflatoxin_Site_Interaction<-aov(Concentration~Aflatoxin*Site, data = df)
summary(Aflatoxin_Site_Interaction)

#The low sum of square of th interaction between Aflatoxin and Sites and the high p-value suggest that the very little avriatin in the concetration can be explained by the interaction between the two feautures/Variables.

best_fit_model<-list(Aflatoxin_one_way_Anova,Aflatoxin_two_way_Anova,Aflatoxin_Site_Interaction)
best_fit_modelName<-c("Aflatoxin_one_way_Anova","Aflatoxin_two_way_Anova","Aflatoxin_Site_Interaction")
aictab(best_fit_model, modnames=best_fit_modelName)

par(mfrow=c(2,2))
plot(Aflatoxin_two_way_Anova)
par(mfrow=c(1,1))

turkey4_2way<-TukeyHSD(Aflatoxin_two_way_Anova)
turkey4_2way




#