MVT <- read.table("Data/mvtweek1.csv",sep=",", header=TRUE)

#How many rows of data (observations) are in this dataset?
nrow(MVT)

#How many variables are in this dataset?
ncol(MVT)

#Using the "max" function, what is the maximum value of the variable "ID"?
max(MVT$ID)
 
# What is the minimum value of the variable "Beat"?
min(MVT$Beat)
 
#How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
nrow(MVT[MVT$Arrest==TRUE,])

#How many observations have a LocationDescription value of ALLEY?
nrow(MVT[MVT$LocationDescription=="ALLEY",])

#Now, let's convert these characters into a Date object in R.
DateConvert = as.Date(strptime(MVT$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

#Extract the month and the day of the week, and add these variables to our data frame mvt. We can do this with two simple functions
MVT$Months<-months(DateConvert)
MVT$Days <- weekdays(DateConvert)

#replace the old Date variable with DateConvert by typing:
  MVT$Date <- DateConvert

#In which month did the fewest motor vehicle thefts occur?
MonthlyThefts <- ftable(MVT$Month)

# On which weekday did the most motor vehicle thefts occur?
Weekends <- c("Sunday","Saturday")
DailyTheft <- table(MVT$Days)
WeekdayThefts <- DailyTheft[!dimnames(DailyTheft)[[1]] %in% Weekends]# Lets exclude Weekend stats from the list. Store it in WeekdayThefts
WeekdayThefts[sort(desc(WeekdayThefts))][1]


#Which month has the largest number of motor vehicle thefts for which an arrest was made?
MonthlyTheft <- data.frame(table(MVT$Months,MVT$Arrest))
colnames(MonthlyTheft) <- c("Month","Arrested_Status","NumberofThefts")
ArrestedMonthlyLeft <- MonthlyTheft[which(MonthlyTheft$Arrested_Status==TRUE),] # Find thefts where arrested in every month
ArrestedMonthlyLeft[order(desc(ArrestedMonthlyLeft$NumberofThefts)),][1,1]

hist(MVT$Date,breaks=100)

boxplot(MVT$Date~MVT$Arrest)

#For what proportion of motor vehicle thefts in 2001 was an arrest made?
MVT$Year <- as.numeric(format(DateConvert,"%y"))
MVT2001 <- MVT[MVT$Year==01,]
table(MVT2001$Arrest)# See frequency table for Arrested in 2001
prop.table(table(MVT2001$Arrest))# We can generate proportion from the table

#Alternatively, we can use prop.table for all
prop.table(table(MVT$Year,MVT$Arrest),1)# We can generate proportion from the table

#Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?
sort(table(MVT$LocationDescription),decreasing =TRUE)

Top5 <- MVT[MVT$LocationDescription %in% c("STREET","PARKING LOT/GARAGE(NON.RESID.)","ALLEY","GAS STATION","DRIVEWAY - RESIDENTIAL"),]

#R will remember the other categories of the LocationDescription variable from the original dataset, 
#so running table(Top5$LocationDescription) will have a lot of unnecessary output. To make our tables 
#bit nicer to read, we can refresh this factor variable.
table(Top5$LocationDescription<- factor(Top5$LocationDescription))

#One of the locations has a much higher arrest rate than the other locations. Which is it?
prop.table(table(Top5$LocationDescription, Top5$Arrest),1)

#On which day of the week do the most motor vehicle thefts at gas stations happen?
prop.table(table(Top5$LocationDescription,Top5$Days),1)
