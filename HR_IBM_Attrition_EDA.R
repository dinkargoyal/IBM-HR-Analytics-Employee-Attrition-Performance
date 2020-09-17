#import file in Rstudio
att <- read.csv("Attrition.csv", header = TRUE, sep = ",")
View(att)
str(att)
summary(att)

#import library
library(dplyr)
library(plyr)

#check unique values
unique(att$EmployeeCount)
unique(att$StandardHours)
unique(att$Over18)

#check age distribution
hist(att$Age)

#check Daily rate distribution
x = mean(att$DailyRate)
count(att$DailyRate > x) 
hist(att$DailyRate)
table(att$Attrition)
#plotting of attrition counts
barplot(table(att$Attrition), col = ('green'), border = 'blue')

#plotting of gender counts
barplot(table(att$Gender), col = c('Pink','blue'),  ylim = c(0,1000))

# relation between attrition and gender
counts <- table(att$Gender, att$Attrition)
barplot(counts, col = c('pink','blue'), main = "Gender vs Attrition", legend = rownames(counts))
barplot(counts, col = c('pink','blue'), main = "Gender vs Attrition", beside = TRUE, legend = rownames(counts))

#relation b/w attrition and Business Travel
count1 <- table(att$Attrition, att$BusinessTravel)
barplot(count1, col = c('red','green'), main = 'Attrition v/s Business travel', xlim = c(0,6), las = 2, legend.text = TRUE)

#stock option level
summary(att$StockOptionLevel)
hist(att$StockOptionLevel)

#relation b/w attrition and distance
summary(att$DistanceFromHome)
hist(att$DistanceFromHome, ylim = c(0,500), col = 'Green')
count2 <- table(att$Attrition, att$DistanceFromHome)
barplot(count2, col = c('Red', 'Dark Green'), ylim = c(0,250), legend.text = TRUE)

#relation b/w attrition and Environment Satisfaction
count3 <- table(att$Attrition, att$EnvironmentSatisfaction)
barplot(count3, xlab = 'Environment Satisfaction', beside = TRUE, col = c('Red', 'Dark Green'), xlim = c(0,20), legend.text = TRUE)

#relation b/w attrition and WorkLifeBalance
count4 <- table(att$Attrition, att$WorkLifeBalance)
barplot(count4, beside = TRUE, col = c('Red', 'Dark Green'), xlim = c(0,20), legend.text = TRUE)

#relation b/w attrition and years at company
hist(att$YearsAtCompany, col = 'Blue')

##relation b/w attrition and OverTime
count5 <- table(att$Attrition, att$OverTime)
barplot(count5, beside = TRUE, col = c('Red', 'Dark Green'), xlim = c(0,20), legend.text = TRUE)

##relation b/w attrition and percentage salary hike
summary(att$PercentSalaryHike)
hist(att$PercentSalaryHike, col = 'Blue')
count6 <- table(att$Attrition, att$PercentSalaryHike)
barplot(count6,  col = c('Red', 'Dark Green'), xlab = 'Percentage salary hike', xlim = c(0,20), legend.text = TRUE)

#relation b/w attrition and percentage salary hike
summary(att$HourlyRate)
hist(att$HourlyRate)

#joblevel
summary(att$JobLevel)
count7 <- table(att$Attrition, att$JobLevel)
barplot(count7,  col = c('Red', 'Dark Green'), xlab = 'Job Level', xlim = c(0,10), legend.text = TRUE)

#job Satisfaction
summary(att$JobSatisfaction)
count8 <- table(att$Attrition, att$JobSatisfaction)
barplot(count8,  col = c('Red', 'Dark Green'), xlab = 'Job Satisfaction', xlim = c(0,10), legend.text = TRUE)

#jobrole
summary(att$JobRole)
count9 <- table(att$Attrition, att$JobRole)
barplot(count9,  col = c('Red', 'Dark Green'), las = 2, xlim = c(0,15), ylim = c(0,400), legend.text = TRUE)

#Department
summary(att$Department)
count10 <- table(att$Attrition, att$Department)
barplot(count10,  col = c('Red', 'Dark Green'), las = 2, xlim = c(0,5), ylim = c(0,1000), legend.text = TRUE)

#education
summary(att$Education)
count11 <- table(att$Attrition, att$Education)
barplot(count11,  col = c('Red', 'Dark Green'), las = 2, xlim = c(0,5), ylim = c(0,1000), legend.text = TRUE)

# Education Field
summary(att$EducationField)
count12 <- table(att$Attrition, att$EducationField)
barplot(count12,  col = c('Red', 'Dark Green'), las = 2, xlim = c(0,10), ylim = c(0,1000), legend.text = TRUE)

#hourly Rate
summary(att$HourlyRate)
hist(att$HourlyRate)
count(att$HourlyRate > mean(att$HourlyRate))

#job involvement
summary(att$JobInvolvement)
count13 <- table(att$Attrition, att$JobInvolvement)
barplot(count13,  col = c('Red', 'Dark Green'), xlim = c(0,10), ylim = c(0,1000), legend.text = TRUE)

#Marital status
summary(att$MaritalStatus)
count14 <- table(att$Attrition, att$MaritalStatus)
barplot(count14,  col = c('Red', 'Dark Green'), xlim = c(0,5), ylim = c(0,1000), legend.text = TRUE)

#monthly income
summary(att$MonthlyIncome)
hist(att$MonthlyIncome, col = 'Blue', breaks = 15,  labels = TRUE)

#monthly Rate
summary(att$MonthlyRate)
hist(att$MonthlyRate, labels = TRUE)

#No of companies worked
summary(att$NumCompaniesWorked)
hist(att$NumCompaniesWorked)
count15 <- table(att$Attrition, att$NumCompaniesWorked)
barplot(count15,  col = c('Red', 'Dark Green'), xlim = c(0,10), ylim = c(0,1000), legend.text = TRUE)

#performance rating
summary(att$PerformanceRating)
count16 <- table(att$Attrition, att$PerformanceRating)
barplot(count16,  col = c('Red', 'Dark Green'), xlim = c(0,10), ylim = c(0,1500), legend.text = TRUE)

#Relationship Satisfaction
summary(att$RelationshipSatisfaction)
count17 <- table(att$Attrition, att$RelationshipSatisfaction)
barplot(count17,  col = c('Red', 'Dark Green'), xlim = c(0,10), ylim = c(0,1500), legend.text = TRUE)

#worklife balance
summary(att$WorkLifeBalance)
count18 <- table(att$Attrition, att$WorkLifeBalance)
barplot(count18,  col = c('Red', 'Dark Green'), xlim = c(0,10), ylim = c(0,1500), legend.text = TRUE)

#removal of the column
att <- att[,-9]
att <- att[,-9]
att <- att[,-20]
att <- att[,-24]

View(att)

#conversion attrition column from factor to numeric 
att$Attrition <- mapvalues(att$Attrition, from=c('Yes', 'No'), to = c(1,0))
att$Attrition <- as.numeric(as.character(att$Attrition))
View(att)
table(att$Attrition)


library(dummies)
att <- dummy.data.frame(att)
View(att)

#removal of one column after dummification
att <- att[,-3]
att <- att[,-6]
att <- att[,-15]
att <- att[,-16]
att <- att[,-21]
att <- att[,-29]
att <- att[,-34]

#install.packages('corrplot')
library(corrplot)
corrplot(cor(att, method = 'spearman'))

names(att)[names(att) == "DepartmentResearch & Development"] <- "Department_Research_and_Developement"
names(att)[names(att) == "EducationFieldHuman Resources"] <- "EducationFieldHumanResources"
names(att)[names(att) == "EducationFieldLife Sciences"] <- "EducationFieldLifeSciences"
names(att)[names(att) == "JobRoleHealthcare Representative"] <- "JobRoleHealthcareRepresentative"
names(att)[names(att) == "JobRoleLaboratory Technician"] <- "JobRoleLaboratoryTechnician"
names(att)[names(att) == "JobRoleManufacturing Director"] <- "JobRoleManufacturingDirector"
names(att)[names(att) == "JobRoleResearch Scientist"] <- "JobRoleResearchScientist"
names(att)[names(att) == "JobRoleSales Executive"] <- "JobRoleSalesExecutive"
names(att)[names(att) == "JobRoleSales Representative"] <- "JobRoleSalesRepresentative"
names(att)[names(att) == "JobRoleResearch Director"] <- "JobRoleResearchDirector"

View(att)
str(att)

#removal of highly correlated column
att <- att[,-42]
att <- att[,-19]
att <- att[,-7]

#response variable data type conversion
att$Attrition <- as.factor(as.numeric(att$Attrition))
View(att)
table(att$Attrition)

#splitting the data 
library(caTools)
set.seed(118)
split = sample.split(att,SplitRatio = 0.75)
train <- subset(att, split ==TRUE)
test <- subset(att, split == FALSE)

View(train)
View(test)



