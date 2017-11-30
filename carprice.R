
installed_packages <- installed.packages()[,1]
isTidyrInstalled <- "tidyr" %in% installed_packages
isDplyrInstalled <- "dplyr" %in% installed_packages
isStringrInstalled <- "stringr" %in% installed_packages
isMassinstalled <- "Mass" %in% installed_packages
isCarinstalled <- "Car" %in% installed_packages
isggPlotinstalled <- "ggplot2" %in% installed_packages

if(!isTidyrInstalled) {
  install.packages("tidyr")
}

if(!isDplyrInstalled) {
  install.packages("dplyr")
}

if(!isStringrInstalled) {
  install.packages("stringr")
}

if(!isMassinstalled) {
  install.packages("Mass")
}

if(!isCarinstalled) {
  install.packages("Car")
}

if(!isggPlotinstalled){
  install.packages("ggplot2")
}

library("MASS")
library("car")
library("stringr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("dplyr")

carprice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = TRUE)
View(carprice)


#EDA Analysis 
#Check for na values
sum(is.na(carprice))

#check for unique
duplicated(nrow(carprice[,-1]))

str(carprice)


#Split car company name from car name
carprice$CarName <- as.character(carprice$CarName)
carprice <- separate(carprice, c("CarName"), c("Companyname","Modelname"),sep=" ")
str(carprice$Companyname)
carprice$Companyname <- as.factor(carprice$Companyname)
levels(carprice$Companyname)


#Cleaning the company names
#Mazda has been mispelled as masda
carprice$Companyname[which(carprice$Companyname=="maxda")] <- "mazda"
#Caps irror for Nissan
carprice$Companyname[which(carprice$Companyname=="nissan")] <- "Nissan"
#porsche has been mispelled as porcshce
carprice$Companyname[which(carprice$Companyname=="porcshce")] <- "porsche"
#toyota has been mispelled as toyouta
carprice$Companyname[which(carprice$Companyname=="toyouta")] <- "toyota"
#volkswagen has been mispelled as vokswagen and also been abbrevated as vw
carprice$Companyname[which(carprice$Companyname=="vokswagen" | carprice$Companyname=="vw")] <- "volkswagen"
carprice$Companyname <- as.character(carprice$Companyname)
carprice$Companyname <- as.factor(carprice$Companyname)


#Symboling converted to factors
carprice$symboling <- as.factor(carprice$symboling)
levels(carprice$symboling)

#Treating outliers

#Checking outliers for Wheelbse and found that there are no outliers
quantile(carprice$wheelbase,seq(0,1,0.01))
quantile(carprice$carlength,seq(0,1,0.01))
quantile(carprice$carwidth,seq(0,1,0.01))
quantile(carprice$carheight,seq(0,1,0.01))
#Curb weight outliers at 98% are handled
quantile(carprice$curbweight,seq(0,1,0.01))
carprice$curbweight[which(carprice$curbweight > 3768.40)] = 3768.40

quantile(carprice$boreratio,seq(0,1,0.01))
quantile(carprice$stroke,seq(0,1,0.01))

#copressionration outliers at 90% handled
quantile(carprice$compressionratio,seq(0,1,0.01))
carprice$compressionratio[which(carprice$compressionratio > 10.9400)] = 10.9400

#horsepower outliers at 97%
quantile(carprice$horsepower,seq(0,1,0.01))
carprice$horsepower[which(carprice$horsepower > 184)] = 184.00

quantile(carprice$peakrpm,seq(0,1,0.01))

#Citympg outliers at 98%
quantile(carprice$citympg,seq(0,1,0.01))
carprice$citympg[which(carprice$citympg > 38)] = 38

#Highwaympg outliers at 97%
quantile(carprice$highwaympg,seq(0,1,0.01))
carprice$highwaympg[which(carprice$highwaympg > 45.64)] = 45.64

#Derived variables
#Derived average mpg
carprice$avgmpg <- (carprice$citympg + carprice$highwaympg) / 2


#Calculate PerCylindersize
carprice$cylindernumber <- as.numeric(carprice$cylindernumber)
carprice$percylindersize <- carprice$enginesize / carprice$cylindernumber
carprice$percylindersize <- round(carprice$percylindersize,digits=2)
carprice$cylindernumber <- as.factor(carprice$cylindernumber)

str(carprice)


#Handling Symboling

# -2 to -1 -> safe
levels(carprice$symboling)[1:2] <- "Safe"
# 0 to 1 -> Risky
levels(carprice$symboling)[2:3] <- "Risky"
# 2 to 3 -> High risk
levels(carprice$symboling)[3:4] <- "High risk"

levels(carprice$symboling)

#Handle the factor with two levels

#Fuel type categorical vairable have 2 levels diesel and gas replaced with 1 and 0
levels(carprice$fueltype) <- c(1,0)

#Aspiration categorical varibale with two levels std and turbo replaced with 1 and 0
levels(carprice$aspiration) <- c(1,0)

#Door number categoricla variables with two levels two and four rplaced with 0 and 1
levels(carprice$doornumber) <- c(1,0)

#Engine localtion has two levels front and rear replaced with 1 and 0
levels(carprice$enginelocation) <- c(1,0)

#handle columns with more than two levels
#Converting symboling into dummies
dummy_1 <- data.frame(model.matrix( ~symboling, data = carprice))
View(dummy_1)

carprice_1 <- cbind(carprice[,-2],dummy_1[,-1])
View(carprice_1)

#Converting companyname into dummies
dummy_2 <- data.frame(model.matrix(~Companyname, data= carprice_1))
View(dummy_2)

carprice_2 <- cbind(carprice_1[,-2], dummy_2[,-1]) 
View(carprice_2)

#Converting engine type into dummies
dummy_3 <- data.frame(model.matrix(~enginetype, data = carprice_2))
View(dummy_3)

carprice_3 <- cbind(carprice_2[,-14], dummy_3[,-1])
View(carprice_3)

#Converting cylinder number into dummies
dummy_4 <- data.frame(model.matrix(~cylindernumber, data = carprice_3))
View(dummy_4)


carprice_4 <- cbind(carprice_3[,-14], dummy_4[,-1])
View(carprice_4)

#Converting fuelsystem into dummies
dummy_5 <- data.frame(model.matrix(~fuelsystem, data = carprice_4))
View(dummy_5)


carprice_5 <- cbind(carprice_4[,-15], dummy_5[,-1])
str(carprice_5)
View(carprice_5)

#Converting carbody into dummies
dummy_6 <- data.frame(model.matrix(~carbody, data = carprice_5))
View(dummy_6)


carprice_6 <- cbind(carprice_5[,-6], dummy_6[,-1])
View(carprice_6)

#Converting drivewheel into dummies
dummy_7 <- data.frame(model.matrix(~drivewheel, data = carprice_6))
View(dummy_7)


carprice_7 <- cbind(carprice_6[,-6], dummy_7[,-1])
View(carprice_7)



#Remove unwnated coulmns car id and car model name
carprice_finaldata <- carprice_7[,-(1:2)]
View(carprice_finaldata)

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice_finaldata), 0.7*nrow(carprice_finaldata))
# generate the train data set
train = carprice_finaldata[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice_finaldata[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
#R square value R square value = 0.9694
summary(model_1)


#We have total fo 67 variables considered into account lets perform stepAIC

step <- stepAIC(model_1, direction="both")

step

#No of variables has been reduced to 34 after perfoorming stepAIC 
#Using the model proposed after stepAIC

model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynameporsche + 
                Companynamerenault + Companynamesaab + Companynamesubaru + 
                Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber3 + cylindernumber4 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd, 
              data = train)

summary(model_2)
#R square value for model 2 is 0.9749
vif(model_2)

#Variable Companynamesaab has very high p value of 0.16751 hence removing it

model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynameporsche + 
                Companynamerenault + Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber3 + cylindernumber4 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd, 
              data = train)
summary(model_3)
##R square value for model 3 is 0.9747
vif(model_3)

#Variable Companynameporsche has high vif values of 13.879006 and high p vlaue of 0.011290 hence removing it
model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber3 + cylindernumber4 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd, 
              data = train)
summary(model_4)
#R square value for model 4 is 0.9734
vif(model_4)

#cylindernumber3 has high vif value of 176.431106 hence removing it

model_5 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber4 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd, data = train)
summary(model_5)
#R square value for model 5 is 0.9723
vif(model_5)

#drivewheelrwd has high vif value 12.894730

model_6 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber4 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon , data = train)
summary(model_6)
#R square value for model 6 is 0.9717
vif(model_6)

#carbodyhatchback has high vif value of 12.841827

model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber4 + carbodyhardtop + carbodysedan + carbodywagon , data = train)
summary(model_7)
#R square value for model 7 is 0.9693
vif(model_7)

#carbodyhardtop has high p vlaue of 0.816947

model_8 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber4 + carbodysedan + carbodywagon , data = train)
summary(model_8)
#R square value of model 8 is 0.9696
vif(model_8)

#carbodysedan has high p vlaue of 0.865718 hence removing it
model_9 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                Companynamebuick + Companynamechevrolet + Companynamedodge + 
                Companynamehonda + Companynamejaguar + Companynamemazda + 
                Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                cylindernumber2 + cylindernumber4 + carbodywagon , data = train)
summary(model_9)
#R square value for the model 9 is 0.9699
vif(model_9)

#Companynamechevrolet has high p vlaue of 0.227087 hence removing it
model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                 Companynamebuick + Companynamedodge + Companynamehonda + Companynamejaguar + Companynamemazda + 
                 Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                 Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                 Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                 cylindernumber2 + cylindernumber4 + carbodywagon , data = train)
summary(model_10)
#R square value for the model 10 is 0.9697
vif(model_10)

#cylindernumber2 has high p value of 0.247470 hence removing it
model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                 Companynamebuick + Companynamedodge + Companynamehonda + Companynamejaguar + Companynamemazda + 
                 Companynamemercury + Companynamemitsubishi + CompanynameNissan + 
                 Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                 Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                 cylindernumber4 + carbodywagon , data = train)
summary(model_11)
#R square value of model 11 is 0.9696
vif(model_11)

#Companynamemercury has p value of 0.158116 hence removing it
model_12 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                 Companynamebuick + Companynamedodge + Companynamehonda + Companynamejaguar + Companynamemazda + 
                 Companynamemitsubishi + CompanynameNissan + Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                 Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + 
                 cylindernumber4 + carbodywagon , data = train)
summary(model_12)
#R square value of model 12 is 0.9694
vif(model_12)

#carbodywagon has high pvalue 0.110815 hence removing it
model_13 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + stroke + peakrpm + percylindersize + Companynamebmw + 
                 Companynamebuick + Companynamedodge + Companynamehonda + Companynamejaguar + Companynamemazda + 
                 Companynamemitsubishi + CompanynameNissan + Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                 Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + cylindernumber4, data = train)
summary(model_13)
#R square value of model 13 is 0.969
vif(model_13)

#stroke has p value 0.111767  hence removing it
model_14 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + peakrpm + percylindersize + Companynamebmw + Companynamebuick + 
                 Companynamedodge + Companynamehonda + Companynamejaguar + Companynamemazda + 
                 Companynamemitsubishi + CompanynameNissan + Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                 Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + cylindernumber4, data = train)
summary(model_14)
#R square value for model 14 is 0.9686
vif(model_14)

#peakrpm has high p value of 0.033198 hence removing it
model_15 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamebuick + Companynamedodge + 
                 Companynamehonda + Companynamejaguar + Companynamemazda + 
                 Companynamemitsubishi + CompanynameNissan + Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                 Companynamesubaru + Companynametoyota + Companynamevolkswagen + enginetyperotor + cylindernumber4, data = train)
summary(model_15)
#R square value for model 15 is 0.9676
vif(model_15)

#Companynamebuick has high p vlaue of 0.002385 hence removing it
model_16 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamedodge + Companynamehonda + 
                 Companynamejaguar + Companynamemazda + Companynamemitsubishi + CompanynameNissan + 
                 Companynamepeugeot + Companynameplymouth + Companynamerenault + Companynamesubaru + 
                 Companynametoyota + Companynamevolkswagen + enginetyperotor + cylindernumber4, data = train)
summary(model_16)
#R square value for model 16 is 0.9653
vif(model_16)

#Companynamehonda has high p vlaue of 0.00175 hence removing it
model_17 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamedodge +
                 Companynamejaguar + Companynamemazda + Companynamemitsubishi + CompanynameNissan + 
                 Companynamepeugeot + Companynameplymouth + Companynamerenault + Companynamesubaru + 
                 Companynametoyota + Companynamevolkswagen + enginetyperotor + cylindernumber4, data = train)
summary(model_17)
#R square value for model 17 is 0.947
vif(model_17)

#CompanynameNissan has high p value of 0.00747 hence removing it
model_18 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamedodge +
                 Companynamejaguar + Companynamemazda + Companynamemitsubishi + 
                 Companynamepeugeot + Companynameplymouth + Companynamerenault + Companynamesubaru + 
                 Companynametoyota + Companynamevolkswagen + enginetyperotor + cylindernumber4, data = train)
summary(model_18)
#R square value for model 18 is 0.9465
vif(model_18)

#Companynamemazda has high p value of 0.005761 hence removing it
model_19 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamedodge + Companynamejaguar + 
                 Companynamemitsubishi + Companynamepeugeot + Companynameplymouth + Companynamerenault + Companynamesubaru + 
                 Companynametoyota + Companynamevolkswagen + enginetyperotor + cylindernumber4, data = train)
summary(model_19)
#R square value for model 19 is 0.9587
vif(model_19)

#Companynamevolkswagen has p value of 0.023085 hence removing it
model_20 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamedodge + Companynamejaguar + 
                 Companynamemitsubishi + Companynamepeugeot + Companynameplymouth + Companynamerenault + 
                 Companynamesubaru + Companynametoyota  + enginetyperotor + cylindernumber4, data = train)
summary(model_20)
#R square value for model 20 is 0.9573
vif(model_20)

#Companynameplymouth has p value of 0.16129 hence removing it
model_21 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamedodge + Companynamejaguar + 
                 Companynamemitsubishi + Companynamepeugeot + Companynamerenault + 
                 Companynamesubaru + Companynametoyota  + enginetyperotor + cylindernumber4, data = train)
summary(model_21)
#R square value for model 21 is 0.9556
vif(model_21)

#Companynamedodge has p value of 0.025392 hence removing it
model_22 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamejaguar + 
                 Companynamemitsubishi + Companynamepeugeot + Companynamerenault + Companynamesubaru + 
                 Companynametoyota  + enginetyperotor + cylindernumber4, data = train)
summary(model_22)
#R square value for model 22 is 0.9542
vif(model_22)

#Companynamesubaru has p vlaue of 0.03649 hence removing it
model_23 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamejaguar + 
                 Companynamemitsubishi + Companynamepeugeot + Companynamerenault +  
                 Companynametoyota  + enginetyperotor + cylindernumber4, data = train)
summary(model_23)
#R square value for model 23 is 0.9529
vif(model_23)

#Companynamerenault has p value of 0.02306 hence removing it
model_24 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamejaguar +Companynamemitsubishi + 
                 Companynamepeugeot + Companynametoyota  + enginetyperotor + cylindernumber4, data = train)
summary(model_24)
#R square value for model 24 is 0.9514
vif(model_24)

#Companynametoyota high p value of 0.012471 hence removing it
model_25 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamejaguar +Companynamemitsubishi + 
                 Companynamepeugeot + enginetyperotor + cylindernumber4, data = train)
summary(model_25)
#R square value for model 25 is 0.9494
vif(model_25)

#Companynamemitsubishi has p vlaue of 0.008415 hence removing it to make it significant
model_26 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamejaguar +
                 Companynamepeugeot + enginetyperotor + cylindernumber4, data = train)
summary(model_26)
#R square value for model 26 is 0.947
vif(model_26)

#cylindernumber4 has p value of 0.005817 hence removing
model_27 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + percylindersize + Companynamebmw + Companynamejaguar +
                 Companynamepeugeot + enginetyperotor , data = train)
summary(model_27)
#R square value for model 27 is 0.9443
vif(model_27)

#enginetyperotor has p value of 0.00229 hence removing
model_28<- lm(formula = price ~ aspiration + enginelocation + carwidth + curbweight + 
                percylindersize + Companynamebmw + Companynamejaguar + Companynamepeugeot, data = train)
summary(model_28)
#R square value for model 28 is 0.9407
vif(model_28)

#aspiration0 has p value of 0.0215 hence removing

model_29<- lm(formula = price ~ enginelocation + carwidth + curbweight + percylindersize + 
                Companynamebmw + Companynamejaguar + Companynamepeugeot, data = train)
summary(model_29)
#R square value for model 29 is 0.9387
vif(model_29)

#All the p values are highly singificant hence we stop with model no 29
#Equation is price = 1.09e^(-08) + 2e^(-16) * enginelocation + 6.19e^(-06) * carwidth + 3.37e^(-14) * curbweight + 2e^(-16) * percylindersize + 
###                  6.76e^(-13) * Companynamebmw + 3.73e^(-08) * Companynamejaguar + 8.29e^(-06) * Companynamepeugeot


#Predicting the price for the train data
Predict_1 <- predict(model_29,train)
train$predicted_price <- Predict_1

#Correlate the price and predicted price
r <- cor(train$price,train$predicted_price)

#R suqre value
rsquare <- r^2

# check R-squared
rsquare

#r square value is 0.941759 for the train data.



###### Test data

#Lets check it for the test data
Predict_2 <- predict(model_29,test)
test$predicted_price <- Predict_2

#Correlate the price and predicted price
r <- cor(test$price,test$predicted_price)

#R suqre value
rsquare <- r^2

# check R-squared
rsquare

#r square value is 0.8669205 for the test data set



#Varibales that are considered are enginelocation, carwidth, curbweight, percylindersize, Companynamebmw, Companynamejaguar and Companynamepeugeot


#Plot price with engine location

