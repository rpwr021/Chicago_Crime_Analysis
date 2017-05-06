Crw1 <- read.csv("/home/raz/Crw1.csv", header = TRUE)
colnames(Crw1)

summary(aov(tmp.avg~Primary.Type , data = Crw1))

x <- c("Primary.Type", "Description" , "Location.Description",  "Arrest" , "Domestic"  , "Beat" ,
       "District",  "Ward"  , "tmp.avg"  , "dp.avg", "hum.avg" ,   "P.avg" , "wind.avg",   "events" )



require(psych)
require(dplyr)
require(plyr)


####
Crw1$crime <- as.character(Crw1$Primary.Type)
Crw1$crime <-
  ifelse(
    Crw1$crime %in% c("CRIM SEXUAL ASSAULT",
                            "PROSTITUTION", "SEX OFFENSE"),
    'SEX',
    Crw1$crime
  )
Crw1$crime <-
  ifelse(Crw1$crime %in% c("MOTOR VEHICLE THEFT"),
         "MVT",
         Crw1$crime)
Crw1$crime <-
  ifelse(
    Crw1$crime %in% c(
      "GAMBLING",
      "INTERFERE WITH PUBLIC OFFICER",
      "INTERFERENCE WITH PUBLIC OFFICER",
      "INTIMIDATION",
      "LIQUOR LAW VIOLATION",
      "OBSCENITY",
      "NON-CRIMINAL",
      "PUBLIC PEACE VIOLATION",
      "PUBLIC INDECENCY",
      "STALKING",
      "NON-CRIMINAL (SUBJECT SPECIFIED)"
    ),
    "NONVIO",
    Crw1$crime
  )

Crw1$crime <-
  ifelse(Crw1$crime == "CRIMINAL DAMAGE",
         "DAMAGE",
         Crw1$crime)
Crw1$crime <- ifelse(Crw1$crime == "CRIMINAL TRESPASS",
                           "TRESPASS",
                           Crw1$crime)
Crw1$crime <-
  ifelse(
    Crw1$crime %in% c(
      "NARCOTICS",
      "OTHER
      NARCOTIC VIOLATION",
      "OTHER NARCOTIC VIOLATION"
    ),
    "DRUG",
    Crw1$crime
  )
Crw1$crime <- ifelse(Crw1$crime == "DECEPTIVE PRACTICE",
                           "FRAUD",
                           Crw1$crime)
Crw1$crime <-
  ifelse(Crw1$crime %in% c("OTHER OFFENSE", "OTHER
                                 OFFENSE"),
         "OTHER",
         Crw1$crime)
Crw1$crime <-
  ifelse(
    Crw1$crime %in% c("KIDNAPPING", "WEAPONS
                            VIOLATION", "OFFENSE INVOLVING CHILDREN"),
    "VIO",
    Crw1$crime
  )

ctypes<- levels(Crw1$Primary.Type)
crime <- matrix(ncol=2, nrow=33)
crime1 <- matrix(ncol=2, nrow=33)
Crw1$crime<- as.factor(Crw1$crime)

for( l in levels(Crw1$Primary.Type)){
  print(length(levels(Crw1$Primary.Type)))
  tmp <- Crw1[which(Crw1$Primary.Type == l ),]
  rsq <- summary(lm(ddply(tmp , .(wind.avg) , summarise , Count = length(Primary.Type) )))$r.squared
  i <- which(ctypes == l )
  crime[i,] <- c(l , rsq)
  rsq1 <- summary(lm(ddply(tmp , .(tmp.avg) , summarise , Count = length(Primary.Type) )))$r.squared
  i <- which(ctypes == l )
  crime1[i,] <- c(l , rsq1)
  remove(tmp)
}


ctypes<- levels(Crw1$crime)
crime <- matrix(ncol=2, nrow=20)
crime1 <- matrix(ncol=2, nrow=20)
Crw1$crime<- as.factor(Crw1$crime)

for( l in levels(Crw1$crime)){
  print(length(levels(Crw1$crime)))
  tmp <- Crw1[which(Crw1$crime == l ),]
  rsq <- summary(lm(ddply(tmp , .(wind.avg) , summarise , Count = length(crime) )))$r.squared
  i <- which(ctypes == l )
  crime[i,] <- c(l , rsq)
  rsq1 <- summary(lm(ddply(tmp , .(tmp.avg) , summarise , Count = length(crime) )))$r.squared
  i <- which(ctypes == l )
  crime1[i,] <- c(l , rsq1)
  remove(tmp)
}

h

crime <- as.data.frame(crime)
crime1 <- as.data.frame(crime1)
names(crime1) <- c("Wind" , "Adj. R Squared")
View(crime1)
require(ggplot2)
crime$V1<- unclass(crime$V1)
crime$V2<- unclass(crime$V2)
crime$V2<- as.numeric(crime$V2)

crime1$V1<- unclass(crime1$V1)
crime1$V2<- unclass(crime1$V2)
crime1$V2<- as.numeric(crime1$V2)
View(crime1)
ggplot(crime, aes(V2, V1)) + geom_point() + geom_point(aes(size = V2)) + scale_colour_gradient(low = "blue") 
ggplot(crime1, aes(V2, V1)) + geom_point() + geom_point(aes(size = V2)) + scale_colour_gradient(low = "blue") 
#p + geom_point(aes(size = qsec))
remove(crime1)
X <- names(Crw1[c(-1,-3)])
#Crime Vs tmp.avg
crimeSum <- ddply(Crw1[X] , .(tmp.avg) , summarise , ccount= length(crime))
ggplot(crimeSum, aes(ccount, tmp.avg)) + geom_point(aes(size = ccount)) + geom_point(aes(colour = tmp.avg)) 

crimeSum <- ddply(Crw1[X] , .(wind.avg) , summarise , ccount= length(crime))
ggplot(crimeSum, aes(ccount, wind.avg)) + geom_point(aes(size = ccount)) + geom_point(aes(colour = wind.avg)) 

summary(lm(ddply(Crw1[which(Crw1$Primary.Type == "THEFT" ),] , .(tmp.avg) , summarise , Count = length(Primary.Type) )))




