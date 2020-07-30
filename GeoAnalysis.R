#packages
install.packages("ggplot2")
library(ggplot2)
library(scales)
library(readr)
library(plyr)

geoExData <- read.csv(file.choose())
geoExData$
school <- geoExData$School
semester <- geoExData$Semester
manual <- geoExData$Sc1_1_manual
stopDepth <- geoExData$Sc1_1_stoppingdepth
coneBreak <- geoExData$Sc1_1_conebreaking
soilType_correct <-geoExData$Sc1_1_soiltype_correct
scene_11 <- data.frame(school, semester, manual, stopDepth, coneBreak)

#does automatic_manual influences cone breaking (from scene 1.1 to 2.3)
mcnemarTestCone(school, semester, manual, coneBreak)
mcnemarTestCone(school, semester, manual, test = geoExData$Sc1_2_conebreaking)
mcnemarTestCone(school, semester, manual, test = geoExData$Sc1_3_conebreaking)
mcnemarTestCone(school, semester, manual, test = geoExData$Sc2_1_conebreaking)
mcnemarTestCone(school, semester, manual, test = geoExData$Sc2_2_conebreaking)
mcnemarTestCone(school, semester, manual, test = geoExData$Sc2_3_conebreaking)

# influence of manual vs automatic on soil type identification (from scene 1.1 to 2.3)
mcnemarTestSoilType(school, semester, manual, soilType_correct)
mcnemarTestSoilType(school, semester, manual, test = geoExData$Sc1_2_soiltype_correct)
mcnemarTestSoilType(school, semester, manual, test = geoExData$Sc1_3_soiltype_correct)
mcnemarTestSoilType(school, semester, manual, test = geoExData$Sc2_1_soiltype_correct)
mcnemarTestSoilType(school, semester, manual, test = geoExData$Sc2_2_soiltype_correct)
mcnemarTestSoilType(school, semester, manual, test = geoExData$Sc2_3_soiltype_correct)

#difference of stop depths given manual or automatic (from scene 1.1 to 2.3)
generalTests(scene_11$manual, scene_11$stopDepth, "two.sided")
generalTests(geoExData$Sc1_2_manual, geoExData$Sc1_2_stoppingdepth, "two.sided")
generalTests(geoExData$Sc1_3_manual, geoExData$Sc1_3_stoppingdepth, "two.sided")
generalTests(geoExData$Sc2_1_manual, geoExData$Sc2_1_stoppingdepth, "two.sided")
generalTests(geoExData$Sc2_2_manual, geoExData$Sc2_2_stoppingdepth, "two.sided")
generalTests(geoExData$Sc2_3_manual, geoExData$Sc2_3_stoppingdepth, "two.sided")

#difference of cpt total given manual or automatic (from scene 1.1 to 2.3)
generalTests(scene_11$manual, geoExData$Sc1_1_CPTtotal, "two.sided")
generalTests(geoExData$Sc1_2_manual, geoExData$Sc1_2_CPTtotal, "two.sided")
generalTests(geoExData$Sc1_3_manual, geoExData$Sc1_3_CPTtotal, "two.sided")
generalTests(geoExData$Sc2_1_manual, geoExData$Sc2_1_CPTtotal, "two.sided")
generalTests(geoExData$Sc2_2_manual, geoExData$Sc2_2_CPTtotal, "two.sided")
generalTests(geoExData$Sc2_3_manual, geoExData$Sc2_3_CPTtotal, "two.sided")

#difference of analysis total given manual or automatic (from scene 1.1 to 2.3)
generalTests(scene_11$manual, geoExData$Sc1_1_analysistotal, "two.sided")
generalTests(geoExData$Sc1_2_manual, geoExData$Sc1_2_analysistotal, "two.sided")
generalTests(geoExData$Sc1_3_manual, geoExData$Sc1_3_analysistotal, "two.sided")
generalTests(geoExData$Sc2_1_manual, geoExData$Sc2_1_analysistotal, "two.sided")
generalTests(geoExData$Sc2_2_manual, geoExData$Sc2_2_analysistotal, "two.sided")
generalTests(geoExData$Sc2_3_manual, geoExData$Sc2_3_analysistotal, "two.sided")

###############################################################################
#                                                                             #
#                           FUNCTIONS                                         #
#                                                                             #
###############################################################################

mcnemarTestCone <- function(school, semester, manual, test)
{
  ## categorizing
  test[test=="TRUE"] <- "1"
  test[test=="FALSE"] <- "0"
  brokenCone <- test
  autoVsManual <- revalue(manual, c("TRUE"="1", "FALSE"="0"))
  ##framing
  autoManual_test <- data.frame(autoVsManual, brokenCone)
  ##removing NA
  autoManual_test<-autoManual_test[-which(is.na(autoManual_test$autoVsManual)),]
  ##removing None
  autoManual_test <- subset(autoManual_test, autoVsManual != "None")
  ##taking care of the annoying factors
  autoManual_test[] <- lapply(autoManual_test, function(x) if(is.factor(x)) factor(x) else x)
  ##test
  contingency = table(autoManual_test$autoVsManual, autoManual_test$brokenCone) 
  chisq.test(contingency)
  mcnemar.test(contingency, correct = FALSE)
}

mcnemarTestSoilType <- function(school, semester, manual, test)
{
  "framing"
  autoManual_soilCorrect = data.frame(autoVsManual, test)
  #categorizing
  autoManual_soilCorrect$autoVsManual <- revalue(manual,
                                                 c("TRUE"="1", "FALSE"="0"))
  autoManual_soilCorrect$test <- ifelse(test == "TRUE", 1, 0)
  #removing NA
  autoManual_soilCorrect<-autoManual_soilCorrect[-which(is.na(autoManual_soilCorrect$autoVsManual)),]
  #removing None
  autoManual_soilCorrect <- subset(autoManual_soilCorrect, autoVsManual != "None")
  #taking care of annoying factors
  autoManual_soilCorrect[] <- lapply(autoManual_soilCorrect, function(x) if(is.factor(x)) factor(x) else x)
  #test
  contingency = table(autoManual_soilCorrect$autoVsManual, autoManual_soilCorrect$test) 
  chisq.test(contingency)
  mcnemar.test(contingency)
}

generalTests <- function(manual, varToTest, alt)
{
  dataGivenManual <- data.frame(manual, varToTest)
  dataGivenManual <- subset(dataGivenManual, manual != "None")
  dataGivenManual <- dataGivenManual[dataGivenManual$varToTest != "None",]
  auto <- dataGivenManual[dataGivenManual$manual == "TRUE",]
  manual <- dataGivenManual[dataGivenManual$manual == "FALSE",]
  
  auto[] <- lapply(auto, function(x) if(is.factor(x)) factor(x) else x)
  manual[] <- lapply(manual, function(x) if(is.factor(x)) factor(x) else x)
  autoVector <- as.numeric(as.matrix(auto$varToTest))
  manualVector <- as.numeric(as.matrix(manual$varToTest))
  t.test(autoVector, manualVector, alternative = alt)
}
