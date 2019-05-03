#############################################################################################
# clean_data.R
# By : Rudi Kruger
# Cleans the data for the "Pump it Up:Data Mining the Water Table" competition 
# on drivendata.org
# This includes dropping some features, reducing the number of levels for a few factors
# and creating 2 new derived features
# Do whatever you want with this
#############################################################################################

train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Date recorded - remove, 
#but add 2 derived features : # of days since Jan 1 2014, month recorded as factor
#############################################################################################
date_recorded_offset_days <- as.numeric(as.Date("2014-01-01") - as.Date(train$date_recorded))
date_recorded_month <- factor(format(as.Date(train$date_recorded), "%b"))
train <- train[, -which(names(train) == "date_recorded")]
train <- cbind(train, date_recorded_offset_days)
train <- cbind(train, date_recorded_month)

date_recorded_offset_days <- as.numeric(as.Date("2014-01-01") - as.Date(test$date_recorded))
date_recorded_month <- factor(format(as.Date(test$date_recorded), "%b"))
test <- test[, -which(names(test) == "date_recorded")]
test <- cbind(test, date_recorded_offset_days)
test <- cbind(test, date_recorded_month)
#############################################################################################

#Funder - reduce factor levels
NUM_LEVELS_FUNDER = 10 #Funder will have this many + 1 levels
#############################################################################################
funderNames <- names(summary(train$funder)[1:NUM_LEVELS_FUNDER])
funder <- factor(train$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
train$funder <- funder

funder <- factor(test$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
test$funder <- funder
#############################################################################################

#Installer - reduce factor levels
NUM_LEVELS_INSTALLER = 10 #Installer will have this many + 1 levels
#############################################################################################
installerNames <- names(summary(train$installer)[1:NUM_LEVELS_INSTALLER])
installer <- factor(train$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
train$installer <- installer

installer <- factor(test$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
test$installer <- installer
#############################################################################################

#wpt_name - remove. Too many levels
train <- train[, -which(names(train) == "wpt_name")]
test <- test[, -which(names(test) == "wpt_name")]

#num_private - remove. No idea what this is
train <- train[, -which(names(train) == "num_private")]
test <- test[, -which(names(test) == "num_private")]

#subvillage - remove. Too many levels
train <- train[, -which(names(train) == "subvillage")]
test <- test[, -which(names(test) == "subvillage")]

#region_code - remove, looks like a proxy for region
train <- train[, -which(names(train) == "region_code")]
test <- test[, -which(names(test) == "region_code")]

#district_code - remove, may also be a proxy for region. Not sure.
train <- train[, -which(names(train) == "district_code")]
test <- test[, -which(names(test) == "district_code")]

#lga - remove, may also be a proxy for region.
train <- train[, -which(names(train) == "lga")]
test <- test[, -which(names(test) == "lga")]

#ward - remove. Too many levels
train <- train[, -which(names(train) == "ward")]
test <- test[, -which(names(test) == "ward")]

#recorded_by - remove. Constant
train <- train[, -which(names(train) == "recorded_by")]
test <- test[, -which(names(test) == "recorded_by")]

#scheme_name - remove. Too many levels
train <- train[, -which(names(train) == "scheme_name")]
test <- test[, -which(names(test) == "scheme_name")]

#scheme_management - Change level "None"(Not present in test) to "" 
train$scheme_management[train$scheme_management=="None"] <- ""
train$scheme_management <- factor(as.character(train$scheme_management))


#Construction year - turn into factor, reduce factor levels
NUM_LEVELS_CONSTRUCTION_YEAR = 20 #construction_year will have this many + 1 levels
#############################################################################################
train$construction_year <- factor(paste0("y",as.character(train$construction_year)))
cyears <- names(summary(train$construction_year)[order(-summary(train$construction_year))][1:NUM_LEVELS_CONSTRUCTION_YEAR])
cy <- factor(train$construction_year, levels=c(cyears, "Other"))
cy[is.na(cy)] <- "Other"
train$construction_year <- cy

test$construction_year <- factor(paste0("y",as.character(test$construction_year)))
cy <- factor(test$construction_year, levels=c(cyears, "Other"))
cy[is.na(cy)] <- "Other"
test$construction_year <- cy
#############################################################################################

#extraction_type - Change level 
#"other - mkulima/shinyanga"(Not present in test) to "Other"
train$extraction_type[train$extraction_type=="other - mkulima/shinyanga"] <- "other"
train$extraction_type <- factor(as.character(train$extraction_type))

write.csv(train, "myTrain.csv", row.names=FALSE)
write.csv(test, "myTest.csv", row.names=FALSE)
