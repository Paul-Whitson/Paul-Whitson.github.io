#DEPA Final Project
#Data cleaning


#Initial data cleanup of cancer rate data from wonder.cdc database
#Data represents totals for 2012-2016
#Note:  data is pipe-delimited text file.
#Data represent total cancer cases (both incidence and mortality) for the 5-year period 2012-2016

#Raw data is a text file.
#Read in data.  Replace blank rows with NA
cancer_data_raw <- read.table(file = "BYAREA_COUNTY.TXT", header = TRUE, sep = "|", 
                              na.strings = c("", "~", "+", ".", "-"))
#raw data has about 2.73 MM rows

#create working copy
cancer_data <-cancer_data_raw

#remove unwanted (empty) columns:
cancer_data$AGE_ADJUSTED_CI_LOWER <- NULL
cancer_data$AGE_ADJUSTED_CI_UPPER <- NULL
cancer_data$CRUDE_CI_LOWER <- NULL
cancer_data$CRUDE_CI_UPPER <- NULL
cancer_data$YEAR <- NULL
str(cancer_data)

#remove all rows with NAs in COUNT, AGE_ADJUSTED_RATE, and CRUDE_RATE
cancer_data <- cancer_data[!is.na(cancer_data$AGE_ADJUSTED_RATE) & !is.na(cancer_data$COUNT) &
                             !is.na(cancer_data$CRUDE_RATE),]
sum(cancer_data$COUNT) #is 84,833,021 (but note this includes subtotals and redundancy)


#Consolidate some cancer types (column "SITE") for easier analysis:

#view current levels of "SITE:"
levels(cancer_data$SITE)

#Code below maps existing levels to new levels.  In some cases, multiple old levels are mapped to 
#the same new level ("All Other").
new_levels<- c("All Cancer Sites Combined", "All Other",
                  "All Other", "Colorectal",
                  "All Other", "All Other",
                  "Breast", "Breast",
                  "All Other","All Other",
                  "Kidney","All Other",
                  "Leukemias", "Liver",
                  "Lung", "Breast",
                  "Breast", "Breast",
                  "Melanoma", "All Other",
                  "All Other","Non-Hodgkin Lymphoma",
                  "Mouth","All Other",
                  "Pancreas", "Prostate",
                  "Stomach","All Other",
                  "Thyroid","Bladder")
print(new_levels)
levels(cancer_data$SITE) <- new_levels

#Check that the new mapping worked:
summary(cancer_data$SITE)
levels(cancer_data$SITE)

#aggregate data - sum all values that have same label for "SITE"
cancer_data <- aggregate(cbind(AGE_ADJUSTED_RATE,COUNT,POPULATION,CRUDE_RATE) ~
                              STATE+AREA+EVENT_TYPE+RACE+SEX+SITE, 
                            data = cancer_data, FUN = sum)

summary(cancer_data$SITE)
sum(cancer_data$COUNT) #is still 84.833 MM, so we haven't lost any data

#*************************************************************************************************
#Create dim table to normalize SITE:

levels(cancer_data$SITE)
dim_cancer_site <- data.frame(cbind(levels(cancer_data$SITE), 1:16))
names(dim_cancer_site)<- c("CANCER_SITE", "CANCER_SITE_CODE")
print(dim_cancer_site)

#create vector of CANCER_SITE_CODE to use in the main data table:
CANCER_SITE_CODE <- vector(mode = "numeric", length = length(cancer_data$SITE))

#Replace cancer site designation with numerical code:
for (i in 1:length(CANCER_SITE_CODE)) {
  CANCER_SITE_CODE[i] <- dim_cancer_site[grep(cancer_data$SITE[i], dim_cancer_site[,1], value = FALSE),2]
}
CANCER_SITE_CODE <- as.factor(CANCER_SITE_CODE)
head(CANCER_SITE_CODE)
summary(CANCER_SITE_CODE)

#*************************************************************************************************
#Cleaning Up State and County Designations:
#(to facilitate joins in SQL)
 
install.packages('maps')
library(maps)
str(state.fips)
str(county.fips)

#cancer_data$STATE already contains the 2-character state abbreviation

#County codes (FIPS) are buried in a text string along with the name of the county.
#Extract county name into a separate character vector

#Extract county name (text) into a separate vector:
COUNTY_NAME <- vector(mode = "character", length = length(cancer_data$AREA))
for(i in 1:length(cancer_data$AREA)) {
  COUNTY_NAME[i]<- substr(cancer_data$AREA[i], 
                          start = regexpr(" ", cancer_data$AREA[i], fixed = TRUE)[1]+1, 
                          stop = regexpr(" (", cancer_data$AREA[i], fixed = TRUE)[1]-1)
}
head(COUNTY_NAME)
COUNTY_NAME <- as.factor(COUNTY_NAME)
length(COUNTY_NAME)

#Extract 5-digit FIPS code for county into a separate character vector
COUNTY_CODE <- vector(mode = "character", length = length(cancer_data$AREA))
for(i in 1:length(cancer_data$AREA)) {
  COUNTY_CODE[i]<- substr(cancer_data$AREA[i], 
                          start = regexpr("(", cancer_data$AREA[i], fixed = TRUE)[1]+1, 
                          stop = regexpr(")", cancer_data$AREA[i], fixed = TRUE)[1]-1)
}
COUNTY_CODE <- as.factor(COUNTY_CODE)
length(COUNTY_CODE)


#Create vector of FIPS codes for each STATE:


STATE_CODE = vector(mode = "character", length = length(fact_cancer$AREA))
STATE_CODE = substr(COUNTY_CODE, 1, 2)
head(STATE_CODE)
summary(as.numeric(as.character(STATE_CODE)))

#*****************************************************
#Save "cancer_data" table as-is in case it is necessary to go back to it:  
#rename to fact_cancer for further processing.

fact_cancer <- cancer_data

#create empty columns for SQL auto-increment ID and for Timestamp:

AUTO_ID <- vector(mode = "integer", length = length(cancer_data$STATE))
LAST_UPDATED <- vector(mode = "integer", length = length(cancer_data$STATE))


#In the following files, I am removing "AREA", as it is a large text field, and because this field is 
#not needed as long as the STATE FIPS and COUNTY FIPS are correct.


#**************************************************************************************************
#Normalization of Primary Fact Table:

#Convert SITE to CANCER_SITE_CODE:
levels(fact_cancer$SITE) <- dim_cancer_site$CANCER_SITE_CODE
names(fact_cancer)[names(fact_cancer)=="SITE"] <- "CANCER_SITE_CODE"

#Normalize factors RACE and SEX, and create corresponding dim tables:

#Create table assigning each level of "RACE" to a numerical value:
dim_race <- data.frame(cbind(RACE_CODE = c(1:6), RACE_DESCRIPTION = c(levels(cancer_data$RACE))))
print(dim_race)

#Convert the data in the fact_cancer table to use these codes:
levels(fact_cancer$RACE) <- dim_race$RACE_CODE
names(fact_cancer)[names(fact_cancer)=="RACE"] <- "RACE_CODE"

#Create table assigning each level of "SEX" to a numerical value:
dim_gender <- data.frame(cbind(GENDER_CODE = c(1:3), GENDER_DESCRIP = c(levels(cancer_data$SEX))))
print(dim_gender)
#convert the data in the original cancer_data table to use these numerical values:
levels(fact_cancer$SEX) <- dim_gender$GENDER_CODE
names(fact_cancer)[names(fact_cancer)=="SEX"] <- "GENDER_CODE"

#Create table assigning each level of "EVENT_TYPE" to a numerical value:
dim_event_type <- data.frame(cbind(EVENT_TYPE_CODE = c(0,1), EVENT_TYPE_DESCRIP = c(levels(cancer_data$EVENT_TYPE))))
print(dim_event_type)
#convert the data in the original cancer_data table to use these numerical values:
levels(fact_cancer$EVENT_TYPE) <- dim_event_type$EVENT_TYPE_CODE
names(fact_cancer)[names(fact_cancer)=="EVENT_TYPE"] <- "EVENT_TYPE_CODE"
str(fact_cancer)


#Save backup version of data frame:
fact_cancer_saved <- fact_cancer
fact_cancer <- fact_cancer_saved



#Save all columns to individual data vectors

#Already created:  
#AUTO_ID
#LAST_UPDATED
#STATE_CODE
#COUNTY_CODE

#Cast each vector to the appropriate data type:
STATE_ABBREV <- as.character(fact_cancer$STATE)
GENDER_CODE <- as.numeric(fact_cancer$GENDER_CODE)
RACE_CODE <- as.numeric(fact_cancer$RACE_CODE)
CANCER_SITE_CODE<- as.numeric(fact_cancer$CANCER_SITE_CODE)
EVENT_TYPE_CODE <- as.character(fact_cancer$EVENT_TYPE_CODE)
POPULATION <-as.numeric(fact_cancer$POPULATION)
COUNT <- as.numeric(fact_cancer$COUNT)
CRUDE_RATE <- as.numeric(fact_cancer$CRUDE_RATE)
AGE_ADJUSTED_RATE <- as.numeric(fact_cancer$AGE_ADJUSTED_RATE)

#Bind vectors together into final data frame:
fact_cancer_v2 <- data.frame(cbind(
                  AUTO_ID, 
                  LAST_UPDATED,
                  STATE_CODE, 
                  COUNTY_CODE,
                  STATE_ABBREV,
                  COUNTY_NAME,
                  RACE_CODE, 
                  GENDER_CODE, 
                  POPULATION,
                  CANCER_SITE_CODE,
                  EVENT_TYPE_CODE,
                  COUNT,
                  CRUDE_RATE,
                  AGE_ADJUSTED_RATE),
                  stringsAsFactors = FALSE)

#Convert COUNTY_NAME from factor back to character.
fact_cancer_v2$COUNTY_NAME <- as.character(COUNTY_NAME)
#COUNTY_CODE is incorrect; it read the numeric values of the factors rather than the literal
#values of the strings.  Will coerce to correct values:
fact_cancer_v2$COUNTY_CODE <- as.numeric(as.character(COUNTY_CODE))

#Inspect data types:
summary(fact_cancer_v2$COUNTY_CODE)
  
#Check some sums / totals:
table(fact_cancer_v2$EVENT_TYPE_CODE)
sum(as.numeric(fact_cancer_v2$COUNT))

#Remove headers and write to CSV for import into GCP Cloud SQL instance:
temp.fact_cancer_v2 <- fact_cancer_v2
sum(as.numeric(temp.fact_cancer_v2[,12]))
names(temp.fact_cancer_v2) <- NULL
write.csv(temp.fact_cancer_v2, "fact_cancer_2.csv", row.names = FALSE)


#***********************************************
#Importing U.S. Census Data (income, employment, poverty, health insurance)
#18-Nov-2019

#Imported data from U.S. Census site.  Data comes in a .zip.json format.

#unzip file
unzip("ACSDP5Y2017.DP03_2019-11-18T202428.zip.json")

#File consists of a header row (alphanumeric IDs for each data field), followed
#by a text row with a data description for each column.  Actual data starts in row 3.
#For this reason, read the data with a two-step process: read the headers into their own file,
#then read the body of the data into a separate file.  

#read CSV file:
census_data_headers <- read.csv("ACSDP5Y2017.DP03_data_with_overlays_2019-11-18T202217.csv", 
                                header = TRUE, nrows = 1)
census_data_body <- read.csv("ACSDP5Y2017.DP03_data_with_overlays_2019-11-18T202217.csv", 
                             header = FALSE, skip = 2)

#File has 550 columns; consulted data dictionary (separate downloaded file) 
#to determine which columns to extract

names(census_data_headers)[1]
names(census_data_headers)[2]
names(census_data_headers)[2 + 4*5 - 1]
names(census_data_headers)[2 + 4*62 - 1 - 2]
names(census_data_headers)[2 + 4*63 - 1 - 2]
names(census_data_headers)[2 + 4*96 - 1]
names(census_data_headers)[2 + 4*97 - 1]
names(census_data_headers)[2 + 4*98 - 1]
names(census_data_headers)[2 + 4*128 - 1]

#Create new data frame that will contain only those columns we are including in our analysis:
census_data <- data.frame(cbind(census_data_body[,c(
  1,2,4*5+1, 4*62-1, 4*63-1, 4*96+1, 4*97+1, 4*98+1, 4*128+1)]))
names(census_data)<- c("Geo_id", "County_Name", "Pct_Unemployment", 
                       "Median_HH_Income", "Mean_HH_Income",
                       "Pct_w_Health_Ins", "Pct_Pvt_Ins", "Pct_Pub_Ins", "Pct_Below_Poverty_Line")
head(census_data)

#The "Geo_id" appears to be the County FIPS, but it needs to be re-formatted:
#Extract factor levels to separate variable for cleanup:
census_geo_id_levels <- levels(census_data$Geo_id)
head(census_geo_id_levels)
#Create new vector containing only the last 5 characters (the County FIPS)
census_county_fips <- substr(census_geo_id_levels,
                             start = nchar(as.character(census_geo_id_levels)) - 4,
                             stop = nchar(as.character(census_geo_id_levels)))

#Replace levels of original variable with the numeric FIPS values:
levels(census_data$Geo_id) <- census_county_fips
colnames(census_data)[colnames(census_data)=="Geo_id"] <- "COUNTY_CODE"
#re-title "census_data" to "dim_census_data":
dim_census_data <- census_data

#add column for auto-increment
AUTO_ID <- vector(mode = "numeric", length = length(dim_census_data$COUNTY_CODE))

dim_census_data <- data.frame(cbind(AUTO_ID, dim_census_data))



#************************************************************************************
#WRITING DATA TO FILES AND EXPORTING TO MYSQL:

#Primary Fact Table
#Note that this data includes subtotals ("All Races," "All Cancer Types Combined," etc.)
temp.fact_cancer <- fact_cancer
names(temp.fact_cancer) <- NULL
write.csv(temp.fact_cancer, "fact_cancer.csv", row.names = FALSE)

#Dimensional tables in support of primary fact table:
temp.dim_cancer_site <- dim_cancer_site
names(temp.dim_cancer_site) <- NULL
write.csv(temp.dim_cancer_site, "dim_cancer_site.csv",  row.names = FALSE)

temp.dim_census_data <- dim_census_data
names(temp.dim_census_data) <- NULL
write.csv(temp.dim_census_data, "dim_census_data.csv",  row.names = FALSE)

temp.dim_event_type <- dim_event_type
names(temp.dim_event_type) <- NULL
write.csv(temp.dim_event_type, "dim_event_type.csv",  row.names = FALSE)

temp.dim_gender <- dim_gender
names(temp.dim_gender) <- NULL
write.csv(temp.dim_gender, "dim_gender.csv",  row.names = FALSE)

temp.dim_race <- dim_race
names(temp.dim_race) <- NULL
write.csv(temp.dim_race, "dim_race.csv", row.names = FALSE)
