#DEPA Final Project:  Data Analysis
#Paul Whitson

#Install and initialize packages for pulling data and plotting
install.packages("RMySQL");
install.packages("ggplot2");

library(RMySQL);
library(ggplot2);


#Set up connection to Google Cloud SQL instance:

connection = dbConnect(MySQL(),user="root", password="", 
                       dbname="cancer", host="35.239.204.231");

#ANALYSIS 1:  County-Level Data:
#join data from multiple tables into one master data table for analysis:

myQuery <- "SELECT 
	fc.STATE_CODE, 
	fc.COUNTY_CODE, 
	fc.STATE_ABBREV, 
	fc.COUNTY_NAME, 
	dr.RACE_DESCRIPTION,
	dg.GENDER_DESCRIP, 
	fc.POPULATION, 
	dcs.CANCER_SITE, 
	det.EVENT_TYPE_DESCRIP,
	fc.COUNT,
	fc.CRUDE_RATE,
	fc.AGE_ADJUSTED_RATE,
	dc.PCT_UNEMPLOYMENT,
	dc.MEDIAN_HH_INCOME,
	dc.PCT_W_HEALTH_INS,
	dc.PCT_BELOW_POVERTY_LINE,
	uv.uv_wh_m2

FROM
	`fact_cancer` fc, `dim_race` dr, `dim_gender` dg, `dim_event_type` det, `dim_cancer_site` dcs, 
    `dim_census` dc, `fact_uv_exposure_county` uv

WHERE 
#denormalizing fact_cancer table for easier analysis:
	fc.`RACE_CODE` = dr.`RACE_CODE`
	AND fc.`GENDER_CODE` = dg.`GENDER_CODE`
	AND fc.`EVENT_TYPE_CODE` = det.`EVENT_TYPE_CODE`
	AND fc.`CANCER_SITE_CODE` = dcs.`CANCER_SITE_CODE`
#connecting to dim_census
	AND fc.`COUNTY_CODE` = dc.`COUNTY_CODE`
#connecting to fact_uv_exposure_county
	AND fc.`COUNTY_CODE` = uv.`county_code`;"

master_data <- as.data.frame(dbGetQuery(connection, myQuery))

#Rename columns and convert selected columns to factors for improved performance:
master_data$COUNTY_NAME <- as.factor(master_data$COUNTY_NAME)
master_data$STATE_ABBREV <- as.factor(master_data$STATE_ABBREV)
master_data$CANCER_SITE <- as.factor(master_data$CANCER_SITE)
master_data$EVENT_TYPE_DESCRIP <- as.factor(master_data$EVENT_TYPE_DESCRIP)
master_data$RACE_DESCRIPTION<- as.factor(master_data$RACE_DESCRIPTION)
master_data$GENDER_DESCRIP<- as.factor(master_data$GENDER_DESCRIP)

colnames(master_data)[5]<- "RACE"
colnames(master_data)[6]<- "GENDER"
colnames(master_data)[9]<- "EVENT_TYPE"

#Write modified data frame to text file for analysis in Tableau:
write.csv(master_data, file = "DEPA_Master_Data_Table.csv")

#Fit linear (ANOVA) model to determine relationship of race and gender with cancer rates:
mortality_all <- lm(AGE_ADJUSTED_RATE~RACE+GENDER, 
                      data = master_data,
                   subset = as.vector((CANCER_SITE == "All Cancer Sites Combined") &
                    (EVENT_TYPE == "Mortality") &
                    (RACE != "All Races") & 
                    (GENDER != "Male and Female")))

summary(mortality_all)

#Graphical exploration of data:
boxplot(AGE_ADJUSTED_RATE~GENDER+RACE,
        data = master_data[(master_data$CANCER_SITE == "All Cancer Sites Combined") &
                             (master_data$EVENT_TYPE == "Mortality") &
                             (master_data$RACE != "All Races") & 
                             (master_data$GENDER != "Male and Female"),])

#ANALYSIS 2:
#Table dim_health_index is aggregated at State level, not county level.
#Used the following SQL query to aggregate into statewide totals and pull in 
#state-level data on nutrition, smoking, binge drinking, etc.

myQuery2 <- "SELECT 
fc.`STATE_ABBREV`, 
dr.`RACE_DESCRIPTION` AS `RACE`, 
dg.`GENDER_DESCRIP` AS `GENDER`, 
det.`EVENT_TYPE_DESCRIP` as `EVENT_TYPE`, 
dcs.`CANCER_SITE`, 
sum(fc.`POPULATION`), 
sum(fc.`COUNT`), 
sum(fc.`COUNT`) / sum(fc.`POPULATION`) * 100000 AS `EST_CRUDE_RATE_1`,
sum(fc.`AGE_ADJUSTED_RATE`*fc.`POPULATION`)/sum(fc.`POPULATION`) AS `EST_AGE_ADJ_RATE`,
avg(dhi.`ADULT_SMOKING`), avg(dhi.`Adult_Physical_Activity`), avg(dhi.`Adult_Nutrition`), avg(dhi.`Adult_Binge_Drinking`)

FROM
`fact_cancer` fc, `dim_race` dr, `dim_gender` dg, `dim_event_type` det, `dim_cancer_site` dcs, `dim_health_index` dhi

WHERE fc.`RACE_CODE` = dr.`RACE_CODE`
AND fc.`GENDER_CODE` = dg.`GENDER_CODE`
AND fc.`EVENT_TYPE_CODE` = det.`EVENT_TYPE_CODE`
AND fc.`CANCER_SITE_CODE` = dcs.`CANCER_SITE_CODE`
AND fc.`STATE_CODE` = dhi.`STATE_CODE`

GROUP BY fc.`STATE_ABBREV`, `RACE`, `GENDER`, `EVENT_TYPE`, dcs.`CANCER_SITE`

ORDER BY fc.`STATE_ABBREV`, `RACE`, `GENDER`, `EVENT_TYPE`, dcs.`CANCER_SITE`;"

#Pull in data from GCP CloudSQL and put in a data frame
state_data <- as.data.frame(dbGetQuery(connection, myQuery2))

#Rename some columns for easier analysis:
colnames(state_data)[6]<- 'POPULATION'
colnames(state_data)[7]<- 'COUNT'
colnames(state_data)[8]<- 'CRUDE_RATE'
colnames(state_data)[9]<- 'AGE_ADJ_RATE'
colnames(state_data)[10]<- 'SMOKING'
colnames(state_data)[11]<- 'PHYSICAL_ACTIVITY'
colnames(state_data)[12]<- 'NUTRITION'
colnames(state_data)[13]<- 'BINGE_DRINKING'

#Fit linear regression model of cancer incidence rate vs. smoking, physical activity, 
#nutrition, and level of binge drinking in a state:
inc_all_types_1<- lm(AGE_ADJ_RATE~SMOKING+PHYSICAL_ACTIVITY+NUTRITION+BINGE_DRINKING,
                       data = state_data,
                       subset = as.vector(
                         state_data$EVENT_TYPE == "Incidence" &
                           state_data$CANCER_SITE == "All Cancer Sites Combined" &
                           state_data$GENDER == "Male and Female" &
                           state_data$RACE == "All Races") )
summary(inc_all_types_1)

#Similar linear model for mortality
mort_all_types_1 <- lm(AGE_ADJ_RATE~SMOKING+PHYSICAL_ACTIVITY+NUTRITION+BINGE_DRINKING,
                      data = state_data,
                      subset = as.vector(
                          state_data$EVENT_TYPE == "Mortality" &
                          state_data$CANCER_SITE == "All Cancer Sites Combined" &
                          state_data$GENDER == "Male and Female" &
                          state_data$RACE == "All Races") )
summary(mort_all_types_1)

#Analyze correlations with lung cancer specifically:

#Incidence:
lung_inc_1<- lm(AGE_ADJ_RATE~SMOKING+PHYSICAL_ACTIVITY+NUTRITION+BINGE_DRINKING,
                data = state_data,
                subset = as.vector(
                  state_data$EVENT_TYPE == "Incidence" &
                    state_data$CANCER_SITE == "Lung" &
                    state_data$GENDER == "Male and Female" &
                    state_data$RACE == "All Races") )
summary(lung_inc_1)

#Mortality:
lung_mort_1<- lm(AGE_ADJ_RATE~SMOKING+PHYSICAL_ACTIVITY+NUTRITION+BINGE_DRINKING,
                data = state_data,
                subset = as.vector(
                  state_data$EVENT_TYPE == "Mortality" &
                    state_data$CANCER_SITE == "Lung" &
                    state_data$GENDER == "Male and Female" &
                    state_data$RACE == "All Races") )
summary(lung_mort_1)

#analyses show smoking is most significant factor (positive correlation) 
#for both incidence and mortality, followed by level of physical activity (negative correlation)
#Level of binge drinking and nutrition did not show as significant predictors of lung cancer
#incidence or mortality.

#write data table to csv for analysis in tableau:
write.csv(state_data, "state-level_data.csv")
