library(dplyr)
library(ggplot2)
#devtools::install_github("ropensci/plotly")
library(plotly)
#install.packages("datamods")
library(datamods)
source("make_funnelplot.R")

dat <- read.csv("fairlabs_data.csv")
head(dat)

ds <- dat %>% select(mother_id, maternal_race, maternal_age, uds_collection_date, detected_tetrahydrocannabinol, cps_reporting_date)
dim(ds)
head(ds)
length(unique(dat$mother_id)) #6528

table(ds$uds_collection_date == "")
ds$tested <- ifelse(ds$uds_collection_date == "", FALSE, TRUE)

ds %>% group_by(maternal_race) %>%
  summarise(pct_tested = sum(tested)/n(), count = n()) %>%
  arrange(desc(count))

dsf <- filter(ds, maternal_race %in% c("White", "Black or African American", "Asian", "Other Pacific Islander", "American Indian or Alaska Native")) %>%
  arrange(desc(tested)) %>%
  group_by(mother_id) %>%
  filter(row_number() == 1) %>%
  filter(maternal_age < 50)

model <- glm(data = dsf, tested ~ maternal_race + maternal_age)
summary(model)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                             0.1680939  0.0565256   2.974  0.00295 ** 
# maternal_raceAsian                     -0.0639663  0.0566950  -1.128  0.25926    
# maternal_raceBlack or African American  0.0667733  0.0540058   1.236  0.21635    
# maternal_raceOther Pacific Islander    -0.0170488  0.0670680  -0.254  0.79935    
# maternal_raceWhite                     -0.0089774  0.0540109  -0.166  0.86799    
# maternal_age                           -0.0028083  0.0006424  -4.372 1.25e-05 ***
### highly dependent on maternal age, must take this into account 

## need to test for colinearity between race and age
tapply(dsf$maternal_age, dsf$maternal_race,
       function(x) format(summary(x)))
# $`American Indian or Alaska Native`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "20.00" "24.00" "27.00" "27.48" "30.00" "37.00" 
# 
# $Asian
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "17.00" "30.00" "33.00" "32.27" "35.00" "47.00" 
# 
# $`Black or African American`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "12.00" "23.00" "27.00" "27.59" "32.00" "49.00" 
# 
# $`Other Pacific Islander`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "18.00" "24.00" "30.00" "29.64" "34.50" "43.00" 
# 
# $White
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "14.00" "26.00" "30.00" "30.03" "34.00" "47.00" 

dsf %>% group_by(maternal_age) %>%
  summarise(pos = sum(tested)/n()) %>%
  filter(maternal_age > 15 & maternal_age < 45) %>%
  ggplot(data = ., aes(x = maternal_age, y = pos)) + geom_line()

dsf$age_cat <- cut(dsf$maternal_age, breaks = c(12,19,24,29,34,39,49), include.lowest = TRUE)

dsfbyrace <- dsf %>% group_by(maternal_race) %>%
  summarise(total = n(), 
            num_tested = sum(tested), 
            thc_pos = sum(detected_tetrahydrocannabinol == 1, na.rm = TRUE), 
            reported = sum(cps_reporting_date != ""), 
            reported_for_thc = sum(detected_tetrahydrocannabinol == 1 & cps_reporting_date != "", na.rm = TRUE))

#https://dreamrs.github.io/datamods/reference/edit-data.html

make_funnelplot(dsfbyrace[,c(1:4,6)])
