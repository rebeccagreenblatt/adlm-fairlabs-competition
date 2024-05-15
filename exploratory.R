library(dplyr)
library(ggplot2)
#devtools::install_github("ropensci/plotly")
library(plotly)
library(DT)
source("make_funnelplot.R")
library(shiny)

dat <- read.csv("fairlabs_data.csv")
head(dat)

# get number of substances mother screened positive for 
dat$num_pos <- rep(NA, nrow(dat))
for(i in 1:nrow(dat)){
  dat$num_pos[i] <- sum(dat[i,9:54], na.rm = TRUE)
}

# view number of screens positive for each substances
colSums(dat[,9:54], na.rm = TRUE)

ds <- dat %>% 
  select(mother_id, maternal_race, maternal_age, 
         uds_collection_date, detected_tetrahydrocannabinol,
         num_pos, cps_reporting_date)
nrow(ds) ## 6643
length(unique(dat$mother_id)) #6528 

table(ds$uds_collection_date == "")
ds$tested <- ifelse(ds$uds_collection_date == "", FALSE, TRUE)

ds %>% group_by(maternal_race) %>%
  summarise(pct_tested = sum(tested)/n(), count = n()) %>%
  arrange(desc(count))

# Filter out "other" and mixed race categories to start; while these are important, sample size is smaller and it will be less straightforward to assess for implicit bias 
dsf <- filter(ds, maternal_race %in% c("White", "Black or African American", "Asian", "Other Pacific Islander", "American Indian or Alaska Native")) %>%
  #select one row per mother, but if they were ever screened, select that row 
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
### highly correlated with maternal age

model <- glm(data = dsf, tested ~ maternal_race + maternal_age + maternal_race*maternal_age)
summary(model)
# Nothing significant 

## race and age 
tapply(dsf$maternal_age, dsf$maternal_race,
       function(x) format(summary(x)))
# $`American Indian or Alaska Native`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "20.00" "24.00" "27.00" "27.48" "30.00" "37.00" 
# $Asian
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "17.00" "30.00" "33.00" "32.27" "35.00" "47.00" 
# $`Black or African American`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "12.00" "23.00" "27.00" "27.59" "32.00" "49.00" 
# $`Other Pacific Islander`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "18.00" "24.00" "30.00" "29.64" "34.50" "43.00" 
# $White
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# "14.00" "26.00" "30.00" "30.03" "34.00" "47.00" 

#age may be used as proxy for race if included

dsf %>% group_by(maternal_age) %>%
  summarise(p_tested = sum(tested)/n()) %>%
  filter(maternal_age > 15 & maternal_age < 40) %>%
  ggplot(data = ., aes(x = maternal_age, y = p_tested)) + geom_line()

dsf$age_cat <- cut(dsf$maternal_age, breaks = c(12,19,24,29,34,39,44,49), include.lowest = TRUE)

dsf %>% group_by(age_cat) %>%
  summarise(p_tested = sum(tested)/n(), count = n()) 

dsfbyrace <- dsf %>% group_by(maternal_race) %>%
  summarise(total = n(), 
            num_tested = sum(tested), 
            pos_test = sum(num_pos > 0),
            thc_only_pos = sum(detected_tetrahydrocannabinol == 1 & num_pos == 1, na.rm = TRUE), 
            reported_for_thc_only = sum(detected_tetrahydrocannabinol == 1 & num_pos == 1 & cps_reporting_date != "", na.rm = TRUE))
colnames(dsfbyrace) <- c("Maternal Race/Ethnicity", "Total", "Number Tested", "Positive Test", "Positive for THC only", "Reported to CPS for THC only")

write.csv(dsfbyrace, "uds_dat.csv", row.names = FALSE)

make_funnelplot(dsfbyrace)





