# POVERTY DATASET REGRESSION:
rm(list=ls())
data<- read.csv(file.path("final_povertyeduc.csv"), header=TRUE)

options(scipen=999)
str(data)

# Defining the Data
gdp_2013 <- data$gdp_2013
educ_index_2013 <- data$educ_index_2013
pov_2013 <- data$pov_2013
povhc_pct_y2 <- data$povhc_pct_y2
numpoor_y2 <- data$numpoor_y2
educ_index_2000 <- data$educ_index_2000
gdp_2000 <- data$gdp_2000


#Poverty Regions: Basic Regression with outliers:
reg1 <- lm(gdp_2013~educ_index_2013)
summary(reg1)

# Result: 14 Billion increase in gdp when education increases 1 unit, 0.07 significance

# Get rid of outliers
summary(gdp_2013)
boxplot(gdp_2013)
# 3rd - 1st Quartiles
iqr_gdp13 = 155250 - 6797 
upf_gdp13 = 155250 + 1.5*iqr_gdp13
data_clean = subset(data, gdp_2013 <= 377929.5)
summary(data_clean$gdp_2013)
boxplot(data_clean$gdp_2013)

#Poverty Regions: Outliers Regression
main <- lm(data_clean$gdp_2013~data_clean$educ_index_2013)
summary(main)

# 2 Billion increase in gdp when education increases by 1 unit, 0.001 significance
# More Plots/etc.
#coef(main)
#plot(data_clean$gdp_2013)
#hist(data_clean$gdp_2013)
#plot(data_clean$educ_index_2013, data_clean$gdp_2013)

#Poverty Regions: Log regression on Poverty Dataset:
log <- lm(log(data_clean$gdp_2013) ~ data_clean$educ_index_2013)
summary(log)
hist(data_clean$gdp_2013)
hist(log(data_clean$gdp_2013))

# Results of Log: 2.2% increase in GDP for each unit increase in the education index, at 0.08 significance

#########################################################################################################
# Full Dataset Regression:
rm(list=ls())
data<- read.csv(file.path("final_maineduc.csv"), header=TRUE)


options(scipen=999)
str(data)

# Defining the Data
gdp_2013 <- data$gdp_2013
educ_index_2013 <- data$educ_index_2013
pov_2013 <- data$pov_2013
povhc_pct_y2 <- data$povhc_pct_y2
numpoor_y2 <- data$numpoor_y2
educ_index_2000 <- data$educ_index_2000
gdp_2000 <- data$gdp_2000


# Basic Regression with outliers:
reg1 <- lm(gdp_2013~educ_index_2013)
summary(reg1)

# Result: 22 Billion increase in gdp when education increases 1 unit, 0.0029 significance

# Get rid of outliers
summary(gdp_2013)
boxplot(gdp_2013)
# 3rd - 1st Quartiles
iqr_gdp13 = 234648 - 12011
upf_gdp13 = 234648 + 1.5*iqr_gdp13
data_clean = subset(data, gdp_2013 <= 568603.5)
summary(data_clean$gdp_2013)
boxplot(data_clean$gdp_2013)

# Outliers Regression
main <- lm(data_clean$gdp_2013~data_clean$educ_index_2013)
summary(main)

# 1.9 Billion increase in gdp when education increases by 1 unit, 0.00000696 significance

# Log regression on Poverty Dataset:
log <- lm(log(data_clean$gdp_2013) ~ data_clean$educ_index_2013)
summary(log)
hist(data_clean$gdp_2013)
hist(log(data_clean$gdp_2013))


# Results of Log: 3.2% increase in gdp for each 1 unit increase in the education index, 0.0004 significance

# WOOOOOHOOOOO!!!!