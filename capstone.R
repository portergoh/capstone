#-----------------------------------------------------------------------#
# Capstone project for Data Analytics Specialist R Certification Program
#-----------------------------------------------------------------------#
# Tutor: Prof Roh Sungjong (SMU Academy)
# Student: Brian Sum 
# Student: Goh Khee Teck
# Date of completion:
#-----------------------------------------------------------------------#
# Instruction for running this R script:
#-----------------------------------------------------------------------#
# Please ensure that you have completed executing all the functions 
# from line 24 - 225 before running the main code
#
# You may choose to run only a portion of the main code to get the
# results of your analysis
#
# * Data are collected primarily from the site 
# - https://condo.singaporeexpats.com 
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
# Special Gift functions from Prof Roh
#-----------------------------------------------------------------------#
# for Statistical Inference using Regression
OLS_summary_function <- function(my_model_estimation, mydigit){
  mycoefs = tidy(my_model_estimation) %>% 
    mutate(
      est2=format(round(estimate, mydigit),mydigit),
      se2=format(round(std.error, mydigit),mydigit),
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      report=str_c(" ",est2,mystars,"\n(",se2,")",sep="")
    ) %>% select(term,report)
  myGOF=glance(my_model_estimation) %>% 
    mutate_all(
      funs(round(.,mydigit))
    ) %>% 
    mutate(
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      model_dfs = str_c("(",(df-1),", ",df.residual,")"),
      model_F = str_c(statistic,mystars),
      R2=format(round(r.squared,mydigit),mydigit),
      adjR2=format(round(adj.r.squared,mydigit),mydigit)
    ) %>% select(model_F, model_dfs, R2, adjR2) %>% 
    gather(key=term,value=report)
  mytable=bind_rows(mycoefs,myGOF) %>%
    mutate(sortid=row_number()) %>% 
    select(sortid,term,report) %>%
    mutate_at(vars(2:3),funs(as.character(.)))
  mytable
}

#-----------------------------------------------------------------------#
# Functions for Webscraping of the site
# https://condo.singaporeexpats.com
#-----------------------------------------------------------------------#
library(rvest)
library(tidyverse)
library(lubridate)

# Compute districts grouping between 3 main 
# regions -
# CCR: Core Central Region (D01,D02,D06,D09,D10,D11)
# RCR: Rest of Central Region (D03,D04,D05,D07,D08,D12,D13,D14,D15,D20)
# OCR: Outside Central Region (D16,D17,D18,D19,D21,D22...D28)

region_district <- function(){
  CCR <- str_pad(c(1,2,6,9:11),2, "left", pad="0")
  RCR <- str_pad(c(3:5,7,8,12:15,20),2,"left", pad="0")
  OCR <- str_pad(c(16:19,21:28),2, "left", pad="0")
  
  region <- c("CCR", "RCR", "OCR")
  district <- list( CCR, RCR, OCR )
  
  scaffold <- tibble(region, district) %>% 
    unnest()
  
  return(scaffold)
}

#-----------------------------------------------------------------------#
# Compute total number of pages for the webscraping site
#-----------------------------------------------------------------------#
# @param webpage: xml_document object from read_html
# @param records_per_page: total records in a page
# @return: total pages for the targeted site
#-----------------------------------------------------------------------#
compute_total_page <- function(webpage,records_per_page){
  total_page <- html_text(html_node(webpage,".pageno"))
  total_page <- ceiling(as.numeric(total_page)/records_per_page)
  
  return(total_page)
}

#-----------------------------------------------------------------------#
# Parse a string object to check if it contains the keyword
# 'Condominium'
#-----------------------------------------------------------------------#
# @param type: a string object
# @return: Return 'Condo' if found else 'NA'
#-----------------------------------------------------------------------#
check_property_type <- function(type) {
  if (str_detect(type,"Condominium"))
    return("Condo")
  else
    return("NA")
}

#-----------------------------------------------------------------------#
# Parse a xmlnode set object to extract out the fields 
# - top, condo name, district
# and also compute the age of the condo
#-----------------------------------------------------------------------#
# @param nodes: xmlnode set
# @return: A tibble containing condo name, district and age
#-----------------------------------------------------------------------#
parse_nodeContent <- function(nodes) {
  condo_name <- html_nodes(nodes,".title_link") %>% html_attr("title")
  district <- html_nodes(nodes,".listcol2") %>% 
    html_node("div") %>% 
    html_text() %>%
    str_extract("\\d+$") 
  
  top <- html_nodes(nodes,".listcol2") %>% 
    html_node(xpath="div[4]") %>%
    html_text() %>%
    str_extract("\\d+$") 
  
  condo_age <- map(top, ~(year(Sys.Date()) - as.numeric(.x))) %>% 
    unlist()
  
  return(tibble(condo_name,district,condo_age))
}

#-----------------------------------------------------------------------#
# Perform webscraping on url - 
# https://condo.singaporeexpats.com/%d/name/%s
# %d - page number
# %s - A-Z
#-----------------------------------------------------------------------#
# @param url: string object of the targeted url
# @return: A tibble containing condo name, district and age
#-----------------------------------------------------------------------#
scrape_webpage <- function(url) {
  records_per_page <- 50
  webpage <- read_html(url)
  total_page <- compute_total_page(webpage, records_per_page)
  new_url <- str_replace(url,"1","%d")
  dataset <- 1:total_page %>%
    sprintf(new_url,.) %>%
    map(~read_html(.x)) %>%
    map_df(~parse_nodeContent(.x)) 
  
  return (dataset)
}

#-----------------------------------------------------------------------#
# Perform webscraping on url - 
# https://condo.singaporeexpats.com/%d/singapore-property-all/rent
# %d - page number
#-----------------------------------------------------------------------#
# @param url: string object of the targeted url
# @return: A tibble containing name of the property, type, rent and date
# of listing
#-----------------------------------------------------------------------#
scrape_webpage2 <- function(url) {
  webpage<- url %>% read_html() 
  smlistdiv <-html_nodes(webpage,".smlistdiv") 
  
  smcol1 <-  smlistdiv %>%
    html_nodes(".smcol1")
  
  smcol2 <-  smlistdiv %>%
    html_nodes(".smcol2")
  
  name <- smlistdiv %>% 
    html_node(".smtitle") %>% 
    html_node("a") %>% 
    html_text() %>%
    str_extract("-\\s+.+$") %>%
    gsub("- ","",.)
  
  type <- smcol1 %>%
    html_node(xpath="div[2]") %>%
    html_text() %>%
    map(~check_property_type(.x)) %>%
    unlist()
  
  date <- smcol1 %>%
    html_nodes(".smcontact") %>%
    html_node(xpath="span[2]") %>%
    html_text() %>%
    str_extract("\\d+$")
  
  rent <- smcol2 %>%
    html_node(xpath="div[3]") %>%
    html_text() %>%
    str_extract("\\d+\\.*\\d*")
  
  return(tibble(name,type,rent,date))
}

#-----------------------------------------------------------------------#
# Check if input path is empty 
# @param file: image path
# @return True if file path is specified else false
#-----------------------------------------------------------------------#
not_empty <- function(file) {
   return(file !="")  
}

#-----------------------------------------------------------------------#
# Generate 4x residual plots to test for homoscedasticity
# @param fit: lm model
# @param save_to: full path of the image to save
#-----------------------------------------------------------------------#
fit_plot <- function(fit, save_to =""){
  if(not_empty(save_to)) png(save_to)
  par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
  par(mar=c(2.5,3,3,2.5))
  plot(fit)
  par(mfrow=c(1,1)) # Change back to 1 x 1
  if (not_empty(save_to)) dev.off()
}

#-----------------------------End of Functions--------------------------#

#-----------------------------------------------------------------------#
# Main program: 
# - Data Collection
# - Data Tidy & Transformation
# - Data Visualisation
# - Data Modeling
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
# Main program: Data Collection and Tidy 
#-----------------------------------------------------------------------#

# Webscrape site url -
# https://condo.singaporeexpats.com/1/name/0-9
# You may skip this to get the dataset from github instead
noStr <- "0-9"
url <- "https://condo.singaporeexpats.com/1/name/%s"
condo_dataset1 <- noStr %>%
  sprintf(url,.) %>% {
    Sys.sleep(2)  
    map_df(.,~scrape_webpage(.x))
  }

# Run this to get the dataset direct from github
dataset1_url <- "https://raw.githubusercontent.com/portergoh/capstone/master/condo_dataset1.csv"
condo_dataset1 <- read_csv(dataset1_url,
                           col_types = list(condo_name = col_character(),
                                            district = col_character(),
                                            condo_age = col_number()))

# Webscrape site url -
# https://condo.singaporeexpats.com/%d/name/A..Z
# %d - denotes from page 1...x depending on how many pages within each 
# A..Z page
# You may skip this to get the dataset from github instead
condo_dataset2 <- LETTERS %>%
  sprintf(url,.) %>% {
    Sys.sleep(2)  
    map_df(.,~scrape_webpage(.x))
  }

# Run this to get the dataset direct from github
dataset2_url <- "https://raw.githubusercontent.com/portergoh/capstone/master/condo_dataset2.csv"
condo_dataset2 <- read_csv(dataset2_url,
                           col_types = list(condo_name = col_character(),
                                            district = col_character(),
                                            condo_age = col_number()))
# Merge the 2 datasets together
condo_dataset3 <- bind_rows(condo_dataset1,condo_dataset2)

# Perform data cleaning
# Check rows which has age NA and negative which may indicate TOP date 
# in the future
table(condo_dataset3$district,useNA = "always")
table(condo_dataset3$condo_age,useNA = "always")

# Remove NA 
condo_dataset3 <- condo_dataset3 %>%
  filter(complete.cases(.))

# Remove age that is negative ar erronenous 
condo_dataset3 <- condo_dataset3 %>%
  filter(condo_age >0 & condo_age < 2008)

# Add in region into the dataset
condo_dataset3 <- condo_dataset3 %>% left_join(region_district(), by="district")

# Run this to get the dataset direct from github
dataset3_url <- "https://raw.githubusercontent.com/portergoh/capstone/master/condo_dataset3.csv"
condo_dataset3 <- read_csv(dataset3_url,
                           col_types = list(condo_name = col_character(),
                                            district = col_character(),
                                            condo_age = col_number(),
                                            region = col_character()))

# Webscrape site url -
# https://condo.singaporeexpats.com/%d/singapore-property-all/rent
# %d denotes how many pages to scrape
url2 <- "https://condo.singaporeexpats.com/%d/singapore-property-all/rent"
records_per_page <- 30
webpage <- read_html(sprintf(url2,1))
total_page <- compute_total_page(webpage, records_per_page)

# This is a huge dataset that contains more than 70k records 
# It contains rent data that can be use to merge with earlier
# dataset
# *** WARNING, this can take about an hour to complete ***
condo_dataset4 <- 1:total_page %>%
  sprintf(url2,.) %>% {
    Sys.sleep(2)
    map_df(.,~scrape_webpage2(.x))
  }

# Run this to get the dataset direct from github
dataset4_url <- "https://raw.githubusercontent.com/portergoh/capstone/master/condo_dataset4.csv"
condo_dataset4 <- read_csv(dataset4_url,
                           col_types = list(name = col_character(),
                                            type = col_character(),
                                            rent = col_number(),
                                            date = col_character()))

# Transform the dataset by filtering only 'Condo' and find the medium 
# of the rent for each property
condo_dataset4 <- condo_dataset4 %>% 
  filter(type=="Condo") 

# There are 1847 NA for rent which we can use it later for prediction
table(condo_dataset4$rent, useNA = "always")

condo_dataset5 <- condo_dataset4 %>% 
  group_by(name) %>% 
  summarise(median_rent = median(rent, na.rm = TRUE))

# Rename the field names of the dataset in preparation for merging into
# condo_dataset
names(condo_dataset5)[1] <- "condo_name"


# Join condo_dataset5 with condo_dataset3 (condo_dataset1 & condo_dataset2)
condo_dataset <- condo_dataset5  %>% 
   left_join(condo_dataset3, by="condo_name") %>%
   filter(complete.cases(.)) %>%
   mutate_at(vars(region),funs(factor(.,levels=c("CCR","RCR","OCR")))) %>%
   mutate_at(vars(district),funs(factor(.,levels=region_district()$district)))

#-----------------------------------------------------------------------#
# Our condo dataset will be split into 3 different groups
#  - Training data
#  - Testing data
#  - Predicting data
#
# Data that has complete cases will be split into two parts, 
# training(70 %) and testing (30 %)
# 
# Remaining that has incomplete cases will be our predicting 
# dataset
#-----------------------------------------------------------------------#
install.packages("fastDummies")
library(fastDummies)

set.seed(12345)
n <- nrow(condo_dataset)
sample <- sample(1:n ,size= round(0.7*n), replace=F)

data_train <- condo_dataset[sample,]
data_test  <- condo_dataset[-sample,]

# Removes the first dummy from every category. Avoids perfect
# multicollinearity issues in models.
data_train <- dummy_cols(data_train, 
                         select_columns = c("region","district"),
                         remove_first_dummy = T)

data_test <- dummy_cols(data_test, 
                        select_columns = c("region","district"),
                        remove_first_dummy = T)

#-----------------------------------------------------------------------#
#  ***  End of Main program: Data Collection and Tidy  ****
#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
# Main program: Data Visualisation
#-----------------------------------------------------------------------#
library(ggthemes)

data_train %>% 
  ggplot(aes(median_rent)) +
  geom_histogram() +
  labs(title = "Median Rental Rate",
       subtitle = "Is the distribution normal or skewed ?",
       x = "Median Rental Rate/sqft") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# let's do a natural log transformation and see if it distributes better?
data_train <- data_train %>%
  mutate(lmedian_rent = log2(median_rent))

data_test <- data_test %>%
  mutate(lmedian_rent = log2(median_rent))

data_train %>%  
  ggplot(aes(lmedian_rent)) +
  geom_histogram() +
  labs(title = "Median Rental Rate",
       subtitle = "After log2 transformation, seem better now ?",
       x = "Median Rental Rate/sqft") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

data_train %>%
  ggplot(aes(x = region, y = median_rent, fill = region)) + 
  geom_boxplot(stat = "boxplot", varwidth = TRUE) +
  labs(title = "Median Rental Rate vs Region",
       subtitle = "Is there a Relationship between Median Rental Rate and region?",
       x = "Region",
       y = "Median Rental Rate/sqft") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5) ) +
  guides(fill = "none")

# We can see a distinctive relationship on the median rent rate of the 
# condos depending on where it is located. Condo in the core central area 
# command the highest premiun follow by rest of the central area and outside.
# This is what most would have expected on the result.

data_train %>%
  ggplot(aes(condo_age, median_rent)) +
   labs(title = "Median Rental Rate vs Age of Condominium",
        subtitle = "Is there a Relationship?") +
   ylab("Median Rental Rate/sqft") +
   xlab("Age of Condominium") +
   geom_point(stat = "Identity", aes(color=region)) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5) )

# This scatter plot shows the inverse relationship in the median rent rate 
# as the condo ages, depicting the general trend that as your property ages 
# the less attractive it is to users. However, some condos rental rate in 
# the core central region seems inelastic to time. This could be due to the 
# fact that condos in these premium location has very limited supply.

boxplot(median_rent ~ condo_age, data = data_train,
        notch=FALSE, # Show confidence interval
        varwidth=TRUE, # Show sample size
        col="skyblue",
        main="Median Rental Rate vs Age of Condominium",
        xlab="Age of Condominium",
        ylab="Median Rental Rate/sqft")

# This is is boxplot that shows the inverse relationship between
# median rent rate as the condo ages. The turning point appears to be after 
# a decade, the rent rate starts to drop signifcantly in the following decade.
# In subsequent decade, we see that the drop rate seem to have tapered down 
# and become relatively stable implying that it has reached the floor limit 
# and would become inelastic to any further time changes.

#-----------------------------------------------------------------------#
# Main program: Data Transformation & Modeling
#-----------------------------------------------------------------------#
install.packages("GGally")
library(Hmisc)
library(stats)
library(broom)
library(modelr)
library(GGally)

# As we can see from earlier plots during data visualisation
# phase, there appeared to be a relationship between median rent rate 
# and condo age and also its location. How strong is the relationship ?

# Correlation Matrix
corr_res <- data_train %>%
  select(median_rent,condo_age,region_RCR,region_OCR) %>%
  as.matrix(.) %>%
  rcorr(.) %>%
  tidy(.) %>%
  as_tibble(.)

# There appear to be a strong negative relationship between median rent rate
# and condo age (p.value = 0)

# Looking also at the pair-wise correlation among the explanatory variables.
# We want to avoid multicollinearity issues
data_train %>%
  select(condo_age,region_RCR,region_OCR) %>%
  ggpairs()

# The coorelation between condo age and region is insignificant as indicated
# by the high p.value(>0.05)

aov1_res <- aov(median_rent ~ region, data = data_train)
summary(aov1_res)
# Anova test confirm region indeed exhibit contributory behaviour
# to the variation in median rent rate, but which region contribute 
# to the most variation ?

# Let's run TukeyHSD to confirm which region level is significant
TukeyHSD(aov1_res)
plot(TukeyHSD(aov1_res, conf.interval=.95))

# All region levels are away from 0 with confidence level of 95%

# 3 main effects: -
# condo_age, region_RCR and region_OCR
# 2 interaction term: -
# condo_age:regionRCR and condo_age:regionOCR 
#
# Our formula is: -
# median_rent = b0 + b1*condo_age + b2*region_RCR + b3*region_OCR
#               + b4*condo_age:regionRCR + b5*condo_age:regionOCR + e

lm_model1 <- function(df){
  lm(lmedian_rent ~ condo_age * region, data=df)
}

# 3 main effects: -
# condo_age, region_RCR and region_OCR
#
# Our formula is: -
# median_rent = b0 + b1*condo_age + b2*region_RCR + b3*region_OCR + e

lm_model2 <- function(df){
  lm(lmedian_rent ~ condo_age + region , data=df)
}

fit_model1 <- lm_model1(data_train)
fit_model2 <- lm_model2(data_train)

# Let's check out the diagnostic plots which show residuals in 
# four different ways in order to validate our model assumptions
fit_plot(fit_model1)
fit_plot(fit_model2)

# Both models appears to exhibit positive outcome in meeting the linear 
# regression assumptions
#
# 1. Residuals vs Fitted
#------------------------
# Residuals is randomly spread around a horizontal line 
# This suggests that the variances of the error terms are equal.
# So, we can assume the homogeneity of variances.
#
# 2. Normal Q-Q
#------------------------
# Residuals are normally distributed as they follow a straight line well
#
# 3. Scale-Location
#------------------------
# It’s also called Spread-Location plot. 
# This plot shows if residuals are spread equally along 
# the ranges of predictors. 
# We can see a horizontal line with equally (randomly) spread points.
#
# 4. Residuals vs Leverage
#-------------------------
# This plot helps us to find influential cases (i.e., subjects) if any. 
# Not all outliers are influential in linear regression analysis 
# (whatever outliers mean). Even though data have extreme values, they 
# might not be influential to determine a regression line.
# Look for cases outside of a dashed line, Cook’s distance. When cases are 
# outside of the Cook’s distance (meaning they have high Cook’s distance scores), 
# the cases are influential to the regression results. 
# The regression results will be altered if we exclude those cases.


OLS_summary_function(fit_model1,2)
OLS_summary_function(fit_model2,2)

# Both models exhibit relatively good R2 results of 0.58  - which represent 
# 58 % of variance in Median rent rate which could be explained by our model.

grid <- data_train %>%
  data_grid(condo_age,region) %>%
  gather_predictions(fit_model1,fit_model2, .pred = "lmedian_rent") %>%
  mutate(median_rent = 2 ^ lmedian_rent)

ggplot(data_train, aes(condo_age,median_rent, color=region)) +
  geom_point() +
  geom_line(data = grid, aes(y=median_rent)) +
  labs(title = "Model1 vs Model2",
       subtitle = "Which model is better ?") +
  ylab("Median Rental Rate/sqft") +
  xlab("Age of Condominium") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(~model)

# Hard to tell which is better, let's look at the residuals

sim1 <- data_train %>%
  gather_residuals(fit_model1,fit_model2)

ggplot(sim1, aes(condo_age,resid, color=region)) +
  geom_point() +
  labs(title = "Residual vs Age of Condominium",
       subtitle = "Any patterns in the residuals ?") +
  ylab("Resid") +
  xlab("Age of Condominium") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_grid(model ~ region)

pred1 <- predict(fit_model1, data_test, se.fit = TRUE)
pred2 <- predict(fit_model2, data_test, se.fit = TRUE)


# This equation establishes the score, which we want to minimize, is the 
# average of studentized residuals. Studentized residuals, as you may know, are 
# residuals divided by a measure of the standard errors. 
#
# This formula gives us an average measure of how close we are to predicting an 
# observation's value correctly relative to the variance observed for that 
# data range. If we have a high degree of variance 
# (resulting in high standard errors), we don't want to be too strict with 
# the prediction, but if we are in a low-variance area, we want to make sure 
# that our predictions are very accurate:

score <- function(data_test, predictions) {
  # se := standard errors
  se <- predictions$se.fit
  real <- data_test$lmedian_rent
  predicted <- predictions$fit
  
  return(sum((real - predicted)^2 / se^2) / nrow(data_test))
}

score1 <- score(data_test,pred1)
score2 <- score(data_test,pred2)

# model1 having a lower score of 74 appears to be better than model 2 (98)

# TODO
lm_model3 <- function(df){
  lm(log2(median_rent) ~ condo_age + 
       district_01 + district_02 + district_03 +
       district_04 + district_05 + district_07 +
       district_08 + district_10 + district_11 +
       district_12 + district_13 + district_14 + 
       district_15 + district_16 + district_17 + 
       district_18 + district_19 + district_20 + 
       district_21 + district_22 + district_23 + 
       district_25 + district_26 + district_27 +
       district_28
      ,data=df)
}







