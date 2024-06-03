#Clear memory, set working directory and load packages 
rm(list=ls())

#Set a working directory# 
setwd("G:/My Drive/Term-4 Classes/Applied Econometrics for Managers/Project")

options(scipen=999) #Disable scientific notation
options(warn = -1)  # Suppress all warnings

#Load R packages# 
library(psych) 
library(haven) 
library(stargazer) 
library(lmtest) 
library(sandwich) 
library(car)
library(AER)
library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(mfx) 
library(plm) 
library(expss) 
library(tidyr)
library(reshape2)
library(zoo)
library(mlogit)

##################################### DATA PREPARATION & UDNERSTANDING ####################################################################

# Q. Use the “marketing_campaign.csv” dataset for this session. Comment on the structure of the dataset.
#    Provide a table with summary statistics for all the variables in the dataset.
cbb_df <- read_excel("marketing_campaign.xlsx", na="") # Reading the file
View(cbb_df)

names(cbb_df) #Names of all the columns
# "ID"                  "Year_Birth"          "Education"           "Marital_Status"      "Income"             
# "Kidhome"             "Teenhome"            "Dt_Customer"         "Recency"             "MntWines"           
# "MntFruits"           "MntMeatProducts"     "MntFishProducts"     "MntSweetProducts"    "MntGoldProds"       
# "NumDealsPurchases"   "NumWebPurchases"     "NumCatalogPurchases" "NumStorePurchases"   "NumWebVisitsMonth"  
# "AcceptedCmp3"        "AcceptedCmp4"        "AcceptedCmp5"        "AcceptedCmp1"        "AcceptedCmp2"       
# "Complain"            "Z_CostContact"       "Z_Revenue"           "Response"           
#See the structure of the data loaded#
str(cbb_df) # 2240 rows and 29 columns

#Summary
summary(cbb_df) 

# Q. Are there any of the features have missing values exists and impute the mean values of each feature.
# Check if missing values exist
cbb_df[!complete.cases(cbb_df),] # The Income feature contains missing values
# Find out corresponding observations for which a variable is missing# 
count_missing<- table(is.na (cbb_df$Income)) 
prop.table(count_missing, margin=1)
which(is.na(cbb_df$Income)) 
# The Income Feature contains 24 missing values
# Impute missing values with mean for numeric columns
numeric_cols <- sapply(cbb_df, is.numeric)
cbb_df[, numeric_cols] <- na.aggregate(cbb_df[, numeric_cols], FUN = mean)
  
# Q. Is there a statistically significant difference in the average spending on different product categories 
#    (e.g., MntWines, MntMeatProducts) between customers who have lodged complaints (Complain = 1) and those who 
#    haven't (Complain = 0)?
product_categories = c('MntWines','MntFruits','MntMeatProducts', 'MntFishProducts','MntSweetProducts','MntGoldProds')
describeBy(cbb_df[,product_categories], group=cbb_df$Complain, skew=F)


t.test(cbb_df$MntWines[cbb_df$Complain == 0], cbb_df$MntWines[cbb_df$Complain == 1])
t.test(cbb_df$MntFruits[cbb_df$Complain == 0], cbb_df$MntFruits[cbb_df$Complain == 1])
t.test(cbb_df$MntFishProducts[cbb_df$Complain == 0], cbb_df$MntFishProducts[cbb_df$Complain == 1])
t.test(cbb_df$MntSweetProducts[cbb_df$Complain == 0], cbb_df$MntSweetProducts[cbb_df$Complain == 1])
t.test(cbb_df$MntGoldProds[cbb_df$Complain == 0], cbb_df$MntGoldProds[cbb_df$Complain == 1])

# Q. Find out the number of customers based on their education and marital status.
table(cbb_df$Education) 
table(cbb_df$Marital_Status)

# Q. Differentiate the customers into Genz and Millenials
genz_cbb_df <- subset(cbb_df, Year_Birth >= 2000)
mil_cbb_df <- subset(cbb_df,Year_Birth<2000)
View(genz_cbb_df) # 693 observations 
View(mil_cbb_df) # 1547 observations

# Q. Create a feature representing the amount spend on each product category by a customer. 
cbb_df$expenditure <- rowSums(cbb_df[, grep("^Mnt", names(cbb_df))]) 

# Q. Find out the total purchases made by a customer. Comment on its relation with Expenditure. Suggest 
#    what can be done to deal with their relation to improve the quality of your model.
purchase_columns <- grep("Purchases", names(cbb_df), value = TRUE)
(cbb_df$total_purchases <- rowSums(cbb_df[, purchase_columns], na.rm = TRUE))

plot(cbb_df$total_purchases, cbb_df$lnexpenditure, col = "blue", xlab = "Total Purchases", ylab = "Expenditure")
lines(lowess(cbb_df$total_purchases, cbb_df$lnexpenditure), col = "red", lwd = 2)

# We create a Quadratic independent variable to deal with the non-linear relation
cbb_df$purchasesq<-cbb_df$total_purchases^2 

# Q. Is there a statistically difference based on the Customer Response. 
cbb_df$Latest_Offer_Response <- factor(cbb_df$Response,labels=c("Rejected","Accepted"))
head(cbb_df, n=10)
describeBy(cbb_df, group=cbb_df$Latest_Offer_Response, skew=F)

# Q. Find out the number of customers who have been accepted to at least one of the campaigns.  
cbb_df$accepted_atleast_one_campaign <- pmax(cbb_df$AcceptedCmp1,cbb_df$AcceptedCmp2,cbb_df$AcceptedCmp3,cbb_df$AcceptedCmp4,cbb_df$AcceptedCmp5)
(sum(cbb_df$accepted_atleast_one_campaign == 1)) # 463 Customers have accepted at least one of the 5 campaigns

# Q. Create a dummy to understand the customers who have made a purchase in the last 30 days.  
cbb_df$last30days_recency <- 1 * (cbb_df$Recency<=30) 
(sum(cbb_df$last30days_recency == 1)) # 724 customers have made a purchase in the last 30 days

# Q. Create a variable to club the Year of Birth in ranges, having 2000-2010 as the base year.
cutoffs <- c(1960,1970,1980,1990,2000,2010)
# Create YOB range with custom labels
cbb_df$yob_level <- cut(cbb_df$Year_Birth, cutoffs, right = FALSE,
                        labels = c("[1960,1970)", "[1970,1980)", "[1980,1990)", "[1990,2000)", "[2000,2010)"))
#Change the base  
cbb_df$yob_level <- relevel(cbb_df$yob_level,"[2000,2010)") 
table(cbb_df$yob_level) 
#See the range 
table(cbb_df$yob_level)

# Q. Extract the year from the Customer Acquired Date.
cbb_df$Dt_Customer <- as.Date(cbb_df$Dt_Customer)
cbb_df$Yr_Customer <- as.integer(format(as.Date(cbb_df$Dt_Customer), "%Y"))

# Q. To understand the elasticity prepare the Income and Expenditure features for theoritically sound modelling.
cbb_df$Income <- cbb_df$Income + 1 
cbb_df$expenditure <- cbb_df$expenditure + 1
cbb_df$lnincome = log(cbb_df$Income)
cbb_df$lnexpenditure = log(cbb_df$expenditure)

View(cbb_df)

###################################### DATA UNDERSTANDING #################################################################

# Q. Find the average amount spent by customers on each product category and consider only the customers who have 
#   made at least one purchase.
purchase_customers <- cbb_df[cbb_df$NumDealsPurchases > 0, ]
# Calculate the means
mean_MntWines <- mean(purchase_customers$MntWines, na.rm = TRUE)
mean_MntFruits <- mean(purchase_customers$MntFruits, na.rm = TRUE)
mean_MntMeatProducts <- mean(purchase_customers$MntMeatProducts, na.rm = TRUE)
mean_MntFishProducts <- mean(purchase_customers$MntFishProducts, na.rm = TRUE)
mean_MntSweetProducts <- mean(purchase_customers$MntSweetProducts, na.rm = TRUE)
mean_MntGoldProds <- mean(purchase_customers$MntGoldProds, na.rm = TRUE)
# Create a data frame
(means_df <- data.frame(
  Product_Type = c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds"),
  Mean = c(mean_MntWines, mean_MntFruits, mean_MntMeatProducts, mean_MntFishProducts, mean_MntSweetProducts, mean_MntGoldProds)
))

# Q. Find out the Average number of each purchase channel and consider only the customers who have been accepted to at least one
#   marketing campaign
campaign_accepted_customers <- subset(cbb_df,accepted_atleast_one_campaign>0)
# Calculate the means
mean_NumDealsPurchases <- mean(campaign_accepted_customers$NumDealsPurchases, na.rm = TRUE)
mean_NumWebPurchases <- mean(campaign_accepted_customers$NumWebPurchases, na.rm = TRUE)
mean_NumCatalogPurchases <- mean(campaign_accepted_customers$NumCatalogPurchases, na.rm = TRUE)
mean_NumStorePurchases <- mean(campaign_accepted_customers$NumStorePurchases, na.rm = TRUE)
# Create a data frame with the means
(C_means_df <- data.frame(
  Feature = c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases"),
  Mean = c(mean_NumDealsPurchases,mean_NumWebPurchases,mean_NumCatalogPurchases,mean_NumStorePurchases)))

# Q. What is the average age of the customers in the data set? What can you say about the age distribution from this 
#   information?
current_year <- as.numeric(format(Sys.Date(), "%Y"))
cbb_df$Customer_Age <- current_year - cbb_df$Year_Birth
average_age <- mean(cbb_df$Customer_Age, na.rm = TRUE)
hist(cbb_df$Customer_Age, breaks = 20, main = "Customer Age Distribution", xlab = "Age")

# Q. Comment on the distribution of customers based on their income
ggplot(cbb_df, aes(x = Income)) +
  geom_histogram(binwidth = 10000, fill = "green") +
  labs(x = "Income", y = "Frequency", title = "Distribution of Income")

# Q. Comment on number of the customers based on their educational qualification
ggplot(cbb_df, aes(x = Education)) +
  geom_bar(fill = "purple") +
  labs(x = "Education Level", y = "Count", title = "Education Level Distribution")

# Q. Comment on number of the customers based on their marital status
ggplot(cbb_df, aes(x = Marital_Status)) +
  geom_bar(fill = "orange") +
  labs(x = "Marital Status", y = "Count", title = "Marital Status Distribution")

# Q. Comment on income of the customers based on their educational qualification
ggplot(cbb_df, aes(x = Education, y = Income)) +
  geom_boxplot(fill = "pink") +
  labs(x = "Education Level", y = "Income", title = "Income Distribution by Education Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Q. Comment on income, recency and amount spent on each product by customers
scatterplotMatrix(cbb_df[, c("Income", "Recency", "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts")], 
                  col = c("blue", "green", "red"),
                  diagonal = "hist")

# Q. Explain the NumWebVisitsMonth over time
ggplot(cbb_df, aes(x = Dt_Customer, y = NumWebVisitsMonth)) +
  geom_line() +
  labs(x = "Date of Enrollment", y = "Number of Web Visits", title = "Web Visits Over Time")

# Q. Comment on the campaign exposure of customers
campaign_data <- cbb_df %>%
  select(AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5)
campaign_acceptance <- colMeans(campaign_data)
campaign_names <- names(campaign_acceptance)
ggplot(data.frame(campaign = campaign_names, acceptance_rate = campaign_acceptance), aes(x = campaign, y = acceptance_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Campaign", y = "Acceptance Rate", title = "Campaign Acceptance Rates") +
  ylim(0, 1)

# Q. Comment on the distribution of customers based on the purchasing channel
purchase_data <- cbb_df %>%
  select(NumWebPurchases, NumCatalogPurchases, NumStorePurchases)

purchase_data %>%
  mutate(Customer_ID = seq_len(nrow(purchase_data))) %>%
  gather(key = "Channel", value = "Count", -Customer_ID) %>%
  ggplot(aes(x = Customer_ID, y = Count, fill = Channel)) +
  geom_bar(stat = "identity") +
  labs(x = "Customer ID", y = "Count", title = "Purchase Channels") +
  theme(axis.text.x = element_blank(), legend.position = "top") +
  scale_fill_manual(values = c("NumWebPurchases" = "blue", "NumCatalogPurchases" = "green", "NumStorePurchases" = "red"))

# Q. Visualize the correlation of Income, Recency and the amount spent by customers in each product category
sum(is.na(cbb_df))
correlation_matrix <- cor(cbb_df[, c("Income", "Recency", "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts")])
heatmap(correlation_matrix,
        col = colorRampPalette(c("blue", "white", "red"))(20),
        main = "Correlation Heatmap",
        margins = c(10, 10))

###################################### MODELLING, EVALUATION & DEPLOYMENT #################################################################

# Q. Create a regression model to predict customer response (Response) using the Recency as the main predictor 
#    along with other relevant variables like presence of complaints (Complain) and NumDealsPurchases. 
#    Interpret the coefficients and assess the model's goodness of fit.
reg1 <- lm(Response ~ Recency + Complain + NumDealsPurchases, data = cbb_df) # Level - Level Model
summary(reg1)

# Q. Construct a dummy variable "last30days_recency" (1 if "Recency" is 30 days or less, 0 otherwise). Analyze how the 
#    Response affects the last30days_recency on different product categories.
reg2 <- lm(last30days_recency ~ Response, data = cbb_df) # Level - Level Model
summary(reg2)

# Q. Build a model to determine if the number of visits to the company's website (NumWebVisitsMonth) is related to the 
#    customer's likelihood of responding positively to promotional campaigns (AcceptedCmp1, AcceptedCmp2, etc.). Compare
#    the phenomenon with genz and millineal customers as well.
reg3 <- lm(NumWebVisitsMonth ~ AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + Response, data = cbb_df) # Log-Log Model
summary(reg3)
reg3_1 <- lm(NumWebVisitsMonth ~ AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + Response, data = genz_cbb_df) # Log-Log Model
summary(reg3_1)
reg3_2 <- lm(NumWebVisitsMonth ~ AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + Response, data = mil_cbb_df) # Log-Log Model
summary(reg3_2)
stargazer(list(reg3,reg3_1,reg3_2), keep.stat=c("n", "adj.rsq"), type="text")

# Q. Conduct a logistic regression analysis to determine if a customer's education level (Education) influences their 
#    likelihood of accepting various promotional campaigns (AcceptedCmp1, AcceptedCmp2, etc.).
reg4 <- glm( AcceptedCmp1 ~ Education, data = cbb_df ) 
summary(reg4)
reg5 <- glm( AcceptedCmp2 ~ Education, data = cbb_df ) 
summary(reg5)
reg6 <- glm( AcceptedCmp3 ~ Education, data = cbb_df )
summary(reg6)
reg7 <- glm( AcceptedCmp4 ~ Education, data = cbb_df )
summary(reg7)
reg8 <- glm( AcceptedCmp5 ~ Education, data = cbb_df )
summary(reg8)
reg9 <- glm( accepted_atleast_one_campaign ~ Education, data = cbb_df )
summary(reg9)
reg10 <- glm( Response ~ Education, data = cbb_df )
summary(reg10)
stargazer(list(reg4,reg5,reg6,reg7,reg8,reg9,reg10), keep.stat=c("n", "adj.rsq"), type="text")

# Q. Utilize a regression analysis to determine which promotional campaign (AcceptedCmp1-5) had the most substantial 
#    impact on customer acceptance, considering the response to the last campaign (Response = 1). Provide insights 
#    into the overall effectiveness of promotional campaigns in retaining and attracting customers.
reg11<-lm(Response ~ AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5, data=cbb_df) # Regression using standardized variables
summary(reg11)

# Q. Investigate whether the effect of web visits on purchases varies by education level (Education). Is the relationship 
#    between NumWebVisitsMonth and total spending different for customers with different education levels?
reg12 <- glm(NumWebVisitsMonth ~ Education, data = cbb_df)
summary(reg12)

# Q. Investigate whether there is a significant change in customer behavior (e.g., a shift from in-store to online 
#    shopping) over time. Use a difference-in-differences (DiD) analysis to compare the change in web purchases before 
#    and after a specific year (e.g., 2010) for different customer segments.
cbb.pd <- pdata.frame(cbb_df, index = "Yr_Customer")
View(cbb.pd)
pdim(cbb.pd) # Check the Balance of our Panel Data
# Checking for variation of variables within individuals
pvar(cbb.pd)
#First few observations
cbb.pd[1:10,c('Income','Kidhome','Teenhome','Recency','MntWines','MntFruits','MntMeatProducts',
                 'MntFishProducts','MntSweetProducts','MntGoldProds','NumDealsPurchases',
                 'NumWebPurchases','NumCatalogPurchases','NumStorePurchases','NumWebVisitsMonth',
                 'AcceptedCmp3','AcceptedCmp4','AcceptedCmp5','AcceptedCmp1','AcceptedCmp2',
                 'Complain','Z_CostContact','Z_Revenue','Response')] 
cbb.pd$genz <- ifelse(cbb.pd$Year_Birth>2000, 1, 0) 
cbb.pd$phd = 0  
cbb.pd$phd[cbb.pd$Education == "PhD"] <- 1
cbb.pd$genzphd = cbb.pd$genz*cbb.pd$phd
cbb.pd$phd_accepted_atleast_one_campaign = cbb.pd$phd*cbb.pd$accepted_atleast_one_campaign
cbb.pd$genz_accepted_atleast_one_campaign = cbb.pd$genz*cbb.pd$accepted_atleast_one_campaign
cbb.pd$genzphd_accepted_atleast_one_campaign = cbb.pd$genz*cbb.pd$phd*cbb.pd$accepted_atleast_one_campaign
# Basic Diff in Diff (consumer behavior analysis for Genz with phd as educational qualification and have accepted atleast one of our campigns) # 
didreg1 <- lm(Recency ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
           + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg1)
didreg2 <- lm(lnincome ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg2)
didreg3 <- lm(lnexpenditure ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg3)
didreg4 <- lm(NumDealsPurchases ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg4)
didreg5 <- lm(NumWebPurchases ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg5)
didreg6 <- lm(NumCatalogPurchases ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg6)
didreg7 <- lm(NumStorePurchases ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg7)
didreg8 <- lm(NumWebVisitsMonth ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd)
summary(didreg8)
# Create a list of regression models with labels
model_list <- list(
  "Recency" = didreg1,
  "lnincome" = didreg2,
  "lnexpenditure" = didreg3,
  "NumDealsPurchases" = didreg4,
  "NumWebPurchases" = didreg5,
  "NumCatalogPurchases" = didreg6,
  "NumStorePurchases" = didreg7,
  "NumWebVisitsMonth" = didreg8
)
# Generate a summary table for all models
stargazer(model_list, title = "Regression Results", keep.stat=c("n", "rsq"), type = "text")

# Automatic FD estimation 
reg_fd <- plm(lnexpenditure ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
              + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd, model="fd") 
summary(reg_fd)
pbgtest(reg_fd)
coeftest(reg_fd, vcovHC(reg_fd, method="arellano"))
# Pooled, FE, RE 
reg.ols<- (plm(lnexpenditure ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
               + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd, model="pooling") )
reg.fe <- (plm(lnexpenditure ~ phd + genz + genzphd + accepted_atleast_one_campaign + phd_accepted_atleast_one_campaign 
               + genz_accepted_atleast_one_campaign + genzphd_accepted_atleast_one_campaign, data = cbb.pd, model="within") ) 
stargazer(reg.ols,reg.fe, type="text", 
          column.labels=c("OLS","RE","FE"),keep.stat=c("n","rsq"))
# Hausman test
# Null Hypothesis: OLS is preferred model; Alt Hypothesis: FE is preferred 
phtest(reg.fe, reg.ols)

# Q. Build a regression model to examine the impact of income of the consumer on the total amount 
#    spent on different product categories (e.g., MntWines, MntFruits, etc.) while controlling for other relevant 
#    variables like the number of web visits (NumWebVisitsMonth) and education. What is the estimated coefficient for Income, and is it statistically 
#    significant? Also, split the data and make predictions.
reg13 <- glm(lnexpenditure ~ lnincome + Education + NumWebVisitsMonth, data = cbb_df)
summary(reg13)
# Split the data into training and testing sets (e.g., 70% for training, 30% for testing)
set.seed(123) # for reproducibility
sample_index <- sample(1:nrow(cbb_df), 0.7 * nrow(cbb_df))
train_data <- cbb_df[sample_index, ]
test_data <- cbb_df[-sample_index, ]

actual_values <- test_data$lnexpenditure  
# Make predictions on the test data
predictions <- predict(reg13, newdata = test_data, type = "response")
# Calculate residuals
residuals <- actual_values - predictions
# Calculate evaluation metrics
mae <- mean(abs(residuals))
mse <- mean(residuals^2)
rmse <- sqrt(mse)
rsquared <- 1 - sum(residuals^2) / sum((actual_values - mean(actual_values))^2)
# Print or display the metrics
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsquared, "\n")
# Create a scatter plot
plot(predictions, actual_values, main="Actual vs. Predicted Values", xlab="Predicted", ylab="Actual")
abline(0, 1, col="red")  # Add a diagonal line for reference

#Q. Build a linear regression model to investigate the relationship between a customer's yearly household income (Income) 
#   and the children & young adults in their household. Does it significantly affect income 
#   levels?
reg14 <- lm(lnincome ~ Kidhome + Teenhome, data = cbb_df)
summary(reg14)

#Q. We want to know whether we can predict the likelihood of a customer complaining or not. For this, run a logit 
#   model where Complain is the dependent variable and Year_Birth, Education, Marital_Status, Income, Kidhome, 
#   Teenhome, purchasesq, and Recency are the independent variables. What is McFadden's pseudo R-squared in this case? 
#   Why should we use this model instead of LPM?
#   Then, We want to know the effect of the number of marketing campaigns a customer has been accepted to on the 
#   likelihood of them complaining. For this, use Complain as the dependent variable and NumDealsPurchases, AcceptedCmp1, 
#   AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, and AcceptedCmp5 are the independent variables. Explain the results of the 
#   estimated model
# LPM
lpm <- lm(Complain ~ Income + Kidhome + Teenhome + Recency + MntWines + MntFruits + MntMeatProducts + MntFishProducts 
          + MntSweetProducts + MntGoldProds + NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumStorePurchases
          + NumWebVisitsMonth + Response, data=cbb_df)
summary(lpm)
coeftest(lpm, vcov=hccm) 
describe(fitted(lpm), skew=F) 
H0 <- c("Recency","Response")
linearHypothesis(lpm, H0) 
indvalues <- list(Income=c(175000,10000), Kidhome=c(1,0), Teenhome=c(0,1), Recency=c(24,96), MntWines=c(194,1718), 
                  MntFruits=c(148,1009), MntMeatProducts=c(256,128), MntFishProducts=c(1512,311), MntSweetProducts=c(30,0), 
                  MntGoldProds=c(111,500),NumDealsPurchases=c(2,5), NumWebPurchases=c(9,2), NumCatalogPurchases=c(2,7), 
                  NumStorePurchases=c(8,4), NumWebVisitsMonth=c(15,1), Response=c(1,0)) 
predict(lpm, indvalues) 
#Estimating Logit Model 
logit1 <- glm(Complain ~ Year_Birth + Education + Marital_Status + Income + Kidhome + Teenhome + purchasesq 
              + Recency, family=binomial(link=logit), data=cbb_df)
summary(logit1)
logLik(logit1)
#McFadden's pseudo Rsquared 
1 - logit1$deviance/logit1$null.deviance
#Estimating Probit Model 
probit1 <- glm(Complain ~ Year_Birth + Education + Marital_Status + Income + Kidhome + Teenhome + purchasesq 
               + Recency, family=binomial(link=probit), data=cbb_df)
logLik(probit1)
#McFadden's pseudo Rsquared 
1 - probit1$deviance/probit1$null.deviance
#Estimating Logit Model 
logit2 <- glm(Complain ~ NumDealsPurchases + AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 
              + Response + Recency, family=binomial(link=logit), data=cbb_df)
summary(logit2)
logLik(logit2)
#McFadden's pseudo Rsquared 
1 - logit2$deviance/logit2$null.deviance
#Estimating Probit Model 
probit2 <- glm(Complain ~ NumDealsPurchases + AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 
               + Response + Recency, family=binomial(link=probit), data=cbb_df)
logLik(probit2)
#McFadden's pseudo Rsquared 
1 - probit2$deviance/probit2$null.deviance
#LR test statistics for overall  significance  
lrtest(logit1)
#Test a joint hypothesis 
lrtest(logit2, logit1)
lpvalues <- list(Year_Birth=c(1997,1975), Education=c("PhD","Basic"), Marital_Status=c("Single","Married"), Income=c(24000,96000), Kidhome=c(0,1), 
                 Teenhome=c(0,1), purchasesq=c(256,484),Recency = c(78,4)) 
predict(logit1, lpvalues, type="response")
predict(probit1, lpvalues, type="response")
#Calculating APE automatically 
logitmfx(Complain ~ Year_Birth + Education + Marital_Status + Income + Kidhome + Teenhome + purchasesq + Recency, data=cbb_df, atmean=F)
probitmfx(Complain ~ Year_Birth + Education + Marital_Status + Income + Kidhome + Teenhome + purchasesq + Recency, data=cbb_df, atmean=F)
cbb_df$complain_pred <- predict(logit1, type="response");
cbb_df$complain_pred <- as.numeric( with ( cbb_df, ifelse( (complain_pred >= 0.5), 1, 0) ) ); 
# Create the confusion matrix
conf_matrix <- table(cbb_df$complain_pred, cbb_df$Complain)
conf_matrix
# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
TN <- conf_matrix[1, 1]
FP <- conf_matrix[1, 2]
FN <- sum(cbb_df$Complain == 1 & cbb_df$complain_pred == 0)
TP <- sum(cbb_df$Complain == 1 & cbb_df$complain_pred == 1)
# Calculate precision, recall, and F1-score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)
# Print the metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Q. Now we want to know the effect of the number of marketing campaigns a customer has been accepted to on the amount 
#    of money they spend. For this, use OLS to estimate the model where expenditure is the dependent variable and 
#    NumDealsPurchases, AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, and AcceptedCmp5 are the independent 
#    variables. What is this estimation technique known as? Explain this technique using a simple equation in this context.
#    Explain the results of the estimated model.
#    Now, add Education, Marital_Status, Income, Kidhome, Teenhome, Yr_Customer, and Recency to the model, and re-estimate. Explain the results.
reg15 <- lm(lnexpenditure ~ NumDealsPurchases + AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5, data = cbb_df)
summary(reg15)
reg16 <- glm(lnexpenditure ~ lnincome + NumDealsPurchases + AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + 
               AcceptedCmp5 + Education + Marital_Status + Kidhome + Teenhome + Yr_Customer + Recency, data = cbb_df)
summary(reg16)

# Q. Suggest another model and conduct necessary steps to comment on the applicability of the model.
reg <- glm(expenditure ~ Recency + Income+ Education + Marital_Status + Kidhome + Teenhome 
           + NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumStorePurchases + NumWebVisitsMonth 
           + AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + Response + Complain 
           + yob_level + I(total_purchases^2), data = cbb_df) 
summary(reg)

describe(fitted(reg), skew=F) 
cbb_df$fitted <- fitted(reg) 
residual <- resid(reg) 
predvalues <- list(Recency=2, Income=65000, Education="Graduation", Marital_Status="Married", Kidhome=1, Teenhome=0,
                  NumDealsPurchases=5, NumWebPurchases=7, NumCatalogPurchases=4, NumStorePurchases=9, 
                  NumWebVisitsMonth=16, AcceptedCmp1=0, AcceptedCmp2=0, AcceptedCmp3=1, AcceptedCmp4=0, 
                  AcceptedCmp5=1,Response=1, Complain=1, yob_level="[1980,1990)", total_purchases=25) 
(predicted_values<-predict(reg, predvalues)) 
# Based on the expected or declared inputs, we can predict an expenditure of 1259.39 units.
(confidence_predicted_values <- predict(reg, predvalues, interval = "confidence"))
#Make a table to see actual and predicted outcome/dependent variable side-by-side
tab1 <- cbind(cbb_df$Recency, cbb_df$expenditure, predicted_values, residual) 
names1 <- c("Recency", "Expenditure", "Predicted Expenditure", "residual")
colnames(tab1) <- names1 
head(tab1, n=20)  
#Extract the estimated coefficients from stored results
(beta_estimated <- coef(reg)) 
#Get Confidence Intervals
confint(reg)
confint(reg, level=0.99)
#Automatic BP test
# Null: Homoskedasticity 
bptest(reg) #If rejected, then Heteroskedastic
#White heteroscedasticity-robust SE:
coeftest(reg, vcov = vcovHC, type="HC0")
#Joint test of significance
linearHypothesis(reg,c("Response=1", "Complain=0"))

############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

