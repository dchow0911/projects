# -------------------------------------------------------------- Importing necessary libraries -----------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(Hmisc)
library(readxl)
library(ggpubr)
library(sqldf)
library(tidyverse)
library(hexbin)
library(patchwork)
library(plotly)
library(gapminder)
library(corrplot)
library(treemapify)
library(leaflet)
library(ggiraph)
library(shiny)
library(MASS)
library(party)
library(rpart)
library(factoextra)


# -------------------------------------------------------------- Data Understanding -----------------------------------------------------------------------

# Importing Excel File
setwd("G:/My Drive/Term-3 Classes/Business Computing II/Project")
df<-read_excel("flight prices predict.xlsx")
df

# Data Description

dim(df) # Used to find out the number of rows and columns in the data set
glimpse(df) # Getting a Glimpse of the data set and the data types of different attributes
describe(df) # Quantitative and Qualitative Description about each attribute of the data set

# Verifying the Data Quality
colSums(is.na(df)) # The Output shows that our data set is ready for accurate analysis due to absence of unnecessary data.

# Data Preparation
df$rowcount<-c(rep(1,nrow(df))) # Creating an attribute to ease visualization of categorical attributes.


# -------------------------------------------------------------- Descriptive Analytics -----------------------------------------------------------------------

summary(df) # Used to give us the descriptive statistics of all the attributes

descriptive_statistics<-function(dataframe,column_name){
  mean_i<-round(mean(dataframe[[column_name]]),2) # Calculating the mean 
  median_i<-round(median(dataframe[[column_name]]),2) # Calculating the median 
  min_i<-min(dataframe[[column_name]]) # Calculating the minimum 
  max_i<-max(dataframe[[column_name]]) # Calculating the maximum 
  std_i<-round(sd(dataframe[[column_name]]),2) # Calculating the standard deviation 
  
  # Presenting in a data frame
  Stats<-c("Mean","Median","Minimum","Maximum","Standard Deviation")
  Values<-c(mean_i,median_i,min_i,max_i,std_i)
  descriptive_stats<-data.frame(Stats,Values)
  print(paste("Descriptive Statistics of ",column_name," is: "),quote=F)
  print(descriptive_stats) # Viewing the Data Frame
  print("_____________________________________________",quote=F)
}

numerical_cols<-c("duration","days_left","price")
for (i in numerical_cols){
  descriptive_statistics(df,i)
}

# UNDERSTANDING THE CORRELATION
# Extracting the numerical columns
num_cols <- sapply(df, is.numeric)
# Compute the correlation matrix
cor_matrix <- cor(df[, num_cols])
# Create a correlation plot
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = "black")
# The heat map gives us an understanding that price is slightly positively correlated to duration and slightly negatively correlated to days_left
# Hence, as duration increases price increases and vice-versa. Also, as number of days_left decreases, price increases and vice-versa

# -------------------------------------------------------- Data Exploration & Visualization -----------------------------------------------------------------------

# AIRPORT OVERVIEW
# Top 3 busiest airports cities in regards to departure and arrival.

# No. of flights departing to each city
df3<-df %>%
  group_by(destination_city) %>%
  summarise(sum_rowcount = sum(rowcount))
dep<-ggplot(df3, aes(x="", y=sum_rowcount, fill=destination_city))+ geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)+ scale_fill_brewer(palette="Blues")+  theme_minimal()+geom_text(aes(label = paste(round((sum_rowcount/sum(sum_rowcount))*100,2),"%")),position = position_stack(vjust = 0.5), col="black")+ggtitle("Flights by Destination City")+theme(plot.title = element_text(hjust = 0.5))

# No. of flights arriving from each city
df4<-df %>%
  group_by(source_city) %>%
  summarise(sum_rowcount = sum(rowcount))
arr<-ggplot(df4, aes(x="", y=sum_rowcount, fill=source_city))+ geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)+ scale_fill_brewer(palette="Blues")+  theme_minimal()+geom_text(aes(label = paste(round((sum_rowcount/sum(sum_rowcount))*100,2),"%")),position = position_stack(vjust = 0.5), col="black")+ggtitle("Flights by Source City")+theme(plot.title = element_text(hjust = 0.5))
ggarrange(dep,arr,ncol=2,nrow=1)

# Understanding Departures and Arrivals for Each City
df_map<-read.csv("loc_details.csv")
lngmean<- mean(df_map$longitude)
latmean<- mean(df_map$latitude)
map <- df_map %>% leaflet() %>% addTiles() %>% setView(lng = lngmean, lat = latmean, zoom = 4.5) %>% addMarkers(popup = paste(df_map$name," had ",df_map$departure_count," departures and ",df_map$arrival_count," arrivals."),lng = df_map$longitude, lat = df_map$latitude)%>% addCircles(weight=1,radius=100)
map


# PRICING OVERVIEW
# Price of Ticket across Timeline
line_plot<-ggplot(data=df, aes(x=date, y=price, group=airline)) + geom_line(linetype = "dotted",color="green",size=1.5)+ geom_point()+ggtitle("Price of Ticket across time")+labs(x="Date",y="Price")+ scale_fill_brewer(palette="Blues") 
ggplotly(line_plot)

# Price of Ticket by Airlines
plot<-ggplot(df, aes(x=date, y=price, fill=airline))+ geom_area(alpha=0.8)+ scale_fill_brewer(palette="Blues")+ggtitle("Price of Ticket by Airlines")
ggplotly(plot)

# Price of Ticket by Source City
source_vax_graph <- ggplot(df, aes(x = reorder(source_city, price), y = price,tooltip = paste(source_city,": ₹",price), data_id = source_city )) +geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +  theme_minimal() +theme(axis.text=element_text(size = 6)) +  labs(title = "Source City by Price") +ylab("") +xlab("") +coord_flip()
girafe(ggobj = source_vax_graph, width_svg = 5, height_svg = 4)

# Price of Ticket by Destination City
destination_vax_graph <- ggplot(df, aes(x = reorder(destination_city, price), y = price,tooltip = paste(destination_city,": ₹",price), data_id = source_city )) +geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +  theme_minimal() +theme(axis.text=element_text(size = 6)) +  labs(title = "Destination City by Price") +ylab("") +xlab("") +coord_flip()
girafe(ggobj = destination_vax_graph, width_svg = 5, height_svg = 4)

# Price of flight and correlation with duration
p_dur<-ggplot(df, aes(x = duration, y = price, color = airline)) + geom_point(aes(x = duration, y = price, color = airline))+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+ theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))+theme()+ggtitle("Impact of Duration on Price")
ggplotly(p_dur)

# Pricing by Days Left
df %>% 
  plot_ly(x = ~price) %>%
  add_histogram(frame=~days_left)%>%
  layout(title = 'Analyzing Price by Days Left', plot_bgcolor = "#e5ecf6",showlegend=FALSE)

#Average price of ticket GROUPED BY Airlines
df1<-df %>%
  group_by(airline) %>%
  summarise(mean_price = mean(price))
p_air<-ggplot(df1, aes(x=reorder(airline, -mean_price), y=mean_price)) +geom_bar(stat="identity", col="black", fill="green")+xlab("Airlines") + ylab("Avg. Price")+theme(axis.text.x=element_text(angle=90),plot.title = element_text(hjust = 0.5))+geom_text(aes(label = paste("₹",round(mean_price,2))),position = position_stack(vjust = 0.5), col="white")+ggtitle("Average price of ticket by Airlines")
ggplotly(p_air)

# Average Price by Departure time
df5<-df %>%
  group_by(departure_time) %>%
  summarise(mean_price = mean(price))
p_dep<-ggplot(df5, aes(x=reorder(departure_time, -mean_price), y=mean_price)) +geom_bar(stat="identity", col="black", fill="yellow")+xlab("Departure time") + ylab("Avg. Price")+theme(plot.title = element_text(hjust = 0.5))+geom_text(aes(label = paste("₹",round(mean_price,2))),position = position_stack(vjust = 0.5), col="black")+ggtitle("Average price of ticket by Departure Time")
ggplotly(p_dep)

# Average Price by Arrival time
df8<-df %>%
  group_by(arrival_time) %>%
  summarise(mean_price = mean(price))
p_arr<-ggplot(df8, aes(x=reorder(arrival_time, -mean_price), y=mean_price)) +geom_bar(stat="identity", col="black", fill="yellow")+xlab("Arrival time") + ylab("Avg. Price")+theme(plot.title = element_text(hjust = 0.5))+geom_text(aes(label = paste("₹",round(mean_price,2))),position = position_stack(vjust = 0.5), col="black")+ggtitle("Average price of ticket by Arrival Time")
ggplotly(p_arr)

# Average Price by Stops
df6<-df %>%
  group_by(stops) %>%
  summarise(mean_price = mean(price))
p_st<-ggplot(df6, aes(x=reorder(stops, -mean_price), y=mean_price)) +geom_bar(stat="identity", col="black", fill="orange")+xlab("Number of Stops") + ylab("Avg. Price")+theme(plot.title = element_text(hjust = 0.5))+geom_text(aes(label = paste("₹",round(mean_price,2))),position = position_stack(vjust = 0.5), col="black")+ggtitle("Average price of ticket by Number of Stops")
ggplotly(p_st)


# FREQUENCY OVERVIEW
# Frequency of flight based on duration
dur<-ggplot(df, aes(x=duration)) + geom_histogram(binwidth=0.5,color="black", fill="violet")+xlab("Frequency")+ylab("duration")+theme(plot.title = element_text(hjust = 0.5))+ggtitle("Flight Frequency on duration")+geom_density(aes(y=0.5*after_stat(count)))
ggplotly(dur)

# Frequency of flight based on days_left
dyl<-ggplot(df, aes(x=days_left)) + geom_histogram(binwidth=0.5,color="black", fill="magenta")+xlab("Frequency")+ylab("days_left")+theme(plot.title = element_text(hjust = 0.5))+ggtitle("Flight Frequency on Days Left")+geom_density(aes(y=0.5*after_stat(count)))
ggplotly(dyl)

# Frequency of flight based on price
pri<-ggplot(df, aes(x=price)) + geom_histogram(binwidth=200,color="black", fill="purple")+xlab("Frequency")+ylab("price")+theme(plot.title = element_text(hjust = 0.5))+ggtitle("Flight Frequency on Price")+geom_density(aes(y=200*after_stat(count)))
ggplotly(pri)

# Frequency of flight by airline
df2<-df %>%
  group_by(airline) %>%
  summarise(sum_rowcount = sum(rowcount))
fre<-ggplot(df2, aes(x=airline, y=sum_rowcount)) +geom_bar(stat="identity", col="blue", fill="white")+xlab("Airlines") + ylab("Number of Flights")+theme(axis.text.x=element_text(angle=90),plot.title = element_text(hjust = 0.5))+geom_text(aes(label = paste(round((sum_rowcount/sum(sum_rowcount))*100,2),"%")),position = position_stack(vjust = 1.15), col="black")+ggtitle("Frequency of flights by Airlines")
ggplotly(fre)


# FLIGHT DURATION OVERVIEW
# Flight Duration across various departure and arrival time
df9<-df %>%
  group_by(departure_time) %>%
  summarise(total_duration = sum(duration))
t1<-ggplot(df9, aes(area = total_duration, fill = departure_time, label=total_duration)) +geom_treemap() + scale_fill_brewer(palette="Blues")+geom_treemap_text(fontface = "italic", colour = "white", place = "centre")+ggtitle("Flight Duration by Departure Time")
df10<-df %>%
  group_by(arrival_time) %>%
  summarise(total_duration = sum(duration))
t2<-ggplot(df10, aes(area = total_duration, fill = arrival_time, label=total_duration)) +geom_treemap() + scale_fill_brewer(palette="Blues")+geom_treemap_text(fontface = "italic", colour = "white", place = "centre")+ggtitle("Flight Duration by Arrival Time")
ggarrange(t1, t2,ncol = 2, nrow = 1)

# Flight Duration based on Airline
df11<-df %>%
  group_by(airline) %>%
  summarise(total_duration_by_count = round(sum(duration)/sum(rowcount),2))
ggplot(df11, aes(area = total_duration_by_count, fill = airline, label=total_duration_by_count)) +geom_treemap() + scale_fill_brewer(palette="Blues")+geom_treemap_text(fontface = "italic", colour = "white", place = "centre")+ggtitle("Flight Duration by Airlines")


# -------------------------------------------------------------- Inferential Analytics -----------------------------------------------------------------------

#Hypothesis Testing

# One-sided T-test using Average Population Mean for Ticket Price
#H0 (Null Hypothesis): mean sample price  = mean population price of ₹4854 (given population mean)
#H1 (Alternative Hypothesis): mean sample price  != mean population price of ₹4854 (given population mean)
results<-t.test(x=df$price,
                alternative = "two.sided",
                mu=4854)

print(results)

# Since, p-value is less than 0.05, that means we reject the Null Hypothesis.
# Hence, there is a significant difference between the sample mean price with population mean price of ₹4854,
# where the sample mean price (₹5219.848) is greater than that of the population mean price (₹4854)

#---------------------------------------------------------------------------------------------------------------

# Independent T-test for varied departure time
#H0 (Null Hypothesis): Pre-sunset prices >= Post-sunset time prices 
#H1 (Alternative Hypothesis): Pre-sunset prices < Post-sunset time prices 
dep_time_results<-t.test(x=df$price[df$departure_time==c("Early_Morning","Morning","Afternoon")],
                            y=df$price[df$departure_time==c("Evening","Night","Late_Night")],
                            alternative = "less")
print(dep_time_results)

# Since, p-value is greater than 0.05, that means we accept the Null Hypothesis.
# Hence, the prices for tickets Pre-sunset (i.e. Early Morning,Morning and Afternoon) is higher or equal to the
# prices of tickets Post-sunset (Evening, Night and Late Night) while the passenger departures.

#---------------------------------------------------------------------------------------------------------------

# Independent T-test for varied arrival time
#H0 (Null Hypothesis): Pre-sunset prices >= Post-sunset time prices 
#H1 (Alternative Hypothesis): Pre-sunset prices < Post-sunset time prices 
arr_time_results<-t.test(x=df$price[df$arrival_time==c("Early_Morning","Morning","Afternoon")],
                         y=df$price[df$arrival_time==c("Evening","Night","Late_Night")],
                         alternative = "less")
print(arr_time_results)

# Since, p-value is less than 0.05, that means we reject the Null Hypothesis.
# Hence, the prices for tickets Pre-sunset (i.e. Early Morning,Morning and Afternoon) is lesser to the
# prices of tickets Post-sunset (Evening, Night and Late Night) while the passenger arrives.

#---------------------------------------------------------------------------------------------------------------

# One way Anova 
#H0 (Null Hypothesis): All population means are equal
#H1 (Alternative Hypothesis): Not all of the population means are equal 
anova_test_results<-aov(price~airline,data=df) # Comparing price airline wise
summary(anova_test_results)

# Since, p-value is less than 0.05, that means we reject the Null Hypothesis.
# Hence, Price of all Airlines are not significantly same.


# ----------------------------------------------------- Predictive Analytics, Classification & Clustering -----------------------------------------------------------------------

# Regression Analysis
price_model<-lm(price~duration+days_left,
                data=df) 
summary(price_model)
# The Significant Model for Predicting Price would be:
# price= 4819.6004 +43.4959*duration -7.5340*days_left

# Let's say the budget for airfare is 4500 and we want the duration of flight to be 2 hours, how many days earlier, should we book the ticket?
find_days_left<-function(price,Duration){
  days_left= round((4819.6004 +43.4959*Duration - price)/7.5340,2)
  print(paste("You should be booking the ticket ",days_left," days before the departure date."),quote=F)
}
find_days_left(4500,2) 
# Hence, You should be booking the ticket  53.97  days before the departure date.

# Pre- processing Data:

#Creating Empty Columns
df$stop_zero_Dummy<-c() 
df$stop_one_Dummy<-c() 
#Zero Stops
df$stop_zero_Dummy[df$stops =="zero"]<-1
df$stop_zero_Dummy[df$stops == "one" |df$stops == "two_or_more"]<-0
#One Stop
df$stop_one_Dummy[df$stops =="one"]<-1
df$stop_one_Dummy[df$stops == "zero" |df$stops == "two_or_more"]<-0

updated_price_model<-lm(price~duration+days_left+stop_zero_Dummy+stop_one_Dummy,
                     data=df)
summary(updated_price_model)

# The Significant Model for Predicting Price with the inclusion of number of stops would be:
# price= 7338.2588 -15.6204*duration -8.5386*days_left -4286.4288*stop_zero_Dummy-1459.0417*stop_one_Dummy

# ------------------------------------------

# Logistic Regression
model<-glm(price~duration+days_left,data=df)
summary(model)
# price= 4819.6004 +43.4959*duration -7.5340*days_left

# Calculating z score
z_score<-(4819.6004+43.4959*2-7.5340*54)
p_value<-1/(1+exp(-z_score))
p_value # p_value=1

L1<-list("duration"=2,"days_left"=30)
z<-predict(model,L1)
z
# Based on our model, the Price of the ticket would be ₹4680.571 when the duration of flight is 2 hours and 30 days are left for departure.

p<-1/(1+exp(-z))
predict_class=p>0.5
predict_class #The predicted class is loyal because the value is TRUE

# ------------------------------------------

# Linear Discriminant Analysis

model1<-lda(airline~price+duration+days_left,data=df)
model1
# Group means:
# airline   price     duration   days_left
# Air_India 4726.0231 14.693161  25.22848
# AirAsia    300.4250 12.687500  24.50000
# GO_FIRST   667.1000  8.566000  31.20000
# Indigo     387.5667  5.971667  39.16667
# SpiceJet   725.1500 14.585000  23.75000
# Vistara   5505.7651 13.127038  26.12022


D_score<-predict(model1,newdata=df)
D_score
D_score$class

# Predicting a Case with Price of the ticket as ₹6000, Duration as 4.25 hours and Days_left as 6 days
case = list("price"= 6000,"duration" = 4.25, "days_left" = 6)
predict_case1 <- predict(model1,case)$class
predict_case1
# Based on our model and case, the predicted airline for the said parameters is Vistara.

# ------------------------------------------

# Decision Tree 
ct <- ctree(price~days_left, data=df)
plot(ct)

# ------------------------------------------

# Classification

model2 <- rpart(source_city ~ price + duration + days_left, data = df)
model2
# Predicting the source city with price as ₹4500, duration as 3.75 hours and days_left as 18
case <- list("price" = 4500, "duration" = 3.75, "days_left" = 18)
pred_case1 <- predict(model2, newdata = case)
pred_case1
# Based on our case and model, we get the following significance or probabilities for the respective source_city
# Bangalore   Chennai     Delhi  Hyderabad    Kolkata     Mumbai
# 0.4537815 0.2521008 0.1113445 0.05462185 0.04411765 0.08403361
# Hence, Bangalore has the highest probability for our said case.

model3 <- rpart(destination_city ~ price + duration + days_left, data = df)
model3
# Predicting the destination_city with price as ₹4500, duration as 3.75 hours and days_left as 18
pred_case2 <- predict(model3, newdata = case)
pred_case2
# Based on our case and model, we get the following significance or probabilities for the respective destination_city
# Bangalore   Chennai     Delhi  Hyderabad    Kolkata     Mumbai
# 0.04966887 0.6192053 0.2086093 0.06622517 0.02980132 0.02649007
# Hence, Chennai has the highest probability for our said case.

# ------------------------------------------

# K-Means Cluster Analysis
cluster_analysis<-kmeans(df[,c(10,11,12)],3)
fviz_cluster(cluster_analysis, data = df[,c(10,11,12)],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
