# XYZ-supermarket-sales-analysis

# install.packages("readr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("dylyer")
# install.packages("data.table")

library(readr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(data.table)

#Import the data

store <- read_csv("Data/Sample - Superstore.csv")

#Lets check for missing values

missing_data = store[!complete.cases(store),]

#there are no missing data in this dataset
#lets check the structure of the data

missing_data = store[!complete.cases(store),]

#The order date and ship date are in character data type so i will chnage it to date 
#Sample data with mixed date formats

store$`Ship Date` <- as.Date(store$`Ship Date`,format = "%m/%d/%y")
store$`Order Date` <- as.Date(store$`Order Date`,format = "%m/%d/%y")

#SALES TRENDS: 
#Group and summarize sales data by month
monthly_sales <- store %>%
  group_by(YearMonth = format(`Order Date`, "%Y-%m")) %>%
  summarise(TotalSales = sum(Sales))

ggplot(data = monthly_sales, aes(x = YearMonth, y = TotalSales)) +
  geom_bar(stat = "identity") +
  labs(title = "Monthly Sales Trend (Order Date)",
       x = "Year-Month",
       y = "Total Sales") +
  theme_minimal()


#question 2 
#Product or Service Performance:
cat_sales <- store %>%
  group_by (Category) %>%
  summarise(cat_sales = sum(Sales)) %>%
  arrange(desc(cat_sales))
subcat_sales <- store %>%
  group_by (`Sub-Category`) %>%
  summarise(cat_sales = sum(Sales)) %>%
  arrange(desc(cat_sales))

ggplot(cat_sales, aes(x = Category, y = cat_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Category-Level Sales", y = "Category sales", x = "Types of Products Category") + 
  scale_y_continuous(labels = scales::comma_format(scale = 1e+3))


ggplot(subcat_sales , aes(x = reorder(`Sub-Category`, -cat_sales), y = cat_sales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Total Sales by subcategory", x = "Sub categories", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::comma_format(scale = 1e+3))

top_customers_by_city <- store %>%
  group_by(City, `Customer ID`,) %>%
  summarise(TotalSales = sum(Sales)) %>%
  arrange(City, desc(TotalSales)) %>%
  group_by(City) %>%
  top_n(10, wt = TotalSales)


region_sales <- store %>% 
  group_by(Region) %>% 
  summarise(region_sales = sum(Sales)) %>%
  arrange(desc(region_sales))

ggplot(region_sales, aes(x = Region, y = region_sales)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Sales by Region", y = "Total Sales", x = "Region") +
  theme_minimal()  + 
  scale_y_continuous(labels = scales::comma_format(scale = 1e+3))


city_sales<- store %>%
  group_by(City)%>%
  summarise(total_sales = sum(Sales))%>%
  arrange(desc(total_sales))  %>%
  slice(1:10)
  
ggplot(city_sales, aes(x = reorder(City, -total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by City (Top 10)", x = "City", y = "Total Sales") +
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1)  # Format labels without scientific notation
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))#otate x-axis labels for better readability


state_sales<- store %>%
  group_by(State)%>%
  summarise(total_sales = sum(Sales)) %>%
  arrange(desc(total_sales)) 

ggplot(state_sales, aes(x = reorder(State, -total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by City (Top 10)", x = "State", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

state_avg_sales <- store %>%
  group_by(State) %>%
  summarise(average_sales = mean(Sales)) %>%
  arrange(desc(average_sales)) 

ggplot(state_avg_sales, aes(x = reorder(State, -average_sales), y = average_sales)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Sales by Region", x = "Region", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

california_customers <- store %>%
  filter(State == "California") %>%
  summarise(total_customers = n())
##customers are 2001
newyork_customer <- store %>%
  filter(State == "New York")  %>%
  summarise(total_customers = n())
#customers are 1128
Wyoming_customer <- store %>%
  filter(State == "Wyoming")  %>%
  summarise(total_customers = n())
## 1 customer
Vermont_customer <- store %>%
  filter(State == "Wyoming")  %>%
  summarise(total_customers = n())

state_avg_profit <- store %>%
  group_by(State) %>%
  summarise(average_sales = mean(Profit)) %>%
  arrange(desc(average_sales))

state_qunt <- store %>%
  group_by(State) %>%
  summarise(quantity = sum(Quantity)) %>%
  slice(1:10)
  

discount_sales <- store %>%
  group_by(State)  %>%
  summarise(discount = mean(Discount))  %>%
  arrange(desc(discount))

max(store$Profit)  ###8399.976
min(store$Profit)   ###-6599.978
##lets see the total profit
sum(store$Profit)

correlation_matrix <- store %>%
  select(Sales, Quantity, Discount,Profit)  %>%
  cor()
correlation_matrix

heatmap_plot <- ggplot(data = reshape2::melt(correlation_matrix), 
                       aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(size = 3, color = "white", show.legend = FALSE) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap",
       x = "Variables", y = "Variables",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heatmap_plot
