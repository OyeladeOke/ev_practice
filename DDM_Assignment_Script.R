#######################################################
#Section 1 - Load and call the necessary libraries
#######################################################

install.packages("psych")
library(psych)
library(skimr)
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)


# Set the working directory

setwd("C:/Users/user/Desktop/BU_Lectures/Data_Driven_Management/Assignment/DDM_Assignment/Data_File")
getwd() ### Re-confirm working directory has been set

################################################### 
# Section 2.0 - Data Preparation and Transformation
###################################################

# 2.1 Reading and writing files in R. 

EV_Data <- read_excel("ev_06.xlsx")

# 2.2 Data Restriction and skimming for missing values (Note some variables were re-named)

EV_Data <- EV_Data %>%
  select(product, buyer, channel = campaign, commission_earned = commissions, marketing_spend = marketing,period) 

skim(EV_Data)

# 2.3 Removed missing values as they are insignificant

EV_Data <- EV_Data %>%
  na.omit()

skim(EV_Data)

##################################
# 3.0 Creating variables and KPIs
##################################

# 3.1 Created new variables (month = month name and ROI = Return on Investment) 

EV_Data <- EV_Data %>%
  mutate(period = ceiling(period)) %>%
  mutate(month = case_when(period == 1 ~ "Jan",
                           period == 2 ~ "Feb",
                           period == 3 ~ "Mar",
                           period == 4 ~ "Apr",
                           period == 5 ~ "May",
                           period == 6 ~ "Jun",
                           period == 7 ~ "Jul",
                           period == 8 ~ "Aug",
                           period == 9 ~ "Sep",
                           period == 10 ~ "Oct",
                           period == 11 ~ "Nov",
                           period == 12 ~ "Dec",
                           TRUE ~ "Unknown"  )) %>%
  mutate(ROI = (commission_earned / marketing_spend))

# Sort month in Jan to Dec order
EV_Data$month <- factor(EV_Data$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                  "Aug", "Sep", "Oct", "Nov", "Dec"))



##### 3.2 Descriptive statistics of dataset (EV_Data)

DS <- describe(EV_Data)
print(DS)


############################
# Creating KPIs
############################

# 3.3 Total Marketing Spend by marketing channels

Total_Mkting_Spend <- EV_Data %>%
  group_by(channel) %>%
  summarise(Total_Mkting_Spend = sum(marketing_spend))

# Determine the channel with the highest marketing spend
channel_highest_spend <- Total_Mkting_Spend$channel[which.max(Total_Mkting_Spend$Total_Mkting_Spend)]

# 3.4 Total Commission Earned by marketing channels

Total_Comm_Earned <- EV_Data %>%
  group_by(channel) %>%
  summarise(Total_Comm_Earned = sum(commission_earned))

# Determine the channel with the highest commission earned
channel_highest_comm_earned <- Total_Comm_Earned$channel[which.max(Total_Comm_Earned$Total_Comm_Earned)]

# 3.5 Average Marketing Spend and Average Commission Earned by channels

  KPI_Avg_Channel <- EV_Data %>%
  group_by(channel) %>%
  summarise(Avg_Comm_Earned = mean(commission_earned),
  Avg_Mkting_Spend = mean(marketing_spend))

print(KPI_Avg_Channel)

# 3.6 Average ROI across marketing channels

Avg_ROI_by_Channel <- EV_Data %>% 
  group_by(channel) %>%
  summarize(avg_ROI = mean((commission_earned / marketing_spend) * 100, na.rm = TRUE))

print(Avg_ROI_by_Channel)

# 3.7 Total Commission earned by month

Comm_Earned_by_month <- EV_Data %>%
  group_by(month) %>%
  summarize(total_comm_earned = sum(commission_earned, na.rm = TRUE)) %>%
  arrange(month)

# Determine the month with the highest commission earned
month_highest_earned <- Comm_Earned_by_month$month[which.max(Comm_Earned_by_month$total_comm_earned)]

# 3.8 Total Marketing Spend by month

Marketing_Spend_by_month <- EV_Data %>%
  group_by(month) %>%
  summarize(total_mkting_spend = sum(marketing_spend, na.rm = TRUE)) %>%
  arrange(month)

# Determine the month with the highest marketing spend
month_highest_spend <- Marketing_Spend_by_month$month[which.max(Marketing_Spend_by_month$total_mkting_spend)]

## Merge 3.7 and 3.8 dataframes
Combined_Data <- merge(Comm_Earned_by_month, Marketing_Spend_by_month, by = "month")

# 3.9 Average ROI by month

Avg_ROI_by_Month <- EV_Data %>% 
  group_by(month) %>%
  summarize(avg_ROI = mean((commission_earned / marketing_spend) * 100, na.rm = TRUE))

print(Avg_ROI_by_Month)

# 3.8 Total Marketing Spend by Buyers Category

Marketing_Spend_Buyer <- EV_Data %>%
  group_by(buyer) %>%
  summarize(Marketing_Spend_Buyer = sum(marketing_spend))
  print(Marketing_Spend_Buyer)
  
# Determine the buyer category with the highest marketing spend
buyer_highest_spend <- Marketing_Spend_Buyer$buyer[which.max(Marketing_Spend_Buyer$Marketing_Spend_Buyer)]

# 3.9 Total Commission Earned by Buyers Category

Comm_Earned_Buyer <- EV_Data %>%
  group_by(buyer) %>%
  summarize(Comm_Earned_Buyer = sum(commission_earned))

# Determine the buyer category with the highest commission earned
buyer_highest_Comm_Earned <- Comm_Earned_Buyer$buyer[which.max(Comm_Earned_Buyer$Comm_Earned_Buyer)]

# 3.10 Average ROI by Buyers Category

Avg_ROI_by_Buyer <- EV_Data %>% 
  group_by(buyer) %>%
  summarize(avg_ROI = mean((commission_earned / marketing_spend) * 100, na.rm = TRUE))

print(Avg_ROI_by_Buyer)


# 3.11 Total Marketing Spend by Product Type

Marketing_Spend_Product <- EV_Data %>%
  group_by(product) %>%
  summarize(Marketing_Spend_Product = sum(marketing_spend))

# Determine the product with the highest marketing spend
product_highest_spend <- Marketing_Spend_Product$product[which.max(Marketing_Spend_Product$Marketing_Spend_Product)]

# 3.12 Total Commission Earned by Product Type

Comm_Earned_Product <- EV_Data %>%
  group_by(product) %>%
  summarise(Comm_Earned_Product = sum(commission_earned))

# Determine the product with the highest commission earned
product_highest_Comm_Earned <- Comm_Earned_Product$product[which.max(Comm_Earned_Product$Comm_Earned_Product)]

# 3.13 Average ROI by Product Type

Avg_ROI_by_Product <- EV_Data %>% ## Present as a table in report
  group_by(product) %>%
  summarize(avg_ROI = mean((commission_earned / marketing_spend) * 100, na.rm = TRUE))

print(Avg_ROI_by_Product)

########################
# 4.0 Visualizations
########################

# Plot 1 - Relationship between Marketing Spend & Commission Earned

P1 <- ggplot(Combined_Data, aes(x = month, group = 1)) +
  geom_line(aes(y = total_comm_earned, color = "Comm. Earned"), size = 1.2) +
  geom_line(aes(y = total_mkting_spend, color = "Marketing Spend"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = total_comm_earned), color = "darkgrey", size = 3) +  
  geom_point(aes(y = total_mkting_spend), color = "darkgreen", size = 3) +  
  scale_color_manual(values = c("Comm. Earned" = "darkgrey", "Marketing Spend" = "darkgreen")) +
  labs(title = "Clear and positive relationship between Marketing Spending & Commission Earnings", subtitle = "(monthly trend analysis)",
       x = "Month",
       y = "Amount",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(face = "bold", size = 11),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))
P1

# Plot 2 - Top Channel by Marketing Spend

P2 <- ggplot(Total_Mkting_Spend, aes(x = channel, y = Total_Mkting_Spend, fill = channel == channel_highest_spend)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  
  labs(title = "Increase in targeted marketing spending in tiktok",
       x = "Marketing Channels",
       y = "Marketing Spend",
       fill = "Channel with Highest Spend") +
  theme_bw() +  
  theme(panel.background = element_rect(fill = "white", color = NA),  
        panel.border = element_blank(), 
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  guides(fill = FALSE) +  
  scale_y_continuous(limits = c(0, 5000000), breaks = seq(0, 5000000, by = 1000000)) 
P2

# Plot 3 Top Channel by Commission Earned

P3 <- ggplot(Total_Comm_Earned, aes(x = channel, y = Total_Comm_Earned, fill = channel == channel_highest_comm_earned)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  
  labs(title = "Leads to corresponding increase in Commission Earnings in tiktok",
       x = "Marketing Channels",
       y = "Commission Earned",
       fill = "Channel with Highest Comm. Earned") +
  theme_bw() +  
  theme(panel.background = element_rect(fill = "white", color = NA),  
        panel.border = element_blank(), 
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  guides(fill = FALSE) +  
  scale_y_continuous(limits = c(0, 4000000), breaks = seq(0, 4000000, by = 500000)) 
P3

# Plot 4 Top Marketing Spending Allocation by Buyer Demographic

P4 <- ggplot(Marketing_Spend_Buyer, aes(x = buyer, y = Marketing_Spend_Buyer, fill = buyer == buyer_highest_spend)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  
  labs(title = "Demographic allocated the most Marketing Spending",
       x = "Buyer Demographics",
       y = "Marketing Spending",
       fill = "Buyer Category with Highest Spend") +
  theme_bw() +  
  theme(panel.background = element_rect(fill = "white", color = NA),  
        panel.border = element_blank(), 
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  guides(fill = FALSE) +  
  scale_y_continuous(limits = c(0, 5500000), breaks = seq(0, 5500000, by = 1000000)) 
P4


# Plot 5 Top Commissioned Earned by Buyer Demographic

P5 <- ggplot(Comm_Earned_Buyer, aes(x = buyer, y = Comm_Earned_Buyer, fill = buyer == buyer_highest_Comm_Earned)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  
  labs(title = "Targeted increase in Couple marketing spending, leads to increase 
                commission earnings in Couple demographic",
       x = "Buyer Demographic",
       y = "Commission Earned",
       fill = "Buyer Category with Highest Comm Earned") +
  theme_bw() +  
  theme(panel.background = element_rect(fill = "white", color = NA),  
        panel.border = element_blank(), 
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  guides(fill = FALSE) +  
  scale_y_continuous(limits = c(0, 4500000), breaks = seq(0, 4500000, by = 500000)) 
P5


# Plot 6 Top Marketing Spending by Product

P6 <- ggplot(Marketing_Spend_Product, aes(x = product, y = Marketing_Spend_Product, fill = product == product_highest_spend)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  
  labs(title = "Product allocated the most Marketing Spending",
       x = "Product category",
       y = "Marketing Spend",
       fill = "Product Category with Highest Spend") +
  theme_bw() +  
  theme(panel.background = element_rect(fill = "white", color = NA),  
        panel.border = element_blank(), 
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  guides(fill = FALSE) +  
  scale_y_continuous(limits = c(0, 8500000), breaks = seq(0, 8500000, by = 1000000)) 
P6


# Plot 7 Top Commission Earned by Product

P7 <- ggplot(Comm_Earned_Product, aes(x = product, y = Comm_Earned_Product, fill = product == product_highest_Comm_Earned)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  
  labs(title = "Targeted increase in Sedan marketing spending, leads to increase in 
                commission earnings in Sedan subcategory",
       x = "Product Category",
       y = "Commission Earned",
       fill = "Product Category with Highest Comm Earned") +
  theme_bw() +  
  theme(panel.background = element_rect(fill = "white", color = NA),  
        panel.border = element_blank(), 
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  guides(fill = FALSE) +  
  scale_y_continuous(limits = c(0, 7000000), breaks = seq(0, 7000000, by = 500000)) 
P7

# Plot 8 Month with Highest Marketing Spend

P8 <- ggplot(Marketing_Spend_by_month, aes(x = month, y = total_mkting_spend, fill = month == month_highest_spend)) +
  geom_col() +
  coord_flip() +
  labs(title = "Month with Highest Marketing Spend",
       x = "Month",
       y = "Marketing Spend") +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0, 2500000), breaks = seq(0, 2500000, by = 500000)) +  
  guides(fill = FALSE)  

P8

# Plot 9 Month with highest Commission Earned

P9 <- ggplot(Comm_Earned_by_month, aes(x = month, y = total_comm_earned, fill = month == month_highest_earned)) +
  geom_col() +
  labs(title = "Seasonality increase in marketing spending in Dec, leads to increase in 
                commission earnings in Dec",
       x = "Months",
       y = "Commission Earned") +
  scale_fill_manual(values = c("FALSE" = "darkgrey", "TRUE" = "darkgreen")) +  # Set colors for fill
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(face = "bold", size = 9),  
        axis.text.y = element_text(face = "bold", size = 9),  
        axis.title = element_text(face = "bold", size = 11),  
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0, 2100000), breaks = seq(0, 2100000, by = 200000))+
  guides(fill = FALSE)
P9


####################
# 5.0  Dashboard
####################

# Grid plot
Dashboard <- grid.arrange(P1, P2, P3, P5, P7, P9)

# Save the grid plot 
ggsave("grid_plot.png", Dashboard, width = 18, height = 10)


