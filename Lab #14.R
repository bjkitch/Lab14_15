####### Lab #14 - Interpolation###########
###### Brandon Kitch #############
##### AGR 333 #################
#
#Purpose: 
#         
#         
#
#Required Files:
# 
#
########### Required Packages ###############

getwd() #get current working directory
setwd("/Users/brandonkitch/Desktop/Class Files/AGR 333 - Spring 2025/Labs/Lab #14 & #15") #set new working directory

# Load libraries
library(ggplot2)
library(gridExtra)

# Import WASDE data
WASDE <- read.csv("WASDE.csv")
  
# Explore the dataset 
head(WASDE)
str(WASDE)

# Plot corn prices, total corn supply, and total corn demand over time
g_price <- ggplot(data = WASDE, aes(x = year, y = corn_price)) +
  geom_line(color = "Red") +
  ggtitle("Corn Prices over Time") +
  labs(y = "Corn Price ($)", x = "Year")
  
g_demand <- ggplot(data = WASDE, aes(x = year, y = total_use)) +
  geom_line(color = "Blue") +
  ggtitle("Corn Demand over Time") +
  labs(y = "Use (Mil. Bu.)", x = "Year")
  
g_supply <- ggplot(data = WASDE, aes(x = year, y = total_supply)) +
  geom_line(color = "green") +
  ggtitle("Corn Supply over Time") +
  labs(y = "Supply (Mil. Bu.)", x = "Year")
  
grid.arrange(g_price, g_demand, g_supply, nrow=3)

# Create a new variable for stock-to-use ratio and create a scatterplot of corn prices on stock-to-use ratio
WASDE$SUR <- (WASDE$total_supply/WASDE$total_use) -1

ggplot(data = WASDE, aes(x = SUR, y = corn_price)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "Red") +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio") +
  labs(y = "Corn Price ($)", x = "Stock-to-Use Ratio")

reg1 <- lm(corn_price ~ SUR, data = WASDE)
summary(reg1)

# Calculate averages
mean_sur <- mean(WASDE$SUR)
mean_price <- mean(WASDE$corn_price)

# Summary statistics of residuals
summary(resid(reg1))

# Histogram of residuals
hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals")

# Scatterplot of errors vs SUR
ggplot(data=WASDE, aes(x=SUR, y=resid(reg1))) +
  geom_point(shape = 1) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio") +
  labs(y = "Corn Price ($)", x = "Stock-to-Use Ratio")

# Create the inverse of stock-to-use ratio, run the regression, and examine the error terms
WASDE$SUR_Inv <- 1 / WASDE$SUR
reg2 <- lm(corn_price ~ SUR_Inv, data=WASDE)

summary(reg2)

# Residual analysis
summary(resid(reg2))
hist(resid(reg2), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

# Residuals vs SUR plot
ggplot(data=WASDE, aes(x=SUR, y=resid(reg2))) +
  geom_point(shape=1) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")

WASDE$Y2006 <- ifelse(WASDE$year >= 2006, 1, 0)

# Run regression with interaction term
reg3 <- lm(corn_price ~ SUR + Y2006 + SUR:Y2006, data = WASDE)

# Print regression summary
summary(reg3)