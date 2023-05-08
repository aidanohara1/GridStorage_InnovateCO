#Hedy

# Load the tidyverse package
library(tidyverse)


# Read the CSV file
data <- read_csv("Colorado-Project/Colorado_State_Agency_Renewable_Energy_Use_FY15_-_FY21.csv")


# View the first few rows of the data
head(data)


#Rename columns
data <- data %>%
  rename(
    Agency = Agency,
    TotalKWh = Total_kWh,
    RenewableKWh = Renewable_kWh,
    PercentRenewables = Percent_Renewables
  )


#Filter rows
data <- data %>%
  filter(TotalKWh > 0) # Keep rows where TotalKWh is greater than 0

data <- data %>%
  mutate(
    RenewablePercentage = RenewableKWh / TotalKWh * 100 # Calculate the percentage of renewables
  )

data <- data %>%
  arrange(desc(TotalKWh)) # Sort the data frame by TotalKWh in descending order


#Save the cleaned data to a new CSV file
write_csv(data, "cleaned_data_colorado.csv")


#Read cleanede new data
new_data <- read_csv("cleaned_data_colorado.csv")
head(new_data)


#Split the data into training and testing sets
install.packages("caTools")
library(caTools)

set.seed(123) # Set a random seed for reproducibility

sample_size <- floor(0.8 * nrow(new_data)) # Use 80% of the data for training
train_indices <- sample(seq_len(nrow(new_data)), size = sample_size)

train_data <- new_data[train_indices, ] # Extract the training data
test_data <- new_data[-train_indices, ] # Extract the testing data


#Create the linear regression model:
model <- lm(PercentRenewables ~ TotalKWh + RenewableKWh, data = train_data)
summary(model)
#we'll create a linear regression model to predict the PercentRenewables based on the TotalkWh and RenewablekWh features


#Evaluate the model
predictions <- predict(model, newdata = test_data)

# Calculate the mean squared error (MSE)
mse <- mean((test_data$PercentRenewables - predictions)^2)
cat("Mean Squared Error (MSE):", mse)

# Calculate the root mean squared error (RMSE)
rmse <- sqrt(mse)
cat("\nRoot Mean Squared Error (RMSE):", rmse)
#According to Mean Squared Error (MSE): 0.005562974 and RMSE of 0.07458534 might indicate a good model fit





