#### Environment setting ####
rm(list=ls())  # Clear all existing objects from the environment

library(brms)      # Load Bayesian regression modeling package
library(data.table)  # Load fast and efficient data manipulation package

setwd("C:/FERT-master/FER-T 1.0/data")  # Set working directory to the data folder

#### List all CSV files in the folder ####
csv_files <- list.files(pattern = "\\.csv$")  # Identify all CSV files in the directory

#### Data Loading ####
numIter <- 20000  # Number of MCMC iterations for Bayesian estimation
set.seed(108)     # Set seed for reproducibility

#### Initialize a data table to store results ####
results <- data.frame(Participant = character(), Theta = numeric(), stringsAsFactors = FALSE)

#### Loop through each file ####
for (file_name in csv_files) {

  # Extract the participant's ID from the filename
  participant_name <- substr(file_name, 1, 5) # Assumes ID consists of the first 5 characters

  # Load the data from the current CSV file
  rawdata <- fread(file_name)

  # Extract relevant columns: stimulus item and participant response
  data <- data.table(substr(rawdata$Stimulus, 9, 14), rawdata$key_resp_2.keys)
  setnames(data, c("Item", "Answer"))

  # Convert numeric responses to categorical emotion labels
  data[, Answer := as.character(Answer)]
  data[Answer == "1", Answer := "ANG"]
  data[Answer == "2", Answer := "DIS"]
  data[Answer == "3", Answer := "FEA"]
  data[Answer == "4", Answer := "HAP"]
  data[Answer == "5", Answer := "SAD"]
  data[Answer == "6", Answer := "SUR"]

  # Mark responses as Correct (TRUE) or Incorrect (FALSE)
  data[, Correct := substr(Item, 3, 5) == Answer]

  #### Item Parameter Definitions ####

  # Define stimulus items and their corresponding IRT parameters (Alpha & Beta)
  Item <- c("ARANG2", "ARDIS3", "ARFEA2", "ARHAP2", "ARSAD3", "ARSUR2", 
            "EBANG1", "EBDIS2", "EBFEA3", "EBHAP3", "EBSAD3", "EBSUR3", 
            "FFANG3", "FFDIS2", "FFFEA2", "FFHAP3", "FFSAD1", "FFSUR2", 
            "FGANG2", "FGDIS2", "FGFEA2", "FGHAP3", "FGSAD1", "FGSUR3", 
            "LDANG3", "LDDIS3", "LDFEA3", "LDHAP2", "LDSAD3", "LDSUR2", 
            "MGANG1", "MGDIS2", "MGFEA3", "MGHAP3", "MGSAD2", "MGSUR1")

  # Discrimination parameters (Alpha): Measure how well an item differentiates between individuals
  Alpha <- c(0.4651, 0.3005, 0.7169, 0.8780, 0.5474, 0.6123, 0.6064,
             0.2136, 0.7794, 1.4335, 0.5459, 0.4452, 1.6947, 0.4810, 0.5903,
             0.8826, 0.5491, 0.8933, 0.8345, 0.8804, 0.2824, 0.6313, 0.6104,
             0.4571, 0.7794, 0.5261, 0.8495, 0.6296, 0.4887, 0.6038, 1.4067,
             0.5214, 1.0957, 0.7295, 0.6006, 0.3117)

  # Difficulty parameters (Beta): Reflect how difficult each item is
  Beta <- c(-3.3289, -3.9263, -3.8320, -4.1036, -1.4685, -3.7313, -1.1982,
            0.3763, 0.3876, -4.7480, -4.7722, -6.2939, -3.3452, -6.4326, 0.4887,
            -5.8279, -1.7127, -3.8558, -2.8657, -4.3872, 0.1057, -6.1967, -3.4558,
            -3.0521, -2.8014, -4.6856, -0.3597, -3.4142, 0.2852, -3.6963, -0.9332,
            -3.5789, -1.4791, -5.4119, 0.1957, -6.5352)

  # Merge item parameters into the dataset
  Itempars <- data.table(Item, Alpha, Beta)
  data <- merge(data, Itempars, by = "Item")

  #### Prepare data for Bayesian IRT Model ####
  stan_data <- data.table(Correct = as.integer(data$Correct), 
                          Alpha = data$Alpha, 
                          Beta = data$Beta)

  #### Fit Bayesian 2PL Model Using `brms` ####
  twopl.fit <- brm(
  bf(Correct ~ Alpha * (theta - Beta), theta ~ 1, nl = TRUE),  
  data = stan_data,
  family = bernoulli(),
  prior = c(prior(normal(0,1), class = "b", nlpar = "theta")),
  iter = numIter, chains = 4,
  )

  #### Extract Estimated Ability Score (θ) ####
  stanfit <- as_draws_df(twopl.fit)
  theta_estimate <- mean(stanfit$b_theta_Intercept)

  #### Store the participant's Score ####
  results <- rbind(results, data.table(Participant = participant_name, Theta = theta_estimate))
}

#### Compute Mean Theta for All Participants ####
mean_theta <- mean(results$Theta)
results <- rbind(results, data.table(Participant = "MEAN", Theta = mean_theta)) # Append mean value

#### Save Results to CSV ####
output_file_path <- file.path("C:/FERT-master/FER-T 1.0/", "Theta_Scores.csv")
fwrite(results, output_file_path, row.names = FALSE)

print("Mass scoring complete! Results saved to Theta_Scores.csv")
