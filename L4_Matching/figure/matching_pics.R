
rm(list=ls())



if(Sys.info()[["user"]] == "ttyang" & Sys.info()["sysname"] == "Windows"){
  dirpath_code="C:\\nest\\Dropbox\\RA_research\\SHC_method\\data\\R_code\\"
  dirpath_wdata="C:\\nest\\Dropbox\\RA_research\\SHC_method\\data\\wdata\\"
  dirpath_fig <- "C:\\nest\\Dropbox\\RA_research\\SHC_method\\draft\\figure\\"
}

if(Sys.info()[["user"]] == "yungyu" & Sys.info()["sysname"] == "Darwin"){
  dirpath_code <- "/Users/yungyu/YourPath/Dropbox/RA_research/SHC_method/data/R_code/"
  dirpath_wdata <- "/Users/yungyu/YourPath/Dropbox/RA_research/SHC_method/data/wdata/"
  dirpath_fig <- "/Users/yungyu/YourPath/Dropbox/RA_research/SHC_method/data/figures/"
}

source(paste0(dirpath_code,"1_functions_SHC.R")) # Load Functions


# Load necessary libraries
library(ggplot2)

# Create a mock data frame with 'age', 'family_income', and 'group' as columns
set.seed(24) # for reproducibility
data <- data.frame(
  age = sample(20:80, 50, replace = TRUE),
  family_income = sample(12:28, 50, replace = TRUE),
  group = sample(c("C", "T"), 50, replace = TRUE)
)

# Plot
ggplot(data, aes(x = family_income, y = age, label = group)) +
  geom_text(aes(color = group)) +
  scale_color_manual(values = c("T" = "blue", "C" = "red")) + # Swap colors
  labs(x = 'Family Income', y = 'Age') +
  theme_minimal()


# Load necessary libraries
library(ggplot2)

# Define the number of data points for the random and overlapping parts
n_random <- 45
n_overlap <- 5

# Create a data frame with 'age', 'family_income', and 'group' as columns
set.seed(24) # for reproducibility
data <- data.frame(
  age = c(sample(20:80, n_random, replace = TRUE), rep(45:49, each=2)),
  family_income = c(sample(12:28, n_random, replace = TRUE), rep(16:20, each=2)),
  group = c(sample(c("C", "T"), n_random, replace = TRUE), rep(c("C", "T"), each=n_overlap))
)

# Ensure that the number of points for each group is the same
data <- data[1:(n_random + 2 * n_overlap), ]

# Plot
ggplot(data, aes(x = family_income, y = age, label = group)) +
  geom_text(aes(color = group)) +
  scale_color_manual(values = c("C" = "blue", "T" = "red")) + # Assign new colors
  labs(x = 'Family Income', y = 'Age') +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a mock data frame with 'age', 'family_income', 'group', and 'id' as columns
set.seed(24) # for reproducibility
data <- data.frame(
  age = sample(20:80, 50, replace = TRUE),
  family_income = sample(12:28, 50, replace = TRUE),
  group = sample(c("C", "T"), 50, replace = TRUE),
  id = 1:50
)

# A function to find close points and return a data frame with start and end coordinates for lines
find_close_points <- function(data, max_distance) {
  close_points <- data.frame()
  for (i in 1:(nrow(data)-1)) {
    for (j in (i+1):nrow(data)) {
      if (data$group[i] != data$group[j]) {
        distance <- sqrt((data$family_income[i] - data$family_income[j])^2 + (data$age[i] - data$age[j])^2)
        if (distance <= max_distance) {
          close_points <- rbind(close_points, data.frame(x1 = data$family_income[i],
                                                         y1 = data$age[i],
                                                         x2 = data$family_income[j],
                                                         y2 = data$age[j]))
        }
      }
    }
  }
  return(close_points)
}

# Find close points with a maximum distance of 5 (this value may need adjustment)
close_points <- find_close_points(data, max_distance = 5)

# Plot
ggplot(data) +
  geom_text(aes(x = family_income, y = age, label = group, color = group)) +
  geom_segment(data = close_points, aes(x = x1, y = y1, xend = x2, yend = y2),
               linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("C" = "blue", "T" = "red")) +
  labs(x = 'Family Income', y = 'Age') +
  theme_minimal()
