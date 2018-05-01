library(readxl)
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyverse)
library(scales)

##############################################################
# Time Series & Forecasting Final Project Data Visualization #
##############################################################


# Load the dataset
df <- as.data.frame(read_excel("~/Babson 2017-2018/Spring/Time Series & Forecasting/test.xlsx"))

# Structure/dimension of the dataset
str(df)

# Counting the number of missing values in the dataset
sum(is.na(df))

# Summary by group
tapply(df$tourists, df$region, summary)

################################################################
# Percentage of Total Number of Tourists to Thailand by Region #
################################################################

# Getting the percentage of total tourist by assigning proportion
proportion <- df %>%
  group_by(region) %>%
  tally(tourists) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(y_label = paste0(round(pct*100, 1), "%"))

# Percentage of Total Tourists to Thailand by Region 
ggplot(proportion, aes(x = region, y = pct, fill = region)) +
  geom_bar(stat = "identity", position = "dodge", color = "grey40") +
  geom_text(aes(label = y_label), vjust = -0.5, color = "black") +
  theme_bw() +
  labs(title = "Percentage of Total Tourists to Thailand by Region",
       x = "Region",
       y = "Percent")

####################################  
# Horizontal Barplots by Countries #
####################################

# Getting the proportion by region and nationality 
proportion2 <- df %>%
  group_by(region, nationality) %>%
  tally(tourists) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(y_label = paste0(round(pct*100, 1), "%"))

# Graphing a horizontal barplot by nationality 
ggplot(proportion2, aes(x = reorder(nationality, pct), y = pct, fill = region)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = y_label), hjust = -0.2, color = "black") +
  coord_flip() +
  labs(title = 'Percentage of Total Number of Tourists by Countries',
       x = 'Countries',
       y = 'Number of Tourists') +
  theme_bw()


#########################################################
# Percentage of Total Tourists to Thailand by Countries #
#########################################################

proportion2 %>%
  filter(n > 500000) %>%
  ggplot(aes(x = nationality, y = pct, fill = region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = y_label), size = 3, vjust = 0.5, color = "black") +
  facet_wrap( ~ region, scales = "free") +
  theme(axis.text.x = element_text(colour="grey20", size=10, angle=90, hjust=.5, vjust=.5)) +
  labs(title = "Percentage of Total Tourists to Thailand by Countries",
       subtitle = "Filtered by tourists more than 500,000",
       x = "Countries",
       y = "Percent")

########################
# Heatmap by Countries #
########################
df %>% 
  filter(tourists > 50000) %>%
  ggplot(aes(year, nationality)) +
  geom_tile(aes(fill=tourists), colour = 'white') +
  labs(title = 'Heatmap by Countries',
       subtitle = 'Filtered by tourists more than 50,000',
       x = 'Year',
       y = 'Countries') +
  scale_fill_gradient(low="yellow", high = "black", guide = "colorbar") +
  theme_bw()

