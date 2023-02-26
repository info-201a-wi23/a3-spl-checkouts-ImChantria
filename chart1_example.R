# Line graph showing the trend of physical and digital checkouts of all Susan Cain books from 2012 to 2023

# Load libraries
library("ggplot2")
library("dplyr")

# Load data
spl_df <- read.csv("Cain_Checkouts_by_Title.csv", stringsAsFactors = FALSE)

# Create date column
spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

#Filter author
author_df <- spl_df %>% 
  filter(str_detect(Creator, "Susan")) %>% 
  filter(str_detect(Creator, "Cain")) 

# Filter for physical and digital usage class

digital_physical <- author_df %>% 
  filter(UsageClass %in% c("Physical", "Digital"))

# Group by date and usage class and calculate the total checkouts
author_sum <- author_df %>% 
  group_by(date, UsageClass) %>% 
  summarize(Checkouts = sum(Checkouts))

# Create line plot
ggplot(author_sum, aes(x = date, y = Checkouts, color = UsageClass)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 300)) +
  labs(x = "Year", y = "Total Checkouts", title = "Physical vs Digital Library Checkouts for all Susan Cain Books from 2012 to 2023")
