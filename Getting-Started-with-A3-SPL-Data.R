# Getting Started with A3 — SPL Library Checkout Data

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")

# Exercise 1: Load the data
# Download and unzip one or more of the SPL datasets and load here from a file path
spl_df <- read.csv("Cain_Checkouts_by_Title.csv", stringsAsFactors = FALSE)
  
  # Exercise 2: We want to create a new date column ("date") with the month AND year and a default first day of the month
  # Make a new column with checkout month, checkout year, and a default day value ("01") *pasted* together like so: 2020-10-01
  # Then convert that column to a date value
  spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
  spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")
  
  # Exercise 3: Filter using string detect for one of the following authors, or an author of your choice:
  # J.R.R. Tolkien, Colleen Hoover, Jane Austen, George Orwell, Stephen King, Michelle Obama
  author_df <- spl_df %>% 
    filter(str_detect(Creator, "Susan")) %>% 
    filter(str_detect(Creator, "Cain")) 
  
  
  # Exercise 4: Now calculate the sum total checkouts for your author's works for *each month/date*
  
  checkouts_per_month <- author_df %>% group_by(date) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
  
  # Exercise 5: Then plot their checkouts by date, and include a title and x,y axes label
  # If needed, set x and y axis limits
  # Then share in the Discord channel #spl-data
  cain_plot <- ggplot(checkouts_per_month) +
    geom_line(aes(x = date, y = total_checkouts)) +
    scale_y_continuous(limits = c(0, 500)) +
    labs(title = "Susan Cain Library Checkouts from January 2012 to January 2023", 
         x = "Year", 
         y = "Total Checkouts")
  
  ggplotly(cain_plot, tooltip = "text")
  
  