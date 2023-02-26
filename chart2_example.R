#Bar plot for A3

#Load libraries
library("ggplot2")
library("plotly")
library("dplyr")
library("tidyr")

#Load data
spl_df <- read.csv("Cain_Checkouts_by_Title.csv", stringsAsFactors = FALSE)

author_df <- spl_df %>% 
  filter(str_detect(Creator, "Susan")) %>% 
  filter(str_detect(Creator, "Cain")) 


diff_books <- author_df %>% 
  group_by(Title) %>% 
  summarize(count = n())

#Combine data between two books
combined_books <- diff_books %>%
  mutate(Book = ifelse(str_detect(Title, "Quiet"), "Quiet",
                       ifelse(str_detect(Title, "Bittersweet"), "Bittersweet", NA))) %>%
  drop_na(Book) %>%
  group_by(Book) %>%
  summarize(count = sum(count))

#Plot
colors <- c("royalblue", "maroon")

ggplot(combined_books, aes(x = Book, y = count, fill = Book)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(x = "Book", y = "Number of Checkouts", 
       title = "Checkout Count for Susan Cain's Books from January 2012 to January 2023")
