library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(stringr)

# Save the current working directory
original_wd <- getwd()

# load in files 
setwd("data")
file.list <- list.files(pattern = '*.xlsx')
df.list <- lapply(file.list, read_excel)
text_df <- bind_rows(df.list, .id = "id")

# return to original working directory and read in groupings
setwd(original_wd)
group <- read_xlsx("Message Groupings.xlsx")

# join data and groups for analysis using fuzzy join
ci_str_detect <- function(x, y){str_detect(x, regex(y, ignore_case = TRUE))} #ignore the case when matching
text_join <- fuzzy_left_join(text_df, match_fun = ci_str_detect, group, by= c("Body" = "message")) 

# replace na with "other" in text join
text_join$group_name <- text_join$group_name %>% replace_na('other')

## Analysis

#Which group of texts has highest frequency, can we shorten certain texts
#Number of segments by group 
#Average price by group


# view groups by number of segments
analysis <- text_join %>%
  group_by(group_name, NumSegments) %>%
  summarise(count_numsegments = n(), total_price = sum(Price)) %>%
  arrange(group_name, NumSegments)

# visualize bar (send to Clara)
bar <- ggplot(analysis, aes(x = NumSegments, y = count_numsegments)) +
  geom_bar(stat="identity") +
  facet_wrap(~group_name) +
  theme_classic()

ggsave("fig/rc_text_bar.png", plot = bar)

# visualize scatter (send to Clara)
scatter <- ggplot(analysis, aes(x = NumSegments, y = count_numsegments, group=group_name, color=group_name, size = count_numsegments)) +
  geom_point() +
  xlab("Number of Segments") +
  ylab("Count") +
  theme_classic()

ggsave("fig/rc_text_scatter.png", plot = scatter)


# text analysis

# Extract "No. Units Affected:" from each string when it includes
text_join$num_units <- str_extract(text_join$Body, "No. Units Affected: \\d+")

# Calculate the % of "NA" values in the "FYSA Notification" rows
text_join %>% 
  filter(group_name == "FYSA Notification") %>%
  summarise_each(funs(100 * mean(is.na(.))), num_units)
