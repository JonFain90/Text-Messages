library(tidyverse)
library(readxl)
library(fuzzyjoin)

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

# visualize
ggplot(analysis, aes(x = NumSegments, y = count_numsegments)) +
  geom_bar(stat="identity") +
  facet_wrap(~group_name)
