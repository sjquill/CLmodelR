# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2009-2010
# Youth justice annual statistics: 2009 to 2010, Workload tables, Regional Tables
# Sheet R.1: Disposals given to young people, by region, 2009/10

# getting col names
disposal_data <- read_xls("/Users/katehayes/temp_data/New Tables for Website/Regional tables.xls", sheet = 2, skip = 3, n_max = 1, col_names = TRUE)

not_all_na <- function(x) any(!is.na(x))

disposal_data <- disposal_data %>%
  select(where(not_all_na)) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "group_value",
               names_transform = as.character, values_transform = as.character) %>%
  mutate(group = replace(group, grepl('[0-9]', group), NA)) %>%
  mutate(group = vec_fill_missing(group, direction = c("down")))

col_names <- paste(unlist(disposal_data[, 1]), unlist(disposal_data[, 2]), sep = "_")
col_names = c("disposal", col_names)

# now bringing in the data
disposal_data <- read_xls("/Users/katehayes/temp_data/New Tables for Website/Regional tables.xls", sheet = 2, skip = 340, n_max = 32, col_names = FALSE)
disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_types <- c("Pre-court", "First-tier", "Community", "Custody")

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "0910")

disposal0910_data <- disposal_data

# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2010-2011
# Youth justice annual statistics: 2010 to 2011, Disposals, regionally, 2010 - 11
# Sheet R.1: Disposals given to young people, by region, 2009/10

disposals-regionally-2010-11.xls
