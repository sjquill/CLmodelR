
not_all_na <- function(x) any(!is.na(x))

clean_disposal <- function(disposal_data, col_names, disposal_types, current_year, current_level) {

  disposal_data <- disposal_data %>%
    select(where(not_all_na))

  names(disposal_data) <- col_names

  disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
    mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
    mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
    mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
    filter(!is.na(`Age_15`)) %>%
    mutate(year = current_year, level = current_level)

}

disposal_data1 <- clean_disposal(disposal_data = disposal_data1,
                                 col_names = col_names1,
                                 disposal_types = disposal_types1,
                                 current_year = "2009-10",
                                 current_level = "west_midlands")



# assign(paste("disposal_data",  current_year, current_level, sep = ""), )


# SUMMARY of what we have for each year

# 2009/10 -
# 2010/11
# 2011/12
# 2012/13
# 2013/14
# 2014/15
# 2015/16
# 2016/17
# 2017/18
# 2018/19
# 2019/20
# 2020/21





# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2009-2010
# Youth justice annual statistics: 2009 to 2010, Workload tables, Regional Tables
# Sheet R.1: Disposals given to young people, by region, 2009/10
# WEST MIDS LEVEL

# getting col names
disposal_data1 <- read_xls("/Users/katehayes/temp_data/New Tables for Website/Regional tables.xls", sheet = 2, skip = 3, n_max = 1, col_names = TRUE)

disposal_data1 <- disposal_data1 %>%
  select(where(not_all_na)) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "group_value",
               names_transform = as.character, values_transform = as.character) %>%
  mutate(group = replace(group, grepl('[0-9]', group), NA)) %>%
  mutate(group = vec_fill_missing(group, direction = c("down")))

col_names1 <- paste(unlist(disposal_data1[, 1]), unlist(disposal_data1[, 2]), sep = "_")
col_names1 = c("disposal", col_names1)

# now bringing in the data
disposal_data1 <- read_xls("/Users/katehayes/temp_data/New Tables for Website/Regional tables.xls", sheet = 2, skip = 340, n_max = 32, col_names = FALSE)
disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_types1 <- c("Pre-court", "First-tier", "Community", "Custody")

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "0910", level = "west_midlands")

disposal0910wm_data <- disposal_data

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2010-2011
# Youth justice annual statistics: 2010 to 2011, Disposals, regionally, 2010 - 11
# sheet 10 - west midlands

# WEST MIDS LEVEL
# exact same strcuture as above so using the above code:
disposal_data <- read_xls("/Users/katehayes/temp_data/disposals-regionally-2010-11.xls", sheet = 10, skip = 4, n_max = 32, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1011", level = "west_midlands")

disposal1011wm_data <- disposal_data

# BIRMINGHAM LEVEL

disposal_data <- read_xls("/Users/katehayes/temp_data/disposals-regionally-2010-11.xls", sheet = 10, skip = 42, n_max = 32, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1011", level = "birmingham")

disposal1011b_data <- disposal_data


########################NEW YEAR ################################################
#https://www.gov.uk/government/statistics/youth-justice-statistics-2011-12
# Youth justice annual statistics: 2011 to 2012,Youth justice statistics - regional data, Disposals by region 2011-12 v1.1
# sheet 10 - west midlands

# WEST MIDS LEVEL
# exact same strcuture as above so using the above code:

disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-11-12/Disposals by region 2011-12 v1.1.xls", sheet = 10, skip = 4, n_max = 33, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1112", level = "west_midlands")

disposal1112wm_data <- disposal_data

# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-11-12/Disposals by region 2011-12 v1.1.xls", sheet = 10, skip = 43, n_max = 33, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1112", level = "birmingham")

disposal1112b_data <- disposal_data


########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-statistics
# Youth justice annual statistics: 2012 to 2013,YRegional tables - 2013, Disposals by region 2012-13 Table.xls
# sheet 10 - west midlands

# WEST MIDS LEVEL
# exact same strcuture as above so using the above code:

excel_sheets("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls")

disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls", sheet = 10, skip = 4, n_max = 26, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1213", level = "west_midlands")

disposal1213wm_data <- disposal_data

# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls", sheet = 10, skip = 36, n_max = 26, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1213", level = "birmingham")

disposal1213b_data <- disposal_data

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2013-to-2014
# Youth justice annual statistics: 2013 to 2014, Regional tables - 2013, Disposals by region table 2013-14.xls
# sheet 10 - west midlands

# WEST MIDS LEVEL
# exact same strcuture as above so using the above code:

disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables 2/Disposals by region table 2013-14.xls", sheet = 10, skip = 4, n_max = 22, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1314", level = "west_midlands")

disposal1314wm_data <- disposal_data


# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables 2/Disposals by region table 2013-14.xls", sheet = 10, skip = 32, n_max = 22, col_names = FALSE)

disposal_data <- disposal_data %>%
  select(where(not_all_na))

names(disposal_data) <- col_names

disposal_data <- disposal_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
  mutate(disposal_type = if_else(disposal %in% disposal_types, disposal, "")) %>%
  mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
  mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
  filter(!is.na(`Age_15`)) %>%
  mutate(year = "1314", level = "birmingham")

disposal1314b_data <- disposal_data


########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2014-to-2015
# Youth justice annual statistics: 2014 to 2015, local level data, disposal by YOT
# ODD HIDDEN SHEETS STARTS HERE
# CHANGE OF CODE BC DIFFERENT FORMAT

excel_sheets("/Users/katehayes/temp_data/local-level-data (1)/Disposal_by_YOT.xls")
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data (1)/Disposal_by_YOT.xls", sheet = 6, col_names = TRUE)

disposal_data < disposal_data %>%
  mutate(year = "2014-15")



########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-statistics-2015-to-2016

excel_sheets("/Users/katehayes/temp_data/local-level-data-2015-to-2016/Disposals_by_YOT.xls")
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-2015-to-2016/Disposals_by_YOT.xls", sheet = 6, col_names = TRUE)
# col names are not the same as previous year - they're being less careful bc they expect it to be hidden

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2016-to-2017
# had to resave as an xlsx - originally xls?

excel_sheets("/Users/katehayes/temp_data/regional-level-tables.xlsx")
disposal_data <- read_xlsx("/Users/katehayes/temp_data/regional-level-tables.xlsx", sheet = 7, col_names = TRUE)

# is in a weird format - need to slplit into three for the three characteristics

disposal_a_data <- disposal_data[, 1:4]
disposal_g_data <- disposal_data[, 6:9]
disposal_e_data <- disposal_data[, 11:14]

################################################################################################################################
# WHAT WAY DOES IT MAKE SENSE TO BIND EVERYTHING TOGETHER
# PROBABLY BY CHARACTERISTIC group
# LIKE ONE DATAFRAME FOR SEX, ONE FOR GENDER, ETC



