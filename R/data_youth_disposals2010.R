library(stringr)

# functions #


not_all_na <- function(x) {
  any(!is.na(x))
}

clean_disposal <- function(disp_data, col_nm, disp_type, current_year, current_level) {

  disp_data <- disp_data %>%
    select(where(not_all_na))

  names(disp_data) <- col_nm

  disp_data <- disp_data[, 1:14] %>% # dropped the total columnn in v ugly manner here
    mutate(disposal_type = if_else(disposal %in% disp_type, disposal, "")) %>%
    mutate(disposal_type = replace(disposal_type, disposal_type == "", NA)) %>%
    mutate(disposal_type = vec_fill_missing(disposal_type, direction = c("down"))) %>%
    filter(!is.na(`Age_15`)) %>%
    mutate(year = current_year, level = current_level)

}


# split_disposal <- function(disp_data, group) { #group has to be the right case - uppercase first letter but not all caps
#   disp_data %>%
#     select(year, level, disposal_type, disposal, starts_with(group)) %>%
#     rename_with(str_replace, pattern = paste(group,"_", sep = ""), replacement = "")
# }

# newer version where you pivot long at same time as split
split_disposal <- function(disp_data, group) { #group has to be the right case - uppercase first letter but not all caps
  disp_data %>%
    select(year, level, disposal_type, disposal, starts_with(group)) %>%
    pivot_longer(cols = starts_with(group), names_to = tolower(group), values_to = "count") %>%
    mutate(across(starts_with(tolower(group)), str_replace, pattern = paste(group,"_", sep = ""), replacement = ""))
}


# SUMMARY of what we have for each year

# 2009/10 - age OR gender OR ethnicity --  west mids
# 2010/11 - age OR gender OR ethnicity -- birmingham and west mids
# 2011/12 - age OR gender OR ethnicity -- birmingham and west mids
# 2012/13 - age OR gender OR ethnicity -- birmingham and west mids
# 2013/14 - age OR gender OR ethnicity -- birmingham and west mids
# 2014/15 - age AND gender AND ethnicity -- birmingham and west mids
# 2015/16 - age AND gender AND ethnicity -- birmingham and west mids
# 2016/17 - age OR gender OR ethnicity -- birmingham and west mids
# 2017/18 - no more disposals by characteristics!
# 2018/19
# 2019/20
# 2020/21




# start of data
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2009-2010
# Youth justice annual statistics: 2009 to 2010, Workload tables, Regional Tables
# Sheet R.1: Disposals given to young people, by region, 2009/10
# WEST MIDS LEVEL

# getting col names and setting disposal types
disposal_data <- read_xls("/Users/katehayes/temp_data/New Tables for Website/Regional tables.xls", sheet = 2, skip = 3, n_max = 1, col_names = TRUE)

disposal_data <- disposal_data %>%
  select(where(not_all_na)) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "group_value",
               names_transform = as.character, values_transform = as.character) %>%
  mutate(group = replace(group, grepl('[0-9]', group), NA)) %>%
  mutate(group = vec_fill_missing(group, direction = c("down")))

col_names <- paste(unlist(disposal_data[, 1]), unlist(disposal_data[, 2]), sep = "_")
col_names = c("disposal", col_names)

disposal_types <- c("Pre-court", "First-tier", "Community", "Custody")
group_types <- c("Age", "Gender", "Ethnicity")

# now bringing in the data
disposal_data <- read_xls("/Users/katehayes/temp_data/New Tables for Website/Regional tables.xls", sheet = 2, skip = 340, n_max = 32, col_names = FALSE)

disposal0910wm_data <- clean_disposal(disp_data = disposal_data,
                                col_nm = col_names,
                                disp_type = disposal_types,
                                current_year = "2009-10",
                                current_level = "west_midlands")


disposal0910wm_a_data <- split_disposal(disposal0910wm_data, group = "Age")
disposal0910wm_g_data <- split_disposal(disposal0910wm_data, group = "Gender")
disposal0910wm_e_data <- split_disposal(disposal0910wm_data, group = "Ethnicity")

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2010-2011
# Youth justice annual statistics: 2010 to 2011, Disposals, regionally, 2010 - 11
# sheet 10 - west midlands

# WEST MIDS LEVEL
# exact same strcuture as above so using the above code
disposal_data <- read_xls("/Users/katehayes/temp_data/disposals-regionally-2010-11.xls", sheet = 10, skip = 4, n_max = 32, col_names = FALSE)

disposal1011wm_data <- clean_disposal(disp_data = disposal_data,
                                col_nm = col_names,
                                disp_type = disposal_types,
                                current_year = "2010-11",
                                current_level = "west_midlands")

disposal1011wm_a_data <- split_disposal(disposal1011wm_data, group = "Age")
disposal1011wm_g_data <- split_disposal(disposal1011wm_data, group = "Gender")
disposal1011wm_e_data <- split_disposal(disposal1011wm_data, group = "Ethnicity")


# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/disposals-regionally-2010-11.xls", sheet = 10, skip = 42, n_max = 32, col_names = FALSE)

disposal1011b_data <- clean_disposal(disp_data = disposal_data,
                             col_nm = col_names,
                             disp_type = disposal_types,
                             current_year = "2010-11",
                             current_level = "birmingham")

disposal1011b_a_data <- split_disposal(disposal1011b_data, group = "Age")
disposal1011b_g_data <- split_disposal(disposal1011b_data, group = "Gender")
disposal1011b_e_data <- split_disposal(disposal1011b_data, group = "Ethnicity")
########################NEW YEAR ################################################
#https://www.gov.uk/government/statistics/youth-justice-statistics-2011-12
# Youth justice annual statistics: 2011 to 2012,Youth justice statistics - regional data, Disposals by region 2011-12 v1.1
# sheet 10 - west midlands

# WEST MIDS LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-11-12/Disposals by region 2011-12 v1.1.xls", sheet = 10, skip = 4, n_max = 33, col_names = FALSE)

disposal1112wm_data <- clean_disposal(disp_data = disposal_data,
                                      col_nm = col_names,
                                      disp_type = disposal_types,
                                      current_year = "2011-12",
                                      current_level = "west_midlands")

disposal1112wm_a_data <- split_disposal(disposal1112wm_data, group = "Age")
disposal1112wm_g_data <- split_disposal(disposal1112wm_data, group = "Gender")
disposal1112wm_e_data <- split_disposal(disposal1112wm_data, group = "Ethnicity")

# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-11-12/Disposals by region 2011-12 v1.1.xls", sheet = 10, skip = 43, n_max = 33, col_names = FALSE)


disposal1112b_data <- clean_disposal(disp_data = disposal_data,
                                     col_nm = col_names,
                                     disp_type = disposal_types,
                                     current_year = "2011-12",
                                     current_level = "birmingham")

disposal1112b_a_data <- split_disposal(disposal1112b_data, group = "Age")
disposal1112b_g_data <- split_disposal(disposal1112b_data, group = "Gender")
disposal1112b_e_data <- split_disposal(disposal1112b_data, group = "Ethnicity")
########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-statistics
# Youth justice annual statistics: 2012 to 2013,YRegional tables - 2013, Disposals by region 2012-13 Table.xls
# sheet 10 - west midlands

# WEST MIDS LEVEL
excel_sheets("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls")

disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls", sheet = 10, skip = 4, n_max = 26, col_names = FALSE)

disposal1213wm_data <- clean_disposal(disp_data = disposal_data,
                                      col_nm = col_names,
                                      disp_type = disposal_types,
                                      current_year = "2012-13",
                                      current_level = "west_midlands")

disposal1213wm_a_data <- split_disposal(disposal1213wm_data, group = "Age")
disposal1213wm_g_data <- split_disposal(disposal1213wm_data, group = "Gender")
disposal1213wm_e_data <- split_disposal(disposal1213wm_data, group = "Ethnicity")

# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls", sheet = 10, skip = 36, n_max = 26, col_names = FALSE)

disposal1213b_data <- clean_disposal(disp_data = disposal_data,
                                     col_nm = col_names,
                                     disp_type = disposal_types,
                                     current_year = "2012-13",
                                     current_level = "birmingham")

disposal1213b_a_data <- split_disposal(disposal1213b_data, group = "Age")
disposal1213b_g_data <- split_disposal(disposal1213b_data, group = "Gender")
disposal1213b_e_data <- split_disposal(disposal1213b_data, group = "Ethnicity")


########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2013-to-2014
# Youth justice annual statistics: 2013 to 2014, Regional tables - 2013, Disposals by region table 2013-14.xls
# sheet 10 - west midlands

# WEST MIDS LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables 2/Disposals by region table 2013-14.xls", sheet = 10, skip = 4, n_max = 22, col_names = FALSE)

disposal1314wm_data <- clean_disposal(disp_data = disposal_data,
                                      col_nm = col_names,
                                      disp_type = disposal_types,
                                      current_year = "2013-14",
                                      current_level = "west_midlands")

disposal1314wm_a_data <- split_disposal(disposal1314wm_data, group = "Age")
disposal1314wm_g_data <- split_disposal(disposal1314wm_data, group = "Gender")
disposal1314wm_e_data <- split_disposal(disposal1314wm_data, group = "Ethnicity")


# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables 2/Disposals by region table 2013-14.xls", sheet = 10, skip = 32, n_max = 22, col_names = FALSE)

disposal1314b_data <- clean_disposal(disp_data = disposal_data,
                                     col_nm = col_names,
                                     disp_type = disposal_types,
                                     current_year = "2013-14",
                                     current_level = "birmingham")

disposal1314b_a_data <- split_disposal(disposal1314b_data, group = "Age")
disposal1314b_g_data <- split_disposal(disposal1314b_data, group = "Gender")
disposal1314b_e_data <- split_disposal(disposal1314b_data, group = "Ethnicity")



########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2014-to-2015
# Youth justice annual statistics: 2014 to 2015, local level data, disposal by YOT
# ODD HIDDEN SHEETS STARTS HERE
# CHANGE OF CODE BC DIFFERENT FORMAT

excel_sheets("/Users/katehayes/temp_data/local-level-data (1)/Disposal_by_YOT.xls")
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data (1)/Disposal_by_YOT.xls", sheet = 6, col_names = TRUE)

disposal_data <- disposal_data %>%
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



