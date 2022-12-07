library(stringr)
library(tidyverse)
library(readxl)
library(vctrs)
library(readODS)


# SUMMARY of what we have for each year

# 2009/10 - age OR gender OR ethnicity --  west mids
# 2010/11 - age OR gender OR ethnicity -- birmingham and west mids
# 2011/12 - age OR gender OR ethnicity -- birmingham and west mids
# 2012/13 - age OR gender OR ethnicity -- birmingham and west mids
# 2013/14 - age OR gender OR ethnicity -- birmingham and west mids
# 2014/15 - age AND gender AND ethnicity -- birmingham and west mids - a non-grouped age variable exists for this too
# 2015/16 - age AND gender AND ethnicity -- birmingham and west mids
# 2016/17 - age OR gender OR ethnicity -- west mids |
# 2017/18 - no more disposals by characteristics!
# 2018/19
# 2019/20
# 2020/21


# do i want them all in some sort of big list

 #          west mids         birmingham
 #
 # year1    list of groups    list of groups
 #
 # year2    list of groups    list of groups
 #
 # year3    list of groups    list of groups
 #
 #
 # and so on

# start of data
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2009-2010
# Youth justice annual statistics: 2009 to 2010, Workload tables, Regional Tables
# Sheet R.1: Disposals given to young people, by region, 2009/10
# WEST MIDS LEVEL
# getting col names and setting disposal types & the disposals within them
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


# start of data
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2009-2010
# Youth justice annual statistics: 2009 to 2010, Workload tables, Regional Tables
# Sheet R.1: Disposals given to young people, by region, 2009/10
# WEST MIDS LEVEL
# now bringing in the data
disposal_data <- read_xls("/Users/katehayes/temp_data/New Tables for Website/Regional tables.xls", sheet = 2, skip = 340, n_max = 32, col_names = FALSE)

disposal0910wm_data <- disposal_data %>%
            clean_disposal_09to14(col_nm = col_names,
                      disp_type = disposal_types,
                      current_year = "2009-10",
                      current_level = "west_midlands") %>%
            split_disposal_09to14(current_group_types = group_types)

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2010-2011
# Youth justice annual statistics: 2010 to 2011, Disposals, regionally, 2010 - 11
# sheet 10 - west midlands

# WEST MIDS LEVEL
# exact same strcuture as above so using the above code
disposal_data <- read_xls("/Users/katehayes/temp_data/disposals-regionally-2010-11.xls", sheet = 10, skip = 4, n_max = 32, col_names = FALSE)

disposal1011wm_data <- disposal_data %>%
                      clean_disposal_09to14(col_nm = col_names,
                                disp_type = disposal_types,
                                current_year = "2010-11",
                                current_level = "west_midlands") %>%
          split_disposal_09to14(current_group_types = group_types)


# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/disposals-regionally-2010-11.xls", sheet = 10, skip = 42, n_max = 32, col_names = FALSE)

disposal1011b_data <- disposal_data %>%
  clean_disposal_09to14(col_nm = col_names,
                        disp_type = disposal_types,
                        current_year = "2010-11",
                        current_level = "birmingham") %>%
  split_disposal_09to14(current_group_types = group_types)

########################NEW YEAR ################################################
#https://www.gov.uk/government/statistics/youth-justice-statistics-2011-12
# Youth justice annual statistics: 2011 to 2012,Youth justice statistics - regional data, Disposals by region 2011-12 v1.1
# sheet 10 - west midlands

# WEST MIDS LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-11-12/Disposals by region 2011-12 v1.1.xls", sheet = 10, skip = 4, n_max = 33, col_names = FALSE)

disposal1112wm_data <- disposal_data %>%
  clean_disposal_09to14(col_nm = col_names,
                        disp_type = disposal_types,
                        current_year = "2011-12",
                        current_level = "west_midlands") %>%
  split_disposal_09to14(current_group_types = group_types)

# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-11-12/Disposals by region 2011-12 v1.1.xls", sheet = 10, skip = 43, n_max = 33, col_names = FALSE)

disposal1112b_data <- disposal_data %>%
        clean_disposal_09to14(col_nm = col_names,
                              disp_type = disposal_types,
                              current_year = "2011-12",
                              current_level = "birmingham") %>%
        split_disposal_09to14(current_group_types = group_types)

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-statistics
# Youth justice annual statistics: 2012 to 2013,YRegional tables - 2013, Disposals by region 2012-13 Table.xls
# sheet 10 - west midlands

# WEST MIDS LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls", sheet = 10, skip = 4, n_max = 26, col_names = FALSE)

disposal1213wm_data <- disposal_data %>%
          clean_disposal_09to14(col_nm = col_names,
                                disp_type = disposal_types,
                                current_year = "2012-13",
                                current_level = "west_midlands") %>%
          split_disposal_09to14(current_group_types = group_types)


# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables-2013/Disposals by region 2012-13 Table.xls", sheet = 10, skip = 36, n_max = 26, col_names = FALSE)

disposal1213b_data <- disposal_data %>%
                        clean_disposal_09to14(col_nm = col_names,
                                        disp_type = disposal_types,
                                        current_year = "2012-13",
                                        current_level = "birmingham") %>%
                        split_disposal_09to14(current_group_types = group_types)

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2013-to-2014
# Youth justice annual statistics: 2013 to 2014, Regional tables - 2013, Disposals by region table 2013-14.xls
# sheet 10 - west midlands

# WEST MIDS LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables 2/Disposals by region table 2013-14.xls", sheet = 10, skip = 4, n_max = 22, col_names = FALSE)

disposal1314wm_data <- disposal_data %>%
          clean_disposal_09to14(col_nm = col_names,
                                disp_type = disposal_types,
                                current_year = "2013-14",
                                current_level = "west_midlands") %>%
          split_disposal_09to14(current_group_types = group_types)


# BIRMINGHAM LEVEL
disposal_data <- read_xls("/Users/katehayes/temp_data/regional-tables 2/Disposals by region table 2013-14.xls", sheet = 10, skip = 32, n_max = 22, col_names = FALSE)

disposal1314b_data <- disposal_data %>%
          clean_disposal_09to14(col_nm = col_names,
                                disp_type = disposal_types,
                                current_year = "2013-14",
                                current_level = "birmingham") %>%
          split_disposal_09to14(current_group_types = group_types)



########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2014-to-2015
# Youth justice annual statistics: 2014 to 2015, local level data, disposal by YOT
# ODD HIDDEN SHEETS STARTS HERE
# CHANGE OF CODE BC DIFFERENT FORMAT

# excel_sheets("/Users/katehayes/temp_data/local-level-data (1)/Disposal_by_YOT.xls")
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data (1)/Disposal_by_YOT.xls", sheet = 6, col_names = TRUE)

disposal1415_data <- disposal_data %>%
  clean_disposal_1415() %>%
  split_disposal_14to16()



########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-statistics-2015-to-2016

# excel_sheets("/Users/katehayes/temp_data/local-level-data-2015-to-2016/Disposals_by_YOT.xls")
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-2015-to-2016/Disposals_by_YOT.xls", sheet = 6, col_names = TRUE)
# col names are not the same as previous year - they're being less careful bc they expect it to be hidden

disposal1516_data <- disposal_data %>%
clean_disposal_1516() %>%
split_disposal_14to16()


########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2016-to-2017
# had to resave as an xlsx - originally xls?

excel_sheets("/Users/katehayes/temp_data/regional-level-tables.xlsx")
# is in a weird format
disposal_data <- read_xlsx("/Users/katehayes/temp_data/regional-level-tables.xlsx", sheet = 7, col_names = TRUE)


disposal1617_data <- disposal_data %>%
  clean_and_split_disposal_1617()


##########MOST RECENT ONE###############################################################

readin_data <- read_ods("/Users/katehayes/temp_data/Outcome_table.ods", sheet = 3)

##########adding together######################################################################################################################
# WHAT WAY DOES IT MAKE SENSE TO BIND EVERYTHING TOGETHER
# PROBABLY BY CHARACTERISTIC group
# LIKE ONE DATAFRAME FOR SEX, ONE FOR GENDER, ETC

# getting disposal groupings
disposals_gender_data <- bind_rows(disposal0910wm_g_data, disposal1011wm_g_data) %>%
  bind_rows(disposal1011b_g_data) %>%
  bind_rows(disposal1112wm_g_data) %>%
  bind_rows(disposal1112b_g_data) %>%
  bind_rows(disposal1213wm_g_data) %>%
  bind_rows(disposal1213b_g_data) %>%
  bind_rows(disposal1314wm_g_data) %>%
  bind_rows(disposal1314b_g_data)


precourt_list <- disposals_gender_data %>%
  filter(disposal_type == "Pre-court") %>%
  distinct(disposal)
precourt_list <- unlist(precourt_list$disposal)

firsttier_list <- disposals_gender_data %>%
  filter(disposal_type == "First-tier") %>%
  distinct(disposal)
firsttier_list <- unlist(firsttier_list$disposal)

community_list <- disposals_gender_data %>%
  filter(disposal_type == "Community") %>%
  distinct(disposal)
community_list <- unlist(community_list$disposal)

custody_list <- disposals_gender_data %>%
  filter(disposal_type == "Custody") %>%
  distinct(disposal)
custody_list <- unlist(custody_list$disposal)



# now making egnder the dataset
disposals_gender_data <- bind_rows(disposal0910wm_g_data, disposal1011wm_g_data) %>%
  bind_rows(disposal1011b_g_data) %>%
  bind_rows(disposal1112wm_g_data) %>%
  bind_rows(disposal1112b_g_data) %>%
  bind_rows(disposal1213wm_g_data) %>%
  bind_rows(disposal1213b_g_data) %>%
  bind_rows(disposal1314wm_g_data) %>%
  bind_rows(disposal1314b_g_data) %>%
  bind_rows(disposal1415wm_g_data) %>%
  bind_rows(disposal1415b_g_data) %>%
  bind_rows(disposal1516wm_g_data) %>%
  bind_rows(disposal1516b_g_data) %>%
  bind_rows(disposal1617wm_g_data)

  disposals_gender_data <- disposals_gender_data %>%
  mutate(disposal_type = case_when(disposal %in% precourt_list ~ "Pre-court",
                                   disposal %in% firsttier_list ~ "First-tier",
                                   disposal %in% community_list ~ "Community",
                                   disposal %in% custody_list ~ "Custody"))

  disposals_gender_data <- disposals_gender_data %>%
    pivot_wider(names_from = level, values_from = count)



  # check3 <- disposals_gender_data %>%
  #   filter(level == "west_midlands") %>%
  #   group_by(year) %>%
  #   summarise(count = sum(count))
  # check3



  #

  disposals_gender_data1 <- disposals_gender_data %>%
    filter(year == "2009-10" | year == "2010-11") %>%
    mutate(year = as.numeric(substr(year, 1, 4))) %>%
    mutate(birmingham = 0, rest_wm = 0) %>%
    pivot_longer(cols =  west_midlands:rest_wm , names_to = "level", values_to = "count") %>%
    mutate(graph = "first")
  disposals_gender_data2 <- disposals_gender_data %>%
    filter(year != "2009-10" & year != "2016-17") %>%
    mutate(birmingham = replace_na(birmingham, 0)) %>%
    mutate(rest_wm = west_midlands - birmingham) %>%
    mutate(west_midlands = 0) %>%
    pivot_longer(cols =  west_midlands:rest_wm , names_to = "level", values_to = "count") %>%
    mutate(year = as.numeric(substr(year, 1, 4))) %>%
    mutate(graph = "second")
  disposals_gender_data3 <- disposals_gender_data %>%
    filter(year == "2015-16" | year == "2016-17") %>%
    mutate(birmingham = 0, rest_wm = 0) %>%
    mutate(year = as.numeric(substr(year, 1, 4))) %>%
    pivot_longer(cols =  west_midlands:rest_wm , names_to = "level", values_to = "count") %>%
    mutate(graph = "third")

  disposals_gender_data <- bind_rows(disposals_gender_data1, disposals_gender_data2, disposals_gender_data3)

  #ADD THE ORIGINAL OUTCOMES DATA - NO CATEGORIES
  disposals_data <- disposals_gender_data %>%
    filter(graph != "third") %>%
    group_by(graph, year, level, disposal_type, disposal) %>%
    summarise(count = sum(count))

  later_disposals <- readin_data %>%
    filter(Region == "West Midlands") %>%
    rename(year = Financial_Year,
           disposal = Caution_or_sentence_type,
           disposal_type = Caution_or_sentence_tier,
           count = Number_Cautioned_Sentenced) %>%
    mutate(disposal_type = case_when(disposal_type == "Pre Court" ~ "Pre-court",
                                     disposal_type == "First Tier" ~ "First-tier",
                                     disposal_type == "Custody" ~ "Custody",
                                     disposal_type == "Community" ~ "Community")) %>%
    select(!Region:England_Wales) %>%
    pivot_wider(names_from = YJS, values_from = count, values_fill = 0) %>%
    mutate(west_midlands = Birmingham + Coventry + Dudley + Sandwell + Solihull +
             Staffordshire + `Stoke-on-Trent` + Walsall + Warwickshire + `West Mercia` +
             Wolverhampton) %>%
    select(!Coventry:Wolverhampton) %>%
    rename(birmingham = Birmingham) %>%
    mutate(rest_wm = west_midlands - birmingham) %>%
    mutate(west_midlands  = 0) %>%
    pivot_longer(cols = birmingham:rest_wm, names_to = "level", values_to = "count") %>%
    mutate(year = as.numeric(substr(year, 1, 4))) %>%
    filter(year >= 2015) %>%
    mutate(graph = "third")

  disposals_data <- disposals_data %>%
    bind_rows(later_disposals)

  disposals_data %>%
    mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                     disposal_type == "First-tier" ~ "Non-custodial",
                                     disposal_type == "Community" ~ "Non-custodial",
                                     disposal_type == "Pre-court" ~ "Pre-court")) %>%
    group_by(graph, year, level, disposal_type) %>%
    summarise(count = sum(count)) %>%
    ggplot() +
    geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type))) +
    facet_grid(~graph, scales = "free_x", space = "free") +
    scale_x_continuous(name = "",
                       breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016, 2017, 2018, 2019, 2020),
                       labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016", "2017", "2018", "2019", "2020"),
                       expand = c(0,0)) +
    scale_y_continuous(name = "",
                       expand = c(0,0)) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(c(0), "lines"),
          plot.margin = unit(c(2, 3, 2, 1), "lines"),
          legend.position="none") +
    scale_fill_manual(values = my_colours)
  ggsave(filename = "Output/Graphs/birm_wm_disposals.png")



  check <- disposals_data %>%
    mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                     disposal_type == "First-tier" ~ "Non-custodial",
                                     disposal_type == "Community" ~ "Non-custodial",
                                     disposal_type == "Pre-court" ~ "Pre-court")) %>%
    group_by(graph, year, disposal_type) %>%
    mutate(tot = sum(count)) %>%
    group_by(graph, year, level, disposal_type) %>%
    summarise(pc_count = sum(count)/tot) %>%
    distinct()

  check <- disposals_data %>%
    mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                     disposal_type == "First-tier" ~ "Non-custodial",
                                     disposal_type == "Community" ~ "Non-custodial",
                                     disposal_type == "Pre-court" ~ "Pre-court")) %>%
    group_by(graph, year, level, disposal_type) %>%
    summarise(count = sum(count))


