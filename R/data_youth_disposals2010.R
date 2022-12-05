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

#this is for the newer group of sheets , 14/15 and 15/16
# splits into characteristics and level
split_newer_disposal_wm <- function(disp_data, group) { #need to enter group without quotation marks
    disp_data <- disp_data %>%
    group_by(region, yot, disposal, {{ group }}) %>%
    mutate(count = sum(count)) %>%
    distinct(year, region, yot, disposal, {{ group }}, count) %>%
    mutate(level = "west_midlands") %>%
    filter(region == "West Midlands") %>%
    group_by(disposal, {{ group }}) %>%
    mutate(count = sum(count)) %>%
    distinct(year, level, disposal, {{ group }}, count)
    }

  split_newer_disposal_b <- function(disp_data, group) { #need to enter group without quotation marks
      disp_data <- disp_data %>%
        mutate(level = "birmingham") %>%
        filter(yot == "Birmingham") %>%
        group_by(disposal, {{ group }}) %>%
        mutate(count = sum(count)) %>%
        distinct(year, level, disposal, {{ group }}, count)
  }

  # for the odd 16/17 one - for some reason this doesnt work for gender :) :)
  split_newest_disposal <- function(disp_data, group) {
    if(group == "age") {
      disp_data <- disp_data[, 1:4]
      colnames(disp_data)[1] <- "region"
      colnames(disp_data)[2] <- group
      colnames(disp_data)[3] <- "disposal"
      colnames(disp_data)[4] <- "count"

      disp_data <- disp_data %>%
      filter(region == "West Midlands") %>%
      mutate(year = "2016-17", level = "west_midlands") %>%
      select(-region) %>%
      mutate(age = case_when(age == "17+(1)" ~ "17+"))
    } else if(group == "gender") {
      disp_data <- disp_data[, 6:9]
      colnames(disp_data)[1] <- "region"
      colnames(disp_data)[2] <- group
      colnames(disp_data)[3] <- "disposal"
      colnames(disp_data)[4] <- "count"

      disp_data <- disp_data %>%
        filter(region == "West Midlands") %>%
        mutate(year = "2016-17", level = "west_midlands") %>%
        select(-region)
    } else if(group == "ethnicity") {
      disp_data <- disp_data[, 11:14]
      colnames(disp_data)[1] <- "region"
      colnames(disp_data)[2] <- group
      colnames(disp_data)[3] <- "disposal"
      colnames(disp_data)[4] <- "count"

      disp_data <- disp_data %>%
        filter(region == "West Midlands") %>%
        mutate(year = "2016-17", level = "west_midlands") %>%
        select(-region)
    }
  }





# SUMMARY of what we have for each year

# 2009/10 - age OR gender OR ethnicity --  west mids
# 2010/11 - age OR gender OR ethnicity -- birmingham and west mids
# 2011/12 - age OR gender OR ethnicity -- birmingham and west mids
# 2012/13 - age OR gender OR ethnicity -- birmingham and west mids
# 2013/14 - age OR gender OR ethnicity -- birmingham and west mids
# 2014/15 - age AND gender AND ethnicity -- birmingham and west mids
# 2015/16 - age AND gender AND ethnicity -- birmingham and west mids
# 2016/17 - age OR gender OR ethnicity -- west mids |
# 2017/18 - no more disposals by characteristics!
# 2018/19
# 2019/20
# 2020/21




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

# colnames(disposal_data)[17] <- "total"
# disposal_data %>% filter(!is.na(total)) %>%  summarise(total = sum(total))



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

disposal1415_data <- disposal_data[, -8] %>%
  mutate(year = "2014-15") %>%
  rename(yot = YOT, region = Region,
         disposal = Outcome, age = Age, ethnicity = Ethn,
         gender = Gen, count = Count)

disposal1415wm_a_data <- split_newer_disposal_wm(disp_data = disposal1415_data, group = age)
disposal1415b_a_data <- split_newer_disposal_b(disp_data = disposal1415_data, group = age)

disposal1415wm_g_data <- split_newer_disposal_wm(disp_data = disposal1415_data, group = gender)
disposal1415b_g_data <- split_newer_disposal_b(disp_data = disposal1415_data, group = gender)

disposal1415wm_e_data <- split_newer_disposal_wm(disp_data = disposal1415_data, group = ethnicity)
disposal1415b_e_data <- split_newer_disposal_b(disp_data = disposal1415_data, group = ethnicity)

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-statistics-2015-to-2016

excel_sheets("/Users/katehayes/temp_data/local-level-data-2015-to-2016/Disposals_by_YOT.xls")
disposal_data <- read_xls("/Users/katehayes/temp_data/local-level-data-2015-to-2016/Disposals_by_YOT.xls", sheet = 6, col_names = TRUE)
# col names are not the same as previous year - they're being less careful bc they expect it to be hidden

disposal1516_data <- disposal_data[, -8] %>%
  mutate(year = "2015-16") %>%
  rename(yot = YOT2, region = Region,
         disposal = OutRevised, age = Age_Revised, ethnicity = Ethnicity,
         gender = Gen, count = CountOfSent_Out_ID)


disposal1516wm_a_data <- split_newer_disposal_wm(disp_data = disposal1516_data, group = age)
disposal1516b_a_data <- split_newer_disposal_b(disp_data = disposal1516_data, group = age)

disposal1516wm_g_data <- split_newer_disposal_wm(disp_data = disposal1516_data, group = gender)
disposal1516b_g_data <- split_newer_disposal_b(disp_data = disposal1516_data, group = gender)

disposal1516wm_e_data <- split_newer_disposal_wm(disp_data = disposal1516_data, group = ethnicity)
disposal1516b_e_data <- split_newer_disposal_b(disp_data = disposal1516_data, group = ethnicity)

########################NEW YEAR ################################################
# https://www.gov.uk/government/statistics/youth-justice-annual-statistics-2016-to-2017
# had to resave as an xlsx - originally xls?

excel_sheets("/Users/katehayes/temp_data/regional-level-tables.xlsx")
disposal_data <- read_xlsx("/Users/katehayes/temp_data/regional-level-tables.xlsx", sheet = 7, col_names = TRUE)

# is in a weird format - need to split into three for the three characteristics
disposal1617wm_a_data <- split_newest_disposal(disp_data = disposal_data, group = "age")

disposal1617wm_g_data <- disposal_data[, 6:9]
colnames(disposal1617wm_g_data)[1] <- "region"
colnames(disposal1617wm_g_data)[2] <- "gender"
colnames(disposal1617wm_g_data)[3] <- "disposal"
colnames(disposal1617wm_g_data)[4] <- "count"

disposal1617wm_g_data <- disposal1617wm_g_data %>%
  filter(region == "West Midlands") %>%
  mutate(year = "2016-17", level = "west_midlands") %>%
  select(-region)

disposal1617wm_e_data <- split_newest_disposal(disp_data = disposal_data, group = "ethnicity")


################################################################################################################################
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


#### #### START TO DO GRAPHS #### #### #### #### #### #### ####

  # %>%
    group_by(Financial_Year, ) %>%
    summarise(count = sum(Number_Cautioned_Sentenced))

my_colours <- c("brown", "brown2", "deepskyblue4", "deepskyblue")

disposals_gender_data %>%
   group_by(graph, year, level) %>%
   summarise(count = sum(count)) %>%
    ggplot() +
    geom_area(aes(x = year, y = count, fill = fct_rev(level))) +
    facet_grid(~graph, scales = "free_x", space = "free") +
    scale_x_continuous(name = "",
                       breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                       labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                       expand = c(0,0)) +
    scale_y_continuous(name = "",
                       expand = c(0,0)) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(0, "lines")) +
  scale_fill_manual(values = my_colours)

my_colours3 <- c("#922B21", "#D98880", "#D98880",
                              "#CB4335", "#FADBD8", "#FADBD8",
                              "#D35400", "#EDBB99", "#EDBB99")

my_colours <- c("#922B21", "#D98880", "#D98880",
                "#CB4335", "#FADBD8", "#FADBD8",
                "#D35400", "#EDBB99", "#EDBB99",
                "#626567", "#B2BABB", "#B2BABB",
                 "#2980B9", "#D6EAF8", "#D6EAF8",
                 "#17A589", "#D1F2EB", "#D1F2EB")

# my_colours4 <- c("#440154FF", "#481F70FF", "#481F70FF",
#                  "#443A83FF", "#3B528BFF", "#3B528BFF",
#                  "#2C728EFF", "#21908CFF", "#21908CFF")
#
#
# my_colours4 <- c("#440154FF", "#481F70FF", "#481F70FF",
#                   "#404688FF", "#31688EFF", "#31688EFF",
#                 "#2C728EFF", "#21908CFF", "#21908CFF")
# my_colours4 <- c("#440154FF", "#472D7BFF", "#472D7BFF",
#                  "#3B528BFF", "#31688EFF", "#31688EFF",
#                  "#21908CFF", "#20A486FF", "#20A486FF")
#
# my_colours4 <- c("#440154FF", "#443A83FF", "#443A83FF",
#                  "#2C728EFF", "#20A486FF", "#20A486FF",
#                  "#75DO54FF", "#C7E020FF", "#C7E020FF")

my_colours4 <- c("#440154FF", "#443A83FF", "#443A83FF",
                 "#2C728EFF", "#20A486FF", "#20A486FF",
                 "#75D054FF", "#C7E020FF", "#C7E020FF")

my_colours5 <- c("#042333b2", "#403891b2", "#403891b2",
                 "#a65c85b2", "#DE7065B2", "#DE7065B2",
                 "#F68F46B2", "#F7CB44B2", "#F7CB44B2")

# "#626567", "#B2BABB", "#B2BABB",
# "#2980B9", "#D6EAF8", "#D6EAF8",
# "#17A589", "#D1F2EB", "#D1F2EB"

# red - C44B43, and lighter E39893
# orange - D7813E and lighter EFB07E
# yellow - C9A848 and lighter FAE8B3

disposals_gender_data %>%
  filter(gender != "Not Known") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, gender, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type, gender))) +
  facet_grid(~interaction(graph), scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"))  +
  scale_fill_manual(values = my_colours)

disposals_gender_data %>%
  filter(gender != "Not Known") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, gender, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type, gender))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = c(my_colours5, my_colours4))
ggsave(filename = "Output/Graphs/birm_wm_disposals_bygender.png")


disposals_gender_data %>%
  filter(gender != "Not Known") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, gender, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type))) +
  facet_grid(~interaction(graph, fct_rev(gender)), scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours4)
ggsave(filename = "Output/Graphs/birm_wm_disposals_bygender.png")


check <- disposals_gender_data %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, gender, disposal_type) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, gender, disposal_type, level) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()





legend.position="none"

disposals_gender_data %>%
  filter(gender == "Female") %>%
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
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours)
ggsave(filename = "Output/Graphs/birm_wm_disposals_female.png")

disposals_gender_data %>%
  filter(gender == "Male") %>%
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
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
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
ggsave(filename = "Output/Graphs/birm_wm_disposals_male.png")



disposals_gender_data %>%
  filter(gender == "Male") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_count, fill = interaction(level, disposal_type))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2014", "2016"),
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

check <- disposals_gender_data %>%
  filter(gender == "Male") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, disposal_type) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()


check <- disposals_gender_data %>%
  filter(gender == "Female") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, disposal_type) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()

# breakdown of custody into its parts
# 4D47A1 ddark blue
# 8D8AB6 lighter

# 445BB6 different darkish blue?
# 8A93B6 lighter
#
# 4480B6 darker
# 93AEC6 lighter
#
# 4CB2C8 darker
# 93BDC6 lighter

my_colours2 <- c("#7E215D", "#DCBBD0", "#DCBBD0",
                  "#773795", "#B9A0C4", "#B9A0C4",
                 "#4D47A1", "#B8B6D4", "#B8B6D4")

disposals_gender_data %>%
  filter(gender == "Male", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  mutate(disposal = case_when(disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226" ~ "Other",
                              disposal == "Section 228" ~ "Other")) %>%
  mutate(disposal = factor(disposal, levels = c("Other", "Section 90-92 Detention", "Detention and Training Order"))) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours2)
ggsave(filename = "Output/Graphs/birm_wm_custody_male.png")


disposals_gender_data %>%
  filter(gender == "Male", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  group_by(graph, year) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_count, fill = interaction(level, fct_rev(disposal)))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours2)




# something a little wrong with this graph

check <- disposals_gender_data %>%
  filter(gender == "Female", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  mutate(disposal = case_when(disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226" ~ "Other",
                              disposal == "Section 228" ~ "Other"))


check1 <- check[106:108, ]

check1 <- check1 %>%
  mutate(count = 0, disposal = "Section 90-92 Detention")

check <- bind_rows(check, check1)

check %>% # disposals_gender_data %>%
  # filter(gender == "Female", disposal_type == "Custody") %>%
  # mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
  #                             disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
  #                             disposal == "Detention and Training Order" ~ "Detention and Training Order",
  #                             disposal == "Section 226 (Life)" ~ "Section 226",
  #                             disposal == "Section 226 (Public Protection)" ~ "Section 226",
  #                             disposal == "Section 226b (*)" ~ "Section 226",
  #                             disposal == "Section 228" ~ "Section 228",
  #                             disposal == "Section 226b" ~ "Section 226")) %>%
  # # mutate(disposal = case_when(disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
  #                             disposal == "Detention and Training Order" ~ "Detention and Training Order",
  #                             disposal == "Section 226" ~ "Other",
  #                             disposal == "Section 228" ~ "Other")) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, fct_rev(disposal)))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
scale_fill_manual(values = my_colours2)
ggsave(filename = "Output/Graphs/birm_wm_custody_female.png")

  # and of course this is v deeply shit xx
  disposals_gender_data %>%
    filter(gender == "Female", disposal_type == "Custody") %>%
    mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                                disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                                disposal == "Detention and Training Order" ~ "Detention and Training Order",
                                disposal == "Section 226 (Life)" ~ "Section 226",
                                disposal == "Section 226 (Public Protection)" ~ "Section 226",
                                disposal == "Section 226b (*)" ~ "Section 226",
                                disposal == "Section 228" ~ "Section 228",
                                disposal == "Section 226b" ~ "Section 226")) %>%
    group_by(graph, year) %>%
    mutate(tot = sum(count)) %>%
    group_by(graph, year, level, disposal) %>%
    summarise(pc_count = sum(count)/tot) %>%
    distinct() %>%
    ggplot() +
    geom_area(aes(x = year, y = pc_count, colour = "black", fill = interaction(level, fct_rev(disposal)))) +
    facet_grid(~graph, scales = "free_x", space = "free") +
    scale_x_continuous(name = "",
                       breaks = c(2009, 2011, 2012, 2012, 2014, 2016),
                       labels = c("2009", "2011", "2012", "2012", "2014", "2016"),
                       expand = c(0,0)) +
    scale_y_continuous(name = "",
                       expand = c(0,0)) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(0, "lines")) +
    scale_fill_viridis(discrete = TRUE, direction = 1)



check <-disposals_gender_data %>%
  filter(gender == "Female", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  group_by(graph, year, disposal) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()








  # disposals_gender_data <- bind_rows(disposals_gender_data1, disposals_gender_data2, disposals_gender_data3) %>%
  #   mutate(rest_wm = west_midlands - birmingham)

  # doing weird stuff with the data to see what graphs i can make

  disposals_gender_data1 <- disposals_gender_data %>%
    filter(year == "2009-10")
  disposals_gender_data2 <- disposals_gender_data %>%
    filter(year != "2009-10" & year != "2016-17") %>%
    mutate(west_midlands = NA)
  disposals_gender_data3 <- disposals_gender_data %>%
    filter(year == "2016-17")

disposals_gender_data <- bind_rows(disposals_gender_data1, disposals_gender_data2, disposals_gender_data3) %>%
  pivot_longer(cols =  west_midlands:rest_wm , names_to = "level", values_to = "count")

check <- disposals_gender_data %>%
  mutate(year = as.numeric(substr(year, 1, 4))) %>%
  group_by(year, level) %>%
  summarise(count = sum(count)) %>%
  filter(year %in% 2010:2015) %>%
  ggplot(aes(x = year, y = count, fill = level, colour = level)) +
  geom_area() +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
check

# add on the last bit where you can't split by characteristics at all?
readin_data <- read_ods("/Users/katehayes/temp_data/Outcome_table.ods", sheet = 3)
