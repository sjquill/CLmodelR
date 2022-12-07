library(stringr)
library(tidyverse)
library(readxl)
library(vctrs)
library(readODS)

##-------------####DISPOSAL DATA####--------------------------------------------------------------------
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

