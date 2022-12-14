library(stringr)
library(tidyverse)
library(readxl)
library(vctrs)
library(readODS)


not_all_na <- function(x) {
  any(!is.na(x))
}

##-------------####DISPOSAL DATA####--------------------------------------------------------------------

##-------------09/10 to 13/14--------------------------------------------------------------------

clean_disposal_09to14 <- function(disp_data, col_nm, disp_type, current_year, current_level) {
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


#old version where you pivot long at same time as split - you have to specify group and it only returns one
# split_disposal <- function(disp_data, group) { #group has to be the right case - uppercase first letter but not all caps
#   disp_data %>%
#     select(year, level, disposal_type, disposal, starts_with(group)) %>%
#     pivot_longer(cols = starts_with(group), names_to = tolower(group), values_to = "count") %>%
#     mutate(across(starts_with(tolower(group)), str_replace, pattern = paste(group,"_", sep = ""), replacement = ""))
# }

#new version - splits the dataset into the three groups and returns them in a list
split_disposal_09to14 <- function(disp_data, current_group_types) {
  disp_data_split <- list()
  i <- 1
  for (group in current_group_types)  {
    disp_data_split[[i]] <- disp_data %>%
      select(year, level, disposal_type, disposal, starts_with(group)) %>%
      pivot_longer(cols = starts_with(group), names_to = tolower(group), values_to = "count") %>%
      mutate(across(starts_with(tolower(group)), str_replace, pattern = paste(group,"_", sep = ""), replacement = ""))
    i = i+1
  }
  return(disp_data_split)
}

##-------------14/15 and 15/16--------------------------------------------------------------------
clean_disposal_1415 <- function(disp_data) {
  disp_data[, -8] %>%
    mutate(year = "2014-15") %>%
    rename(yot = YOT, region = Region,
           disposal = Outcome, age = Age, ethnicity = Ethn,
           gender = Gen, count = Count)
}

clean_disposal_1516 <- function(disp_data) {
  disp_data[, -8] %>%
    mutate(year = "2015-16") %>%
    rename(yot = YOT2, region = Region,
           disposal = OutRevised, age = Age_Revised, ethnicity = Ethnicity,
           gender = Gen, count = CountOfSent_Out_ID)
}


split_disposal_14to16 <- function(disp_data) { # returns list of lists
     disp_data_split_wm <- list()
     disp_data_split_b <- list()
     group_types <- alist(age, gender, ethnicity)
     i <- 1
       for (group in group_types) {

         disp_data_split_wm[[i]] <- disp_data %>%
           group_by(region, yot, disposal, !!group) %>%
           mutate(count = sum(count)) %>%
           distinct(year, region, yot, disposal, !!group, count) %>%
           mutate(level = "west_midlands") %>%
           filter(region == "West Midlands") %>%
           group_by(disposal, !!group) %>%
           mutate(count = sum(count)) %>%
           distinct(year, level, disposal, !!group, count)

         disp_data_split_b[[i]] <- disp_data %>%
           mutate(level = "birmingham") %>%
           filter(yot == "Birmingham") %>%
           group_by(disposal, !!group) %>%
           mutate(count = sum(count)) %>%
           distinct(year, level, disposal, !!group, count)

    i <- i+1
    }

return(list(disp_data_split_wm, disp_data_split_b))
}

##-------------16/17--------------------------------------------------------------------

clean_and_split_disposal_1617 <- function(disp_data) {
  disp_data_split_wm <- list()
  group_types <- c("age", "gender", "ethnicity")

   i <- 1
   j <- 1
   k <- j+3

  for (group in group_types) {

    data_splitting <- disp_data[, j:k]
    colnames(data_splitting)[1] <- "region"
    colnames(data_splitting)[2] <- group
    colnames(data_splitting)[3] <- "disposal"
    colnames(data_splitting)[4] <- "count"

    disp_data_split_wm[[i]] <- data_splitting %>%
      filter(region == "West Midlands") %>%
      mutate(year = "2016-17", level = "west_midlands") %>%
      select(-region)

    if (i == 1) {
     disp_data_split_wm[[i]] <- disp_data_split_wm[[i]] %>%
       mutate(age = sub("\\(1\\)", "", age))
    }

    i <- i+1
    j <- j+5
    k <- j+3
  }

return(disp_data_split_wm)

}


