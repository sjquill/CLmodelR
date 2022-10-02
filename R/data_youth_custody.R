##here is a useful resource:
##https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1049959/A_Guide_to_Youth_Justice_Statistics.pdf

##############WHERE IS THE DATA########################################################
##https://www.gov.uk/government/publications/youth-custody-data
##Monthly(!!!) statistics on the population in custody of children and young people within the secure estate.
##broken down by gender, age, race, some other stuff
##this is national data i think (maybe england and wales, need to check)

##if we want local level, we need to go here: https://www.gov.uk/government/statistics/youth-justice-statistics-2020-to-2021
## and go to the local level open data. the relevant table for youth custody is the outcome table
## outcome table: at birmingham level, yearly 2013-14 to 2020-21, number of children that have each outcome, including
##a detention and training order or a section 90-91 detention
##THIS IS JUST PEOPLE SENTENCED - YOU ALSO NEED REMAND

##there is also, on this page https://www.gov.uk/government/statistics/youth-justice-statistics-2020-to-2021, the supplementary tables
##in it there is chapter 7, children in youth custody. in it we have sheet 7.21, which gives yearly figure 'avg monthly custody population'
##at the west midlands level, from 2012 to 2021, and broken down by remand, DTO, section 91 and other.
##there is also sheet 7.32, which gives some distribution of nights spent in custody in West midlands (1 to 91 nights, 92 to 182 nights,
## 183 to 273 nights, 274+ nights) yearly from 2019 to 2021
## you can also get dist of nights by custody type, for the whole country only though, on sheet 7.27

##also in the supplementary tables! is chapter 6, use of remand... i do not think local level data is available :()


##https://www.gov.uk/government/statistics/length-of-time-spent-in-youth-custody-2016-to-2017
##This is a one-off publication, with the focus on episodes children and young people spent in custody which ended
##between 1 April 2016 to 31 March 2017. In previous years, these statistics have been included in the annual Youth
##Justice Statistics, however due to a change in data source and subsequent methodology, these are being published
##separately for 2016/17. Data for the year ending March 2018 will be included in the Youth Justice Statistics, 2017/18 publication.

##############POTENTIAL INTERSECTIONS########################################################
##MoJ/DfE Experimental Statistics ‘Understanding the educational background of young offenders’:
##  https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/577542/

## Assessing the needs of sentenced children in the Youth Justice System:
## https://www.gov.uk/government/statistics/assessing-the-needs-of-sentenced-children-in-the-youth-justice-system

##############POTENTIAL OUTCOMES########################################################
## also in the supplementary tables there is a workbook about behaviour management in the secure estate - come back to this
##https://www.gov.uk/government/statistics/safety-in-the-children-and-young-people-secure-estate-update-to-march-2022

##############OK LETS TRY THIS########################################################
## i am going to temporarily read in from a file saved on my laptop.
## will later change to good practise - say include in script code to scrape from web


install.packages("readODS")
install.packages("readxl")
install.packages('ggplot2')
library(readODS)
library(readxl)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyverse)
library(vctrs)
##figure out what the good practise is about installing and loading packages
##leaving them in the code or not... also, loading tidyverse vs loading its consitutent parts

##coming here from local level open data, outcomes table
##children sentenced to custody in birmingham, yearly, 2013/14 t0 2020/21
custody_data <- read_ods("/Users/katehayes/temp_data/Outcome_table.ods", sheet = 3)

custody_plot <- ggplot(custody_data[custody_data$YJS == "Birmingham" & custody_data$Caution_or_sentence_tier == "Custody", ], aes(x = Financial_Year, y = Number_Cautioned_Sentenced, fill = Caution_or_sentence_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

custody_plot

custody_data <- custody_data[,1:8]

custody_group_data <- custody_data %>%
  filter(YJS == "Birmingham") %>%
  group_by(Financial_Year, Caution_or_sentence_type, Caution_or_sentence_tier) %>%
  summarise(tot_caut_sent = sum(Number_Cautioned_Sentenced))

cust_data <- custody_group_data %>%
  filter(Caution_or_sentence_tier == "Custody") %>%
  group_by(Financial_Year) %>%
    summarise(tot_cust = sum(tot_caut_sent))

##ok for my model groupings i need custody, i also need
##compensation, conditional discharge, referral order, youth conditional caution, youth rehabilitation order as restricted

custody_data <- custody_data %>%
                  filter(YJS == "Birmingham") %>%
                    group_by(Financial_Year, Caution_or_sentence_type) %>%
                      summarise(tot_caut_sent = sum(Number_Cautioned_Sentenced))


rest_data <- custody_data %>%
  filter(Caution_or_sentence_type %in% c("Referral Order", "Youth Conditional Caution", "Youth Rehabilitation Order", "Conditional Discharge", "Compensation Order", "Reparation Order") ) %>%
  group_by(Financial_Year) %>%
  summarise(tot_rest = sum(tot_caut_sent))




custody_plot2 <- ggplot(custody_data, aes(x = Financial_Year, y = tot_caut_sent)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Caution_or_sentence_type) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


custody_plot2
##where is children on remand data? only national and yearly.
##so if this is the case then i might have to construct yearly average probabilities of remand into bail, custody, whatever
##and then apply these to the (hopefully) local numbers of children getting charged in birmingham

##asummarise(tot_caut_sent = sum(Number_Cautioned_Sentenced))

##SO this is phenomenally messy - come back and re-do entirely!!!!
##would be a good exercise in how to clean data sanely and not just in a big rush/dangerously

##year ending march 21
remand_data <- read_xls("/Users/katehayes/temp_data/Ch 6 - Use of remand for children.xls", sheet = 2, skip = 3, n_max = 17)
remand_data <- remand_data[!is.na(remand_data$`Remand type`), ]
remand_data <- remand_data[ , !names(remand_data) %in% "Total"]

remand_group_names <- remand_data[is.na(remand_data$`Black`), 1]
remand_group_names <- unlist(remand_group_names, use.names = FALSE)

child_groups <- read_xls("/Users/katehayes/temp_data/Ch 6 - Use of remand for children.xls", sheet = 2, skip = 2, n_max = 1, col_names = FALSE)
child_groups <- child_groups[!is.na(child_groups)]
child_groups <- unlist(child_groups, use.names = FALSE)

colnames(remand_data)[7] <- paste("Unknown", child_groups[1])
colnames(remand_data)[11] <- paste("Unknown", child_groups[2])


remand_data <- remand_data[!str_detect(remand_data$`Remand type`, "Total"),]
remand_data <- remand_data[!is.na(remand_data$`Asian`), ]
remand_data <- remand_data[ , colSums(is.na(remand_data)) == 0]
remand_data$`Asian` <- as.numeric(remand_data$`Asian`)


remand_data <- remand_data %>%
                      pivot_longer(!`Remand type`,
                                  names_to="child_characteristics",
                                  values_to="number_remands")


remand_data <- remand_data %>%
                      mutate(remand_groups=case_when(
                        `Remand type`=="Unconditional Bail" | `Remand type`=="Conditional Bail" ~ remand_group_names[1],
                        `Remand type`=="Bail Supervision and Support" | `Remand type`=="ISS Bail" | `Remand type`=="Remand to Local Authority Accommodation" ~ remand_group_names[2],
                        `Remand type`=="Remand to Youth Detention Accommodation" ~ remand_group_names[3],
                      ))

remand_data$year <-  2021

remand_data_21 <-  remand_data

##year ending march 20

remand_data <- read_xls("/Users/katehayes/temp_data/youth-justice-statistics-2019-20-supplementary-tables/Ch 6 - Use of remand for children.xls", sheet = 2, skip = 3, n_max = 17)
remand_data <- remand_data[!is.na(remand_data$`Remand type`), ]
remand_data <- remand_data[ , !names(remand_data) %in% "Total"]

remand_group_names <- remand_data[is.na(remand_data$`Black`), 1]
remand_group_names <- unlist(remand_group_names, use.names = FALSE)

child_groups <- read_xls("/Users/katehayes/temp_data/youth-justice-statistics-2019-20-supplementary-tables/Ch 6 - Use of remand for children.xls", sheet = 2, skip = 2, n_max = 1, col_names = FALSE)
child_groups <- child_groups[!is.na(child_groups)]
child_groups <- unlist(child_groups, use.names = FALSE)

colnames(remand_data)[7] <- paste("Unknown", child_groups[1])
colnames(remand_data)[11] <- paste("Unknown", child_groups[2])


remand_data <- remand_data[!str_detect(remand_data$`Remand type`, "Total"),]
remand_data <- remand_data[!is.na(remand_data$`Asian`), ]
remand_data <- remand_data[ , colSums(is.na(remand_data)) == 0]
remand_data$`Asian` <- as.numeric(remand_data$`Asian`)


remand_data <- remand_data %>%
  pivot_longer(!`Remand type`,
               names_to="child_characteristics",
               values_to="number_remands")


remand_data <- remand_data %>%
  mutate(remand_groups=case_when(
    `Remand type`=="Unconditional Bail" | `Remand type`=="Conditional Bail" ~ remand_group_names[1],
    `Remand type`=="Bail Supervision and Support" | `Remand type`=="ISS Bail" | `Remand type`=="Remand to Local Authority Accommodation" ~ remand_group_names[2],
    `Remand type`=="Remand to Youth Detention Accommodation" ~ remand_group_names[3],
  ))

remand_data$year <-  2020

remand_data_20 <-  remand_data

##lol, so it worked for year ending March 20 (against all odds)
##obv need to come back and make it less hardcody and then make it a function that returns a dataset of all the years
##now year end march 2019.....
##this one won't open - i had to open and resave as .xlsx - annoying

remand_data <- read_xlsx("/Users/katehayes/temp_data/youth-justice-stats-supplementary-tables-march-2019/Ch 6 - Use of remand for children.xls", sheet = 2, skip = 3, n_max = 17)
remand_data <- remand_data[!is.na(remand_data$`Remand type`), ]
remand_data <- remand_data[ , !names(remand_data) %in% "Total"]

remand_group_names <- remand_data[is.na(remand_data$`Black`), 1]
remand_group_names <- unlist(remand_group_names, use.names = FALSE)

child_groups <- read_xlsx("/Users/katehayes/temp_data/youth-justice-stats-supplementary-tables-march-2019/Ch 6 - Use of remand for children.xls", sheet = 2, skip = 2, n_max = 1, col_names = FALSE)
child_groups <- child_groups[!is.na(child_groups)]
child_groups <- unlist(child_groups, use.names = FALSE)

colnames(remand_data)[7] <- paste("Unknown", child_groups[1])
colnames(remand_data)[11] <- paste("Unknown", child_groups[2])


remand_data <- remand_data[!str_detect(remand_data$`Remand type`, "Total"),]
remand_data <- remand_data[!is.na(remand_data$`Asian`), ]
remand_data <- remand_data[ , colSums(is.na(remand_data)) == 0]
remand_data$`Asian` <- as.numeric(remand_data$`Asian`)


remand_data <- remand_data %>%
  pivot_longer(!`Remand type`,
               names_to="child_characteristics",
               values_to="number_remands")


remand_data <- remand_data %>%
  mutate(remand_groups=case_when(
    `Remand type`=="Unconditional Bail" | `Remand type`=="Conditional Bail" ~ remand_group_names[1],
    `Remand type`=="Bail Supervision and Support" | `Remand type`=="ISS Bail" | `Remand type`=="Remand to Local Authority Accommodation" ~ remand_group_names[2],
    `Remand type`=="Remand to Youth Detention Accommodation" ~ remand_group_names[3],
  ))

remand_data$year <-  2019
remand_data_19 <-  remand_data


##now year end march 2018....LOL
##ITS COMPLETELY DIFFERENT LAYOUT
##FUCK
##remand_data <- read_xlsx("/Users/katehayes/temp_data/youth_justice_statistics_supplementary_tables_2017_2018/Ch 6 - Use of remand for children.xls", sheet = 2, skip = 3, n_max = 17)


##ok just going to bind the first three together and see what i get
##then plotting it
##lol..when you do this command:
##remand_plot <- ggplot(remand_data[remand_data$`Remand type` == "Remand to Youth Detention Accommodation", ], aes(x = year, y = number_remands)) +
##geom_bar(stat = "identity") +
## theme(axis.text.x=element_text(angle = -90, hjust = 0))
##.....you're adding so many more remands than there are.... 3 times more...because youre adding all ages, all genders, all ethnicities...
## fix that.. for the moment just subset

remand_data <- rbind(remand_data_19, remand_data_20, remand_data_21)
remand_data <- remand_data[remand_data$child_characteristics == "Boys" | remand_data$child_characteristics == "Girls" | remand_data$child_characteristics == "Unknown Sex" , ]


tot_remand <- remand_data %>%
  group_by(year) %>%
  summarise(tot_remand = sum(number_remands))

rem_type <- remand_data %>%
  group_by(year, `Remand type`) %>%
  summarise(rem_type = sum(number_remands))

tot_rem_cus <- rem_type %>%
  filter(`Remand type` == "Remand to Youth Detention Accommodation" )

tot_rem_rest <- rem_type %>%
  filter(`Remand type` != "Remand to Youth Detention Accommodation" & `Remand type` != "Unconditional Bail") %>%
    group_by(year)  %>%
      summarise(tot_rem_rest = sum(rem_type))



r_sex <- remand_data[remand_data$child_characteristics == "Boys" | remand_data$child_characteristics == "Girls" | remand_data$child_characteristics == "Unknown Sex" , ]


remand_plot <- ggplot(remand_data, aes(x = year, y = number_remands, fill = remand_groups)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

remand_plot

## lets say remand into youth detention accommodation is about 10% of all remand episodes

##################CHILDREN PROCEEDED AGAINST######################################
##in the youth justice statistics supplementaries you can get children proceeded against in magistrates court


################# Average monthly youth custody population by legal basis for detention######################################
##in the youth justice statistics supplementaries chapter 7 , sheet 7.21, at west midlands level
##Average monthly youth custody population by region of home Youth Justice Service
## and legal basis for detention (under 18s only), years ending March 2012 to 2021




custody_data <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 22, skip = 3, n_max = 48)
colnames(custody_data)[2] <- "region_home_yjs"

custody_data <- custody_data %>%
  mutate(`Legal basis` = vec_fill_missing(`Legal basis`, direction = c("down"))) %>% ## i like this
      filter(region_home_yjs == "West Midlands")  %>%
        pivot_longer(starts_with("20"),
                     names_to="year",
                     values_to="number_in_custody")


cust_remand <- custody_data %>%
  filter(`Legal basis` == "Remand")  %>%
    group_by(year) %>%
      summarise(cust_remand = sum(number_in_custody))

cust_sentence <- custody_data %>%
  filter(`Legal basis` != "Remand")  %>%
  group_by(year) %>%
  summarise(cust_sentence = sum(number_in_custody))

##custody_data <- custody_data[!is.na(custody_data$region_home_yjs), ]

##custody_data <- custody_data %>%
##pivot_longer(starts_with("20"),
##names_to="year",
##values_to="number_in_custody")

custody_plot <- ggplot(custody_data[custody_data$region_home_yjs == "West Midlands", ], aes(x = year, y = number_in_custody, fill = `Legal basis`)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

custody_plot

################# Average monthly youth custody population by ethnicty######################################
##in the youth justice statistics supplementaries chapter 7 , sheet 7.20, at west midlands level
##ATable 7.20: Average monthly youth custody population by region of home Youth Justice Service
## aand ethnicity (under 18s only), years ending March 2012 to 2021

custody_eth_data <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 21, skip = 3, n_max = 62)
colnames(custody_eth_data)[2] <- "region_home_yjs"

custody_eth_data <- custody_eth_data %>%
  mutate(Ethnicity = vec_fill_missing(Ethnicity, direction = c("down")))

custody_eth_data <- custody_eth_data[!is.na(custody_eth_data$region_home_yjs), ]

custody_eth_data <- custody_eth_data %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_custody")


custody_eth_plot <- ggplot(custody_eth_data[custody_eth_data$region_home_yjs == "West Midlands", ], aes(x = year, y = number_in_custody, fill = Ethnicity)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

custody_eth_plot
##ethnic minority groups are being counted twice! remove the summary measure


################# Average monthly youth custody population by gender######################################
custody_gen_data <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 20, skip = 3, n_max = 24)
colnames(custody_gen_data)[2] <- "region_home_yjs"

custody_gen_data <- custody_gen_data %>%
  mutate(Gender = vec_fill_missing(Gender, direction = c("down")))

custody_gen_data <- custody_gen_data[!is.na(custody_gen_data$region_home_yjs), ]

custody_gen_data <- custody_gen_data %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_custody")


custody_gen_plot <- ggplot(custody_gen_data[custody_gen_data$region_home_yjs == "West Midlands", ], aes(x = year, y = number_in_custody, fill = Gender)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

custody_gen_plot




################# Average monthly youth custody population by age UNDER !8######################################
custody_age_data <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 19, skip = 3, n_max = 48)
colnames(custody_age_data)[2] <- "region_home_yjs"

custody_age_data <- custody_age_data %>%
  mutate(Age = vec_fill_missing(Age, direction = c("down")))

custody_age_data <- custody_age_data[!is.na(custody_age_data$region_home_yjs), ]

custody_age_data <- custody_age_data %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_custody")


custody_age_plot <- ggplot(custody_age_data[custody_age_data$region_home_yjs == "West Midlands", ], aes(x = year, y = number_in_custody, fill = Age)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

custody_age_plot


################# time in custody######################################
##Table 7.32: Legal basis episodes ending by Youth Justice Service region(1) and nights, years ending March 2019 to 2021

custody_time_data <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 33, skip = 3, n_max = 71)
custody_time_data <- custody_time_data[,1:5]
colnames(custody_time_data)[1] <- "region_home_yjs"
colnames(custody_time_data)[2] <- "number_nights"
colnames(custody_time_data)[3] <- "2019"
colnames(custody_time_data)[4] <- "2020"
colnames(custody_time_data)[5] <- "2021"

custody_time_data <- custody_time_data %>%
  mutate(region_home_yjs = vec_fill_missing(region_home_yjs, direction = c("down")))

custody_time_data <- custody_time_data[!is.na(custody_time_data[2]), ]

custody_time_data <- custody_time_data %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_timespan")

#####this one requires more careful thinking about how to graph

##Table 7.27: Legal Basis ending by legal basis type and nights, years ending March 2019 to 2021
custody_time_data2 <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 28, skip = 3, n_max = 22)
custody_time_data2 <- custody_time_data2[,1:5] ##lol find a better name than 2
colnames(custody_time_data2)[1] <- "legal_basis"
colnames(custody_time_data2)[2] <- "number_nights"
colnames(custody_time_data2)[3] <- "2019"
colnames(custody_time_data2)[4] <- "2020"
colnames(custody_time_data2)[5] <- "2021"

custody_time_data2 <- custody_time_data2 %>%
  mutate(legal_basis = vec_fill_missing(legal_basis, direction = c("down")))

custody_time_data2 <- custody_time_data2[!is.na(custody_time_data2[2]), ]

custody_time_data2 <- custody_time_data2 %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_timespan") %>%
  filter(number_nights == "Median number of nights")



