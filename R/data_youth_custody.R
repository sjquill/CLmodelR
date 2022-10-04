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


################################################################################################################################
################################################################################################################################
############clocal level open data, outcomes table###############################
################################################################################################################################
##children sentenced and cautioned in birmingham, yearly, financial year 2013/14 to 2020/21

#read in the data
caut_sent_data <- read_ods("/Users/katehayes/temp_data/Outcome_table.ods", sheet = 3)

##ok for my model groupings i need to separate out custody and 'restrictions'
##define restrictions as:
    ##compensation, conditional discharge, referral order, youth conditional caution, youth rehabilitation order

##drop the NA column at the end, keep only Birmingham, drop also any columns i dont want
caut_sent_data <- caut_sent_data[,1:8] %>%
  filter(YJS == "Birmingham") %>%
    group_by(Financial_Year, Caution_or_sentence_type, Caution_or_sentence_tier) %>%
      summarise(Number_Cautioned_Sentenced = sum(Number_Cautioned_Sentenced))

total <- caut_sent_data  %>%
group_by(Financial_Year) %>%
summarise(Number_Cautioned_Sentenced = sum(Number_Cautioned_Sentenced))

##separate out custody
cust_data <- caut_sent_data %>%
  filter(Caution_or_sentence_tier == "Custody")

cust_data$Caution_or_sentence_type <- factor(cust_data$Caution_or_sentence_type, levels = c("Section 226b", "Section 90-91 Detention", "Detention and Training Order"))

cust_plot <- cust_data %>%
      ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced, fill = Caution_or_sentence_type)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0))

cust_plot



##separate out restricted
rest_data <- caut_sent_data %>%
  filter(Caution_or_sentence_type %in% c("Referral Order", "Youth Conditional Caution", "Youth Rehabilitation Order", "Conditional Discharge", "Compensation Order", "Reparation Order"))


sum_rest_data <- rest_data %>%
  group_by(Financial_Year) %>%
    summarise(Number_Cautioned_Sentenced = sum(Number_Cautioned_Sentenced))


rest_plot <- rest_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Caution_or_sentence_type) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

rest_plot

rest_plot2 <- rest_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced, fill = Caution_or_sentence_type)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

rest_plot2
##referral orders are getting more popular, rehavilitation orders less


sum_rest_plot <- sum_rest_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

sum_rest_plot

##SO we have numbers of children sentenced and cautioned
## or in model terms, sentenced to custody or restricted, in birmingham yearly
##additionally it would be useful to know how long each type of caution/sentence lasts
##ideally at the birmingham or west midlands level, but county-level if necessary

##here are the median figures for 2019 and 2020, at total country level:
#DTO - 91.50
#Other -292.25
#Remand - 41.00
################################################################################################################################
################################################################################################################################
################################################################################################################################


##where is children on remand data? only national and yearly.
##so if this is the case then i might have to construct yearly average probabilities of remand into bail, custody, whatever
##and then apply these to the (hopefully) local numbers of children getting charged in birmingham

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
  filter(region_home_yjs == "West Midlands") %>%
  mutate(rem_cus = if_else(`Legal basis` == "Remand", "Remand", "Sentenced")) %>%
        pivot_longer(starts_with("20"),
                     names_to="year",
                     values_to="number_in_custody")




##making plots to visualise
rvs_plot <- custody_data %>%
  ggplot(aes(x = year, y = number_in_custody, fill = rem_cus)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

rvs_plot

rvs_plot2 <- custody_data %>%
  ggplot(aes(x = year, y = number_in_custody, fill = rem_cus)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

rvs_plot2


remand_plot <- custody_data %>%
  filter(`Legal basis` == "Remand") %>%
    ggplot(aes(x = year, y = number_in_custody)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0))

remand_plot

sent_cust_plot <- custody_data %>%
  filter(`Legal basis` != "Remand") %>%
  ggplot(aes(x = year, y = number_in_custody, fill = `Legal basis`)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

sent_cust_plot
#basically remand comes down then goes back up, while total sentenced coming down, mostly due to drop in DTOs

#just computing the total numbers
remand_data <- custody_data %>%
  filter(`Legal basis` == "Remand")  %>%
    group_by(year) %>%
      summarise(number_in_custody = sum(number_in_custody))

sent_cust_data <- custody_data %>%
  filter(`Legal basis` != "Remand")  %>%
  group_by(year) %>%
  summarise(number_in_custody = sum(number_in_custody))

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
##Table 7.32: Legal basis episodes ending by Youth Justice Service region and nights, years ending March 2019 to 2021
##so we also get total episodes ending each year??

##i have no idea why this particular year only goes to 2019 since other years go back way further - change the sheet you're reading


custody_time_data <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 33, skip = 3, n_max = 71)
custody_time_data <- custody_time_data[,1:5]
colnames(custody_time_data)[1] <- "region_home_yjs"
colnames(custody_time_data)[2] <- "number_nights"
colnames(custody_time_data)[3] <- "2019"
colnames(custody_time_data)[4] <- "2020"
colnames(custody_time_data)[5] <- "2021"

custody_time_data <- custody_time_data %>%
  mutate(region_home_yjs = vec_fill_missing(region_home_yjs, direction = c("down"))) %>%
  filter(region_home_yjs == "West Midlands")

custody_time_data <- custody_time_data[!is.na(custody_time_data[2]), ]

custody_time_data <- custody_time_data %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_timespan")


cust_time_plot <- custody_time_data %>%
  filter(number_nights != "Total", number_nights != "Median number of nights") %>%
  ggplot(aes(x = number_nights, y = number_in_timespan)) +
  facet_wrap(~year) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

  cust_time_plot

  (45*173+((182-92)/2)*68+((273-183)/2)*34+330*55)/330
  #its 92.5, so i obv guessed everything here but mayeb 90 as mean days makes sense
  ##this is remand PLUS sentenced to custody

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

###getting averages, leaving out 21 bc it was weird
custody_time_data2 <- custody_time_data2 %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_timespan") %>%
  filter(number_nights == "Median number of nights", year != "2021") %>%
  group_by(legal_basis) %>%
  summarise(median = mean(number_in_timespan))


################# little extra pieces of detail######################################
##Chapter 2: First time entrants to the Youth Justice System########################
##Table 2.9: Numbers of child first time entrants(1)(2), by Local Authority of residence, years ending December 2010 to 2020
first_time_data <- read_xlsx("/Users/katehayes/temp_data/Ch 2 - First time entrants to the youth justice system.xls", sheet = 10, skip = 3, n_max = 199)

first_time_data <- first_time_data %>%
  filter(`Local Authority` == "Birmingham") %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="ft_entrants")

first_time_plot <- first_time_data %>%
  ggplot(aes(x = year, y = ft_entrants)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

first_time_plot
#note: i checked this against total cautions/sentences in Birmingham and it is rising - was 40% in 2013 and is now 60%

#Table 2.4: Number of child first time entrants(1)(2) by sex and type of disposal given on first offence, years ending March 2011 to 2021
#THIS IS NOT GOING WELL LOL

first_cs_data <- read_xlsx("/Users/katehayes/temp_data/Ch 2 - First time entrants to the youth justice system.xls", sheet = 5, skip = 4, n_max = 38)
first_cs_data <- first_cs_data[,1:8]
colnames(first_cs_data)[1] <- "year"
colnames(first_cs_data)[2] <- "youth_caution"

first_cs_data_g <- first_cs_data[28:38,] %>%
  mutate(sex = "female")

first_cs_data_b <- first_cs_data[15:25,] %>%
  mutate(sex = "male")

first_cs_data <- rbind(first_cs_data_g, first_cs_data_b)
first_cs_data$youth_caution <- as.numeric(first_cs_data$youth_caution)

first_cs_data <- first_cs_data %>%
  pivot_longer(youth_caution:`Other(4)`,
               names_to="sentence_caution_type",
               values_to="ft_entrants")


first_cs_plot <- first_cs_data %>%
  filter(sentence_caution_type == "Community sentence") %>%
  ggplot(aes(x = year, y = ft_entrants, fill = sex)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


first_cs_plot <- first_cs_data %>%
  filter(sentence_caution_type == "youth_caution") %>%
  ggplot(aes(x = year, y = ft_entrants, fill = sex)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

first_cs_plot <- first_cs_data %>%
  filter(sentence_caution_type == "Immediate custody") %>%
  ggplot(aes(x = year, y = ft_entrants, fill = sex)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

  first_cs_plot

first_cs_pc <- first_cs_data %>%
  group_by(sex) %>%
  mutate(tot = sum(ft_entrants)) %>%
  group_by(sex, sentence_caution_type) %>%
  mutate(pc = sum(ft_entrants)/tot)

#this doesnt quite work lol....

#data %>%
#group_by(month) %>%
#mutate(per =  100 *count/sum(count)) %>%
#ungroup

pc_plot <- first_cs_pc %>%
  filter(year == "2021") %>%
  ggplot(aes(x = sentence_caution_type, y = pc)) +
  facet_wrap(~sex) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

pc_plot

#female first time entrants - 80% are youth caution, 15% are community sentence, 0.3% are immediate custody, 2% are conditional discharge
#male first time entrants - 70% are youth caution, 25% community sentence, 1.5% immediate custody, 2% conditional discharge

#Table 2.2: Number of chid first time entrants(1)(2), by type of first offence and sex, years ending March 2011 to 2021

first_offence_data <- read_xlsx("/Users/katehayes/temp_data/Ch 2 - First time entrants to the youth justice system.xls", sheet = 3, skip = 3, n_max = 53)
first_offence_data <- first_offence_data[,1:13]
colnames(first_offence_data)[1] <- "year"


first_offence_data_g <- first_offence_data[29:39,] %>%
  mutate(sex = "female")

first_offence_data_b <- first_offence_data[16:26,] %>%
  mutate(sex = "male")

first_offence_data <- rbind(first_offence_data_g, first_offence_data_b)

first_offence_data$`Violence against the person` <- as.numeric(first_offence_data$`Violence against the person`)


first_offence_data <- first_offence_data %>%
  pivot_longer(`Violence against the person`:`Summary motoring offences`,
               names_to="offence_type",
               values_to="ft_entrants")


#ok so this is the average across the years
first_offence_pc <- first_offence_data %>%
  group_by(sex) %>%
  mutate(count = sum(ft_entrants)) %>%
  group_by(offence_type, .add = TRUE) %>%
  mutate(pc = sum(ft_entrants)/count)

pc2_plot <- first_offence_pc %>%
  filter(year == "2021") %>%
  ggplot(aes(x = offence_type, y = pc)) +
  facet_wrap(~sex) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

pc2_plot

#obut now year by year?

pc3_plot <- first_offence_pc %>%
  group_by(sex, year) %>%
  mutate(count = sum(ft_entrants)) %>%
  group_by(offence_type, .add = TRUE) %>%
  mutate(pc = sum(ft_entrants)/count) %>%
  ggplot(aes(x = year, y = pc, fill = sex)) +
  facet_wrap(~offence_type) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

pc3_plot

#for first time entrants, percentage by offence
#possession of weapons growing for both girls & boys
#average across last few years is 3% girl 8% boy

#drug offenses pretty steady, maybe growth in boys
# av girl 5% boy 13%

#violence against the persson increasing in both but more sharply in girls
#av girl 4% boy 5%

#boys commit relatively more drug and weapon offenses (even as they commit many more in absolute terms)

################################################################################################
########Chapter 9: Proven reoffending by children in England and Wales########################
#Can do time to reoffending, reoddending by disposal, by offence, by the young offenders institue you were in (think about which one birmingham kids are sent to)
######Table 9.7: Proven reoffending data for children in England and Wales by index disposal(1),  years ending March 2010 to 2020 and quarters for the year ending March 2020


