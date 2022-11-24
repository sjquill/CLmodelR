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

remand_data_21 <- remand_data

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

remand_data$year <- 2020

remand_data_20 <- remand_data

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

##################SENTENCING OUTCOMES FOR CHILDREN ON REMAND######################################
remand_out_data <- read_xls("/Users/katehayes/temp_data/Ch 6 - Use of remand for children.xls", sheet = 7, skip = 3, n_max = 24)
remand_out_data <- remand_out_data[19:21,]
colnames(remand_out_data)[1] <- "outcome"

remand_out_data <- remand_out_data %>%
  mutate(`2015` = as.numeric(`2015`)) %>%
  pivot_longer(!outcome, names_to = "year", values_to = "number")


remand_out_data %>%
  mutate(year = as.numeric(year)) %>%
  mutate(outcome = factor(outcome, levels = c("Acquitted", "Non-custodial sentence", "Immediate custody"))) %>%
  ggplot() +
  geom_area(aes(x = year, y = number,
                group = outcome, fill = outcome),
            colour = "black", size = .2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_brewer(palette = "Blues")
ggsave(filename = "Output/Graphs/eng_remand_outcome.png")

remand_out_data %>%
  mutate(year = as.numeric(year)) %>%
  mutate(outcome = factor(outcome, levels = c("Acquitted", "Non-custodial sentence", "Immediate custody"))) %>%
  group_by(year) %>%
  mutate(tot = sum(number)) %>%
  mutate(number = number/tot) %>%
  ggplot() +
  geom_area(aes(x = year, y = number,
                group = outcome, fill = outcome),
            colour = "black", size = .2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_brewer(palette = "Blues")
ggsave(filename = "Output/Graphs/eng_remand_outcome_pc.png")



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
custody_data %>%
  mutate(`Legal basis` = factor(`Legal basis`,
                levels = c("DTO", "Section 91", "Remand", "Other sentences(2)"))) %>%
ggplot() +
  geom_area(aes(x = as.numeric(year), y = number_in_custody, fill = fct_rev(`Legal basis`))) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/westmid_custody_bylegal.png")
#basically remand comes down then goes back up, while total sentenced coming down, mostly due to drop in DTOs

#data output
custody_pc_av_data <- custody_data %>%
  filter(year != "2021")  %>%
  mutate(tot_custody = sum(number_in_custody)) %>%
  group_by(`Legal basis`) %>%
    summarise(av_number = mean(number_in_custody), av_percent = sum(number_in_custody)/tot_custody) %>%
  distinct(`Legal basis`, av_percent, av_number) %>%
kable("latex", booktabs = TRUE)
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
  mutate(Gender = vec_fill_missing(Gender, direction = c("down"))) %>%
  filter(!is.na(region_home_yjs)) %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_custody")


  custody_gen_data %>%
  filter(region_home_yjs == "West Midlands") %>%
    ggplot() +
    geom_area(aes(x = as.numeric(year), y = number_in_custody, fill = fct_rev(Gender))) +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "") +
    theme_classic() +
    theme(strip.background = element_blank())
  ggsave(filename = "Output/Graphs/westmid_custody_bygender.png")

  av_custody_gen_data <- custody_gen_data %>%
    filter(region_home_yjs == "West Midlands", year != "2021") %>%
    mutate(tot = sum(number_in_custody)) %>%
    group_by(Gender) %>%
    summarise(av_percent = sum(number_in_custody)/tot, av_number = mean(number_in_custody)) %>%
    distinct(Gender, av_percent, av_number) %>%
    kable("latex", booktabs = TRUE)

################# Average monthly youth custody population by age UNDER !8######################################
custody_age_data <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 19, skip = 3, n_max = 48)
colnames(custody_age_data)[2] <- "region_home_yjs"

custody_age_data <- custody_age_data %>%
  mutate(Age = vec_fill_missing(Age, direction = c("down"))) %>%
  filter(!is.na(region_home_yjs)) %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_custody")


custody_age_data %>%
  filter(region_home_yjs == "West Midlands") %>%
  ggplot() +
  geom_area(aes(x = as.numeric(year), y = number_in_custody, fill = Age)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/westmid_custody_byage.png")


av_custody_age_data <- custody_age_data %>%
  filter(region_home_yjs == "West Midlands", year != "2021") %>%
  mutate(tot = sum(number_in_custody)) %>%
  group_by(Age) %>%
  summarise(av_percent = sum(number_in_custody)/tot, av_number = mean(number_in_custody)) %>%
  distinct(Age, av_percent, av_number) %>%
  kable("latex", booktabs = TRUE)

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
  filter(region_home_yjs == "West Midlands", !is.na(custody_time_data[2])) %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_timespan") %>%
  filter(number_nights != "Total", number_nights != "Median number of nights")


custody_time_data %>%
  mutate(number_nights = factor(number_nights, levels = c("1 to 91 nights", "92 to 182 nights", "183 to 273 nights", "274+ nights"))) %>%
ggplot() +
  geom_area(aes(x = as.numeric(year), y = number_in_timespan, fill = fct_rev(number_nights))) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE)
ggsave(filename = "Output/Graphs/westmid_custody_bydur.png")

#
# custody_time_data %>%
#   ggplot() +
#   geom_area(aes(x = number_nights, y = number_in_timespan, fill = fct_rev(year))) +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "") +
#   theme_classic() +
#   theme(strip.background = element_blank()) +
#   scale_fill_viridis(discrete = TRUE, direction = -1)
# ggsave(filename = "Output/Graphs/westmid_custody_bydur.png")



av_cus_time_dist <- custody_time_data %>%
  filter(number_nights != "Total", number_nights != "Median number of nights", year !=2021) %>%
  mutate(tot = sum(number_in_timespan)) %>%
  group_by(number_nights) %>%
  summarise(av_percent = sum(number_in_timespan)/tot) %>%
  distinct(number_nights, av_percent) %>%
  kable("latex", booktabs = TRUE)


##Table 7.27: Legal Basis ending by legal basis type and nights, years ending March 2019 to 2021
custody_time_data2 <- read_xlsx("/Users/katehayes/temp_data/Ch 7 - Children in youth custody.xls", sheet = 28, skip = 3, n_max = 22)
custody_time_data2 <- custody_time_data2[,1:5] ##lol find a better name than 2
colnames(custody_time_data2)[1] <- "legal_basis"
colnames(custody_time_data2)[2] <- "number_nights"
colnames(custody_time_data2)[3] <- "2019"
colnames(custody_time_data2)[4] <- "2020"
colnames(custody_time_data2)[5] <- "2021"

custody_time_data2 <- custody_time_data2 %>%
  filter(!is.na(custody_time_data2[2])) %>%
  mutate(legal_basis = vec_fill_missing(legal_basis, direction = c("down")))%>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="number_in_timespan") %>%
  filter(number_nights != "Total")


###getting averages, leaving out 21 bc it was weird
av_cust_time_data2 <- custody_time_data2 %>%
  filter(year != "2021", number_nights != "Median number of nights") %>%
  group_by(legal_basis) %>%
  mutate(tot = sum(number_in_timespan)) %>%
  group_by(legal_basis, number_nights) %>%
  summarise(av_percent = sum(number_in_timespan)/tot) %>%
  distinct(number_nights, legal_basis, av_percent)%>%
  kable("latex", booktabs = TRUE)

# obviously this is not a proper way to calculate a median
median <- custody_time_data2 %>%
  filter(year != "2021", number_nights == "Median number of nights") %>%
  group_by(legal_basis) %>%
  summarise(median = mean(number_in_timespan)) %>%
  kable("latex", booktabs = TRUE)



################# little extra pieces of detail######################################
##Chapter 2: First time entrants to the Youth Justice System########################
##Table 2.9: Numbers of child first time entrants(1)(2), by Local Authority of residence, years ending December 2010 to 2020
first_time_data <- read_xlsx("/Users/katehayes/temp_data/Ch 2 - First time entrants to the youth justice system.xls", sheet = 10, skip = 3, n_max = 199)

first_time_data <- first_time_data %>%
  filter(`Local Authority` == "Birmingham") %>%
  pivot_longer(starts_with("20"),
               names_to="year",
               values_to="ft_entrants") %>%
  mutate(ft_entrants = as.numeric(ft_entrants), year = as.numeric(year))


av <- first_time_data %>%
  filter(year != "2021") %>%
  summarise(av = mean(ft_entrants))

first_time_data %>%
ggplot() +
  geom_line(aes(x = year, y = ft_entrants)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_fte.png")


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

first_cs_data <- rbind(first_cs_data_g, first_cs_data_b) %>%
  mutate(youth_caution = as.numeric(youth_caution)) %>%
  pivot_longer(youth_caution:`Other(4)`,
               names_to="sentence_caution_type",
               values_to="ft_entrants")


av_first_cs_data <- first_cs_data %>%
  filter(year != "2021") %>%
  mutate(tot = sum(ft_entrants)) %>%
  group_by(sex, sentence_caution_type) %>%
  summarise(av_percent = sum(ft_entrants)/tot) %>%
  distinct(av_percent, sex, sentence_caution_type) # %>%
  kable("latex", booktabs = TRUE)

# pc_gender <- first_cs_data %>%
#   filter(year != "2021") %>%
#   group_by(sex) %>%
#   summarise(tot_gender = sum(ft_entrants)/10)

first_cs_data %>%
  group_by(year, sex) %>%
  summarise(ft_entrants = sum(ft_entrants)) %>%
  ggplot() +
  geom_area(aes(x = as.numeric(year), y = ft_entrants, fill = sex)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/eng_fte.png")


# percentage they were of ftes that year
first_cs_data %>%
  group_by(year) %>%
  mutate(tot_yearly = sum(ft_entrants)) %>%
  group_by(year, sex, sentence_caution_type) %>%
  mutate(percent = ft_entrants/tot_yearly) %>%
  filter(sentence_caution_type != "Other(4)") %>%
  filter(sentence_caution_type == "Immediate custody") %>%
  ggplot() +
    geom_area(aes(x = as.numeric(year), y = percent, fill = sex)) +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "") +
    theme_classic() +
    theme(strip.background = element_blank())
  ggsave(filename = "Output/Graphs/eng_fte_custody.png")


  first_cs_data %>%
    group_by(year) %>%
    mutate(tot_yearly = sum(ft_entrants)) %>%
    group_by(year, sex, sentence_caution_type) %>%
    mutate(percent = ft_entrants/tot_yearly) %>%
    filter(sentence_caution_type != "Other(4)") %>%
    filter(sentence_caution_type == "Community sentence") %>%
    ggplot() +
    geom_area(aes(x = as.numeric(year), y = percent, fill = sex)) +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "") +
    theme_classic() +
    theme(strip.background = element_blank())
  ggsave(filename = "Output/Graphs/eng_fte_commsent.png")

  first_cs_data %>%
    group_by(year) %>%
    mutate(tot_yearly = sum(ft_entrants)) %>%
    group_by(year, sex, sentence_caution_type) %>%
    mutate(percent = ft_entrants/tot_yearly) %>%
    filter(sentence_caution_type != "Other(4)") %>%
    filter(sentence_caution_type == "youth_caution") %>%
    ggplot() +
    geom_area(aes(x = as.numeric(year), y = percent, fill = sex)) +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "") +
    theme_classic() +
    theme(strip.background = element_blank())
  ggsave(filename = "Output/Graphs/eng_fte_caut.png")

  first_cs_data %>%
    group_by(year) %>%
    mutate(tot_yearly = sum(ft_entrants)) %>%
    group_by(year, sex, sentence_caution_type) %>%
    mutate(percent = ft_entrants/tot_yearly) %>%
    filter(sentence_caution_type != "Other(4)") %>%
    filter(sentence_caution_type == "youth_caution") %>%
    ggplot() +
    geom_area(aes(x = as.numeric(year), y = percent, fill = sex)) +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "") +
    theme_classic() +
    theme(strip.background = element_blank())
  ggsave(filename = "Output/Graphs/eng_fte_caut.png")



#Table 2.2: Number of chid first time entrants(1)(2), by type of first offence and sex, years ending March 2011 to 2021

first_offence_data <- read_xlsx("/Users/katehayes/temp_data/Ch 2 - First time entrants to the youth justice system.xls", sheet = 3, skip = 3, n_max = 53)
first_offence_data <- first_offence_data[,1:13]
colnames(first_offence_data)[1] <- "year"


first_offence_data_g <- first_offence_data[29:39,] %>%
  mutate(sex = "female")

first_offence_data_b <- first_offence_data[16:26,] %>%
  mutate(sex = "male")

first_offence_data <- rbind(first_offence_data_g, first_offence_data_b) %>%
  mutate(`Violence against the person` = as.numeric(`Violence against the person`)) %>%
  pivot_longer(`Violence against the person`:`Summary motoring offences`,
               names_to="offence_type",
               values_to="ft_entrants")



av_first_offence_data <- first_offence_data %>%
  filter(year != "2021") %>%
  mutate(tot = sum(ft_entrants)) %>%
  group_by(sex, offence_type) %>%
  summarise(av_percent = sum(ft_entrants)/tot) %>%
  distinct(av_percent, sex, offence_type) %>%
kable("latex", booktabs = TRUE)


first_offence_data %>%
  filter(offence_type %in% c("Drug offences", "Possession of weapons",
                             "Violence against the person", "Sexual offences")) %>%
  ggplot() +
  geom_area(aes(x = as.numeric(year), y = ft_entrants, fill = sex)) +
  facet_wrap(~offence_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/eng_fte_offence_tot.png")


first_offence_data %>%
  group_by(year) %>%
  mutate(tot = sum(ft_entrants)) %>%
  group_by(sex, offence_type, year) %>%
  summarise(percent = ft_entrants/tot) %>%
  filter(offence_type %in% c("Drug offences", "Possession of weapons",
                             "Violence against the person", "Sexual offences")) %>%
  ggplot() +
  geom_area(aes(x = as.numeric(year), y = percent, fill = sex)) +
  facet_wrap(~offence_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/eng_fte_offence_percent.png")




################################################################################################
########Chapter 9: Proven reoffending by children in England and Wales########################
#Can do time to reoffending, reoddending by disposal, by offence, by the young offenders institue you were in (think about which one birmingham kids are sent to)
######Table 9.7: Proven reoffending data for children in England and Wales by index disposal(1),  years ending March 2010 to 2020 and quarters for the year ending March 2020

reoffend_data <- read_xlsx("/Users/katehayes/temp_data/KH-Ch 9 - Proven reoffending by children.xls", sheet = 3, skip = 3, n_max = 53)

