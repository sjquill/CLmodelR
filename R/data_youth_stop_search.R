##seems like the data is actually decent on this lol
##https://www.gov.uk/government/statistics/police-powers-and-procedures-stop-and-search-and-arrests-england-and-wales-year-ending-31-march-2021

##so we have three spreedsheets to work from
##this one A (stop-search-open-data-tables-ppp-mar2021.ods) is 20/21 and
##this one B (stop-search-open-data-tables-ppp.ods) is 2016/17 to 2019/20 and
##this one C (stop-search-open-data-tables-ppp-mar2020.ods) is 2006/07 to 2019/20

##this one (stop-search-data-tables-outcomes-ppp-mar2021.ods) is just summarised versions of the others

##all at police force ie west midlands level

##in A, reasons for search is in the same sheet as search outcomes - they're easily linked
##in B, these two things are seperate
##in C, we are missing the more detailed outcomes measures that A and B have.
##Arrest, Summons, Caution, Community resolution, Khat/cannabis warning,
##Penalty notice for disorder, Voluntary attendance, Verbal warning,
##Seizure of property, Other action, No further action
##C only has whether or not a resultant arrest happened
##it also does have linkage info like the others - as in whether the outcome is related to the reason for search
##C does have reason for searhc though, which is good.

##i think ultimately try to work with A and B for the time period they cover
## then use C if you need
##A is quarterly as well - it indicates the quarter of the financial year that the search was recorded in
##ONLY A HAS AGE FUCK

##note - to do all this i had to install libreoffice and convert the ods files to csv
##at the command line
##also, the sheet you want needs to be first. lol. and when i converted the second file i changed names AGH

##MESS MESS MESS COME BACK AND REDO NICELY

ss21_data <- read.csv("/Users/katehayes/temp_data/stop-search-open-data-tables-ppp-mar2021.csv")
ss16_reason_data <- read.csv("/Users/katehayes/temp_data/stop-search-open-data-tables-ppp_reason.csv")
ss16_outcome_data <- read.csv("/Users/katehayes/temp_data/stop-search-open-data-tables-ppp_outcome.csv")

##so, what do i want to find out?
##how many children are the west midlands police searching over time
##what percentage leads to arrest (over time)

##but only a has age - so lets see what percentage of stop and searches are children over the 4 quarters of this year.
##total stop&searches on kids, young people, versis the rest, each quarter, just west midlands

##NOTE!! this is weighted data - some rows are weighted bc that same combo has been observed multiple times


ss21_data <- ss21_data %>%
  filter(police_force_area == "West Midlands")

ss_sum_data <- ss21_data %>%
  filter(age_group == "10-17", gender != "Unknown") %>%
  group_by(gender, outcome) %>%
  summarise(tot = sum(number_of_searches))

drug_ss_data <- ss21_data %>%
  filter(age_group == "10-17", gender != "Unknown", reason_for_search == "Drugs", link == "Linked") %>%
  group_by(gender, outcome) %>%
  summarise(tot = sum(number_of_searches))


plot1 <- ss21_data %>%
  filter(age_group == "10-17", gender != "Unknown", outcome == "Seizure of Property") %>%
  group_by(financial_year_quarter) %>%
  ggplot(aes(x = reason_for_search, y = number_of_searches)) +
  facet_wrap(~gender) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

plot1


ss_drug <- ss21_data %>%
  filter(age_group == "10-17", gender != "Unknown", outcome == "Seizure of Property", reason_for_search == "Drugs") %>%
  group_by(gender) %>%
  summarise(tot = sum(number_of_searches))

ss21_data <- ss21_data[ , !names(ss21_data) %in% c("geocode")]



ss21_plot <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = age_group)) +
  geom_bar(stat = "count", position = "fill") +

ss21_plot


ss21_gender_plot <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = gender)) +
  geom_bar(stat = "count", position = "fill") +
  facet_wrap(~age_group)

ss21_gender_plot
##age proportions pretty even between the genders
##women lets say 7% of total stop and searches


ss21_gender_plot2 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "10-17", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = outcome)) +
    geom_bar(stat = "count", position = "fill") +
    facet_wrap(~gender)

ss21_gender_plot2
##for kids, girls a lot less likely to be arrested than boys - hover at 5%, less?

ss21_gender_plot3 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "10-17", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = reason_for_search)) +
  geom_bar(stat = "count", position = "fill") +
  facet_wrap(~gender)

ss21_gender_plot3
##girls more likely to be searched for stolen propery and less for firearms


##note - stop and searches of kids are about 12pc of total, across the year (thats 10-17yos)
##now i want to know how the reasons & outcomes of the ss's differed for kids compared to the rest this year
##because maybe i can extrapolate from that to the rest of the years where we dont have age data

ss21_outcome_plot <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = outcome)) +
  geom_bar(stat = "count") + #, position = "fill"
  facet_wrap(~age_group)

ss21_outcome_plot

##note - about 12 pc of kids that get stop and searched get arrested - seems pretty similar across age groups

ss21_reason_plot <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = reason_for_search)) +
  geom_bar(stat = "count", position = "fill") + #, position = "fill"
  facet_wrap(~age_group)

ss21_reason_plot

##note - more searches for 'going equipped' and offensive weapons compared to other groups
##comparatively less for drugs

ss21_link_plot <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = link)) +
  geom_bar(stat = "count", position = "fill") + #, position = "fill"
  facet_wrap(~age_group)

ss21_link_plot

##note - lower percentage of link! like 15 rather than 25 for the others
##higher percentage of nothing found.

ss21_lr_plot <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "10-17", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = link)) +
  geom_bar(stat = "count", position = "fill") + #, position = "fill"
  facet_wrap(~reason_for_search)

ss21_lr_plot

ss21_lr_plot2 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "18-24", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = link)) +
  geom_bar(stat = "count", position = "fill") + #, position = "fill"
  facet_wrap(~reason_for_search)

ss21_lr_plot2

##i wonder how what they find (outcomes, link etc) is related to the volume of ss's they make

##in general, for kids, drugs is the reason where they're most likely to find what they're looking for, followed by offensive weapons
##in going equipped and stolen property they dont find very much stuff, and they're as likely to find something else as they are the thing they seaid they were searching for

ss21_lr_plot3 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "10-17", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = link)) +
  geom_bar(stat = "count", position = "fill") + #, position = "fill"
  facet_wrap(~outcome)

ss21_lr_plot3

##some stuff is confusing - like how can you have a cannabis warning or seizure of property if there is 'nothing found'
##about 50% of arrests for kids are due to stuff being found that is linked to reason for search - about the same with 18-24
##in about 30 decreasing to 25% of arrests nothing is found.

ss21_lr_plot4 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "10-17", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = outcome)) +
  geom_bar(stat = "count", position = "fill") + #, position = "fill"
  facet_wrap(~reason_for_search)

ss21_lr_plot4

ss21_lr_plot5 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "10-17", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = reason_for_search)) +
  geom_bar(stat = "count", position = "fill") + #, position = "fill"
  facet_wrap(~outcome)

ss21_lr_plot5

##OK ! you can ask kind of interesting questions from this. im most interested in arrests i guess
##for the kids that get arrested, more of them were stopped for offensive weapons than drugs
##lets check that? whats the linking look like?

ss21_lr_plot6 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands" & ss21_data$age_group == "10-17" & ss21_data$outcome == "Arrest", ], aes(x = financial_year_quarter, weight = number_of_searches, fill = link)) +
  geom_bar(stat = "count") + #, position = "fill"
  facet_wrap(~reason_for_search)

ss21_lr_plot6

##more of them were BEING stopped for offensive weapons, but they actually had a higher hit rate in the drug searches


ss21_lr_plot7 <- ggplot(ss21_data[ss21_data$police_force_area == "West Midlands", ], aes(x = financial_year_quarter, weight = number_of_searches)) +
  geom_bar(stat = "count") #, position = "fill"

ss21_lr_plot7

ss21_data_wm <- ss21_data[ss21_data$police_force_area == "West Midlands", ]

searches21 <- aggregate(ss21_data_wm$number_of_searches, by = list(age=ss21_data_wm$age_group), FUN = sum)

##25895	searches total in year ending 21, 0.132380768 percent are under 17

##################################FINE - lets bring in the later years##################################& ss21_data$age_group == "10-17"  facet_wrap(~reason_for_search)  , fill = link
ss16_reason_data$Searches <- as.numeric(ss16_reason_data$Searches)
ss16_outcome_data$Searches <- as.numeric(ss16_outcome_data$Searches)

ss16_plot <- ggplot(ss16_reason_data[ss16_reason_data$Force.Name == "West Midlands", ], aes(x = Financial.Year, weight = Searches)) +
  geom_bar(stat = "count") #, position = "fill"

ss16_plot

ss16_plot2 <- ggplot(ss16_outcome_data[ss16_outcome_data$Force.Name == "West Midlands", ], aes(x = Financial.Year, weight = Searches)) +
  geom_bar(stat = "count")

ss16_plot2

####cool so it looks like they're the exact same number (good)

ss16_data_wm <- ss16_outcome_data[ss16_outcome_data$Force.Name == "West Midlands", ]

searches16 <- aggregate(ss16_data_wm$Searches, by = list(year = ss16_data_wm$Financial.Year), FUN = sum)

searches16

searches_out_16 <- aggregate(ss16_data_wm$Searches, by = list(year = ss16_data_wm$Financial.Year, outcome = ss16_data_wm$Outcome), FUN = sum)

searches_out_16

###

arrest <- ss21_data %>%
  filter(police_force_area == "West Midlands", outcome == "Arrest") %>%
  summarise(pc_arrests_age_gen = sum(number_of_searches))

arrest_pc_age_gender <- ss21_data %>%
  filter(police_force_area == "West Midlands", outcome == "Arrest") %>%
  group_by(age_group, gender) %>%
  summarise(pc_arrests_age_gen = sum(number_of_searches)/arrest)

kate_data <- ss21_data %>%
  filter(police_force_area == "West Midlands") %>%
  group_by(age_group, gender) %>%
  summarise(tot_searches_age_gen = sum(number_of_searches))

kate_data3 <- ss21_data %>%
  filter(police_force_area == "West Midlands") %>%
  group_by(financial_year) %>%
  summarise(tot_searches = sum(number_of_searches))

kate_data4 <- ss21_data %>%
  filter(police_force_area == "West Midlands") %>%
  group_by(financial_year, outcome) %>%
  summarise(searches = sum(number_of_searches))

kate_data2 <- ss16_outcome_data %>%
  filter(Force.Name == "West Midlands") %>%
  group_by(Financial.Year) %>%
  summarise(tot_searches = sum(Searches))

kate_data5 <- ss16_outcome_data %>%
  filter(Force.Name == "West Midlands") %>%
  group_by(Financial.Year, Outcome) %>%
  summarise(searches = sum(Searches))

kate_data5 <- rename(kate_data5, financial_year = Financial.Year, outcome = Outcome)

kate_bind <- rbind(kate_data5, kate_data4)
kate_bind$outcome <- str_to_lower(kate_bind$outcome)

####actually at this point i can probably make the graph i care about?

k8_plot <- ggplot(kate_bind, aes(x = financial_year, y = searches, fill = outcome)) +
  geom_bar(stat = "identity", position = "fill")

k8_plot




kate_bind$outcome <- str_to_lower(kate_bind$outcome)

kate_bind <- kate_bind %>%
  pivot_wider(id_cols = financial_year,
              names_from = outcome,
              values_from = searches)

kate_data2 <- rename(kate_data2, financial_year = Financial.Year)

kate_bind2 <- rbind(kate_data3, kate_data2)

total_kate <- merge(kate_bind, kate_bind2, by="financial_year")

no_further_pc <- total_kate %>%
  summarise(no_further_pc = `no further action`/tot_searches)

arrest_pc <- total_kate %>%
  summarise(arrest = arrest/tot_searches)

age_gen_pc <- kate_data %>%
  mutate(age_gen_pc = tot_searches_age_gen/25895)


a <- sum()


####so i kind of lost focus.... i should have been thinking about what are the
####outcomes in terms of parameters for the model
####like where does stop and search send you, and what percentage of the time




####so i want to like graph the 4 years and then have the 5th there, witth the kids highlighted.
####and you can see do the trends kind of line up
