
#"https://www.gov.uk/government/statistics/police-powers-and-procedures-stop-and-search-and-arrests-england-and-wales-year-ending-31-march-2021/data/arrest-police-powers-procedures-mar21-tables-2e.ods"
#NOTE!!!!!!!!!DOES THE ARREST DATA CONTAIN THE STOP AND SEARCH ARRESTS????
#NOTE also !!! what is a notifiable offense and are these just arrrests for noitifyable in which case,,, are there others.


# there is also: table 1.6: Number of arrests for recorded crime (notifiable offences) by
# police force area, by sex and age group, year ending March 2021
# <- chapter one in youth justice supplementaries

arrest_data <- read.csv("/Users/katehayes/temp_data/arrests-open-data-tables-ppp-2e (1).csv")

arrest_data <- arrest_data %>%
  filter(Force.Name == "West Midlands", Age.group == "10 - 17 years", Financial.Year %in% c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21"))

arrest_data$Arrests <- as.numeric(arrest_data$Arrests)

arrest_plot <- ggplot(arrest_data, aes(x = Financial.Year, y = Arrests, fill = Reason.for.arrest..offence.group.)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Gender)

arrest_plot

arrest_sum <- arrest_data %>%
  group_by(Financial.Year, Gender, Reason.for.arrest..offence.group.) %>%
  summarise(Arrests = sum(Arrests)) %>%
  ungroup() %>%
  group_by(Gender, Reason.for.arrest..offence.group.) %>%
  summarise(tot_arrests = mean(Arrests))



cl_arrest_data <- arrest_data %>%
  filter(Reason.for.arrest..offence.group. %in% c("2015/16 onwards - Drug offences", "2015/16 onwards - Possession of weapons offences", "2015/16 onwards - Violence against the person"))


cl_arrest_plot <- ggplot(cl_arrest_data, aes(x = Financial.Year, y = Arrests, fill = Reason.for.arrest..offence.group.)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Gender)

cl_arrest_plot

arrest_sum <- arrest_data %>%
  group_by(Financial.Year, Gender) %>%
  summarise(tot_arrests = sum(Arrests))

cl_arrest_sum <- cl_arrest_data %>%
  group_by(Financial.Year, Gender, Reason.for.arrest..offence.group.) %>%
  summarise(tot_arrests = sum(Arrests))


cl_arrest_sum_m <- cl_arrest_sum %>%
  group_by(Gender) %>%
  summarise(av_arrests = sum(tot_arrests)/6)

arrest_sum_m <- arrest_sum %>%
  group_by(Gender) %>%
  summarise(av_arrests = sum(tot_arrests)/6)

#acl_arrest_sum <- cl_arrest_sum %>%
#a pivot_wider(id_cols = c(Financial.Year,Gender), names_from = Reason.for.arrest..offence.group., values_from = tot_arrests)



#are there any reasons we especially care about?
#for county lines we care about drugs, possession of weapons, and violence against the person

#"%>%   , fill = arrest_data[,8])
#" group_by(age_group, gender) %>%
    #" summarise(tot_searches_age_gen = sum(number_of_searches))
