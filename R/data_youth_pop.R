# HERE IS COLIN READING IN DATA FROM INTERNET
#Read in data by age
# temp1 <- tempfile()
# source1 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsrelatedtodrugpoisoningenglandandwalesreferencetable%2fcurrent/2019maindataset1.xls"
# temp1 <- curl_download(url=source1, destfile=temp1, quiet=FALSE, mode="wb")
# raw.u20 <- read_excel(temp1, sheet="Table 2", range="L9:R91", col_names=FALSE)[-c(28,56),-c(4)]
# colnames(raw.u20) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning",
#                        "Combined_Misuse", "England_Misuse", "Wales_Misuse")
#


##POPULATION GRAPH
readin_df <- read_excel("/Users/katehayes/CL model/data4model/population/nomis_2022_07_19POP DATA FROM GOVT WEBSITE.xlsx")

##for our purposes here im going to drop below fives, and the summary ages every five years, and above 24
pop_df <-  readin_df[readin_df$age != 'All Ages' & readin_df$age != 'Age 0 - 4' & !grepl("Aged", readin_df$age), ]

pop_df$age <- as.numeric(gsub('[Age ]', '', pop_df$age))
# i added this later to fix something in the pop plot but it might make the next plot weird so just check


#Set up grouped path plot
test_plot_data <- pop_df %>%
  filter(age %in% 10:17, year %in% 2005:2006) %>%
  arrange(age, year) %>%
  mutate(index=c(1:32))

x1k <- c(0, test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==10],
        test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==11],
        test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==12],
        test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==13],
        test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==14],
        test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==15],
        test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==16],
        test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Male" & test_plot_data$age==17],0)


x1kg <- c(0, test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==10],
          test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==11],
          test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==12],
          test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==13],
          test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==14],
          test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==15],
          test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==16],
          test_plot_data$population[test_plot_data$year==2005 & test_plot_data$gender=="Female" & test_plot_data$age==17],0)

x2k <- c(0, test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==10],
         test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==11],
         test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==12],
         test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==13],
         test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==14],
         test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==15],
         test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==16],
         test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Male" & test_plot_data$age==17],0)

x2kg <- c(0, test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==10],
          test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==11],
          test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==12],
          test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==13],
          test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==14],
          test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==15],
          test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==16],
          test_plot_data$population[test_plot_data$year==2006 & test_plot_data$gender=="Female" & test_plot_data$age==17],0)



plot <- ggplot()+
  geom_polygon(aes(x=c(1, 1:7, 7), y=x1k), fill="Tomato")+
  geom_polygon(aes(x=c(8, 8:15, 15), y=x1kg), fill="Tomato")+
  geom_polygon(aes(x=c(16, 16:23, 23), y=x2k), fill="Tomato")+
  geom_polygon(aes(x=c(24, 24:31, 31), y=x2kg), fill="Tomato")+
  geom_path(data=test_plot_data, aes(x=index, y=population, group=year))+
  theme_classic()+
  scale_x_continuous(breaks=c(4,11,19,27), labels=c("2005-Boys", "2005-Girls",
                           "2006-Boys", "2006-Girls"), name = "Year")+
  scale_y_continuous(name="Pop", limits=c(0,1500))+
  labs(title="Population")

plot









pop_plot <- pop_df %>%
  ggplot() +
  geom_line(aes(x=year, y=Deaths, colour=sex)) +
  scale_x_continuous(name="") +
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="Sex") +
  facet_wrap(~age) +
  theme_classic() +
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Most drug-related deaths are among men",
       subtitle="Deaths from drug poisoning in England & Wales",
       caption="Date from Office for National Statistics | Plot by @VictimOfMaths")


pop_plot <- pop_df %>%
  filter(age %in% 10:17) %>%
  group_by(gender, year) %>%
  mutate(tot_pop = sum(population)) %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_pop, colour = gender)) +
  scale_x_continuous(name="") +
  scale_y_continuous(name="") +
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="Sex") +
  theme_classic() +
  theme(strip.background=element_blank())+
  labs(title="Population aged 10-17 in Birmingham 2005-2020")

pop_plot


pop_plot <- pop_df %>%
  filter(age %in% 10:17) %>%
  ggplot(aes(x = age, y = population, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year) +
  theme_classic() +
  theme(strip.background=element_blank())+
  labs(title="Population aged 10-17 in Birmingham 2005-2020")

pop_plot




pop_plot <- pop_df %>%
  filter(age %in% 10:17) %>%
  ggplot() +
  geom_line(aes(x = age, y = population, colour = gender)) +
  scale_x_continuous(name="") +
  scale_y_continuous(name="") +
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="Sex") +
  facet_wrap(~year) +
  theme_classic() +
  theme(strip.background=element_blank())+
  labs(title="Population aged 10-17 in Birmingham 2005-2020")

pop_plot

pop_plot <- ggplot(pop_df, aes(x = age, y = population)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  theme_bw(axis.text.x=element_text(angle = -90, hjust = 0))


pop_plot

pop_age <- pop_df %>%
  filter(year %in% seq(2016, 2020), age %in% c("Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age 15", "Age 16", "Age 17")) %>%
  group_by(gender, age) %>%
  summarise(av = mean(population)) %>%
  ungroup() %>%
  group_by(gender) %>%
  mutate(av_single_year = mean(av))

age_plot <- pop_age %>%
  ggplot(aes(x = age, y = av, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

age_plot

pop <- pop_df %>%
    filter(year %in% seq(2016, 2020), age %in% c("Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age 15", "Age 16", "Age 17") ) %>%
    group_by(gender, year) %>%
    summarise(total_pop = sum(population))

arrest <- ss21_data %>%
  filter(police_force_area == "West Midlands", outcome == "Arrest") %>%
  summarise(pc_arrests_age_gen = sum(number_of_searches))


pop_plot

pop_plot2 <- ggplot(pop_df, aes(x = year, y = population, fill = interaction(age, gender))) +
  geom_bar(position = "dodge", stat = "identity")

pop_plot2
