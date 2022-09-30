##POPULATION GRAPH
readin_df <- read_excel("/Users/katehayes/CL model/data4model/population/nomis_2022_07_19POP DATA FROM GOVT WEBSITE.xlsx")

##for our purposes here im going to drop below fives, and the summary ages every five years, and above 24
pop_df <-  readin_df[readin_df$age != 'All Ages', ]
pop_df <-  pop_df[pop_df$age != 'Age 0 - 4', ]
pop_df <-  pop_df[!grepl("Aged", pop_df$age), ]
#pop_df$age <- as.numeric(gsub('[Age ]', '', pop_df$age)) i added this later to fix something in the pop plot but it might make the next plot weird so just check


pop_plot <- ggplot(pop_df, aes(x = age, y = population, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~year) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))



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
