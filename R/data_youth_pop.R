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

pop_plot

pop_plot2 <- ggplot(pop_df, aes(x = year, y = population, fill = interaction(age, gender))) +
  geom_bar(position = "dodge", stat = "identity")

pop_plot2
