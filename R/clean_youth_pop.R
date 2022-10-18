library(knitr)
library(kableExtra)


readin_df <- read_excel("/Users/katehayes/CL model/data4model/population/nomis_2022_07_19POP DATA FROM GOVT WEBSITE.xlsx")

##drop below fives, and the summary ages every five years, and above 24
pop_data <-  readin_df[readin_df$age != 'All Ages' & readin_df$age != 'Age 0 - 4' & !grepl("Aged", readin_df$age), ]
##make age numeric
pop_data$age <- as.numeric(gsub('[Age ]', '', pop_data$age))



##GRAPH OUTPUT

pop_data %>%
  filter(age %in% 10:17) %>%
  group_by(gender, year) %>%
  summarise(tot_pop = sum(population)) %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_pop, colour = gender)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  scale_colour_manual(values = c("#00cc99", "#6600cc"), name="Sex") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/tot_pop_10-17.png")


pop_data %>%
  filter(age %in% 10:17) %>%
  ggplot() +
  geom_line(aes(x = age, y = population, colour = gender)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  scale_colour_manual(values = c("#00cc99", "#6600cc"), name="Sex") +
  facet_wrap(~year) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/pop_byage_10-17.png")


##DATA OUTPUT - summary numbers for the model
sum_pop_data <- pop_data %>%
  filter(age %in% 10:17) %>%
  group_by(gender, year) %>%
  summarise(tot_pop = sum(population))

in_pop_data <- pop_data %>%
  filter(age == 10) %>%
  group_by(gender, year) %>%
  summarise(in_pop = population)

out_pop_data <- pop_data %>%
  filter(age == 17) %>%
  group_by(gender, year) %>%
  summarise(out_pop = population)

sum_pop_data <- sum_pop_data %>%
  left_join(in_pop_data, by = c("gender", "year")) %>%
  left_join(out_pop_data, by = c("gender", "year"))

mean_pop <- sum_pop_data %>%
  group_by(gender) %>%
  summarise(av_tot_pop = mean(tot_pop), av_in_pop = mean(in_pop), av_out_pop = mean(out_pop)) %>%
    kable("latex", booktabs = TRUE)
