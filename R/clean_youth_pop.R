library(knitr)
library(kableExtra)
library(readxl)
library(tidyverse)

readin_df <- read_excel("/Users/katehayes/CL model/data4model/population/nomis_2022_07_19POP DATA FROM GOVT WEBSITE.xlsx")

##drop below fives, and the summary ages every five years, and above 24
pop_data <-  readin_df[readin_df$age != 'All Ages' & readin_df$age != 'Age 0 - 4' & !grepl("Aged", readin_df$age), ]
##make age numeric
pop_data$age <- as.numeric(gsub('[Age ]', '', pop_data$age))

pop_data %>%
  filter(age %in% 9:18, year %in% 2009:2020) %>%
  group_by(gender) %>%
  pivot_wider(names_from = "age", values_from = "population") %>%
  kable("latex", booktabs = TRUE)

# lets try do ACTUAL total ACTUAL in and ACTUAL out each year for m/f
# so thats SUM of 10:17 each year
# nine year olds last year? or is it ten year olds this year?
# then, 17 year olds last year?
# so lets say you START with a certain amount of 10:17 year olds
# at, lets think of it as, JAN 1 2010.
# Over the next year, till JAN 1 2011, an amount of children will flow in
# and that number is all of the people that are ten in 2011 - I THINK
# Over the next year, till JAN 1 2011, an amount of children will flow out
# and that number is all of the people who are 17 THIS YEAR, 2010 - i think!!!


# so thats AGE IN and AGE OUT. but there are other reasons why people go inand out!


sum_pop_data <- pop_data %>%
  filter(age %in% 10:17, year %in% 2010:2020) %>%
  group_by(gender, year) %>%
  summarise(tot_pop = sum(population))

in_pop_data <- pop_data %>%
  filter(age == 10, year %in% 2011:2020) %>%
  group_by(gender, year) %>%
  summarise(in_pop = population) %>%
  mutate(year = year - 1)

av_in <- in_pop_data %>%
  group_by(gender) %>%
  summarise(av_in = mean(in_pop))

out_pop_data <- pop_data %>%
  filter(age == 17,  year %in% 2010:2020) %>%
  group_by(gender, year) %>%
  summarise(out_pop = population)

av_out <- out_pop_data %>%
  group_by(gender) %>%
  summarise(av_out = mean(out_pop))

av_diff <- av_in %>%
  inner_join(av_out, by = "gender") %>%
  group_by(gender) %>%
  summarise(av_diff = av_in - av_out)

sum_in_out_pop_data <- sum_pop_data %>%
  left_join(in_pop_data, by = c("gender", "year")) %>%
  left_join(out_pop_data, by = c("gender", "year"))

sum_in_out_pop_data %>%
  kable("latex", booktabs = TRUE)


sum_in_out_pop_data %>%
  ggplot() +
  geom_line(aes(x = year, y = in_pop, colour = gender)) +
  geom_line(aes(x = year, y = out_pop, colour = gender)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  scale_colour_manual(values = c("#00cc99", "#6600cc"), name="Sex") +
  theme_classic() +
  theme(strip.background = element_blank())

##GRAPH OUTPUT
# first one try combine poverty with population

# OH GOD this depends on something from poverty
pov_pop_data <- pop_data %>%
  filter(age %in% 10:17, year %in% 2015:2020) %>%
  group_by(year) %>%
  summarise(tot_yearly = sum(population)) %>%
  left_join(poverty_data, by = "year") %>%
  mutate(number_pov = tot_yearly*percent, number_not = tot_yearly*(1-percent)) %>%
  pivot_longer(cols = starts_with("number_"), names_to = "poverty", values_to = "number") %>%
  ggplot() +
  geom_area(aes(x = year, y = number, fill = fct_rev(poverty))) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  scale_colour_manual(values = c("#00cc99", "#6600cc")) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/tot_pop_10-17.png")

pov_pop_data


pop_data %>%
  filter(age %in% 10:17, gender == "Female") %>%
  mutate(age = as.character(age)) %>%
  ggplot() +
  geom_area(aes(x = year, y = population, fill = age)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "")
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


