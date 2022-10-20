# numbers
# la_children_looked_after_at_31_march_by_characteristics.csv
#
# timings
# la_children_who_started_to_be_looked_after_during_the_year.csv
# la_children_who_ceased_during_the_year.csv
# national_duration_of_placement_ceasing_during_the_year.csv
#
# outcomes
# la_children_who_were_missing_or_away_from_placement_during_the_year.csv
# la_conviction_and_health_outcomes_cla.csv

readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/la_children_looked_after_at_31_march_by_characteristics.csv")

# relevant topics - age, gender, type of placement provider(including whether private),
# type of placement (ie youth accomodation etc), legal status (including justice system)
care_data <- readin_data


#########AGE GROUPS######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  mutate(characteristic = fct_relevel(characteristic,
                            "Under 1 year", "1 to 4 years", "5 to 9 years",
                            "10 to 15 years", "16 years and over")) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())

#########GENDER######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())

#########LA OF PLACEMENT######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "LA of placement", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())

#########LEGAL STATUS######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Legal status", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())
# note - the two years they were counting youth justice legal statuses one year was 16 and the other 21

#########PLACE PROVIDERS######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Place providers", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())

#########TYPE OF PLACEMENT######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Placement", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Placement", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  filter(!is.na(number)) %>%
  mutate(residential = if_else(characteristic == "Residential schools" |
                                   characteristic == "Other residential settings" |
                                   characteristic == "Secure units, children's homes and semi-independent living accommodation",
                                  "residential", "non-residential")) %>%
  group_by(residential, time_period) %>%
  mutate(res_number = sum(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = res_number,
                group = residential, colour = residential)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())

#########CATEGORY OF NEED######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Category of need", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())


########summary######################################################################################################

av_gender_care_data <- care_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)


av_age_care_data <- care_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(characteristic = fct_relevel(characteristic,
                                      "Under 1 year", "1 to 4 years", "5 to 9 years",
                                      "10 to 15 years", "16 years and over")) %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)

av_res_care_data <- care_data %>%
  filter(la_name == "Birmingham", cla_group == "Placement", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  filter(!is.na(number)) %>%
  mutate(residential = if_else(characteristic == "Residential schools" |
                                 characteristic == "Other residential settings" |
                                 characteristic == "Secure units, children's homes and semi-independent living accommodation",
                               "residential", "non-residential")) %>%
  group_by(time_period, residential) %>%
  mutate(res_number = sum(number)) %>%
  distinct(residential, time_period, res_number) %>%
  ungroup() %>%
  group_by(residential) %>%
  summarise(av_number = mean(res_number)) %>%
  kable("latex", booktabs = TRUE)

#####################################################################################################
#########NEW DATA - OUTCOMES######################################################################################################
#####################################################################################################
# second dataset - convictions
readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/la_conviction_and_health_outcomes_cla.csv")
care_outcomes_data <- readin_data

av_care_convict_data <- care_outcomes_data %>%
  filter(la_name == "Birmingham", cla_group == "Convictions: Children looked after Ages 10 to 17 years") %>%
  group_by(outcome) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)


# second dataset - missing
readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/la_children_who_were_missing_or_away_from_placement_during_the_year.csv")
care_outcomes_data <- readin_data

av_care_missing_data <- care_outcomes_data %>%
  filter(la_name == "Birmingham") %>%
  mutate(number = as.numeric(number)) %>%
  filter(!is.na(number)) %>%
  group_by(cla_group) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)

#####################################################################################################
#########NEW DATA - time of inflow/outflow, duration######################################################################################################
#####################################################################################################
# INFLOW############################################################################################
readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/la_children_who_started_to_be_looked_after_during_the_year.csv")
care_in_data <- readin_data

care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  mutate(characteristic = fct_relevel(characteristic,
                                      "Under 1 year", "1 to 4 years", "5 to 9 years",
                                      "10 to 15 years", "16 years and over")) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())

#########GENDER######################################################################################################

care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())


#########LEGAL STATUS######################################################################################################

care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Legal status", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())
# REMANd!!!!


#########CATEGORY OF NEED######################################################################################################

care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Category of need", characteristic != "Total") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())


########summary######################################################################################################

av_gender_care_in_data <- care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)


av_age_care_in_data <- care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(characteristic = fct_relevel(characteristic,
                                      "Under 1 year", "1 to 4 years", "5 to 9 years",
                                      "10 to 15 years", "16 years and over")) %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)


av_legstat_care_in_data <- care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Legal status", characteristic != "Total") %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)


#outflow###########################################################################################

readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/la_children_who_ceased_during_the_year.csv")
care_out_data <- readin_data

########summary#####################################################################################################

av_gender_care_out_data <- care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)

av_age_care_out_data <- care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(characteristic = fct_relevel(characteristic,
                                      "Under 1 year", "1 to 4 years", "5 to 9 years",
                                      "10 to 15 years", "16 years", "17 years", "18 years and over")) %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)

av_reason_care_out_data <- care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Reason episode ceased", characteristic == "Sentenced to custody") %>%
  group_by(characteristic, time_period) %>%
  summarise(number = number) %>%
  kable("latex", booktabs = TRUE)

#########AGE#####################################################################################################

care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  mutate(characteristic = fct_relevel(characteristic,
                                      "Under 1 year", "1 to 4 years", "5 to 9 years",
                                      "10 to 15 years", "16 years", "17 years", "18 years and over")) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())

#########GENDER######################################################################################################

care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())

#########reason######################################################################################################

care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Reason episode ceased", characteristic == "Sentenced to custody") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number,
                group = characteristic, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())


#########rNEW OUTFLOW DATA######################################################################################################
# national_duration_of_placement_ceasing_during_the_year.csv

readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/national_duration_of_placement_ceasing_during_the_year.csv")
care_duration_data <- readin_data

care_duration_data %>%
  filter(placement_group == "Secure units, children’s homes and semi-independent living accommodation") %>%
  filter(substr(placement_duration, start = 2, stop = 2) == ".") %>%
  group_by(placement_duration, time_period) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(number = number) %>%
  ggplot() +
  geom_line(aes(x = placement_duration, y = number,
                group = time_period, colour = time_period)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank(), axis.text.x=element_text(angle = -35, hjust = 0)) +
  scale_colour_viridis(option = "viridis", discrete = FALSE)
ggsave(filename = "Output/Graphs/eng_headcount_care_duration_yearly.png")


