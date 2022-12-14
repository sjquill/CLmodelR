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
  geom_area(aes(x = time_period, y = number,
                group = fct_rev(characteristic), fill = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/birm_care_byage.png")

#########GENDER######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_area(aes(x = time_period, y = number,
                group = characteristic, fill = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_care_bygender.png")

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
ggsave(filename = "Output/Graphs/birm_care_byLA.png")
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
ggsave(filename = "Output/Graphs/birm_care_bylegal.png")
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
ggsave(filename = "Output/Graphs/birm_care_byprovider.png")
#########TYPE OF PLACEMENT######################################################################################################

care_data %>%
  filter(la_name == "Birmingham", cla_group == "Placement", characteristic != "Total") %>% #
  filter(characteristic != "Other placements in the community", characteristic != "Other residential settings", characteristic != "Other placements") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_area(aes(x = time_period, y = number,
                group = fct_reorder(characteristic, number, max), fill = fct_reorder(characteristic, number, max)),
                colour = "black", size = .2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_brewer(palette = "Blues")
ggsave(filename = "Output/Graphs/birm_care_byplacetype.png")

# care_data %>% i broke this when i tried to change it to area
#   filter(la_name == "Birmingham", cla_group == "Placement", characteristic != "Total") %>% #
#   mutate(number = as.numeric(number)) %>%
#   filter(!is.na(number)) %>%
#   mutate(residential = if_else(characteristic == "Residential schools" |
#                                    characteristic == "Other residential settings" |
#                                    characteristic == "Secure units, children's homes and semi-independent living accommodation",
#                                   "residential", "non-residential")) %>%
#   group_by(residential, time_period) %>%
#   mutate(res_number = sum(number)) %>%
#   ggplot() +
#   geom_area(aes(x = time_period, y = number,
#                 group = residential, colour = residential)) +
#   scale_x_continuous(name = "") +
#   scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
#   theme_classic() +
#   theme(strip.background = element_blank())

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
ggsave(filename = "Output/Graphs/birm_care_byneed.png")


########summary######################################################################################################

gender_care_data <- care_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number))

av_gender <- gender_care_data %>%
  summarise(av_number = mean(number))


age_care_data <- care_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(characteristic = fct_relevel(characteristic,
                                      "Under 1 year", "1 to 4 years", "5 to 9 years",
                                      "10 to 15 years", "16 years and over")) %>%
  group_by(characteristic) %>%
  mutate(number = as.numeric(number))

tenplus_age_care_data <- age_care_data %>%
  filter(characteristic == "10 to 15 years" | characteristic == "16 years and over") %>%


av_age <- age_care_data %>%
  summarise(av_number = mean(number)) %>%
  kable("latex", booktabs = TRUE)

res_care_data <- care_data %>%
  filter(la_name == "Birmingham", cla_group == "Placement", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  filter(!is.na(number)) %>%
  mutate(residential = if_else(characteristic == "Residential schools" |
                                 characteristic == "Other residential settings" |
                                 characteristic == "Secure units, children's homes and semi-independent living accommodation",
                               "residential", "non-residential")) %>%
  group_by(time_period, residential) %>%
  mutate(res_number = sum(number)) %>%
  distinct(residential, time_period, res_number)



av_res <- res_care_data %>%
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

# note - this is JUST ten to 17 year olds
care_outcomes_data %>%
  filter(la_name == "Birmingham", cla_group == "Convictions: Children looked after Ages 10 to 17 years") %>%
  mutate(number = ifelse(number == "c", 0, number)) %>%
  mutate(number = as.numeric(number)) %>%
  pivot_wider(id_cols = time_period, names_from = outcome, values_from = number) %>%
  mutate(`Not convicted, cautioned` = `Total ages 10 to 17 years` - `Convicted or subject to youth cautions, or youth conditional cautions during the year`) %>%
  select(-`Total ages 10 to 17 years`) %>%
  pivot_longer(!time_period, names_to = "outcome", values_to = "number")  %>%
  ggplot() +
  geom_area(aes(x = time_period, y = number,
                group = outcome, fill = outcome),
            colour = "black", size = .2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_brewer(palette = "Blues")
ggsave(filename = "Output/Graphs/birm_care_conviction.png")


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


care_outcomes_data %>%
  filter(la_name == "Birmingham") %>%
  mutate(number = ifelse(number == "c" | number == "z", 0, number)) %>%
  mutate(number = as.numeric(number)) %>%
  pivot_wider(id_cols = time_period, names_from = cla_group, values_from = number) %>%
  mutate(`Did not go missing` = `Children looked after during the year` - `Children who had a missing incident during the year`,
         `Went missing once` = `Children who had a missing incident during the year` - `Children who went missing more than once during the year`) %>%
  select(time_period, `Did not go missing`, `Went missing once`, `Children who went missing more than once during the year`) %>%
  pivot_longer(!time_period, names_to = "missing", values_to = "number")  %>%
  mutate(missing = fct_relevel(missing, "Children who went missing more than once during the year", "Went missing once", "Did not go missing")) %>%
  ggplot() +
  geom_area(aes(x = time_period, y = number,
                group = missing, fill = fct_rev(missing)),
            colour = "black", size = .2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_brewer(palette = "PuRd")
ggsave(filename = "Output/Graphs/birm_care_missing.png")

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
  geom_area(aes(x = time_period, y = number,
                group = characteristic, fill = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/birm_care_in_byage.png")

#########GENDER######################################################################################################

care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_area(aes(x = time_period, y = number,
                group = characteristic, fill = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_care_in_bygender.png")

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
ggsave(filename = "Output/Graphs/birm_care_in_bylegal.png")

# REMANd!!!!
care_in_data %>%
  filter(la_name == "Birmingham", cla_group == "Legal status", characteristic == "Remand") %>% #
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) + # , expand = c(0, 0), limits = c(0, NA)
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_headcount_remand.png")

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
ggsave(filename = "Output/Graphs/birm_care_in_byneed.png")

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

# care_out_data %>%
#   filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
#   mutate(number = as.numeric(number)) %>%
#   mutate(characteristic = fct_relevel(characteristic,
#                                       "Under 1 year", "1 to 4 years", "5 to 9 years",
#                                       "10 to 15 years", "16 years", "17 years", "18 years and over")) %>%
#   ggplot() +
#   geom_line(aes(x = time_period, y = number,
#                 group = characteristic, colour = characteristic)) +
#   scale_x_continuous(name = "") +
#   scale_y_continuous(name = "") +
#   theme_classic() +
#   theme(strip.background = element_blank())


care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Age group", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  mutate(characteristic = fct_relevel(characteristic,
                                      "Under 1 year", "1 to 4 years", "5 to 9 years",
                                      "10 to 15 years", "16 years", "17 years", "18 years and over")) %>%
  ggplot() +
  geom_area(aes(x = time_period, y = number,
                group = characteristic, fill = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/birm_care_out_byage.png")
#########GENDER######################################################################################################

care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Gender", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_area(aes(x = time_period, y = number,
                group = characteristic, fill = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_care_out_bygender.png")

#########reason######################################################################################################

care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Reason episode ceased", characteristic == "Sentenced to custody") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_headcount_care2custody.png")

care_out_data %>%
  filter(la_name == "Birmingham", cla_group == "Reason episode ceased", characteristic != "Total") %>%
  mutate(number = as.numeric(number)) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = number, colour = characteristic)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_care_out_byreason.png")


#########rNEW OUTFLOW DATA######################################################################################################
# national_duration_of_placement_ceasing_during_the_year.csv

readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/national_duration_of_placement_ceasing_during_the_year.csv")
care_duration_data <- readin_data

care_duration_data %>%
  filter(placement_group == "Secure units, children???s homes and semi-independent living accommodation") %>%
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


#########rNEW DATA - repeat episodes of care######################################################################################################
readin_data <- read.csv("/Users/katehayes/temp_data/children-looked-after-in-england-including-adoptions_2021/data/national_cla_ceased_during_year_placements_care_periods.csv")
care_repeat_data <- readin_data

# care_repeat_data %>%
#  filter(cla_group == "Number of periods of care", periods_or_placements != "Total",
#         periods_or_placements != "10 or more",
#         age_on_ceasing != "Total",  age_on_ceasing != "Under 1 year",  age_on_ceasing != "1 to 4 years",  age_on_ceasing != "5 to 9 years",) %>%
#  group_by(age_on_ceasing, periods_or_placements, time_period) %>%
#   mutate(number = as.numeric(number)) %>%
#   summarise(number = number) %>%
#   mutate(number = if_else(is.na(number), 0, number)) %>%
#   ggplot() +
#   geom_line(aes(x = periods_or_placements, y = number,
#                 group = age_on_ceasing, colour = age_on_ceasing)) +
#   facet_wrap(~time_period) +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "") +
#   theme_classic() +
#   theme(strip.background = element_blank(), axis.text.x=element_text(angle = -35, hjust = 0)) +
#   scale_colour_viridis(option = "viridis", discrete = TRUE)

care_repeat_data %>%
  filter(cla_group == "Number of periods of care", periods_or_placements != "Total",
         periods_or_placements != "10 or more",
         age_on_ceasing != "Total",  age_on_ceasing != "Under 1 year",  age_on_ceasing != "1 to 4 years",  age_on_ceasing != "5 to 9 years",) %>%
  group_by(age_on_ceasing, periods_or_placements, time_period) %>%
  mutate(number = as.numeric(number)) %>%
  summarise(number = number) %>%
  mutate(number = if_else(is.na(number), 0, number)) %>%
  ggplot() +
  geom_bar(aes(x = age_on_ceasing, y = number,
                fill = fct_rev(periods_or_placements)), position="stack", stat="identity") +
  facet_wrap(~time_period) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  theme(strip.background = element_blank(), axis.text.x=element_text(angle = -35, hjust = 0))
ggsave(filename = "Output/Graphs/eng_headcount_care_duration_yearly.png")

# essentially, most people have one placement, not that many have more than 3




