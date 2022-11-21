library(viridis)

#
# THIS IS THE EXCLUSIONS DATA GROUP
# # exc_av_days_missed_susp.csv - details about suspensions - rates, average days, number of suspensions per pupil etc
# #
# # exc_characteristics_20210916.csv - details about the children suspended/excluded - ages
# #
# # exc_idaci_sen_need_fsm6.csv - interactions of suspension/exclusion with FSM/other need characteristics,
# # but only at English level - includes Income Deprivation Affecting Children Index
# #
# # exc_pru.csv - pupil referral units & susp/exclusions within them? /to them?
#
# THIS IS THE SCHOOL?PUPILS CHARACTERISTICS GROUp
# spc_pupils_age_and_gender_.csv

readin_data <- read.csv("/Users/katehayes/temp_data/permanent-and-fixed-period-exclusions-in-england_2020-21/data/exc_av_days_missed_susp.csv")

susp_data <- readin_data
names(susp_data)[2] <- "academic_year"
susp_data <- susp_data %>%
  mutate(year = as.numeric(paste(substr(susp_data$academic_year, start = 1, stop = 2),
                                 substr(susp_data$academic_year, start = 5, stop = 6), sep = ""))) %>%
  mutate(academic_year = paste(substr(susp_data$academic_year, start = 1, stop = 4),
                               substr(susp_data$academic_year, start = 5, stop = 6), sep = "-"))

av_susp_durnum_data <- susp_data %>%
  filter(la_name == "Birmingham", school_type != "Total") %>%
  group_by(school_type) %>%
  summarise(average_days_missed_susp = mean(average_days_missed_susp), average_number_susp = mean(average_number_susp)) %>%
  kable("latex", booktabs = TRUE)

susp_data %>%
  filter(la_name == "Birmingham", school_type != "Total") %>%
  ggplot() +
  geom_line(aes(x = year, y = average_days_missed_susp, colour = school_type)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())

susp_data %>%
  filter(la_name == "Birmingham", school_type != "Total") %>%
  ggplot() +
  geom_line(aes(x = year, y = average_number_susp, colour = school_type)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_susp_number.png")



# ok actually this dataset is more relevant for me
readin_data <- read.csv("/Users/katehayes/temp_data/permanent-and-fixed-period-exclusions-in-england_2020-21/data/exc_la_district.csv")

susp_data <- readin_data
names(susp_data)[2] <- "academic_year"
susp_data <- susp_data %>%
  mutate(year = as.numeric(paste(substr(susp_data$academic_year, start = 1, stop = 2),
                                 substr(susp_data$academic_year, start = 5, stop = 6), sep = ""))) %>%
  mutate(academic_year = paste(substr(susp_data$academic_year, start = 1, stop = 4),
                               substr(susp_data$academic_year, start = 5, stop = 6), sep = "-"))


susp_data %>%
  filter(school_type != "Total", lad_name == "Birmingham") %>%
  group_by(year, school_type) %>%
  mutate(birm_susp_rate = one_plus_susp/headcount) %>%
  mutate(birm_excl_rate = perm_excl/headcount) %>%
    ggplot() +
      geom_line(aes(x = year, y = birm_excl_rate, colour = school_type)) +
      scale_x_continuous(name = "") +
      scale_y_continuous(name = "") +
      theme_classic() +
      theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_excl.png")

susp_data %>%
  filter(school_type != "Total", lad_name == "Birmingham") %>%
  group_by(year, school_type) %>%
  mutate(birm_susp_rate = one_plus_susp/headcount) %>%
  mutate(birm_excl_rate = perm_excl/headcount) %>%
  ggplot() +
  geom_line(aes(x = year, y = birm_susp_rate, colour = school_type)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_susp.png")


av_susp_data <- susp_data %>%
  filter(lad_name == "Birmingham", school_type != "Total") %>%
  group_by(school_type) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  kable("latex", booktabs = TRUE)

############################################################################################################
# # # # # # new data# # # # # # # # # # # # # # # # # #
############################################################################################################

readin_data <- read.csv("/Users/katehayes/temp_data/permanent-and-fixed-period-exclusions-in-england_2020-21/data/exc_characteristics.csv")

susp_data <- readin_data
names(susp_data)[2] <- "academic_year"
susp_data <- susp_data %>%
  mutate(year = as.numeric(paste(substr(susp_data$academic_year, start = 1, stop = 2),
                                 substr(susp_data$academic_year, start = 5, stop = 6), sep = ""))) %>%
  mutate(academic_year = paste(substr(susp_data$academic_year, start = 1, stop = 4),
                               substr(susp_data$academic_year, start = 5, stop = 6), sep = "-"))

# # # # # # GENDER # # # # # # # # # # # # # # # # #
gender_susp_data <-susp_data %>%
  filter(school_type != "Total", la_name == "Birmingham", characteristic_group == "Gender") %>%
  group_by(year, school_type, characteristic) %>%
  summarise(one_plus_susp = one_plus_susp, susp_rate = one_plus_susp/headcount,
            perm_excl = perm_excl, excl_rate = perm_excl/headcount,
            headcount = headcount)

av_gender_susp_data <- gender_susp_data %>%
  group_by(school_type, characteristic) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  kable("latex", booktabs = TRUE)


gender_susp_data %>%
  ggplot() +
  geom_line(aes(x = year, y = susp_rate, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_susp_bygender.png")


gender_susp_data %>%
  ggplot() +
  geom_line(aes(x = year, y = excl_rate, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_excl_bygender.png")


# # # # # #AGE# # # # # # # # # # # # # # # # # #
age_susp_data <- susp_data %>%
  filter(school_type != "Total", la_name == "Birmingham", characteristic_group == "Age") %>%
  group_by(year, school_type, characteristic) %>%
  summarise(one_plus_susp = one_plus_susp, susp_rate = one_plus_susp/headcount,
            perm_excl = perm_excl, excl_rate = perm_excl/headcount,
            headcount = headcount)

regexp <- "[[:digit:]]+"
age_susp_data$characteristic <- as.numeric(str_extract(age_susp_data$characteristic, regexp))

av_age_susp_data <- age_susp_data %>%
  group_by(school_type, characteristic) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  kable("latex", booktabs = TRUE)


age_susp_data %>%
  filter((school_type == "State-funded primary" & characteristic <= 10) | school_type != "State-funded primary") %>%
  ggplot() +
  geom_line(aes(x = characteristic, y = susp_rate, group = year, colour = year)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_viridis(option = "viridis", discrete = FALSE)
ggsave(filename = "Output/Graphs/birm_susp_byage.png")


age_susp_data %>%
  filter((school_type == "State-funded primary" & characteristic <= 10) | school_type != "State-funded primary") %>%
  ggplot() +
  geom_line(aes(x = characteristic, y = excl_rate, group = year, colour = year)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_viridis(option = "viridis", discrete = FALSE)
ggsave(filename = "Output/Graphs/birm_excl_byage.png")


# age_susp_data %>%
#   group_by(school_type, characteristic) %>%
#   summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
#             av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
#             av_headcount = mean(headcount)) %>%
#   filter(school_type != "State-funded primary") %>%
#   ggplot() +
#   geom_line(aes(x = characteristic, y = av_susp_rate, colour = school_type)) +
#   scale_x_continuous(name = "") +
#   scale_y_continuous(name = "") +
#   theme_classic() +
#   theme(strip.background = element_blank())
# ggsave(filename = "Output/Graphs/birm_susp_byage.png")



# age_susp_data %>%
#   group_by(school_type, characteristic) %>%
#   summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
#             av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
#             av_headcount = mean(headcount)) %>%
#   ggplot() +
#   geom_line(aes(x = characteristic, y = av_excl_rate, colour = school_type)) +
#   scale_x_continuous(name = "") +
#   scale_y_continuous(name = "") +
#   theme_classic() +
#   theme(strip.background = element_blank())
# ggsave(filename = "Output/Graphs/birm_excl_byage.png")







age_susp_data %>%
  filter(characteristic %in% 10:18) %>%
  mutate(characteristic = as.factor(characteristic)) %>%
  mutate(characteristic = fct_rev(characteristic)) %>%
  filter((school_type == "State-funded primary" & characteristic == 10) | school_type != "State-funded primary") %>%
  ggplot() +
  geom_line(aes(x = year, y = susp_rate, group = characteristic, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_viridis(option = "viridis", discrete = TRUE)
ggsave(filename = "Output/Graphs/birm_susp_byage_yearly.png")


age_susp_data %>%
  filter(characteristic %in% 10:18, school_type != "State-funded special") %>%
  filter((school_type == "State-funded primary" & characteristic == 10) | (school_type != "State-funded primary" & characteristic != 10)) %>%
  mutate(characteristic = as.factor(characteristic)) %>%
  mutate(characteristic = fct_rev(characteristic)) %>%
  ggplot() +
  geom_line(aes(x = year, y = excl_rate, group = characteristic, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_viridis(option = "viridis", discrete = TRUE)
ggsave(filename = "Output/Graphs/birm_excl_byage_yearly.png")




# # # # # # FREE SCHOOL MEALS# # # # # # # # # # # # # # # # # #

fsm_susp_data <-susp_data %>%
  filter(school_type != "Total", la_name == "Birmingham", characteristic_group == "FSM", year != 2007, characteristic != "FSM - Unclassified") %>%
  group_by(year, school_type, characteristic) %>%
  summarise(one_plus_susp = one_plus_susp, susp_rate = one_plus_susp/headcount,
            perm_excl = perm_excl, excl_rate = perm_excl/headcount,
            headcount = headcount)


av_fsm_susp_data <- fsm_susp_data %>%
  group_by(school_type, characteristic) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  kable("latex", booktabs = TRUE)



fsm_susp_data %>%
  ggplot() +
  geom_line(aes(x = year, y = susp_rate, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_susp_byFSM.png")

fsm_susp_data %>%
  ggplot() +
  geom_line(aes(x = year, y = excl_rate, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_excl_byFSM.png")


############################################################################################################
#######NEW DATA#######################################################################################################
############################################################################################################
readin_data <- read.csv("/Users/katehayes/temp_data/permanent-and-fixed-period-exclusions-in-england_2020-21/data/exc_idaci_sen_need_fsm6.csv")
##only interested in the index of childhood deprivation for this one
##AT THE ENGLAND LEVEL _ NOT JUST BIRMINGHAM


IDACI_susp_data <- readin_data
names(IDACI_susp_data)[2] <- "academic_year"
IDACI_susp_data <- IDACI_susp_data %>%
  mutate(year = as.numeric(paste(substr(IDACI_susp_data$academic_year, start = 1, stop = 2),
                                 substr(IDACI_susp_data$academic_year, start = 5, stop = 6), sep = ""))) %>%
  mutate(academic_year = paste(substr(IDACI_susp_data$academic_year, start = 1, stop = 4),
                               substr(IDACI_susp_data$academic_year, start = 5, stop = 6), sep = "-")) %>%
filter(characteristic_group == "IDACI", school_type != "Total", characteristic != "IDACI_decile_unclassified") %>%
group_by(year, school_type, characteristic) %>%
  summarise(one_plus_susp = one_plus_susp, susp_rate = one_plus_susp/headcount,
            perm_excl = perm_excl, excl_rate = perm_excl/headcount,
            headcount = headcount)


av_IDACI_susp_data <- IDACI_susp_data %>%
  group_by(school_type, characteristic) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  kable("latex", booktabs = TRUE)


IDACI_susp_data %>%
  filter(school_type != "Special") %>%
  ggplot() +
  geom_line(aes(x = year, y = excl_rate, group = characteristic, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_viridis(option = "viridis", discrete = TRUE)
ggsave(filename = "Output/Graphs/ENG_excl_byIDACI_yearly.png")

IDACI_susp_data %>%
  filter(school_type != "Special") %>%
  ggplot() +
  geom_line(aes(x = year, y = susp_rate, group = characteristic, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_viridis(option = "viridis", discrete = TRUE)
ggsave(filename = "Output/Graphs/ENG_susp_byIDACI_yearly.png")

############################################################################################################
#######NEW DATA#######################################################################################################
############################################################################################################
readin_data <- read.csv("/Users/katehayes/temp_data/permanent-and-fixed-period-exclusions-in-england_2020-21/data/exc_pru.csv")


pru_susp_data <- readin_data
names(pru_susp_data)[2] <- "academic_year"
pru_susp_data <- pru_susp_data %>%
  mutate(year = as.numeric(paste(substr(pru_susp_data$academic_year, start = 1, stop = 2),
                                 substr(pru_susp_data$academic_year, start = 5, stop = 6), sep = ""))) %>%
  mutate(academic_year = paste(substr(pru_susp_data$academic_year, start = 1, stop = 4),
                               substr(pru_susp_data$academic_year, start = 5, stop = 6), sep = "-")) %>%
  filter(la_name == "Birmingham", geographic_level == "Local authority") %>%
  group_by(year) %>%
  summarise(one_plus_susp = one_plus_susp, susp_rate = one_plus_susp/headcount,
            perm_excl = perm_excl, excl_rate = perm_excl/headcount,
            headcount = headcount)

av_pru_susp_data <- pru_susp_data %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  kable("latex", booktabs = TRUE)


pru_susp_data %>%
  ggplot() +
  geom_line(aes(x = year, y = headcount)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_headcount_pru.png")

pru_susp_data %>%
  ggplot() +
  geom_line(aes(x = year, y = susp_rate)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_susp_pru.png")

pru_susp_data %>%
  ggplot() +
  geom_line(aes(x = year, y = excl_rate)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_excl_pru.png")



############################################################################################################
#######NEW DATA#######################################################################################################
############################################################################################################
readin_data <- read.csv("/Users/katehayes/temp_data/school-pupils-and-their-characteristics_2021-22 (1)/data/spc_pupils_age_and_gender_.csv")

school_data <- readin_data
names(school_data)[1] <- "academic_year"
school_data <- school_data %>%
  mutate(year = as.numeric(paste(substr(school_data$academic_year, start = 1, stop = 2),
                                 substr(school_data$academic_year, start = 5, stop = 6), sep = ""))) %>%
  mutate(academic_year = paste(substr(school_data$academic_year, start = 1, stop = 4),
                               substr(school_data$academic_year, start = 5, stop = 6), sep = "-")) %>%
  filter(gender != "Total", age != "Total") %>%
  mutate(age = as.numeric(age)) %>%
  arrange(age)



school_data <- school_data %>%
  filter(la_name == "Birmingham", phase_type_grouping == "Pupil referral unit", age %in% 10:15)

tot_school_data <- school_data %>%
group_by(gender, year) %>%
summarise(headcount = sum(headcount))


school_data %>%
  mutate(age = as.factor(age)) %>%
  ggplot() +
  geom_area(aes(x = year, y = headcount, group = age, fill = fct_rev(age))) +
  facet_wrap(~gender) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_headcount_pru_byagegender_yearly.png")

# school_data %>%
#   filter(la_name == "Birmingham", phase_type_grouping == "Pupil referral unit", age %in% 10:16) %>%
#   group_by(age, gender) %>%
#   summarise(headcount = mean(headcount)) %>%
#   ggplot() +
#   geom_line(aes(x = age, y = headcount, group = gender, colour = gender)) +
#   scale_x_continuous(name = "") +
#   scale_y_continuous(name = "") +
#   theme_classic() +
#   theme(strip.background = element_blank())
# ggsave(filename = "Output/Graphs/birm_headcount_pru_byagegender.png")




############################################################################################################
#######NEW DATA#######################################################################################################
############################################################################################################
readin_data <- read.csv("/Users/katehayes/temp_data/school-pupils-and-their-characteristics_2021-22 (1)/data/spc_pupils_fsm_20220701.csv")

fsm_school_data <- readin_data
names(fsm_school_data)[1] <- "academic_year"
fsm_school_data <- fsm_school_data %>%
  mutate(year = as.numeric(paste(substr(fsm_school_data$academic_year, start = 1, stop = 2),
                                 substr(fsm_school_data$academic_year, start = 5, stop = 6), sep = ""))) %>%
  mutate(academic_year = paste(substr(fsm_school_data$academic_year, start = 1, stop = 4),
                               substr(fsm_school_data$academic_year, start = 5, stop = 6), sep = "-"))



fsm_school_data %>%
  filter(phase_type_grouping != "State-funded special school", phase_type_grouping != "State-funded nursery", phase_type_grouping != "Total", la_name == "Birmingham") %>%
  filter(fsm == "known to be eligible for free school meals (used for FSM in Performance Tables)") %>%
ggplot() +
  geom_line(aes(x = year, y = percent_of_pupils, group = phase_type_grouping, colour = phase_type_grouping)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_manual(values = c("red", "blue", "green"))
ggsave(filename = "Output/Graphs/birm_percentfsm_byschool_yearly.png")


# check <- fsm_school_data %>%
#   filter(phase_type_grouping != "State-funded special school", phase_type_grouping != "State-funded nursery", phase_type_grouping != "Total", la_name == "Birmingham") %>%
#   filter(fsm == "known to be eligible for free school meals" | fsm == "Total") %>%
#   pivot_wider(names_from = fsm, values_from = headcount) %>%
#   mutate(not_eligible = Total - `known to be eligible for free school meals`)



# av_fsm_school_data <- fsm_school_data %>%
#   filter(phase_type_grouping != "State-funded nursery", phase_type_grouping != "Total", la_name == "Birmingham") %>%
#   filter(fsm == "known to be eligible for free school meals (used for FSM in Performance Tables)" |
#            fsm == "number of pupils (used for FSM in Performance Tables)") %>%
#   mutate(fsm = if_else(fsm == "number of pupils (used for FSM in Performance Tables)", "total_headcount", "fsm_headcount")) %>%
#   group_by(phase_type_grouping, year, fsm) %>%
#   summarise(headcount = mean(headcount)) %>%
#   pivot_wider(names_from = fsm, values_from = headcount) %>%
#   ungroup() %>%
#   group_by(phase_type_grouping) %>%
#   summarise(av_fsm_rate = mean(fsm_headcount)/mean(total_headcount), av_headcount = mean(fsm_headcount)) %>%
#   kable("latex", booktabs = TRUE)


pru_fsm_school_data <- fsm_school_data %>%
  filter(phase_type_grouping == "Pupil referral unit", phase_type_grouping != "Total", la_name == "Birmingham") %>%
  filter(fsm == "known to be eligible for free school meals" |
           fsm == "number of pupils (used for FSM in Performance Tables)")

# %>%
#   mutate(fsm = if_else(fsm == "number of pupils (used for FSM in Performance Tables)", "total_headcount", "fsm_headcount")) %>%
#   group_by(phase_type_grouping, year, fsm) %>%
#   summarise(headcount = mean(headcount)) %>%
#   pivot_wider(names_from = fsm, values_from = headcount) %>%
#   ungroup() %>%
#   group_by(phase_type_grouping) %>%
#   summarise(av_fsm_rate = mean(fsm_headcount)/mean(total_headcount), av_headcount = mean(fsm_headcount)) %>%
#   kable("latex", booktabs = TRUE)



