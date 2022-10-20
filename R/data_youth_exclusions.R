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

susp_data %>%
  filter(la_name == "Birmingham", school_type != "Total") %>%
  ggplot() +
  geom_line(aes(x = year, y = sessions_susp, colour = school_type)) +
  scale_x_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())

# what the fuck is suspensions
check <- susp_data %>%
  mutate(check = suspension/sessions_susp)

# scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +


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


av_gender_susp_data <- gender_susp_data %>%
  group_by(school_type, characteristic) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  kable("latex", booktabs = TRUE)

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
  group_by(school_type, characteristic) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  filter(school_type != "State-funded primary") %>%
  ggplot() +
  geom_line(aes(x = characteristic, y = av_susp_rate, colour = school_type)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_susp_byage.png")

age_susp_data %>%
  group_by(school_type, characteristic) %>%
  summarise(av_susp = mean(one_plus_susp), av_susp_rate = sum(one_plus_susp)/sum(headcount),
            av_excl = mean(perm_excl), av_excl_rate = sum(perm_excl)/sum(headcount),
            av_headcount = mean(headcount)) %>%
  ggplot() +
  geom_line(aes(x = characteristic, y = av_excl_rate, colour = school_type)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_excl_byage.png")


age_susp_data %>%
  filter(characteristic %in% 10:18) %>%
  mutate(characteristic = as.factor(characteristic)) %>%
  mutate(characteristic = fct_rev(characteristic)) %>%
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
  filter(characteristic %in% 10:18) %>%
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
  ggplot() +
  geom_line(aes(x = year, y = susp_rate, group = characteristic, colour = characteristic)) +
  facet_wrap(~school_type) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_viridis(option = "viridis", discrete = TRUE)
ggsave(filename = "Output/Graphs/ENG_susp_byIDACI_yearly.png")


