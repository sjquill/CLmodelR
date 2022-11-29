################################################################################################################################
################################################################################################################################
############local level open data, outcomes table###############################
################################################################################################################################
##children sentenced and cautioned in birmingham, yearly, financial year 2013/14 to 2020/21

#read in the data
readin_data <- read_ods("/Users/katehayes/temp_data/Outcome_table.ods", sheet = 3)

#drop the NA column at the end, drop also any columns i don't want

caut_sent_data <- readin_data[,1:8]
names(caut_sent_data) <- tolower(names(caut_sent_data))

########## GRAPHS###########################################################################

check <- caut_sent_data %>%
  filter(pcc == "West Midlands") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  mutate(birm = if_else(yjs == "Birmingham", "Birmingham", "Rest of west mids")) %>%
  group_by(year, birm) %>%
  summarise(number_cautioned_sentenced = sum(number_cautioned_sentenced))



caut_sent_data %>%
  filter(pcc == "West Midlands") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  mutate(birm = if_else(yjs == "Birmingham", "Birmingham", "Rest of west mids")) %>%
  group_by(year, birm) %>%
  summarise(number_cautioned_sentenced = sum(number_cautioned_sentenced)) %>%
  ggplot( aes(x = year, y = number_cautioned_sentenced, fill = birm)) +
  geom_area() +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_v_westmids_caut_sent.png")

my_colours <- c("brown", "brown2", "deepskyblue4", "deepskyblue")

caut_sent_data %>%
  filter(pcc == "West Midlands") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  mutate(birm = if_else(yjs == "Birmingham", "Birmingham", "Rest of west mids")) %>%
  mutate(custody = if_else(caution_or_sentence_tier == "Custody", "Custodial", "Non-custodial")) %>%
  group_by(year, birm, custody) %>%
  summarise(number_cautioned_sentenced = sum(number_cautioned_sentenced)) %>%
  ggplot() +
  geom_area(aes(x = as.numeric(year), y = number_cautioned_sentenced, fill = interaction(custody, birm))) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())+
  scale_fill_manual(values = my_colours)
ggsave(filename = "Output/Graphs/birm_v_westmids_custodial_v_not.png")



caut_sent_data %>%
  filter(pcc == "West Midlands") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  mutate(birm = if_else(yjs == "Birmingham", "Birmingham", "Rest of west mids")) %>%
  mutate(custody = if_else(caution_or_sentence_tier == "Custody", "Custodial", "Non-custodial")) %>%
  group_by(year) %>%
  mutate(tot = sum(number_cautioned_sentenced)) %>%
  group_by(year, birm, custody) %>%
  summarise(pc_cautioned_sentenced = sum(number_cautioned_sentenced)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_cautioned_sentenced, fill = interaction(custody, birm))) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = my_colours)
ggsave(filename = "Output/Graphs/birm_v_westmids_custodial_v_not_pc.png")


caut_sent_data %>%
  filter(yjs == "Birmingham") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  mutate(custody = if_else(caution_or_sentence_tier == "Custody", "Custodial", "Non-custodial")) %>%
  group_by(year, custody) %>%
  summarise(number_cautioned_sentenced = sum(number_cautioned_sentenced)) %>%
  ggplot() +
  geom_area(aes(x = year, y = number_cautioned_sentenced, fill = custody)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(filename = "Output/Graphs/birm_custodial_v_not.png")


caut_sent_data %>%
  filter(yjs == "Birmingham") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  filter(caution_or_sentence_tier == "Custody") %>%
  mutate(caution_or_sentence_type = factor(caution_or_sentence_type,
         levels = c("Section 226b", "Section 90-91 Detention", "Detention and Training Order"))) %>%
  ggplot() +
  geom_area(aes(x = year, y = number_cautioned_sentenced, fill = caution_or_sentence_type)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/birm_cust_bytype.png")


caut_sent_data %>%
  filter(yjs == "Birmingham") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  filter(caution_or_sentence_tier == "Custody") %>%
  mutate(caution_or_sentence_type = factor(caution_or_sentence_type,
                levels = c("Section 226b", "Section 90-91 Detention", "Detention and Training Order"))) %>%
  group_by(year) %>%
  mutate(tot = sum(number_cautioned_sentenced)) %>%
  group_by(year, caution_or_sentence_type) %>%
  summarise(pc_cautioned_sentenced = sum(number_cautioned_sentenced)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_cautioned_sentenced, fill = caution_or_sentence_type)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/birm_cust_bytype_pc.png")



caut_sent_data %>%
  filter(yjs == "Birmingham") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  filter(caution_or_sentence_tier != "Custody") %>%
  mutate(caution_or_sentence_type2 =
           if_else(caution_or_sentence_type == "Absolute Discharge" |
                   caution_or_sentence_type == "Bind Over" |
                   caution_or_sentence_type == "Reparation Order" |
                   caution_or_sentence_type == "Sentence Deferred",
                   "Other", caution_or_sentence_type)) %>%  # suddently i decided other doesnt work but im keeping the code in bc maybe ill bring it back
  filter(caution_or_sentence_type2 != "Other") %>%
  group_by(caution_or_sentence_type2) %>%
  mutate(av_number = mean(number_cautioned_sentenced)) %>%
  mutate(caution_or_sentence_type2 = fct_rev(factor(caution_or_sentence_type2,
        levels = c("Referral Order", "Youth Rehabilitation Order", "Compensation Order",
                   "Youth Conditional Caution", "Youth Caution", "Fine",
                   "Conditional Discharge")))) %>% # reset your factor-column based on that order
  ggplot() +
  geom_area(aes(x = year, y = number_cautioned_sentenced, fill = caution_or_sentence_type2)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/birm_non-cust_bytype.png")


caut_sent_data %>%
  filter(yjs == "Birmingham") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  filter(caution_or_sentence_tier != "Custody") %>%
  mutate(caution_or_sentence_type2 =
           if_else(caution_or_sentence_type == "Absolute Discharge" |
                     caution_or_sentence_type == "Bind Over" |
                     caution_or_sentence_type == "Reparation Order" |
                     caution_or_sentence_type == "Sentence Deferred",
                   "Other", caution_or_sentence_type)) %>%  # suddently i decided other doesnt work but im keeping the code in bc maybe ill bring it back
  filter(caution_or_sentence_type2 != "Other") %>%
  group_by(caution_or_sentence_type2) %>%
  mutate(av_number = mean(number_cautioned_sentenced)) %>%
  mutate(caution_or_sentence_type2 = fct_rev(factor(caution_or_sentence_type2,
                                                    levels = c("Referral Order", "Youth Rehabilitation Order", "Compensation Order",
                                                               "Youth Conditional Caution", "Youth Caution", "Fine",
                                                               "Conditional Discharge")))) %>% # reset your factor-column based on that order
  group_by(year) %>%
  mutate(tot = sum(number_cautioned_sentenced)) %>%
  group_by(year, caution_or_sentence_type2) %>%
  summarise(pc_cautioned_sentenced = sum(number_cautioned_sentenced)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_cautioned_sentenced, fill = caution_or_sentence_type2)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, direction = -1)
ggsave(filename = "Output/Graphs/birm_non-cust_bytype_pc.png")





########## DATA SUMMARY###########################################################################
# taking out 2020 from the averages bc of the covid

sum <- caut_sent_data %>%
  filter(yjs == "Birmingham") %>%
  mutate(year = as.numeric(substr(financial_year, 1, 4))) %>%
  mutate(custody = if_else(caution_or_sentence_tier == "Custody", "Custodial", "Non-custodial")) %>%
  group_by(year, custody) %>%
  summarise(number_cautioned_sentenced = sum(number_cautioned_sentenced))
# # %>%
#   group_by(caution_or_sentence_type) %>%
#   summarise(av_sentenced = mean(number_cautioned_sentenced)) %>%
# kable("latex", booktabs = TRUE)
##separate out new thing which is just any csj not custody





