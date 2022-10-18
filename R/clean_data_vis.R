################################################################################################################################
################################################################################################################################
############local level open data, outcomes table###############################
################################################################################################################################
##children sentenced and cautioned in birmingham, yearly, financial year 2013/14 to 2020/21

#read in the data
readin_data <- read_ods("/Users/katehayes/temp_data/Outcome_table.ods", sheet = 3)

##ok for my model groupings i need to separate out custodial and non-custodial
#caut_sent_data$Caution_or_sentence_type <- factor(caut_sent_data$Caution_or_sentence_type, levels = c("Section 226b", "Section 90-91 Detention", "Detention and Training Order"))

#drop the NA column at the end, drop also any columns i don't want   if keep only Birmingham, filter(YJS == "Birmingham") %>%


# ggplot(aes(x = financial_year, y = split_caut_sent, fill = split)) +
# geom_bar(stat = "identity", position = "dodge") +
# theme(axis.text.x=element_text(angle = -90, hjust = 0))



caut_sent_data <- readin_data[,1:8]
names(caut_sent_data) <- tolower(names(caut_sent_data))



# save this once you figure out how to put the labels on
caut_sent_data %>%
  filter(pcc == "West Midlands") %>%
  mutate(year = case_when(financial_year == "2013-14" ~ 2013, financial_year == "2014-15" ~ 2014,
                          financial_year == "2015-16" ~ 2015, financial_year == "2016-17" ~ 2016,
                          financial_year == "2017-18" ~ 2017, financial_year == "2018-19" ~ 2018,
                          financial_year == "2019-20" ~ 2019, financial_year == "2020-21" ~ 2020)) %>%
  group_by(year) %>%
  mutate(wm_caut_sent = sum(number_cautioned_sentenced)) %>%
  filter(yjs == "Birmingham") %>%
  mutate(birm_caut_sent = sum(number_cautioned_sentenced)) %>%
  ggplot() +
  geom_line(aes(x = year, y = wm_caut_sent, colour = "West Midlands")) +
  geom_line(aes(x = year, y = birm_caut_sent, colour = "Birmingham")) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())
# ggsave(filename = "Output/Graphs/tot_pop_10-17.png")


caut_sent_data %>%
  filter(pcc == "West Midlands") %>%
  mutate(year = case_when(financial_year == "2013-14" ~ 2013, financial_year == "2014-15" ~ 2014,
                          financial_year == "2015-16" ~ 2015, financial_year == "2016-17" ~ 2016,
                          financial_year == "2017-18" ~ 2017, financial_year == "2018-19" ~ 2018,
                          financial_year == "2019-20" ~ 2019, financial_year == "2020-21" ~ 2020)) %>%
  mutate(custody = case_when(caution_or_sentence_tier == "Custody" ~ "custodial", caution_or_sentence_tier != "Custody" ~ "non-custodial")) %>%
  group_by(year, custody) %>%
  mutate(wm_caut_sent = sum(number_cautioned_sentenced)) %>%
  ggplot() +
  geom_line(aes(x = year, y = wm_caut_sent, colour = custody)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())


caut_sent_data %>%
  filter(yjs == "Birmingham") %>%
  mutate(year = case_when(financial_year == "2013-14" ~ 2013, financial_year == "2014-15" ~ 2014,
                          financial_year == "2015-16" ~ 2015, financial_year == "2016-17" ~ 2016,
                          financial_year == "2017-18" ~ 2017, financial_year == "2018-19" ~ 2018,
                          financial_year == "2019-20" ~ 2019, financial_year == "2020-21" ~ 2020)) %>%
  mutate(custody = case_when(caution_or_sentence_tier == "Custody" ~ "custodial", caution_or_sentence_tier != "Custody" ~ "non-custodial")) %>%
  group_by(year, custody) %>%
  mutate(birm_caut_sent = sum(number_cautioned_sentenced)) %>%
  ggplot() +
  geom_line(aes(x = year, y = birm_caut_sent, colour = custody)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())



plot1 <- caut_sent_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0))

plot1

plot2 <- caut_sent_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced)) +
  geom_bar(stat = "identity") +
  facet_wrap(~PCC) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

plot2


# +
#     facet_wrap(~Caution_or_sentence_type) +
#     theme(axis.text.x=element_text(angle = -90, hjust = 0))


#
# %>%
#   group_by(YJS, Financial_Year, Caution_or_sentence_type, Caution_or_sentence_tier) %>%
#   summarise(Number_Cautioned_Sentenced = Number_Cautioned_Sentenced)

# total <- caut_sent_data  %>%
#   group_by(Financial_Year) %>%
#   summarise(Number_Cautioned_Sentenced = sum(Number_Cautioned_Sentenced))












##separate out custody
cust_data <- caut_sent_data %>%
  filter(Caution_or_sentence_tier == "Custody")

cust_data$Caution_or_sentence_type <- factor(cust_data$Caution_or_sentence_type, levels = c("Section 226b", "Section 90-91 Detention", "Detention and Training Order"))

cust_plot <- cust_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced, fill = Caution_or_sentence_type)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

cust_plot

cust_data_av <- cust_data %>%
  filter(Financial_Year != "2020-21") %>%
  group_by(Caution_or_sentence_type) %>%
  summarise(av_sentenced = mean(Number_Cautioned_Sentenced))

##separate out new thing which is just any csj not custody

csj_data <- caut_sent_data %>%
  filter(Caution_or_sentence_tier != "Custody", Financial_Year != "2020-21")

csj_plot <- csj_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Caution_or_sentence_type) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

csj_plot

csj_avs <- csj_data %>%
  ungroup() %>%
  mutate(tot = sum(Number_Cautioned_Sentenced)) %>%
  group_by(Caution_or_sentence_type) %>%
  mutate(av_pc = sum(Number_Cautioned_Sentenced)/tot, av_num = sum(Number_Cautioned_Sentenced)/7) %>%
  ungroup() %>%
  group_by(Caution_or_sentence_type) %>%
  summarise(av_pc = mean(av_pc), av_num = mean(av_num))


##separate out restricted
rest_data <- caut_sent_data %>%
  filter(Caution_or_sentence_type %in% c("Referral Order", "Youth Conditional Caution", "Youth Rehabilitation Order", "Conditional Discharge", "Compensation Order", "Reparation Order"))


sum_rest_data <- rest_data %>%
  group_by(Financial_Year) %>%
  summarise(Number_Cautioned_Sentenced = sum(Number_Cautioned_Sentenced))


rest_plot <- rest_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Caution_or_sentence_type) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

rest_plot

rest_plot2 <- rest_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced, fill = Caution_or_sentence_type)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

rest_plot2
##referral orders are getting more popular, rehavilitation orders less


sum_rest_plot <- sum_rest_data %>%
  ggplot(aes(x = Financial_Year, y = Number_Cautioned_Sentenced)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

sum_rest_plot

##SO we have numbers of children sentenced and cautioned
## or in model terms, sentenced to custody or restricted, in birmingham yearly
##additionally it would be useful to know how long each type of caution/sentence lasts
##ideally at the birmingham or west midlands level, but county-level if necessary

##here are the median figures for 2019 and 2020, at total country level:
#DTO - 91.50
#Other -292.25
#Remand - 41.00
