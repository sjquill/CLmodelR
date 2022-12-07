#### #### START TO DO GRAPHS #### #### #### #### #### #### ####

# # %>%
#   group_by(Financial_Year, ) %>%
#   summarise(count = sum(Number_Cautioned_Sentenced))

my_colours <- c("brown", "brown2", "deepskyblue4", "deepskyblue")

disposals_gender_data %>%
  group_by(graph, year, level) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = fct_rev(level))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines")) +
  scale_fill_manual(values = my_colours)

my_colours3 <- c("#922B21", "#D98880", "#D98880",
                 "#CB4335", "#FADBD8", "#FADBD8",
                 "#D35400", "#EDBB99", "#EDBB99")

my_colours <- c("#922B21", "#D98880", "#D98880",
                "#CB4335", "#FADBD8", "#FADBD8",
                "#D35400", "#EDBB99", "#EDBB99",
                "#626567", "#B2BABB", "#B2BABB",
                "#2980B9", "#D6EAF8", "#D6EAF8",
                "#17A589", "#D1F2EB", "#D1F2EB")

# my_colours4 <- c("#440154FF", "#481F70FF", "#481F70FF",
#                  "#443A83FF", "#3B528BFF", "#3B528BFF",
#                  "#2C728EFF", "#21908CFF", "#21908CFF")
#
#
# my_colours4 <- c("#440154FF", "#481F70FF", "#481F70FF",
#                   "#404688FF", "#31688EFF", "#31688EFF",
#                 "#2C728EFF", "#21908CFF", "#21908CFF")

# my_colours4 <- c("#626567", "#B2BABB", "#B2BABB",
# "#2980B9", "#D6EAF8", "#D6EAF8",
# "#17A589", "#D1F2EB", "#D1F2EB")
#
# my_colours4 <- c("#440154FF", "#472D7BFF", "#472D7BFF",
#                  "#3B528BFF", "#31688EFF", "#31688EFF",
#                  "#21908CFF", "#20A486FF", "#20A486FF")

my_colours3<- c("#440154FF", "#443A83FF", "#443A83FF", # a kind of greeny blue one, nice
                "#2C728EFF", "#20A486FF", "#20A486FF",
                "#75D054FF", "#C7E020FF", "#C7E020FF")

my_colours6<- c("#145A32", "#979A9A", "#979A9A",
                "#20A486FF", "#B3B6B7", "#B3B6B7",
                "#75D054FF", "#D0D3D4", "#D0D3D4")

my_colours4 <- c("#440154FF", "#443A83FF", "#443A83FF",
                 "#2C728EFF", "#20A486FF", "#20A486FF",
                 "#75D054FF", "#C7E020FF", "#C7E020FF")

my_colours5 <- c("#443A83FF", "#403891b2", "#403891b2",
                 "#A65C68", "#DE7065B2", "#DE7065B2",
                 "#F68F46B2", "#F7C144", "#F7C144")

# A65C68 C25B51 <- two nice reds
#F7CB44B2" <- nice yelkow



# red - C44B43, and lighter E39893
# orange - D7813E and lighter EFB07E
# yellow - C9A848 and lighter FAE8B3

disposals_gender_data %>%
  filter(gender != "Not Known") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, gender, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type, gender))) +
  facet_grid(~interaction(graph), scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"))  +
  scale_fill_manual(values = my_colours)

disposals_gender_data %>%
  filter(gender != "Not Known") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, gender, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type, gender))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = c(my_colours4, my_colours5))
ggsave(filename = "Output/Graphs/birm_wm_disposals_bygender.png")


disposals_gender_data %>%
  filter(gender != "Not Known") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, gender, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type))) +
  facet_grid(~interaction(graph, fct_rev(gender)), scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours6)
ggsave(filename = "Output/Graphs/birm_wm_disposals_bygender_greyscale.png")


check <- disposals_gender_data %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, gender, disposal_type) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, gender, disposal_type, level) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()





legend.position="none"

disposals_gender_data %>%
  filter(gender == "Female") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours)
ggsave(filename = "Output/Graphs/birm_wm_disposals_female.png")

disposals_gender_data %>%
  filter(gender == "Male") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal_type))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(c(0), "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours)
ggsave(filename = "Output/Graphs/birm_wm_disposals_male.png")



disposals_gender_data %>%
  filter(gender == "Male") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_count, fill = interaction(level, disposal_type))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(c(0), "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours)

check <- disposals_gender_data %>%
  filter(gender == "Male") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, disposal_type) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()


check <- disposals_gender_data %>%
  filter(gender == "Female") %>%
  mutate(disposal_type = case_when(disposal_type == "Custody" ~ "Custody",
                                   disposal_type == "First-tier" ~ "Non-custodial",
                                   disposal_type == "Community" ~ "Non-custodial",
                                   disposal_type == "Pre-court" ~ "Pre-court")) %>%
  group_by(graph, year, disposal_type) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal_type) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()

# breakdown of custody into its parts
# 4D47A1 ddark blue
# 8D8AB6 lighter

# 445BB6 different darkish blue?
# 8A93B6 lighter
#
# 4480B6 darker
# 93AEC6 lighter
#
# 4CB2C8 darker
# 93BDC6 lighter

my_colours2 <- c("#7E215D", "#DCBBD0", "#DCBBD0",
                 "#773795", "#B9A0C4", "#B9A0C4",
                 "#4D47A1", "#B8B6D4", "#B8B6D4")

disposals_gender_data %>%
  filter(gender == "Male", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  mutate(disposal = case_when(disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226" ~ "Other",
                              disposal == "Section 228" ~ "Other")) %>%
  mutate(disposal = factor(disposal, levels = c("Other", "Section 90-92 Detention", "Detention and Training Order"))) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, disposal))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours2)
ggsave(filename = "Output/Graphs/birm_wm_custody_male.png")


disposals_gender_data %>%
  filter(gender == "Male", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  group_by(graph, year) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_count, fill = interaction(level, fct_rev(disposal)))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours2)




# something a little wrong with this graph

check <- disposals_gender_data %>%
  filter(gender == "Female", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  mutate(disposal = case_when(disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226" ~ "Other",
                              disposal == "Section 228" ~ "Other"))


check1 <- check[106:108, ]

check1 <- check1 %>%
  mutate(count = 0, disposal = "Section 90-92 Detention")

check <- bind_rows(check, check1)

check %>% # disposals_gender_data %>%
  # filter(gender == "Female", disposal_type == "Custody") %>%
  # mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
  #                             disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
  #                             disposal == "Detention and Training Order" ~ "Detention and Training Order",
  #                             disposal == "Section 226 (Life)" ~ "Section 226",
  #                             disposal == "Section 226 (Public Protection)" ~ "Section 226",
  #                             disposal == "Section 226b (*)" ~ "Section 226",
  #                             disposal == "Section 228" ~ "Section 228",
  #                             disposal == "Section 226b" ~ "Section 226")) %>%
  # # mutate(disposal = case_when(disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
  #                             disposal == "Detention and Training Order" ~ "Detention and Training Order",
#                             disposal == "Section 226" ~ "Other",
#                             disposal == "Section 228" ~ "Other")) %>%
group_by(graph, year, level, disposal) %>%
  summarise(count = sum(count)) %>%
  ggplot() +
  geom_area(aes(x = year, y = count, fill = interaction(level, fct_rev(disposal)))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2013, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2013", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.margin = unit(c(2, 3, 2, 1), "lines"),
        legend.position="none") +
  scale_fill_manual(values = my_colours2)
ggsave(filename = "Output/Graphs/birm_wm_custody_female.png")

# and of course this is v deeply shit xx
disposals_gender_data %>%
  filter(gender == "Female", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  group_by(graph, year) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct() %>%
  ggplot() +
  geom_area(aes(x = year, y = pc_count, colour = "black", fill = interaction(level, fct_rev(disposal)))) +
  facet_grid(~graph, scales = "free_x", space = "free") +
  scale_x_continuous(name = "",
                     breaks = c(2009, 2011, 2012, 2012, 2014, 2016),
                     labels = c("2009", "2011", "2012", "2012", "2014", "2016"),
                     expand = c(0,0)) +
  scale_y_continuous(name = "",
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines")) +
  scale_fill_viridis(discrete = TRUE, direction = 1)



check <-disposals_gender_data %>%
  filter(gender == "Female", disposal_type == "Custody") %>%
  mutate(disposal = case_when(disposal == "Section 90-91 Detention" ~ "Section 90-92 Detention",
                              disposal == "Section 90-92 Detention" ~ "Section 90-92 Detention",
                              disposal == "Detention and Training Order" ~ "Detention and Training Order",
                              disposal == "Section 226 (Life)" ~ "Section 226",
                              disposal == "Section 226 (Public Protection)" ~ "Section 226",
                              disposal == "Section 226b (*)" ~ "Section 226",
                              disposal == "Section 228" ~ "Section 228",
                              disposal == "Section 226b" ~ "Section 226")) %>%
  group_by(graph, year, disposal) %>%
  mutate(tot = sum(count)) %>%
  group_by(graph, year, level, disposal) %>%
  summarise(pc_count = sum(count)/tot) %>%
  distinct()








# disposals_gender_data <- bind_rows(disposals_gender_data1, disposals_gender_data2, disposals_gender_data3) %>%
#   mutate(rest_wm = west_midlands - birmingham)

# doing weird stuff with the data to see what graphs i can make

disposals_gender_data1 <- disposals_gender_data %>%
  filter(year == "2009-10")
disposals_gender_data2 <- disposals_gender_data %>%
  filter(year != "2009-10" & year != "2016-17") %>%
  mutate(west_midlands = NA)
disposals_gender_data3 <- disposals_gender_data %>%
  filter(year == "2016-17")

disposals_gender_data <- bind_rows(disposals_gender_data1, disposals_gender_data2, disposals_gender_data3) %>%
  pivot_longer(cols =  west_midlands:rest_wm , names_to = "level", values_to = "count")

check <- disposals_gender_data %>%
  mutate(year = as.numeric(substr(year, 1, 4))) %>%
  group_by(year, level) %>%
  summarise(count = sum(count)) %>%
  filter(year %in% 2010:2015) %>%
  ggplot(aes(x = year, y = count, fill = level, colour = level)) +
  geom_area() +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme_classic() +
  theme(strip.background = element_blank())
check

