# temp <- tempfile()
# source1 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsrelatedtodrugpoisoningenglandandwalesreferencetable%2fcurrent/2019maindataset1.xls"
# temp1 <- curl_download(url=source1, destfile=temp1, quiet=FALSE, mode="wb")
# raw.u20 <- read_excel(temp1, sheet="Table 2", range="L9:R91", col_names=FALSE)[-c(28,56),-c(4)]
# colnames(raw.u20) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning",
#                        "Combined_Misuse", "England_Misuse", "Wales_Misuse")
#
# temp <- tempfile()
# source <- "https://commonslibrary.parliament.uk/constituency-data-child-poverty/Child-poverty-data-download.xlsx"
# temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
# readin_data <- read_excel(temp, sheet = 4)

readin_data <- read_excel("/Users/katehayes/temp_data/children-in-low-income-families-local-area-statistics-2014-to-2021 (1).xlsx", sheet = 4, skip = 9)
poverty_data <- readin_data %>%
  filter(str_detect(`Local Authority [note 2]`, "Birmingham"))
poverty_data <- poverty_data[,10:16] %>%
  pivot_longer(cols = 1:7, names_to = "year", values_to = "percent") %>%
  mutate(year = as.numeric(str_extract(year, "[[:digit:]]+")))
# NOTE!!!! i do not like or trust these numbers. need to have a serious look - but will do for now
kable(poverty_data, "latex", booktabs = TRUE)


poverty_data %>%
  ggplot() +
  geom_line(aes(x = year, y = percent)) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(strip.background = element_blank())



