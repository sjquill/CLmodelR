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

readin_data <- read.csv("/Users/katehayes/temp_data/youth_pov.csv")
