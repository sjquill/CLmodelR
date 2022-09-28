install.packages("readODS")
library(readODS)

#"https://www.gov.uk/government/statistics/police-powers-and-procedures-stop-and-search-and-arrests-england-and-wales-year-ending-31-march-2021/data/arrest-police-powers-procedures-mar21-tables-2e.ods"

dat <- read_ods(, sheet = 3)
