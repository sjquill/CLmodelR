##seems like the data is actually decent on this lol
##https://www.gov.uk/government/statistics/police-powers-and-procedures-stop-and-search-and-arrests-england-and-wales-year-ending-31-march-2021

##so we have three spreedsheets to work from
##this one A (stop-search-open-data-tables-ppp-mar2021.ods) is 20/21 and
##this one B (stop-search-open-data-tables-ppp.ods) is 2016/17 to 2019/20 and
##this one C (stop-search-open-data-tables-ppp-mar2020.ods) is 2006/07 to 2019/20

##this one (stop-search-data-tables-outcomes-ppp-mar2021.ods) is just summarised versions of the others

##in A, reasons for search is in the same sheet as search outcomes - they're easily linked
##in B, these two things are seperate
##in C, we are missing the more detailed outcomes measures that A and B have.
##Arrest, Summons, Caution, Community resolution, Khat/cannabis warning,
##Penalty notice for disorder, Voluntary attendance, Verbal warning,
##Seizure of property, Other action, No further action
##C only has whether or not a resultant arrest happened
##it also does have linkage info like the others - as in whether the outcome is related to the reason for search
##C does have reason for searhc though, which is good.

##i think ultimately try to work with A and B for the time period they cover
## then use C if you need

ss21_data <- read_ods("/Users/katehayes/temp_data/stop-search-open-data-tables-ppp-mar2021.ods", sheet = 3)
ss16_reason_data <- read_ods("/Users/katehayes/temp_data/stop-search-open-data-tables-ppp.ods", sheet = 3)
ss16_outcome_data <- read_ods("/Users/katehayes/temp_data/stop-search-open-data-tables-ppp.ods", sheet = 4)
