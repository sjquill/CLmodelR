##here is a useful resource:
##https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1049959/A_Guide_to_Youth_Justice_Statistics.pdf

##############WHERE IS THE DATA########################################################
##https://www.gov.uk/government/publications/youth-custody-data
##Monthly statistics on the population in custody of children and young people within the secure estate.
##broken down by gender, age, race, some other stuff
##this is national data i think (maybe england and wales, need to check)

##if we want local level, we need to go here: https://www.gov.uk/government/statistics/youth-justice-statistics-2020-to-2021
## and go to the local level open data. the relevant table for youth custody is the outcome table
## outcome table: at birmingham level, yearly 2013-14 to 2020-21, number of children that have each outcome, including
##a detention and training order or a section 90-91 detention
##THIS IS JUST PEOPLE SENTENCED - YOU ALSO NEED REMAND

##there is also, on this page https://www.gov.uk/government/statistics/youth-justice-statistics-2020-to-2021, the supplementary tables
##in it there is chapter 7, children in youth custody. in it we have sheet 7.21, which gives yearly figure 'avg monthly custody population'
##at the west midlands level, from 2012 to 2021, and broken down by remand, DTO, section 91 and other.
##there is also sheet 7.32, which gives some distribution of nights spent in custody in West midlands (1 to 91 nights, 92 to 182 nights,
## 183 to 273 nights, 274+ nights) yearly from 2019 to 2021
## you can also get dist of nights by custody type, for the whole country only though, on sheet 7.27

##https://www.gov.uk/government/statistics/length-of-time-spent-in-youth-custody-2016-to-2017
##This is a one-off publication, with the focus on episodes children and young people spent in custody which ended
##between 1 April 2016 to 31 March 2017. In previous years, these statistics have been included in the annual Youth
##Justice Statistics, however due to a change in data source and subsequent methodology, these are being published
##separately for 2016/17. Data for the year ending March 2018 will be included in the Youth Justice Statistics, 2017/18 publication.

##############POTENTIAL INTERSECTIONS########################################################
##MoJ/DfE Experimental Statistics ‘Understanding the educational background of young offenders’:
##  https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/577542/

## Assessing the needs of sentenced children in the Youth Justice System:
## https://www.gov.uk/government/statistics/assessing-the-needs-of-sentenced-children-in-the-youth-justice-system

##############POTENTIAL OUTCOMES########################################################
## also in the supplementary tables there is a workbook about behaviour management in the secure estate - come back to this
##https://www.gov.uk/government/statistics/safety-in-the-children-and-young-people-secure-estate-update-to-march-2022

##############OK LETS TRY THIS########################################################
## i am going to temporarily read in from a file saved on my laptop.
## will later change to good practise - say include in script code to scrape from web


install.packages("readODS")
library(readODS)

