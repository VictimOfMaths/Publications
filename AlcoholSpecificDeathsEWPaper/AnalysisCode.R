rm(list=ls())

library(tidyverse)
library(curl)
library(scales)
library(readxl)
library(extrafont)
library(ragg)
library(paletteer)
library(patchwork)
library(lubridate)
library(gt)
library(gtools)
library(stats)
library(broom)
library(riskCommunicator)

options(scipen=999999999)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y=element_line(colour="grey95"))
}

#Define European Standard Population 2013
ESP <- data.frame(Age=c("Under 1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                 "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
           ESP=c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 
                 4000 ,2500, 1500, 1000))

#Read in data from ONS website
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1355deathsbysexsingleyearofageunderlyingcauseicd10codeanddeprivationdecileengland2001to2022
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1355deathsbysexsingleyearofageunderlyingcauseicd10codeanddeprivationdecileengland2001to2022/deathsbyimd20012022final.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

rawpersons <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                         sheet="1", range=cell_limits(c(6,1), c(567283, 25))) %>% 
  mutate(Sex="Total")

rawmale <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                      sheet="2", range=cell_limits(c(6,1), c(415178, 25))) %>% 
  mutate(Sex="Male")

rawfemale <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                        sheet="3", range=cell_limits(c(6,1), c(379521, 25))) %>% 
  mutate(Sex="Female")

rawdata <- bind_rows(rawpersons, rawmale, rawfemale) %>% 
  gather(Year, Deaths, c(4:25)) %>% 
  mutate(Year=as.numeric(Year),
         IMD=11-`IMD decile`,
         IMD=case_when(
           IMD=="1" ~ "1 (least deprived)",
           IMD=="10" ~ "10 (most deprived)",
           TRUE ~ as.character(IMD)),
         IMD=factor(IMD, levels=c("1 (least deprived)", "2", "3", "4", "5", "6",
                                  "7", "8", "9", "10 (most deprived)")))

#Collapse to IMD quintiles, 5-year age bands and separate out causes
working1 <- rawdata %>% 
  mutate(Cause=case_when(
    substr(`ICD-10 code`,1,3)=="K70" ~ "Alcohol-related liver disease",
    `ICD-10 code`=="F102" ~ "Alcohol dependence syndrome",
    substr(`ICD-10 code`,1,3) %in% c("F10", "X45", "X65", "Y15") | `ICD-10 code`=="R780" ~ "Acute causes",
    `ICD-10 code` %in% c("E244", "G312", "G621", "G721", "I426", "K292", "K852", "K860", "Q860") ~ 
      "Other alcohol-specific causes",
    substr(`ICD-10 code`,1,3) %in% c("K73", "K74") ~ "Other liver disease",
    TRUE ~ "Other"),
    IMDq=case_when(
      IMD %in% c("1 (least deprived)", "2") ~ "Q1 (least deprived)",
      IMD %in% c("3", "4") ~ "Q2",
      IMD %in% c("5", "6") ~ "Q3",
      IMD %in% c("7", "8") ~ "Q4",
      IMD %in% c("9", "10 (most deprived)") ~ "Q5 (most deprived)"),
    Age5=case_when(
      Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
      Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
      Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
      Age<90 ~ "85-89", Age>=90 ~ "90+"),) %>% 
  group_by(Cause, IMDq, Sex, Year, Age5) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  #Fill in missing age bands with no deaths as zeros
  spread(Age5, Deaths) %>% 
  mutate(across(.cols=c(5:24), ~ if_else(is.na(.x), 0, .x))) %>% 
  gather(Age5, Deaths, c(5:24)) 

#Bring in ONS population estimates
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
#2021-22
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2021andmid2022/sapelsoasyoatablefinal.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Pop2022 <- read_excel(temp, sheet="Mid-2022 LSOA 2021", range="C4:GE35676") %>% 
  gather(Group, Pop, c(4:185)) %>% 
  mutate(Sex=if_else(substr(Group, 1, 1)=="F", "Female", "Male"),
         Age=as.numeric(substr(Group, 2, 3)),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2021 <- read_excel(temp, sheet="Mid-2021 LSOA 2021", range="C4:GE35676") %>% 
  gather(Group, Pop, c(4:185)) %>% 
  mutate(Sex=if_else(substr(Group, 1, 1)=="F", "Female", "Male"),
         Age=as.numeric(substr(Group, 2, 3)),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Pop2020 <- bind_rows(read_excel(temp, sheet="Mid-2020 Males", range="A5:CT34758") %>% 
  gather(Age, Pop, c(8:98)) %>% 
  mutate(Sex="Male"),
  read_excel(temp, sheet="Mid-2020 Females", range="A5:CT34758") %>% 
    gather(Age, Pop, c(8:98)) %>% 
    mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
    Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`LSOA Code`, `LSOA Name`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2019
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2019 <- bind_rows(read_excel(file.path(temp2, "SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"), 
                                sheet="Mid-2019 Males", range="A5:CT34758") %>% 
                       gather(Age, Pop, c(8:98)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"), 
                                sheet="Mid-2019 Females", range="A5:CT34758") %>% 
                       gather(Age, Pop, c(8:98)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`LSOA Code`, `LSOA Name`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2018
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2018 <- bind_rows(read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                                sheet="Mid-2018 Males", range="A5:CQ35097") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                                sheet="Mid-2018 Females", range="A5:CQ35097") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`Area Codes`, `LSOA`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2017
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2017/sape20dt1mid2017lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2017 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS"), 
                                sheet="Mid-2017 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS"), 
                                sheet="Mid-2017 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2016
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2016/sape20dt1mid2016lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2016 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2016 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2016 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2015
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015/sape20dt1mid2015lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2015 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2015 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2015 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2014
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2014/sape20dt1mid2014lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2014 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2014 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2014 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2013
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2013/sape20dt1mid2013lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2013 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2013 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2013 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2012
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2012/sape20dt1mid2012lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2012 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2012 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2012 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2002-11 data is combined in a single file for each sex
#Males
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2002tomid2011males/rftlsoaunformattedtablemales.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2011m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                                sheet="Mid-2011", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2010m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2010", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2009m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2009", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2008m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2008", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2007m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2007", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")
  
Pop2006m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2006", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2005m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2005", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2004m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2004", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2003m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2003", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2002m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2002", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

#Females
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2002tomid2011females/rftlsoaunformattedtablefemales.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2011f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2011", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2010f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2010", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2009f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2009", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2008f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2008", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2007f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2007", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2006f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2006", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2005f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2005", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2004f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2004", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2003f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2003", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2002f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2002", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

#Bring sexes together and tidy up
Pop2011 <- bind_rows(Pop2011m, Pop2011f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2010 <- bind_rows(Pop2010m, Pop2010f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2009 <- bind_rows(Pop2009m, Pop2009f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2008 <- bind_rows(Pop2008m, Pop2008f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2007 <- bind_rows(Pop2007m, Pop2007f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2006 <- bind_rows(Pop2006m, Pop2006f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2005 <- bind_rows(Pop2005m, Pop2005f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2004 <- bind_rows(Pop2004m, Pop2004f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2003 <- bind_rows(Pop2003m, Pop2003f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2002 <- bind_rows(Pop2002m, Pop2002f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age),
         Age5=case_when(Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
                        Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                        Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
                        Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(LSOA11CD, Sex, Age5) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2001 data has to come from a different source, because of ONS reasons
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/009729mid2001tomid2017populationestimatesforlowerlayersuperoutputareaslsoainenglandandwalesbyfiveyearagebandsandsexsupportinginformation/lsoa0117quinaryv1.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Pop2001 <- read_excel(temp, sheet="LSOA_quinary_ages", range=cell_limits(c(2,1), c(NA,45))) %>% 
  filter(year=="mid_2001") %>% 
  gather(Age, Pop, c(6:45)) %>% 
  mutate(Sex=if_else(substr(Age,1,1)=="M", "Male", "Female"),
         Age=gsub("M", "", Age),
         Age=gsub("F", "", Age),
         Age=gsub("_", "-", Age),
         Age5=case_when(
           Age=="0" ~ "Under 1", Age=="90plus" ~ "90+", TRUE ~ Age)) %>% 
  select(LSOA11CD, Age5, Pop, Sex)

#Download IMD data for all available years (updated every 3-4 years) 

#2019
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5d8b3abded915d0373d3540f/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD19 <- read_excel(temp, sheet="IMD2019", range="A1:F32845")

#2015
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5a805f96ed915d74e33fa0df/File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD15 <- read_excel(temp, sheet="IMD 2015", range="A1:F32845")

#2010
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5a79b8c6ed915d042206a8e2/1871524.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD10 <- read_excel(temp, sheet="IMD 2010", range="A1:G32483") %>% 
  mutate(decile=quantcut(`RANK OF IMD SCORE (where 1 is most deprived)`, 10))

IMD10 <- IMD10 %>% 
  mutate(`Index of Multiple Deprivation (IMD) Decile`=case_when(
           decile==levels(IMD10$decile)[1] ~ 1,
           decile==levels(IMD10$decile)[2] ~ 2,
           decile==levels(IMD10$decile)[3] ~ 3,           
           decile==levels(IMD10$decile)[4] ~ 4,           
           decile==levels(IMD10$decile)[5] ~ 5,           
           decile==levels(IMD10$decile)[6] ~ 6,           
           decile==levels(IMD10$decile)[7] ~ 7,           
           decile==levels(IMD10$decile)[8] ~ 8,           
           decile==levels(IMD10$decile)[9] ~ 9,           
           decile==levels(IMD10$decile)[10] ~ 10))

#2007
temp <- tempfile()
temp2 <- tempfile()
source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20100411141238mp_/http://www.communities.gov.uk/documents/communities/zip/indices2007.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

IMD07 <- read_excel(file.path(temp2, "IMD 2007 for DCLG 4 dec.xls"),
                         sheet="IMD 2007", range=c("A1:G32483")) %>% 
  mutate(decile=quantcut(`RANK OF IMD (where 1 is most deprived)`, 10))

IMD07 <- IMD07 %>% 
  mutate(`Index of Multiple Deprivation (IMD) Decile`=case_when(
           decile==levels(IMD10$decile)[1] ~ 1,
           decile==levels(IMD10$decile)[2] ~ 2,
           decile==levels(IMD10$decile)[3] ~ 3,           
           decile==levels(IMD10$decile)[4] ~ 4,           
           decile==levels(IMD10$decile)[5] ~ 5,           
           decile==levels(IMD10$decile)[6] ~ 6,           
           decile==levels(IMD10$decile)[7] ~ 7,           
           decile==levels(IMD10$decile)[8] ~ 8,           
           decile==levels(IMD10$decile)[9] ~ 9,           
           decile==levels(IMD10$decile)[10] ~ 10))

#2004
temp <- tempfile()
temp2 <- tempfile()
source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20100407164233mp_/http://www.communities.gov.uk/documents/communities/zip/soalevelid.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

IMD04 <- read_excel(file.path(temp2, "SOA levelid2004.xls"),
                    sheet="IMD 2004", range=c("A1:G32483")) %>% 
  mutate(decile=quantcut(`RANK OF IMD (where 1 is most deprived)`, 10))

IMD04 <- IMD04 %>% 
  mutate(`Index of Multiple Deprivation (IMD) Decile`=case_when(
           decile==levels(IMD10$decile)[1] ~ 1,
           decile==levels(IMD10$decile)[2] ~ 2,
           decile==levels(IMD10$decile)[3] ~ 3,           
           decile==levels(IMD10$decile)[4] ~ 4,           
           decile==levels(IMD10$decile)[5] ~ 5,           
           decile==levels(IMD10$decile)[6] ~ 6,           
           decile==levels(IMD10$decile)[7] ~ 7,           
           decile==levels(IMD10$decile)[8] ~ 8,           
           decile==levels(IMD10$decile)[9] ~ 9,           
           decile==levels(IMD10$decile)[10] ~ 10))


#Read in LSOA2011 - LSOA2021 code lookup
temp <- tempfile()
url <- "https://hub.arcgis.com/api/v3/datasets/b14d449ba10a48508bd05cd4a9775e2b_0/downloads/data?format=csv&spatialRefId=3857&where=1%3D1"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

LSOA1121Lookup <- read.csv(temp) %>% 
  select(LSOA11CD, LSOA21CD)

#Read in LSOA2001 - LSOA2011 code lookup
temp <- tempfile()
url <- "https://opendata.arcgis.com/api/v3/datasets/3dd1bc5dd053426aa84a068c7afbb3b2_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

LSOA0111Lookup <- read.csv(temp) %>% 
  select(LSOA01CD, LSOA11CD)

#Process to get IMD decile, age and sex-specific populations for each year
#2022 and IMD2019
Pop22 <- Pop2022 %>% 
  merge(LSOA1121Lookup, by.x="LSOA 2021 Code", by.y="LSOA21CD") %>% 
  merge(IMD19, by.x="LSOA11CD", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2021 and IMD2019
Pop21 <- Pop2021 %>% 
  merge(LSOA1121Lookup, by.x="LSOA 2021 Code", by.y="LSOA21CD") %>% 
  merge(IMD19, by.x="LSOA11CD", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2020 and IMD2019
Pop20 <- Pop2020 %>% 
  merge(IMD19, by.x="LSOA Code", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2019 and IMD2019
Pop19 <- Pop2019 %>% 
  merge(IMD19, by.x="LSOA Code", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2018 and IMD2019
Pop18 <- Pop2018 %>% 
  merge(IMD19, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2017 and IMD2015
Pop17 <- Pop2017 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2016 and IMD2015
Pop16 <- Pop2016 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2015 and IMD2015
Pop15 <- Pop2015 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2014 and IMD2015
Pop14 <- Pop2014 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2013 and IMD2015
Pop13 <- Pop2013 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2012 and IMD2010
Pop12 <- Pop2012 %>% 
  merge(LSOA0111Lookup, by.x="Area Codes", by.y="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2011 and IMD2010
Pop11 <- Pop2011 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2010 and IMD2010
Pop10 <- Pop2010 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2009 and IMD2010
Pop09 <- Pop2009 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2008 and IMD2007
Pop08 <- Pop2008 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD07, by.x="LSOA01CD", by.y="LSOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2007 and IMD2007
Pop07 <- Pop2007 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD07, by.x="LSOA01CD", by.y="LSOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2006 and IMD2007
Pop06 <- Pop2006 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD07, by.x="LSOA01CD", by.y="LSOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2005 and IMD2004
Pop05 <- Pop2005 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2004 and IMD2004
Pop04 <- Pop2004 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2003 and IMD2004
Pop03 <- Pop2003 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2002 and IMD2004
Pop02 <- Pop2002 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")

#2001 and IMD2004
Pop01 <- Pop2001 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  set_names("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop")


#Stick all the populations together
PopFinal <- bind_rows(Pop22 %>% mutate(Year=2022),
                      Pop21 %>% mutate(Year=2021),
                      Pop20 %>% mutate(Year=2020),
                      Pop19 %>% mutate(Year=2019),
                      Pop18 %>% mutate(Year=2018),
                      Pop17 %>% mutate(Year=2017),
                      Pop16 %>% mutate(Year=2016),
                      Pop15 %>% mutate(Year=2015),
                      Pop14 %>% mutate(Year=2014),
                      Pop13 %>% mutate(Year=2013),
                      Pop12 %>% mutate(Year=2012),
                      Pop11 %>% mutate(Year=2011),
                      Pop10 %>% mutate(Year=2010),
                      Pop09 %>% mutate(Year=2009),
                      Pop08 %>% mutate(Year=2008),
                      Pop07 %>% mutate(Year=2007),
                      Pop06 %>% mutate(Year=2006),
                      Pop05 %>% mutate(Year=2005),
                      Pop04 %>% mutate(Year=2004),
                      Pop03 %>% mutate(Year=2003),
                      Pop02 %>% mutate(Year=2002),
                      Pop01 %>% mutate(Year=2001)) %>% 
  set_names("Age5", "Sex", "IMDq", "Pop", "Year") %>% 
  #Collapse deciles to quintiles and reverse direction so higher = more deprived
  mutate(IMDq=case_when(
    IMDq %in% c(1,2) ~ "Q5 (most deprived)",
    IMDq %in% c(3,4) ~ "Q4",
    IMDq %in% c(5,6) ~ "Q3",
    IMDq %in% c(7,8) ~ "Q2",
    IMDq %in% c(9,10) ~ "Q1 (least deprived)")) %>% 
  group_by(Age5, Sex, Year, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  mutate(Age5=factor(Age5, levels=c("Under 1", "1-4", "5-9", "10-14", "15-19",
                                    "20-24", "25-29", "30-34", "35-39", "40-44",
                                    "45-49", "50-54", "55-59", "60-64", "65-69",
                                    "70-74", "75-79", "80-84", "85-89", "90+")))

#Plot to check everything looks sensible
ggplot(PopFinal %>% filter(Sex=="Male"), aes(x=Year, y=Age5, fill=Pop))+
  geom_tile()+
  facet_wrap(~IMDq)+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::turbo")+
  theme_custom()+
  coord_equal()

ggplot(PopFinal %>% filter(Sex=="Female"), aes(x=Year, y=Age5, fill=Pop))+
  geom_tile()+
  facet_wrap(~IMDq)+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::turbo")+
  theme_custom()+
  coord_equal()

#Merge it all back into the deaths data
Maindata5 <- working1 %>% 
  merge(PopFinal %>% group_by(Age5, Year, IMDq) %>% 
          summarise(Pop=sum(Pop), .groups="drop") %>% 
          mutate(Sex="Total") %>% 
          bind_rows(PopFinal), all.x=TRUE) %>% 
  mutate(mx=Deaths*100000/Pop)%>% 
  mutate(Age5=factor(Age5, levels=c("Under 1", "1-4", "5-9", "10-14", "15-19",
                                    "20-24", "25-29", "30-34", "35-39", "40-44",
                                    "45-49", "50-54", "55-59", "60-64", "65-69",
                                    "70-74", "75-79", "80-84", "85-89", "90+")))

#Add in additional cause groupings i) All alcohol-specific deaths and 
#ii) Wider defn of alcohol-specific deaths incl. K73 & 74
Maindata5 <- Maindata5 %>% 
  bind_rows(Maindata5 %>% 
              filter(Cause %in% c("Acute causes", "Alcohol-related liver disease",
                                  "Alcohol dependence syndrome",
                                  "Other alcohol-specific causes")) %>% 
              group_by(Age5, Year, Sex, IMDq, Pop) %>% 
              summarise(Deaths=sum(Deaths), .groups="drop") %>% 
              mutate(mx=Deaths*100000/Pop,
                     Cause="All alcohol-specific causes")) 

Maindata5 <- Maindata5 %>% 
  bind_rows(Maindata5 %>% 
              filter(Cause %in% c("All alcohol-specific causes",
                                  "Other liver disease")) %>% 
              group_by(Age5, Year, Sex, IMDq, Pop) %>% 
              summarise(Deaths=sum(Deaths), .groups="drop") %>% 
              mutate(mx=Deaths*100000/Pop,
                     Cause="All alcohol-specific causes - wider"))

#Add in all IMD group
Maindata5 <- Maindata5 %>% 
  bind_rows(Maindata5 %>% 
              group_by(Age5, Year, Sex, Cause) %>% 
              summarise(Deaths=sum(Deaths),
                        Pop=sum(Pop), .groups="drop") %>% 
              mutate(mx=Deaths*100000/Pop,
                     IMDq="Overall"))

#A few test plots to visualise rates
Maindata5 %>% filter(Sex=="Total" & Cause!="Other" & IMDq!="Overall") %>% 
  ggplot(aes(x=Year, y=Age5, fill=mx))+
  geom_tile()+
  facet_grid(IMDq~Cause)+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::turbo")+
  theme_custom()+
  coord_equal()

Maindata5 %>% filter(Sex!="Total" & Cause=="All alcohol-specific causes" &
                       IMDq!="Overall") %>% 
  ggplot(aes(x=Year, y=Age5, fill=mx))+
  geom_tile()+
  facet_grid(Sex~IMDq)+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::turbo")+
  theme_custom()+
  coord_equal()

#All looks ok, so generate datasets required for main analysis

#Keep only the things we actually need
rm(list=setdiff(ls(), c("font", "theme_custom", "Maindata5", "ESP", "working1")))

#Age-standardise
ASData <- Maindata5 %>% 
  merge(ESP, by.x="Age5", by.y="Age") %>% 
  group_by(Sex, Year, IMDq, Cause) %>% 
  summarise(mx=weighted.mean(mx, ESP), 
            Deaths=sum(Deaths), .groups="drop") %>% 
  #Calculate CIs for rates
  rowwise() %>% 
  mutate(LowerCI=mx-1.96*mx/sqrt(Deaths),
         UpperCI=mx+1.96*mx/sqrt(Deaths)) %>% 
  ungroup()

#10 year age band version
AgeBandData <- Maindata5 %>% 
  mutate(Age10=case_when(
    Age5 %in% c("Under 1", "1-4", "5-9") ~ "0-9",
    Age5 %in% c("10-14", "15-19") ~ "10-19",
    Age5 %in% c("20-24", "25-29") ~ "20-29",
    Age5 %in% c("30-34", "35-39") ~ "30-39",
    Age5 %in% c("40-44", "45-49") ~ "40-49",
    Age5 %in% c("50-54", "55-59") ~ "50-59",
    Age5 %in% c("60-64", "65-69") ~ "60-69",
    Age5 %in% c("70-74", "75-79") ~ "70-79",
    Age5 %in% c("80-84", "85-89") ~ "80-89",
    Age5=="90+" ~ "90+")) %>% 
  group_by(Age10, Year, Sex, IMDq, Cause) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop,
         Age10=factor(Age10, levels=c("0-9", "10-19", "20-29", "30-39", "40-49",
                                       "50-59", "60-69", "70-79", "80-89", "90+"))) %>% 
  #Calculate CIs for rates
  rowwise() %>% 
  mutate(LowerCI=mx-1.96*mx/sqrt(Deaths),
         UpperCI=mx+1.96*mx/sqrt(Deaths)) %>% 
  ungroup()

#Save main datasets so we don't have to rerun all of this ^ every time (because it takes ages)
data.table::fwrite(AgeBandData, "Data/ASDPaperAgeBandData.csv")
data.table::fwrite(ASData, "Data/ASDPaperASData.csv")
data.table::fwrite(Maindata5, "Data/ASDPaperMaindata5.csv")

AgeBandData <- data.table::fread("Data/ASDPaperAgeBandData.csv")
ASData <- data.table::fread("Data/ASDPaperASData.csv")
Maindata5 <- data.table::fread("Data/ASDPaperMaindata5.csv")

############################################################################Actual analysis

#RQ1 overall
agg_png("Outputs/OldhamASDFigure1.png", units="in", width=8, height=5, res=800)
ASData %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" &
                    IMDq=="Overall") %>%
  ggplot(aes(x=Year, y=mx))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              fill="tomato", alpha=0.4)+
  geom_line(colour="tomato")+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=2019.4, y=4, colour="grey50", label="COVID-19 pandemic", angle=90, size=4, family="Lato")

dev.off()

#RQ1a by cause
Fig2a <- ASData %>% filter(!Cause %in% c("All alcohol-specific causes", "Other", 
                                "Other liver disease", "All alcohol-specific causes - wider") & 
                    Sex=="Total" & IMDq=="Overall") %>%
  ggplot(aes(x=Year, y=mx, colour=Cause, fill=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_fill_manual(values=c("#0072CE", "#F9423A", "#041E42", "#FFB81C"))+
  scale_colour_manual(values=c("#0072CE", "#F9423A", "#041E42", "#FFB81C"))+
  #scale_colour_paletteer_d("nbapalettes::thunder", name="")+
  #scale_fill_paletteer_d("nbapalettes::thunder", name="")+
  theme_custom()

#Faceted version
ASData %>% filter(!Cause %in% c("All alcohol-specific causes", "Other", 
                                "Other liver disease", "All alcohol-specific causes - wider") & 
                    Sex=="Total" & IMDq=="Overall") %>%
  ggplot(aes(x=Year, y=mx, colour=Cause, fill=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_paletteer_d("nbapalettes::thunder", name="")+
  scale_fill_paletteer_d("nbapalettes::thunder", name="")+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()+
  theme(legend.position="none")

#RQ1b by age
Fig2b <- AgeBandData %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" &
                         IMDq=="Overall") %>% 
  #Remove under 20s due to tiny counts
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=Year, y=mx, colour=Age10, fill=Age10))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()

#faceted version
AgeBandData %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" &
                         IMDq=="Overall") %>% 
  #Remove under 20s due to tiny counts
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=Year, y=mx, colour=Age10, fill=Age10))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  facet_wrap(~Age10, scales="free_y")+
  theme_custom()

#RQ1c by sex
Fig2c <- ASData %>% filter(Cause=="All alcohol-specific causes" & 
                    Sex!="Total" & IMDq=="Overall") %>%
  ggplot(aes(x=Year, y=mx, colour=Sex, fill=Sex))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="", breaks=c("Male", "Female"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="", breaks=c("Male", "Female"))+
  theme_custom()

#RQ1d by deprivation
Fig2d <- ASData %>% filter(Cause=="All alcohol-specific causes" &
                    Sex=="Total" & IMDq!="Overall") %>% 
  ggplot(aes(x=Year, y=mx, colour=IMDq, fill=IMDq))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                               "#7a0177"), name="")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="")+
  theme_custom() + 
  guides(fill = guide_legend(reverse = TRUE), colour=guide_legend(reverse = TRUE))

#Combine into big faceted plot
agg_png("Outputs/OldhamASDFigure2.png", units="in", width=12, height=8, res=800)
(Fig2a+theme(legend.position="top")+guides(fill=guide_legend(nrow=2, byrow=TRUE), 
                                           colour=guide_legend(nrow=2, byrow=TRUE))+
    labs(title="A - Cause")|
    Fig2b+theme(legend.position="top")+guides(fill=guide_legend(nrow=2, byrow=TRUE), 
                                              colour=guide_legend(nrow=2, byrow=TRUE))+
    labs(title="B - Age"))/
  (Fig2c+theme(legend.position="top")+labs(title="C - Sex")|
     Fig2d+theme(legend.position="top")+labs(title="D - IMD quintile"))

dev.off()

#check proportional distribution of deaths between cause groups around pandemic period
agg_png("Outputs/OldhamASDFigureS1.png", units="in", width=8, height=5, res=800)
ASData %>% filter(!Cause %in% c("All alcohol-specific causes", "Other", 
                                "Other liver disease", "All alcohol-specific causes - wider") &
                    Sex=="Total" & IMDq=="Overall" & Year>=2017) %>% 
  ggplot(aes(x=Year, y=mx, fill=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age-standardised deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#0072CE", "#F9423A", "#041E42", "#FFB81C"), name="")+
  theme_custom()

dev.off()

#Proportion of deaths by age
agg_png("Outputs/OldhamASDFigureS2.png", units="in", width=8, height=5, res=800)
AgeBandData %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq=="Overall" & Year>=2017 &
                         !Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=Year, y=Deaths, fill=Age10))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of alcohol-specific deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="Age")+
  
  theme_custom()

dev.off()

#Proportion of deaths by sex
agg_png("Outputs/OldhamASDFigureS3.png", units="in", width=8, height=5, res=800)
ASData %>% filter(Cause=="All alcohol-specific causes" & Sex!="Total" & IMDq=="Overall" & Year>=2017) %>% 
  ggplot(aes(x=Year, y=mx, fill=Sex))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age-standardised deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()

dev.off()

#Proportion of deaths by IMDq
agg_png("Outputs/OldhamASDFigureS4.png", units="in", width=8, height=5, res=800)
ASData %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq!="Overall" & Year>=2017) %>% 
  ggplot(aes(x=Year, y=mx, fill=IMDq))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age-standardised deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="IMD quintile")+
  theme_custom()

dev.off()

#Looks ok, so move onto fit Poisson models comparing 2017-19 with 2020-22
#First pool data for each set of years
ModelData <- Maindata5 %>% 
  filter(Year>=2017) %>% 
  mutate(Period=if_else(Year<=2019, "Pre-pandemic", "Pandemic")) %>% 
  group_by(Age5, Sex, IMDq, Cause, Period) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop) %>% 
  select(Age5, Sex, IMDq, Cause, mx, Period) %>% 
  spread(Period, mx) %>% 
  mutate(RateDiff=Pandemic-`Pre-pandemic`,
         RateRatio=Pandemic/`Pre-pandemic`)

#Age-standardise
ModelDataAS <- ModelData %>% 
  merge(ESP, by.x="Age5", by.y="Age") %>% 
  group_by(Sex, IMDq, Cause) %>% 
  summarise(Pandemic=weighted.mean(Pandemic, ESP), 
            `Pre-pandemic`=weighted.mean(`Pre-pandemic`, ESP), 
            .groups="drop") %>% 
  mutate(RateDiff=Pandemic-`Pre-pandemic`,
         RateRatio=Pandemic/`Pre-pandemic`)

#Collapse to 10 year age bands
ModelData10 <- AgeBandData %>% 
  filter(Year>=2017) %>% 
  mutate(Period=if_else(Year<=2019, "Pre-pandemic", "Pandemic")) %>% 
  group_by(Age10, Sex, IMDq, Cause, Period) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop) %>% 
  select(Age10, Sex, IMDq, Cause, mx, Period) %>% 
  spread(Period, mx) %>% 
  mutate(RateDiff=Pandemic-`Pre-pandemic`,
         RateRatio=Pandemic/`Pre-pandemic`)

#Plot differences
#Overall by cause
ModelDataAS %>% filter(Sex=="Total" & IMDq=="Overall" & 
                         !Cause %in% c("Other", "All alcohol-specific causes - wider",
                                       "Other liver disease")) %>% 
  mutate(Cause=factor(Cause, levels=c("Other alcohol-specific causes",
                                      "Alcohol dependence syndrome",
                                      "Alcohol-related liver disease", 
                                      "Acute causes", 
                                      "All alcohol-specific causes"))) %>% 
  ggplot(aes(x=RateDiff, y=Cause, fill=Cause))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#FFB81C", "#041E42", "#F9423A",  "#0072CE", "grey60"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3a <- ModelDataAS %>% filter(Sex=="Total" & IMDq=="Overall" & 
                         !Cause %in% c("Other", "All alcohol-specific causes - wider",
                                       "Other liver disease")) %>% 
  mutate(Cause=factor(Cause, levels=c("Other alcohol-specific causes",
                                      "Alcohol dependence syndrome",
                                      "Alcohol-related liver disease", 
                                      "Acute causes", 
                                      "All alcohol-specific causes"))) %>% 
  ggplot(aes(x=RateRatio, y=Cause, fill=Cause))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#FFB81C", "#041E42", "#F9423A",  "#0072CE", "grey60"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#By age
ModelData10 %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" &
                       IMDq=="Overall") %>% 
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=RateDiff, y=Age10, fill=Age10))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3b <- ModelData10 %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" &
                         IMDq=="Overall") %>% 
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=RateRatio, y=Age10, fill=Age10))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+  
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#By Sex
ModelDataAS %>% filter(Sex!="Total" & IMDq=="Overall" & 
                        Cause=="All alcohol-specific causes") %>% 
  ggplot(aes(x=RateDiff, y=Sex, fill=Sex))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3c <- ModelDataAS %>% filter(Sex!="Total" & IMDq=="Overall" & 
                         Cause=="All alcohol-specific causes") %>% 
  ggplot(aes(x=RateRatio, y=Sex, fill=Sex))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#By IMD
ModelDataAS %>% filter(Sex=="Total" & IMDq!="Overall" & 
                         Cause=="All alcohol-specific causes") %>% 
  ggplot(aes(x=RateDiff, y=IMDq, fill=IMDq))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="IMD quintile")+  
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3d <- ModelDataAS %>% filter(Sex=="Total" & IMDq!="Overall" & 
                         Cause=="All alcohol-specific causes") %>% 
  ggplot(aes(x=RateRatio, y=IMDq, fill=IMDq))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="IMD quintile")+  
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#Combine relative changes into one big plot
agg_png("Outputs/OldhamASDFigure3.png", units="in", width=12, height=8, res=800)
(Fig3a+theme(legend.position="top")+guides(fill=guide_legend(nrow=2), 
                                           colour=guide_legend(nrow=2))+
    labs(title="A - Cause"))+
    (Fig3b+theme(legend.position="top")+labs(title="B - Age"))+
  (Fig3c+theme(legend.position="top")+labs(title="C - Sex"))+
     (Fig3d+theme(legend.position="top")+labs(title="D - IMD quintile"))+
  plot_layout(widths=c(1,1.7), heights=c(1,1))

dev.off()

#Table of absolute and relative crude changes in rates
Table1Data <- ModelDataAS %>% 
  filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq=="Overall") %>% 
  select(Pandemic, `Pre-pandemic`, RateDiff, RateRatio) %>% 
  mutate(Cat="Overall", SubCat="") %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause %in% c("Other alcohol-specific causes", "Alcohol dependence syndrome",
                                  "Alcohol-related liver disease", "Acute causes") & Sex=="Total" & IMDq=="Overall") %>% 
              mutate(Cat="Cause", SubCat=Cause) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelData10 %>% 
              filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq=="Overall" & !Age10 %in% c("0-9", "10-19")) %>% 
              mutate(Cat="Age", SubCat=Age10) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause=="All alcohol-specific causes" & Sex!="Total" & IMDq=="Overall") %>% 
              mutate(Cat="Sex", SubCat=Sex) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq!="Overall") %>% 
              mutate(Cat="IMD quintile", SubCat=IMDq) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) 

Table1 <- Table1Data %>% mutate(RateRatio=RateRatio-1,
                      RateRatio=if_else(RateRatio>0, paste0("+", as.character(round(RateRatio*100, 1)), "%"), 
                                        paste0(as.character(round(RateRatio, 1)), "%")),
                      RateDiff=if_else(RateDiff>0, paste0("+", as.character(round(RateDiff, 2))), 
                                       as.character(round(RateDiff, 2)))) %>% 
  group_by(Cat) %>% 
  gt(rowname_col="SubCat") %>%
  fmt_number(columns=c("Pandemic", "Pre-pandemic"), decimals=2) %>% 
  cols_move(columns=Pandemic, after=`Pre-pandemic`) %>% 
  cols_label(`Pre-pandemic`="2017-19", Pandemic="2020-22", RateDiff="Absolute change", RateRatio="Relative change") %>% 
  cols_align(columns=everything(), align="right")

#Export table1
gtsave(Table1, "Outputs/OldhamASDTable1.docx") 

#Fit models

#Generate data for models
#Pre-pandemic
PreModelData <- AgeBandData %>% 
  filter(Year %in% c(2017:2019) & Sex!="Total" & IMDq!="Overall" &
           Cause=="All alcohol-specific causes" & 
           !Age10 %in% c("0-9", "10-19")) %>% 
  group_by(Age10, Sex, IMDq) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop,
         logPop=log(Pop))

#Post-pandemic
PostModelData <- AgeBandData %>% 
  filter(Year %in% c(2020:2022) & Sex!="Total" & IMDq!="Overall" &
           Cause=="All alcohol-specific causes" & 
           !Age10 %in% c("0-9", "10-19")) %>% 
  group_by(Age10, Sex, IMDq) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop,
         logPop=log(Pop))

#Fit models
Model1 <- glm(data=PreModelData, formula=Deaths~factor(Age10)+factor(Sex)+factor(IMDq)+offset(logPop), 
              family=quasipoisson)

summary(Model1)

Model1Out <- tidy(Model1) %>% 
  merge(as.data.frame(confint(Model1)) %>% 
          rownames_to_column() %>% 
          set_names("term", "LowerCI", "UpperCI")) %>% 
  mutate(expCoeff=exp(estimate),
         LowerCI=exp(LowerCI),
         UpperCI=exp(UpperCI))

Model2 <- glm(data=PostModelData, formula=Deaths~factor(Age10)+factor(Sex)+factor(IMDq)+offset(logPop), 
              family=quasipoisson)

summary(Model2)

Model2Out <- tidy(Model2) %>% 
  merge(as.data.frame(confint(Model2)) %>% 
          rownames_to_column() %>% 
          set_names("term", "LowerCI", "UpperCI")) %>% 
  mutate(expCoeff=exp(estimate),
         LowerCI=exp(LowerCI),
         UpperCI=exp(UpperCI))

#Get absolute effects
#2017-19
Model1AgeData <- PreModelData %>% 
  mutate(Age10=factor(Age10), Sex=factor(Sex), IMDq=factor(IMDq))

Model1Age <- gComp(Model1AgeData, outcome.type="rate_nb", Y="Deaths", X="Age10", Z=c("Sex", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model1Age)

Model1Sex <- gComp(Model1AgeData, outcome.type="rate_nb", Y="Deaths", X="Sex", Z=c("Age10", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model1Sex)

Model1IMDq <- gComp(Model1AgeData, outcome.type="rate_nb", Y="Deaths", X="IMDq", Z=c("Age10", "Sex"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model1IMDq)

#Tabulate results
PreResults <- Model1Age$results.df %>% 
  mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                       as.character(round(`2.5% CL`,2)), " - ", 
                       as.character(round(`97.5% CL`,2)), ")")) %>% 
  select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
  spread(Parameter, Result) %>% 
  mutate(Cat="Age", SubCat=substr(Comparison, 6,10),
         SubCat=if_else(SubCat=="90+_v", "90+", SubCat)) %>% 
  select(-c(Outcome, Comparison)) %>% 
  set_names("Rate Difference Pre", "Rate Ratio Pre", "Cat", "SubCat") %>% 
  bind_rows(Model1Sex$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="Sex", SubCat="Male") %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Pre", "Rate Ratio Pre", "Cat", "SubCat"),
            Model1IMDq$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="IMDq", SubCat=substr(Comparison, 1,6)) %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Pre", "Rate Ratio Pre", "Cat", "SubCat"))
  
#2020-22
Model2AgeData <- PostModelData %>% 
  mutate(Age10=factor(Age10), Sex=factor(Sex), IMDq=factor(IMDq))

Model2Age <- gComp(Model2AgeData, outcome.type="rate_nb", Y="Deaths", X="Age10", Z=c("Sex", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model2Age)

Model2Sex <- gComp(Model2AgeData, outcome.type="rate_nb", Y="Deaths", X="Sex", Z=c("Age10", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model2Sex)

Model2IMDq <- gComp(Model2AgeData, outcome.type="rate_nb", Y="Deaths", X="IMDq", Z=c("Age10", "Sex"),
                    offset="Pop", R=200, rate.multiplier=100000)

summary(Model2IMDq)

#Tabulate results
PostResults <- Model2Age$results.df %>% 
  mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                       as.character(round(`2.5% CL`,2)), " - ", 
                       as.character(round(`97.5% CL`,2)), ")")) %>% 
  select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
  spread(Parameter, Result) %>% 
  mutate(Cat="Age", SubCat=substr(Comparison, 6,10),
         SubCat=if_else(SubCat=="90+_v", "90+", SubCat)) %>% 
  select(-c(Outcome, Comparison)) %>% 
  set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat") %>% 
  bind_rows(Model2Sex$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="Sex", SubCat="Male") %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat"),
            Model2IMDq$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="IMDq", SubCat=substr(Comparison, 1,6)) %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat"))

#Combined and generate Table 2
Table2 <- merge(PreResults, PostResults) %>% 
  mutate(Cat=factor(Cat, levels=c("Age", "Sex", "IMDq")),
         SubCat=case_when(
           SubCat=="IMDqQ2" ~ "Q2", SubCat=="IMDqQ3" ~ "Q3", SubCat=="IMDqQ4" ~ "Q4",
           SubCat=="IMDqQ5" ~ "Q5 (most deprived)", TRUE ~ SubCat)) %>% 
  arrange(Cat) %>% 
  group_by(Cat) %>% 
  gt(rowname_col="SubCat") %>% 
  cols_move(columns=c(`Rate Difference Post`), after=`Rate Difference Pre`) %>% 
  #cols_move(columns=c(`Rate Ratio Post`), after=`Rate Ratio Pre`) %>% 
  tab_spanner(label="Rate Ratio", columns=c(`Rate Ratio Pre`, `Rate Ratio Post`)) %>% 
  tab_spanner(label="Absolute Rate Difference", columns=c(`Rate Difference Pre`, 
                                                          `Rate Difference Post`)) %>% 
  cols_label(`Rate Ratio Pre`="2017-19",
             `Rate Ratio Post`="2020-22",
             `Rate Difference Pre`="2017-19",
             `Rate Difference Post`="2020-22")
  
#Export table2
gtsave(Table2, "Outputs/OldhamASDTable2.docx") 

##########################################################################################
#Sensitivity analyses#
######################

#SA1 - comparing 2017-19 with 2021-22
#First pool data for each set of years
ModelDataSA1 <- Maindata5 %>% 
  filter(Year>=2017 & Year !=2020) %>% 
  mutate(Period=if_else(Year<=2019, "Pre-pandemic", "Pandemic")) %>% 
  group_by(Age5, Sex, IMDq, Cause, Period) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop) %>% 
  select(Age5, Sex, IMDq, Cause, mx, Period) %>% 
  spread(Period, mx) %>% 
  mutate(RateDiff=Pandemic-`Pre-pandemic`,
         RateRatio=Pandemic/`Pre-pandemic`)

#Age-standardise
ModelDataASSA1 <- ModelDataSA1 %>% 
  merge(ESP, by.x="Age5", by.y="Age") %>% 
  group_by(Sex, IMDq, Cause) %>% 
  summarise(Pandemic=weighted.mean(Pandemic, ESP), 
            `Pre-pandemic`=weighted.mean(`Pre-pandemic`, ESP), 
            .groups="drop") %>% 
  mutate(RateDiff=Pandemic-`Pre-pandemic`,
         RateRatio=Pandemic/`Pre-pandemic`)

#Collapse to 10 year age bands
ModelData10SA1 <- AgeBandData %>% 
  filter(Year>=2017 & Year !=2020) %>% 
  mutate(Period=if_else(Year<=2019, "Pre-pandemic", "Pandemic")) %>% 
  group_by(Age10, Sex, IMDq, Cause, Period) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop) %>% 
  select(Age10, Sex, IMDq, Cause, mx, Period) %>% 
  spread(Period, mx) %>% 
  mutate(RateDiff=Pandemic-`Pre-pandemic`,
         RateRatio=Pandemic/`Pre-pandemic`)

Fig3aSA1 <- ModelDataASSA1 %>% filter(Sex=="Total" & IMDq=="Overall" & 
                                  !Cause %in% c("Other", "All alcohol-specific causes - wider",
                                                "Other liver disease")) %>% 
  mutate(Cause=factor(Cause, levels=c("Other alcohol-specific causes",
                                      "Alcohol dependence syndrome",
                                      "Alcohol-related liver disease", 
                                      "Acute causes", 
                                      "All alcohol-specific causes"))) %>% 
  ggplot(aes(x=RateRatio, y=Cause, fill=Cause))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#FFB81C", "#041E42", "#F9423A",  "#0072CE", "grey60"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3bSA1 <- ModelData10SA1 %>% filter(Cause=="All alcohol-specific causes" & Sex=="Total" &
                                  IMDq=="Overall") %>% 
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=RateRatio, y=Age10, fill=Age10))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+  
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3cSA1 <- ModelDataASSA1 %>% filter(Sex!="Total" & IMDq=="Overall" & 
                                  Cause=="All alcohol-specific causes") %>% 
  ggplot(aes(x=RateRatio, y=Sex, fill=Sex))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3dSA1 <- ModelDataASSA1 %>% filter(Sex=="Total" & IMDq!="Overall" & 
                                  Cause=="All alcohol-specific causes") %>% 
  ggplot(aes(x=RateRatio, y=IMDq, fill=IMDq))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="IMD quintile")+  
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#Combine relative changes into one big plot
agg_png("Outputs/OldhamASDFigure3SA1.png", units="in", width=12, height=8, res=800)
(Fig3aSA1+theme(legend.position="top")+guides(fill=guide_legend(nrow=2), 
                                           colour=guide_legend(nrow=2))+
    labs(title="A - Cause"))+
  (Fig3bSA1+theme(legend.position="top")+labs(title="B - Age"))+
  (Fig3cSA1+theme(legend.position="top")+labs(title="C - Sex"))+
  (Fig3dSA1+theme(legend.position="top")+labs(title="D - IMD quintile"))+
  plot_layout(widths=c(1,1.7), heights=c(1,1))

dev.off()

#Table of absolute and relative crude changes in rates
Table1DataSA1 <- ModelDataASSA1 %>% 
  filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq=="Overall") %>% 
  select(Pandemic, `Pre-pandemic`, RateDiff, RateRatio) %>% 
  mutate(Cat="Overall", SubCat="") %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause %in% c("Other alcohol-specific causes", "Alcohol dependence syndrome",
                                  "Alcohol-related liver disease", "Acute causes") & Sex=="Total" & IMDq=="Overall") %>% 
              mutate(Cat="Cause", SubCat=Cause) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelData10 %>% 
              filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq=="Overall" & !Age10 %in% c("0-9", "10-19")) %>% 
              mutate(Cat="Age", SubCat=Age10) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause=="All alcohol-specific causes" & Sex!="Total" & IMDq=="Overall") %>% 
              mutate(Cat="Sex", SubCat=Sex) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause=="All alcohol-specific causes" & Sex=="Total" & IMDq!="Overall") %>% 
              mutate(Cat="IMD quintile", SubCat=IMDq) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) 

Table1SA1 <- Table1DataSA1 %>% mutate(RateRatio=RateRatio-1,
                                      RateRatio=if_else(RateRatio>0, paste0("+", as.character(round(RateRatio*100, 1)), "%"), 
                                                        paste0(as.character(round(RateRatio, 1)), "%")),
                                      RateDiff=if_else(RateDiff>0, paste0("+", as.character(round(RateDiff, 2))), 
                                                       as.character(round(RateDiff, 2)))) %>% 
  group_by(Cat) %>% 
  gt(rowname_col="SubCat") %>%
  fmt_number(columns=c("Pandemic", "Pre-pandemic"), decimals=2) %>% 
  cols_move(columns=Pandemic, after=`Pre-pandemic`) %>% 
  cols_label(`Pre-pandemic`="2017-19", Pandemic="2021-22", RateDiff="Absolute change", RateRatio="Relative change") %>% 
  cols_align(columns=everything(), align="right")

#Export table1
gtsave(Table1SA1, "Outputs/OldhamASDTable1SA1.docx") 

#Fit models
#Only post-pandemic model is different in this SA

#Post-pandemic
PostModelDataSA1 <- AgeBandData %>% 
  filter(Year %in% c(2021:2022) & Sex!="Total" & IMDq!="Overall" &
           Cause=="All alcohol-specific causes" & 
           !Age10 %in% c("0-9", "10-19")) %>% 
  group_by(Age10, Sex, IMDq) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop,
         logPop=log(Pop))

#Fit models
Model2SA1 <- glm(data=PostModelDataSA1, formula=Deaths~factor(Age10)+factor(Sex)+factor(IMDq)+offset(logPop), 
                 family=quasipoisson)

summary(Model2SA1)

Model2OutSA1 <- tidy(Model2SA1) %>% 
  merge(as.data.frame(confint(Model2SA1)) %>% 
          rownames_to_column() %>% 
          set_names("term", "LowerCI", "UpperCI")) %>% 
  mutate(expCoeff=exp(estimate),
         LowerCI=exp(LowerCI),
         UpperCI=exp(UpperCI))

#Get absolute effects
#2020-22
Model2AgeDataSA1 <- PostModelDataSA1 %>% 
  mutate(Age10=factor(Age10), Sex=factor(Sex), IMDq=factor(IMDq))

Model2AgeSA1 <- gComp(Model2AgeDataSA1, outcome.type="rate_nb", Y="Deaths", X="Age10", Z=c("Sex", "IMDq"),
                      offset="Pop", R=200, rate.multiplier=100000)

summary(Model2AgeSA1)

Model2SexSA1 <- gComp(Model2AgeDataSA1, outcome.type="rate_nb", Y="Deaths", X="Sex", Z=c("Age10", "IMDq"),
                      offset="Pop", R=200, rate.multiplier=100000)

summary(Model2SexSA1)

Model2IMDqSA1 <- gComp(Model2AgeDataSA1, outcome.type="rate_nb", Y="Deaths", X="IMDq", Z=c("Age10", "Sex"),
                       offset="Pop", R=200, rate.multiplier=100000)

summary(Model2IMDqSA1)

#Tabulate results
PostResultsSA1 <- Model2AgeSA1$results.df %>% 
  mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                       as.character(round(`2.5% CL`,2)), " - ", 
                       as.character(round(`97.5% CL`,2)), ")")) %>% 
  select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
  spread(Parameter, Result) %>% 
  mutate(Cat="Age", SubCat=substr(Comparison, 6,10),
         SubCat=if_else(SubCat=="90+_v", "90+", SubCat)) %>% 
  select(-c(Outcome, Comparison)) %>% 
  set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat") %>% 
  bind_rows(Model2SexSA1$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="Sex", SubCat="Male") %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat"),
            Model2IMDqSA1$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="IMDq", SubCat=substr(Comparison, 1,6)) %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat"))

#Combined and generate Table 2
Table2SA1 <- merge(PreResults, PostResultsSA1) %>% 
  mutate(Cat=factor(Cat, levels=c("Age", "Sex", "IMDq")),
         SubCat=case_when(
           SubCat=="IMDqQ2" ~ "Q2", SubCat=="IMDqQ3" ~ "Q3", SubCat=="IMDqQ4" ~ "Q4",
           SubCat=="IMDqQ5" ~ "Q5 (most deprived)", TRUE ~ SubCat)) %>% 
  arrange(Cat) %>% 
  group_by(Cat) %>% 
  gt(rowname_col="SubCat") %>% 
  cols_move(columns=c(`Rate Difference Post`), after=`Rate Difference Pre`) %>% 
  #cols_move(columns=c(`Rate Ratio Post`), after=`Rate Ratio Pre`) %>% 
  tab_spanner(label="Absolute Rate Difference", columns=c(`Rate Difference Pre`, 
                                                          `Rate Difference Post`)) %>% 
  tab_spanner(label="Rate Ratio", columns=c(`Rate Ratio Pre`, `Rate Ratio Post`)) %>% 
  cols_label(`Rate Ratio Pre`="2017-19",
             `Rate Ratio Post`="2021-22",
             `Rate Difference Pre`="2017-19",
             `Rate Difference Post`="2021-22")

#Export table2
gtsave(Table2SA1, "Outputs/OldhamASDTable2SA1.docx") 

#SA2 - wider definition of alcoholic liver disease

#RQ1 overall
agg_png("Outputs/OldhamASDFigure1SA2.png", units="in", width=8, height=5, res=800)
ASData %>% filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" &
                    IMDq=="Overall") %>%
  ggplot(aes(x=Year, y=mx))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              fill="tomato", alpha=0.4)+
  geom_line(colour="tomato")+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=2019.4, y=4, colour="grey50", label="COVID-19 pandemic", angle=90, size=4, family="Lato")

dev.off()

#RQ1a by cause
Fig2aSA2 <- ASData %>% filter(!Cause %in% c("All alcohol-specific causes", "Other", 
                                         "All alcohol-specific causes - wider") & 
                             Sex=="Total" & IMDq=="Overall") %>%
  ggplot(aes(x=Year, y=mx, colour=Cause, fill=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_fill_manual(values=c("#0072CE", "#F9423A", "#041E42", "#FFB81C", "#addd8e"))+
  scale_colour_manual(values=c("#0072CE", "#F9423A", "#041E42", "#FFB81C", "#addd8e"))+
  theme_custom()

#RQ1b by age
Fig2bSA2 <- AgeBandData %>% filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" &
                                  IMDq=="Overall") %>% 
  #Remove under 20s due to tiny counts
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=Year, y=mx, colour=Age10, fill=Age10))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()

#RQ1c by sex
Fig2cSA2 <- ASData %>% filter(Cause=="All alcohol-specific causes - wider" & 
                             Sex!="Total" & IMDq=="Overall") %>%
  ggplot(aes(x=Year, y=mx, colour=Sex, fill=Sex))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="", breaks=c("Male", "Female"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="", breaks=c("Male", "Female"))+
  theme_custom()

#RQ1d by deprivation
Fig2dSA2 <- ASData %>% filter(Cause=="All alcohol-specific causes - wider" &
                             Sex=="Total" & IMDq!="Overall") %>% 
  ggplot(aes(x=Year, y=mx, colour=IMDq, fill=IMDq))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI),
              alpha=0.4, colour="transparent")+
  geom_line()+
  geom_vline(xintercept=2019.1, colour="grey50", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", 
                     limits=c(0,NA))+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                               "#7a0177"), name="")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="")+
  theme_custom() + 
  guides(fill = guide_legend(reverse = TRUE), colour=guide_legend(reverse = TRUE))

#Combine into big faceted plot
agg_png("Outputs/OldhamASDFigure2SA2.png", units="in", width=12, height=8, res=800)
(Fig2aSA2+theme(legend.position="top")+guides(fill=guide_legend(nrow=2,byrow=TRUE), 
                                           colour=guide_legend(nrow=2,byrow=TRUE))+
    labs(title="A - Cause")|
    Fig2bSA2+theme(legend.position="top")+guides(fill=guide_legend(nrow=2,byrow=TRUE), 
                                                 colour=guide_legend(nrow=2,byrow=TRUE))+labs(title="B - Age"))/
  (Fig2cSA2+theme(legend.position="top")+labs(title="C - Sex")|
     Fig2dSA2+theme(legend.position="top")+labs(title="D - IMD quintile"))

dev.off()

#check proportional distribution of deaths between cause groups around pandemic period
agg_png("Outputs/OldhamASDFigureS1SA2.png", units="in", width=8, height=5, res=800)
ASData %>% filter(!Cause %in% c("All alcohol-specific causes", "Other", 
                                "All alcohol-specific causes - wider") &
                    Sex=="Total" & IMDq=="Overall" & Year>=2017) %>% 
  ggplot(aes(x=Year, y=mx, fill=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age-standardised deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#0072CE",  "#F9423A", "#041E42","#FFB81C", "#addd8e"), name="")+
  theme_custom()

dev.off()

#Proportion of deaths by age
agg_png("Outputs/OldhamASDFigureS2SA2.png", units="in", width=8, height=5, res=800)
AgeBandData %>% filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" & IMDq=="Overall" & Year>=2017 &
                         !Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=Year, y=Deaths, fill=Age10))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of alcohol-specific deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="Age")+
  
  theme_custom()

dev.off()

#Proportion of deaths by sex
agg_png("Outputs/OldhamASDFigureS3SA2.png", units="in", width=8, height=5, res=800)
ASData %>% filter(Cause=="All alcohol-specific causes - wider" & Sex!="Total" & IMDq=="Overall" & Year>=2017) %>% 
  ggplot(aes(x=Year, y=mx, fill=Sex))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age-standardised deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()

dev.off()

#Proportion of deaths by IMDq
agg_png("Outputs/OldhamASDFigureS4SA2.png", units="in", width=8, height=5, res=800)
ASData %>% filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" & IMDq!="Overall" & Year>=2017) %>% 
  ggplot(aes(x=Year, y=mx, fill=IMDq))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age-standardised deaths",
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="IMD quintile")+
  theme_custom()

dev.off()

#Plot differences
#Overall by cause
ModelDataAS %>% filter(Sex=="Total" & IMDq=="Overall" & 
                         !Cause %in% c("Other", "All alcohol-specific causes")) %>% 
  mutate(Cause=factor(Cause, levels=c("Other alcohol-specific causes",
                                      "Alcohol dependence syndrome",
                                      "Alcohol-related liver disease", 
                                      "Acute causes", 
                                      "Other liver disease",
                                      "All alcohol-specific causes - wider"))) %>% 
  ggplot(aes(x=RateDiff, y=Cause, fill=Cause))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#FFB81C", "#041E42", "#F9423A",  "#0072CE", "#addd8e", "grey60"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3aSA2 <- ModelDataAS %>% filter(Sex=="Total" & IMDq=="Overall" & 
                                  !Cause %in% c("Other", "All alcohol-specific causes")) %>% 
  mutate(Cause=factor(Cause, levels=c("Other alcohol-specific causes",
                                      "Alcohol dependence syndrome",
                                      "Alcohol-related liver disease", 
                                      "Acute causes", 
                                      "Other liver disease",
                                      "All alcohol-specific causes - wider"))) %>% 
  ggplot(aes(x=RateRatio, y=Cause, fill=Cause))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#FFB81C", "#041E42", "#F9423A",  "#0072CE", "#addd8e", "grey60"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#By age
ModelData10 %>% filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" &
                         IMDq=="Overall") %>% 
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=RateDiff, y=Age10, fill=Age10))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3bSA2 <- ModelData10 %>% filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" &
                                  IMDq=="Overall") %>% 
  filter(!Age10 %in% c("0-9", "10-19")) %>% 
  ggplot(aes(x=RateRatio, y=Age10, fill=Age10))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+  
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#By Sex
ModelDataAS %>% filter(Sex!="Total" & IMDq=="Overall" & 
                         Cause=="All alcohol-specific causes - wider") %>% 
  ggplot(aes(x=RateDiff, y=Sex, fill=Sex))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3cSA2 <- ModelDataAS %>% filter(Sex!="Total" & IMDq=="Overall" & 
                                  Cause=="All alcohol-specific causes - wider") %>% 
  ggplot(aes(x=RateRatio, y=Sex, fill=Sex))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#By IMD
ModelDataAS %>% filter(Sex=="Total" & IMDq!="Overall" & 
                         Cause=="All alcohol-specific causes - wider") %>% 
  ggplot(aes(x=RateDiff, y=IMDq, fill=IMDq))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="IMD quintile")+  
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

Fig3dSA2 <- ModelDataAS %>% filter(Sex=="Total" & IMDq!="Overall" & 
                                  Cause=="All alcohol-specific causes - wider") %>% 
  ggplot(aes(x=RateRatio, y=IMDq, fill=IMDq))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=1, colour="grey30")+
  scale_x_continuous(name="Change in age-standardised deaths per 100,000",
                     trans="log10", breaks=c(1,1.1,1.2,1.3),
                     labels=c("No change", "+10%", "+20%", "+30%"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"), name="IMD quintile")+  
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(colour="grey95"))

#Combine relative changes into one big plot
agg_png("Outputs/OldhamASDFigure3SA2.png", units="in", width=12, height=8, res=800)
(Fig3aSA2+theme(legend.position="top")+guides(fill=guide_legend(nrow=2), 
                                           colour=guide_legend(nrow=2))+
    labs(title="A - Cause"))+
  (Fig3bSA2+theme(legend.position="top")+labs(title="B - Age"))+
  (Fig3cSA2+theme(legend.position="top")+labs(title="C - Sex"))+
  (Fig3dSA2+theme(legend.position="top")+labs(title="D - IMD quintile"))+
  plot_layout(widths=c(1,1.7), heights=c(1,1))

dev.off()

#Table of absolute and relative crude changes in rates
Table1DataSA2 <- ModelDataAS %>% 
  filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" & IMDq=="Overall") %>% 
  select(Pandemic, `Pre-pandemic`, RateDiff, RateRatio) %>% 
  mutate(Cat="Overall", SubCat="") %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause %in% c("Other liver disease", "Other alcohol-specific causes", "Alcohol dependence syndrome",
                                  "Alcohol-related liver disease", "Acute causes") & Sex=="Total" & IMDq=="Overall") %>% 
              mutate(Cat="Cause", SubCat=Cause) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelData10 %>% 
              filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" & IMDq=="Overall" & !Age10 %in% c("0-9", "10-19")) %>% 
              mutate(Cat="Age", SubCat=Age10) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause=="All alcohol-specific causes - wider" & Sex!="Total" & IMDq=="Overall") %>% 
              mutate(Cat="Sex", SubCat=Sex) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) %>% 
  bind_rows(ModelDataAS %>% 
              filter(Cause=="All alcohol-specific causes - wider" & Sex=="Total" & IMDq!="Overall") %>% 
              mutate(Cat="IMD quintile", SubCat=IMDq) %>% 
              select(Cat, SubCat, Pandemic, `Pre-pandemic`, RateDiff, RateRatio)) 

Table1SA2 <- Table1DataSA2 %>% mutate(RateRatio=RateRatio-1,
                                RateRatio=if_else(RateRatio>0, paste0("+", as.character(round(RateRatio*100, 1)), "%"), 
                                                  paste0(as.character(round(RateRatio, 1)), "%")),
                                RateDiff=if_else(RateDiff>0, paste0("+", as.character(round(RateDiff, 2))), 
                                                 as.character(round(RateDiff, 2)))) %>% 
  group_by(Cat) %>% 
  gt(rowname_col="SubCat") %>%
  fmt_number(columns=c("Pandemic", "Pre-pandemic"), decimals=2) %>% 
  cols_move(columns=Pandemic, after=`Pre-pandemic`) %>% 
  cols_label(`Pre-pandemic`="2017-19", Pandemic="2020-22", RateDiff="Absolute change", RateRatio="Relative change") %>% 
  cols_align(columns=everything(), align="right")

#Export table1
gtsave(Table1SA2, "Outputs/OldhamASDTable1SA2.docx") 

#Fit models

#Generate data for models
#Pre-pandemic
PreModelDataSA2 <- AgeBandData %>% 
  filter(Year %in% c(2017:2019) & Sex!="Total" & IMDq!="Overall" &
           Cause=="All alcohol-specific causes - wider" & 
           !Age10 %in% c("0-9", "10-19")) %>% 
  group_by(Age10, Sex, IMDq) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop,
         logPop=log(Pop))

#Post-pandemic
PostModelDataSA2 <- AgeBandData %>% 
  filter(Year %in% c(2020:2022) & Sex!="Total" & IMDq!="Overall" &
           Cause=="All alcohol-specific causes - wider" & 
           !Age10 %in% c("0-9", "10-19")) %>% 
  group_by(Age10, Sex, IMDq) %>% 
  summarise(Deaths=sum(Deaths),
            Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop,
         logPop=log(Pop))

#Fit models
Model1SA2 <- glm(data=PreModelDataSA2, formula=Deaths~factor(Age10)+factor(Sex)+factor(IMDq)+offset(logPop), 
              family=quasipoisson)

summary(Model1SA2)

Model1OutSA2 <- tidy(Model1SA2) %>% 
  merge(as.data.frame(confint(Model1SA2)) %>% 
          rownames_to_column() %>% 
          set_names("term", "LowerCI", "UpperCI")) %>% 
  mutate(expCoeff=exp(estimate),
         LowerCI=exp(LowerCI),
         UpperCI=exp(UpperCI))

Model2SA2 <- glm(data=PostModelDataSA2, formula=Deaths~factor(Age10)+factor(Sex)+factor(IMDq)+offset(logPop), 
              family=quasipoisson)

summary(Model2SA2)

Model2OutSA2 <- tidy(Model2SA2) %>% 
  merge(as.data.frame(confint(Model2SA2)) %>% 
          rownames_to_column() %>% 
          set_names("term", "LowerCI", "UpperCI")) %>% 
  mutate(expCoeff=exp(estimate),
         LowerCI=exp(LowerCI),
         UpperCI=exp(UpperCI))

#Get absolute effects
#2017-19
Model1AgeDataSA2 <- PreModelDataSA2 %>% 
  mutate(Age10=factor(Age10), Sex=factor(Sex), IMDq=factor(IMDq))

Model1AgeSA2 <- gComp(Model1AgeDataSA2, outcome.type="rate_nb", Y="Deaths", X="Age10", Z=c("Sex", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model1AgeSA2)

Model1SexSA2 <- gComp(Model1AgeDataSA2, outcome.type="rate_nb", Y="Deaths", X="Sex", Z=c("Age10", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model1SexSA2)

Model1IMDqSA2 <- gComp(Model1AgeDataSA2, outcome.type="rate_nb", Y="Deaths", X="IMDq", Z=c("Age10", "Sex"),
                    offset="Pop", R=200, rate.multiplier=100000)

summary(Model1IMDqSA2)

#Tabulate results
PreResultsSA2 <- Model1AgeSA2$results.df %>% 
  mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                       as.character(round(`2.5% CL`,2)), " - ", 
                       as.character(round(`97.5% CL`,2)), ")")) %>% 
  select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
  spread(Parameter, Result) %>% 
  mutate(Cat="Age", SubCat=substr(Comparison, 6,10),
         SubCat=if_else(SubCat=="90+_v", "90+", SubCat)) %>% 
  select(-c(Outcome, Comparison)) %>% 
  set_names("Rate Difference Pre", "Rate Ratio Pre", "Cat", "SubCat") %>% 
  bind_rows(Model1SexSA2$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="Sex", SubCat="Male") %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Pre", "Rate Ratio Pre", "Cat", "SubCat"),
            Model1IMDqSA2$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="IMDq", SubCat=substr(Comparison, 1,6)) %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Pre", "Rate Ratio Pre", "Cat", "SubCat"))

#2020-22
Model2AgeDataSA2 <- PostModelDataSA2 %>% 
  mutate(Age10=factor(Age10), Sex=factor(Sex), IMDq=factor(IMDq))

Model2AgeSA2 <- gComp(Model2AgeDataSA2, outcome.type="rate_nb", Y="Deaths", X="Age10", Z=c("Sex", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model2AgeSA2)

Model2SexSA2 <- gComp(Model2AgeDataSA2, outcome.type="rate_nb", Y="Deaths", X="Sex", Z=c("Age10", "IMDq"),
                   offset="Pop", R=200, rate.multiplier=100000)

summary(Model2SexSA2)

Model2IMDqSA2 <- gComp(Model2AgeDataSA2, outcome.type="rate_nb", Y="Deaths", X="IMDq", Z=c("Age10", "Sex"),
                    offset="Pop", R=200, rate.multiplier=100000)

summary(Model2IMDqSA2)

#Tabulate results
PostResultsSA2 <- Model2AgeSA2$results.df %>% 
  mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                       as.character(round(`2.5% CL`,2)), " - ", 
                       as.character(round(`97.5% CL`,2)), ")")) %>% 
  select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
  spread(Parameter, Result) %>% 
  mutate(Cat="Age", SubCat=substr(Comparison, 6,10),
         SubCat=if_else(SubCat=="90+_v", "90+", SubCat)) %>% 
  select(-c(Outcome, Comparison)) %>% 
  set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat") %>% 
  bind_rows(Model2SexSA2$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="Sex", SubCat="Male") %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat"),
            Model2IMDqSA2$results.df %>% 
              mutate(Result=paste0(as.character(round(Estimate, 2)), " (", 
                                   as.character(round(`2.5% CL`,2)), " - ", 
                                   as.character(round(`97.5% CL`,2)), ")")) %>% 
              select(-c(Estimate, `2.5% CL`, `97.5% CL`)) %>% 
              spread(Parameter, Result) %>% 
              mutate(Cat="IMDq", SubCat=substr(Comparison, 1,6)) %>% 
              select(-c(Outcome, Comparison)) %>% 
              set_names("Rate Difference Post", "Rate Ratio Post", "Cat", "SubCat"))

#Combined and generate Table 2
Table2SA2 <- merge(PreResultsSA2, PostResultsSA2) %>% 
  mutate(Cat=factor(Cat, levels=c("Age", "Sex", "IMDq")),
         SubCat=case_when(
           SubCat=="IMDqQ2" ~ "Q2", SubCat=="IMDqQ3" ~ "Q3", SubCat=="IMDqQ4" ~ "Q4",
           SubCat=="IMDqQ5" ~ "Q5 (most deprived)", TRUE ~ SubCat)) %>% 
  arrange(Cat) %>% 
  group_by(Cat) %>% 
  gt(rowname_col="SubCat") %>% 
  cols_move(columns=c(`Rate Difference Post`), after=`Rate Difference Pre`) %>% 
  #cols_move(columns=c(`Rate Ratio Post`), after=`Rate Ratio Pre`) %>% 
  tab_spanner(label="Absolute Rate Difference", columns=c(`Rate Difference Pre`, 
                                                          `Rate Difference Post`)) %>% 
  tab_spanner(label="Rate Ratio", columns=c(`Rate Ratio Pre`, `Rate Ratio Post`)) %>% 
  cols_label(`Rate Ratio Pre`="2017-19",
             `Rate Ratio Post`="2020-22",
             `Rate Difference Pre`="2017-19",
             `Rate Difference Post`="2020-22")

#Export table2
gtsave(Table2SA2, "Outputs/OldhamASDTable2SA2.docx") 

#Figures for text
RateChanges <- ASData %>% filter(Sex=="Total" & IMDq=="Overall" & Cause=="All alcohol-specific causes") %>% 
  mutate(PercChange=(mx-lag(mx,1))/lag(mx,1))

RateChanges %>% filter(Year %in% c(2020, 2022)) %>% 
  mutate(PercChange=(mx-lag(mx,1))/lag(mx,1))

RateChanges %>% filter(Year %in% c(2019, 2022)) %>% 
  mutate(PercChange=(mx-lag(mx,1))/lag(mx,1))

RateChanges %>% filter(Year %in% c(2001, 2008)) %>% 
  mutate(PercChange=(mx-lag(mx,1))/lag(mx,1))

