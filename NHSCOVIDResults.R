rm(list=ls())

library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)
library(scales)
library(ggtext)
library(ggrepel)
library(forcats)
library(readxl)
library(gt)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Calibri"))
}

folder <- "X:/ScHARR/SARG_SAPM_3_5/General/NHS scenairos Dec2021/report/results"

#Outcomes by cause and year
S1cy <- read_excel(paste0(folder, "/S1/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by health conditions", range="B3:BN48") %>% 
  select(-c(22,23,44,45)) %>% 
  set_names("Condition", paste0("Deaths_", 1:20), paste0("Sick_", 1:20), 
            paste0("Admissions_", 1:20)) %>% 
  mutate(Scenario=1) %>% 
  pivot_longer(c(2:61), names_to=c("Metric", "Year"), names_sep="_", values_to="Count")

S2cy <- read_excel(paste0(folder, "/S2/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by health conditions", range="B3:BN48") %>% 
  select(-c(22,23,44,45)) %>% 
  set_names("Condition", paste0("Deaths_", 1:20), paste0("Sick_", 1:20), 
            paste0("Admissions_", 1:20)) %>% 
  mutate(Scenario=2) %>% 
  pivot_longer(c(2:61), names_to=c("Metric", "Year"), names_sep="_", values_to="Count")

S3cy <- read_excel(paste0(folder, "/S3/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by health conditions", range="B3:BN48") %>% 
  select(-c(22,23,44,45)) %>% 
  set_names("Condition", paste0("Deaths_", 1:20), paste0("Sick_", 1:20), 
            paste0("Admissions_", 1:20)) %>% 
  mutate(Scenario=3) %>% 
  pivot_longer(c(2:61), names_to=c("Metric", "Year"), names_sep="_", values_to="Count")

S4cy <- read_excel(paste0(folder, "/S4/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by health conditions", range="B3:BN48") %>% 
  select(-c(22,23,44,45)) %>% 
  set_names("Condition", paste0("Deaths_", 1:20), paste0("Sick_", 1:20), 
            paste0("Admissions_", 1:20)) %>% 
  mutate(Scenario=4) %>% 
  pivot_longer(c(2:61), names_to=c("Metric", "Year"), names_sep="_", values_to="Count")

S5cy <- read_excel(paste0(folder, "/S5/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by health conditions", range="B3:BN48") %>% 
  select(-c(22,23,44,45)) %>% 
  set_names("Condition", paste0("Deaths_", 1:20), paste0("Sick_", 1:20), 
            paste0("Admissions_", 1:20)) %>% 
  mutate(Scenario=5) %>% 
  pivot_longer(c(2:61), names_to=c("Metric", "Year"), names_sep="_", values_to="Count")

#S6cy <- read_excel(paste0(folder, "/S6/SAPM3_C2HHealth_Results.xlsx"), 
#                   sheet="Summary by health conditions", range="B3:BN48") %>% 
#  select(-c(22,23,44,45)) %>% 
#  set_names("Condition", paste0("Deaths_", 1:20), paste0("Sick_", 1:20), 
#            paste0("Admissions_", 1:20)) %>% 
#  mutate(Scenario=6) %>% 
#  pivot_longer(c(2:61), names_to=c("Metric", "Year"), names_sep="_", values_to="Count")

#Read in health conditions list
ConditionsList <- read.csv("X:/ScHARR/SARG_SAPM_3_5/General/NHS scenairos Dec2021/SAPM_v4.1_260121/HealthConditionsList.csv")

datacy <- bind_rows(S1cy, S2cy, S3cy, S4cy, S5cy) %>% 
  mutate(Year=as.numeric(Year), Scenario=as.factor(Scenario),
         Condition=as.numeric(substr(Condition, 17,19))) %>% 
  merge(ConditionsList %>% select(-F4), by.x="Condition", by.y="Key") %>% 
mutate(scenarioname=case_when(
  Scenario==1 ~ "No rebound",
  Scenario==2 ~ "Immediate rebound",
  Scenario==3 ~ "Moderate-only rebound",
  Scenario==4 ~ "Slower heavier rebound",
  Scenario==5 ~ "Increasing consumption"),
  scenarioname=factor(scenarioname, levels=c("Immediate rebound", 
                                           "Slower heavier rebound",
                                           "No rebound",
                                           "Moderate-only rebound",
                                           "Increasing consumption")),
  Year=2019+Year,
  Type=if_else(Condition==38, "Dependence-related", Type))

#Calculate totals
datacy_tot <- datacy %>% 
  group_by(Metric, Year, scenarioname) %>% 
  summarise(Count=sum(Count))

agg_png("Outputs/NHSATSFig11.png", units="in", width=8, height=6, res=500)
ggplot(datacy_tot %>% filter(Metric!="Sick"), 
       aes(x=Year, y=Count, colour=scenarioname))+
  geom_hline(yintercept=0, colour="Grey70")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Change per year vs. baseline")+
  scale_colour_manual(values=c("#e22618", "#eaaf38", "#01ad74", "#08b5d3", "#002e3b", "#8338EC"),
                           name="Scenario", 
                           guide=guide_legend(reverse=TRUE))+
  facet_wrap(~Metric, scales="free_y")+
  theme_custom()+
  labs(title="Changes in health outcomes under modelled scenarios",
       subtitle="Annual changes in alcohol-attributable hospital admissions and deaths compared to baseline")

dev.off()

datacy_grp <- datacy %>% 
  group_by(Type, Metric, Year, scenarioname) %>% 
  summarise(Count=sum(Count))

agg_png("Outputs/NHSATSFig11.png", units="in", width=9, height=6, res=500)
ggplot()+
  geom_area(data=datacy_grp %>% filter(Metric=="Admissions"),
            aes(x=Year, y=Count, fill=Type))+
  geom_line(data=datacy_tot %>% filter(Metric=="Admissions"),
            aes(x=Year, y=Count), colour="Grey30", 
            linetype=2)+
  geom_hline(yintercept=0)+
  facet_grid(~scenarioname)+
  scale_fill_paletteer_d("colorBlindness::paletteMartin", 
                         name="Condition type")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Admissions per year")+
  theme_custom()+
  theme(legend.position = "top")+
  labs(title="Changes in hospital admissions under modelled scenarios",
       subtitle="Annual changes in alcohol-attributable hospital admissions by condition type compared to baseline.\nDashed lines represent the net change.")

dev.off()

agg_png("Outputs/NHSATSFig12.png", units="in", width=9, height=6, res=500)
ggplot()+
  geom_area(data=datacy_grp %>% filter(Metric=="Deaths"),
            aes(x=Year, y=Count, fill=Type))+
  geom_line(data=datacy_tot %>% filter(Metric=="Deaths"),
            aes(x=Year, y=Count), colour="Grey30", 
            linetype=2)+
  geom_hline(yintercept=0)+
  facet_grid(~scenarioname)+
  scale_fill_paletteer_d("colorBlindness::paletteMartin", 
                         name="Condition type")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per year")+
  theme_custom()+
  theme(legend.position = "top")+
  labs(title="Changes in alcohol-attributable deaths under modelled scenarios",
       subtitle="Annual changes in alcohol-attributable deaths by condition type compared to baseline.\nDashed lines represent the net change.")

dev.off()

datacy_grp %>% 
  ungroup() %>% 
  filter(Metric=="Admissions") %>% 
  select(-Metric) %>% 
  spread(Year, Count) %>% 
  gt(rowname_col="Type", groupname_col="scenarioname") %>% 
  fmt_number(columns=as.character(c(2020:2039)), use_seps=TRUE, decimals=0) %>% 
  tab_options(table.font.names="Calibri",
              column_labels.font.size = "small",
              table.font.size = "small",
              row_group.font.size = "small",
              data_row.padding = px(3)) %>% 
  gtsave("Table3.png", path="Outputs/JPEGS", vwidth=1100)

datacy_grp %>% 
  ungroup() %>% 
  filter(Metric=="Deaths") %>% 
  select(-Metric) %>% 
  spread(Year, Count) %>% 
  gt(rowname_col="Type", groupname_col="scenarioname") %>% 
  fmt_number(columns=as.character(c(2020:2039)), use_seps=TRUE, decimals=0) %>% 
  tab_options(table.font.names="Calibri",
              column_labels.font.size = "small",
              table.font.size = "small",
              row_group.font.size = "small",
              data_row.padding = px(3)) %>% 
  gtsave("Table4.png", path="Outputs/JPEGS", vwidth=1100)

#Cumulative by condition
datacy_cumul <- datacy %>% 
  group_by(Metric, Name, Scenario) %>% 
  summarise(Count=sum(Count))

#Cumulative by condition group



###########################
#Cumulative outcomes by subgroup
S1sg <- read_excel(paste0(folder, "/S1/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by subgroups", range="B3:BA81") %>% 
  filter(substr(`...1`, 1,5)=="Cumul") %>% 
  mutate(Metric=c(rep("Deaths", times=5), rep("Sick", times=5),
                  rep("Admissions", times=5), rep("QALY", times=6),
                  rep("Cost", times=5))) %>% 
  group_by(Metric) %>% 
  summarise(across(c(2:52), sum)) %>% 
  gather(Subgroup, Count, c(2:52)) %>% 
  mutate(Scenario=1)

S2sg <- read_excel(paste0(folder, "/S2/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by subgroups", range="B3:BA81") %>% 
  filter(substr(`...1`, 1,5)=="Cumul") %>% 
  mutate(Metric=c(rep("Deaths", times=5), rep("Sick", times=5),
                  rep("Admissions", times=5), rep("QALY", times=6),
                  rep("Cost", times=5))) %>% 
  group_by(Metric) %>% 
  summarise(across(c(2:52), sum)) %>% 
  gather(Subgroup, Count, c(2:52)) %>% 
  mutate(Scenario=2)

S3sg <- read_excel(paste0(folder, "/S3/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by subgroups", range="B3:BA81") %>% 
  filter(substr(`...1`, 1,5)=="Cumul") %>% 
  mutate(Metric=c(rep("Deaths", times=5), rep("Sick", times=5),
                  rep("Admissions", times=5), rep("QALY", times=6),
                  rep("Cost", times=5))) %>% 
  group_by(Metric) %>% 
  summarise(across(c(2:52), sum)) %>% 
  gather(Subgroup, Count, c(2:52)) %>% 
  mutate(Scenario=3)

S4sg <- read_excel(paste0(folder, "/S4/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by subgroups", range="B3:BA81") %>% 
  filter(substr(`...1`, 1,5)=="Cumul") %>% 
  mutate(Metric=c(rep("Deaths", times=5), rep("Sick", times=5),
                  rep("Admissions", times=5), rep("QALY", times=6),
                  rep("Cost", times=5))) %>% 
  group_by(Metric) %>% 
  summarise(across(c(2:52), sum)) %>% 
  gather(Subgroup, Count, c(2:52)) %>% 
  mutate(Scenario=4)

S5sg <- read_excel(paste0(folder, "/S5/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by subgroups", range="B3:BA81") %>% 
  filter(substr(`...1`, 1,5)=="Cumul") %>% 
  mutate(Metric=c(rep("Deaths", times=5), rep("Sick", times=5),
                  rep("Admissions", times=5), rep("QALY", times=6),
                  rep("Cost", times=5))) %>% 
  group_by(Metric) %>% 
  summarise(across(c(2:52), sum)) %>% 
  gather(Subgroup, Count, c(2:52)) %>% 
  mutate(Scenario=5)

#Bring in extreme scenario
SExtsg <- read_excel(paste0(folder, "/Extreme scenario/SAPM3_C2HHealth_Results.xlsx"), 
                   sheet="Summary by subgroups", range="B3:BA81") %>% 
  filter(substr(`...1`, 1,5)=="Cumul") %>% 
  mutate(Metric=c(rep("Deaths", times=5), rep("Sick", times=5),
                  rep("Admissions", times=5), rep("QALY", times=6),
                  rep("Cost", times=5))) %>% 
  group_by(Metric) %>% 
  summarise(across(c(2:52), sum)) %>% 
  gather(Subgroup, Extreme, c(2:52)) %>% 
  mutate(Extreme=-Extreme)

#Bring in populations for rates
SPopssg <- as.data.frame(t(read_excel(paste0(folder, "/Extreme scenario/SAPM3_P2C_Results.xlsx"), 
                     sheet="P2C-Summary", range="C2:BV7", col_names=FALSE))) %>% 
  select(1, 6) %>% 
  set_names("Subgroup", "Drinkers") %>% 
  mutate(Subgroup=case_when(
    Subgroup=="Mod" ~ "Moderate", Subgroup=="Haz" ~ "Hazardous",
    Subgroup=="Harm" ~ "Harmful", Subgroup=="Male" ~ "Males", Subgroup=="Female" ~ "Females",
    TRUE ~ Subgroup),
    Drinkers=as.numeric(Drinkers))

datasg <- bind_rows(S1sg, S2sg, S3sg, S4sg, S5sg) %>% 
  merge(SExtsg) %>% 
  merge(SPopssg, all.x=TRUE) %>% 
  mutate(relchange=Count/Extreme,
         scenarioname=case_when(
           Scenario==1 ~ "No rebound",
           Scenario==2 ~ "Immediate rebound",
           Scenario==3 ~ "Moderate-only rebound",
           Scenario==4 ~ "Slower heavier rebound",
           Scenario==5 ~ "Increasing consumption"),
           scenarioname=factor(scenarioname, 
                               levels=c("Immediate rebound",
                                        "Slower heavier rebound",
                                        "No rebound",
                                        "Moderate-only rebound",
                                        "Increasing consumption")),
         Subgroup=case_when(
           Subgroup=="Hazardous" ~ "Increasing risk",
           Subgroup=="Harmful" ~ "Higher risk",
           TRUE ~ Subgroup),
         Rate=Count*100000/Drinkers)

#Outcomes by scenario
datasg %>% filter(Subgroup=="Population" & 
                    Metric %in% c("Admissions", "Deaths")) %>% 
  arrange(fct_rev(Metric), scenarioname) %>% 
  select(scenarioname, Extreme, Count, relchange) %>% 
  set_names("Scenario", "Baseline", "Difference", "% Difference") %>% 
  gt() %>% 
  tab_row_group(label="Deaths", rows=c(1:5)) %>% 
  tab_row_group(label="Admissions", rows=c(6:10)) %>% 
  fmt_number(columns = c(Baseline,Difference), decimals = 0, use_seps = TRUE) %>% 
  fmt_percent(columns=`% Difference`, decimals=1) %>% 
  cols_align(columns="Scenario", align="left") %>% 
  tab_options(table.font.names="Calibri") %>% 
  gtsave("Table2.png", path="Outputs/JPEGS")
  
  
  
  
agg_png("Outputs/NHSATSFig13.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup=="Population" & 
                           Metric %in% c("Admissions", "Deaths")),
       aes(y=scenarioname, x=relchange, fill=scenarioname))+
  geom_vline(xintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  geom_text(aes(label=paste0("+", round(relchange*100, 1), "%")),
            hjust=0, nudge_x=0.002, colour="Grey40", size=rel(3))+
  scale_x_continuous(name="Cumulative change over 20 years",
                     label=label_percent(accuracy=1),
                     breaks=c(0,0.05,0.1,0.15,0.2),
                     limits=c(0,0.24))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("fishualize::Scarus_tricolor")+
  scale_colour_paletteer_d("fishualize::Scarus_tricolor")+
  theme_custom()+
  facet_wrap(~Metric)+
  labs(title="Modelled changes in health outcomes over 20 years",
       subtitle="Cumulative change in alcohol-attributable hospital admisisons and deaths compared to baseline")

dev.off()

#By drinker group
agg_png("Outputs/NHSATSFig14.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup %in% c("Moderate", "Increasing risk",
                                         "Higher risk") & 
                           Metric=="Admissions") %>% 
         mutate(Subgroup=factor(Subgroup, levels=c("Moderate", "Increasing risk",
                                                   "Higher risk"))),
       aes(x=Subgroup, y=Rate, 
           fill=Subgroup))+
  geom_hline(yintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Cumulative change over 20 years\nper 100,000 drinkers")+
  scale_fill_manual(values=c("#92d050", "#ffc000", "#c00000"))+

  theme_custom()+
  facet_wrap(~scenarioname)+
  labs(title="Modelled changes in hospital admissions over 20 years",
       subtitle="Cumulative change in alcohol-attributable hospital admission rates compared to baseline by drinker group")

dev.off()

agg_png("Outputs/NHSATSFig15.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup %in% c("Moderate", "Increasing risk",
                                         "Higher risk") & 
                           Metric=="Deaths") %>% 
         mutate(Subgroup=factor(Subgroup, levels=c("Moderate", "Increasing risk",
                                                   "Higher risk"))),
       aes(x=Subgroup, y=Rate, 
           fill=Subgroup))+
  geom_hline(yintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Cumulative change over 20 years\nper 100,000 drinkers")+
  scale_fill_manual(values=c("#92d050", "#ffc000", "#c00000"))+
  
  theme_custom()+
  facet_wrap(~scenarioname)+
  labs(title="Modelled changes in deaths over 20 years",
       subtitle="Cumulative change in alcohol-attributable death rates compared to baseline by drinker group")

dev.off()

datasg %>% filter(Subgroup %in% c("Moderate", "Increasing risk",
                                  "Higher risk") & 
                    Metric=="Admissions")%>% 
  mutate(Subgroup=factor(Subgroup, levels=c("Moderate", "Increasing risk",
                                            "Higher risk"))) %>% 
  select(scenarioname, Subgroup, Drinkers, Extreme, Count, Rate, relchange) %>% 
  gt(rowname_col="Subgroup", groupname_col="scenarioname") %>% 
  fmt_number(columns=c(Drinkers, Extreme, Count, Rate), decimals=0, use_seps = TRUE) %>% 
  fmt_percent(columns=relchange, decimals=1) %>% 
  cols_label(Drinkers="Population", Extreme="Baseline", Count="Difference", 
             Rate="Per 100,000", relchange="% Difference") %>% 
  tab_spanner(label="Cumulative change vs. baseline",
              columns=c(Count, Rate, relchange)) %>% 
  tab_options(table.font.names="Calibri") %>% 
  gtsave("Table5.png", path="Outputs/JPEGS")

datasg %>% filter(Subgroup %in% c("Moderate", "Increasing risk",
                                  "Higher risk") & 
                    Metric=="Deaths")%>% 
  mutate(Subgroup=factor(Subgroup, levels=c("Moderate", "Increasing risk",
                                            "Higher risk"))) %>% 
  select(scenarioname, Subgroup, Drinkers, Extreme, Count, Rate, relchange) %>% 
  gt(rowname_col="Subgroup", groupname_col="scenarioname") %>% 
  fmt_number(columns=c(Drinkers, Extreme, Count, Rate), decimals=0, use_seps = TRUE) %>% 
  fmt_percent(columns=relchange, decimals=1) %>% 
  cols_label(Drinkers="Population", Extreme="Baseline", Count="Difference", 
             Rate="Per 100,000", relchange="% Difference") %>% 
  tab_spanner(label="Cumulative change vs. baseline",
              columns=c(Count, Rate, relchange)) %>% 
  tab_options(table.font.names="Calibri") %>% 
  gtsave("Table6.png", path="Outputs/JPEGS")

#By sex
agg_png("Outputs/NHSATSFig18.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup %in% c("Males", "Females") & 
                           Metric=="Admissions") %>% 
         mutate(Subgroup=factor(Subgroup, levels=c("Males", "Females"))),
       aes(x=Subgroup, y=Rate, 
           fill=Subgroup))+
  geom_hline(yintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Cumulative change over 20 years\nper 100,000 drinkers")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"))+
  
  theme_custom()+
  facet_wrap(~scenarioname)+
  labs(title="Modelled changes in hospital admissions over 20 years",
       subtitle="Cumulative change in alcohol-attributable hospital admission rates compared to baseline by sex")

dev.off()

agg_png("Outputs/NHSATSFig19.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup %in% c("Males", "Females") & 
                           Metric=="Deaths") %>% 
         mutate(Subgroup=factor(Subgroup, levels=c("Males", "Females"))),
       aes(x=Subgroup, y=Rate, 
           fill=Subgroup))+
  geom_hline(yintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Cumulative change over 20 years\nper 100,000 drinkers")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"))+
  
  theme_custom()+
  facet_wrap(~scenarioname)+
  labs(title="Modelled changes in deaths over 20 years",
       subtitle="Cumulative change in alcohol-attributable death rates compared to baseline by sex")

dev.off()

datasg %>% filter(Subgroup %in% c("Males", "Females") & 
                    Metric=="Admissions")%>% 
  mutate(Subgroup=factor(Subgroup, levels=c("Males", "Females"))) %>% 
  select(scenarioname, Subgroup, Drinkers, Extreme, Count, Rate, relchange) %>% 
  gt(rowname_col="Subgroup", groupname_col="scenarioname") %>% 
  fmt_number(columns=c(Drinkers, Extreme, Count, Rate), decimals=0, use_seps = TRUE) %>% 
  fmt_percent(columns=relchange, decimals=1) %>% 
  cols_label(Drinkers="Population", Extreme="Baseline", Count="Difference", 
             Rate="Per 100,000", relchange="% Difference") %>% 
  tab_spanner(label="Cumulative change vs. baseline",
              columns=c(Count, Rate, relchange)) %>% 
  tab_options(table.font.names="Calibri") %>% 
  gtsave("Table7.png", path="Outputs/JPEGS")

datasg %>% filter(Subgroup %in% c("Males", "Females") & 
                    Metric=="Deaths")%>% 
  mutate(Subgroup=factor(Subgroup, levels=c("Males", "Females"))) %>% 
  select(scenarioname, Subgroup, Drinkers, Extreme, Count, Rate, relchange) %>% 
  gt(rowname_col="Subgroup", groupname_col="scenarioname") %>% 
  fmt_number(columns=c(Drinkers, Extreme, Count, Rate), decimals=0, use_seps = TRUE) %>% 
  fmt_percent(columns=relchange, decimals=1) %>% 
  cols_label(Drinkers="Population", Extreme="Baseline", Count="Difference", 
             Rate="Per 100,000", relchange="% Difference") %>% 
  tab_spanner(label="Cumulative change vs. baseline",
              columns=c(Count, Rate, relchange)) %>% 
  tab_options(table.font.names="Calibri") %>% 
  gtsave("Table8.png", path="Outputs/JPEGS")

#By IMDq
agg_png("Outputs/NHSATSFig20.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup %in% c("IMDQ1 (least deprived)", "IMDQ2",
                                         "IMDQ3", "IMDQ4",
                                         "IMDQ5 (most deprived)") & 
                           Metric=="Admissions") %>% 
         mutate(Subgroup=factor(Subgroup, levels=c("IMDQ1 (least deprived)", "IMDQ2",
                                                   "IMDQ3", "IMDQ4",
                                                   "IMDQ5 (most deprived)"))),
       aes(x=Subgroup, y=Rate, 
           fill=Subgroup))+
  geom_hline(yintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Cumulative change over 20 years\nper 100,000 drinkers")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"))+
  
  theme_custom()+
  theme(axis.text.x=element_text(angle=80, hjust=1, vjust=1))+
  facet_wrap(~scenarioname)+
  labs(title="Modelled changes in hospital admissions over 20 years",
       subtitle="Cumulative change in alcohol-attributable hospital admisison rates compared to baseline by deprivation quintile")

dev.off()

agg_png("Outputs/NHSATSFig21.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup %in% c("IMDQ1 (least deprived)", "IMDQ2",
                                         "IMDQ3", "IMDQ4",
                                         "IMDQ5 (most deprived)") & 
                           Metric=="Deaths") %>% 
         mutate(Subgroup=factor(Subgroup, levels=c("IMDQ1 (least deprived)", "IMDQ2",
                                                   "IMDQ3", "IMDQ4",
                                                   "IMDQ5 (most deprived)"))),
       aes(x=Subgroup, y=Rate, 
           fill=Subgroup))+
  geom_hline(yintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Cumulative change over 20 years\nper 100,000 drinkers")+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a",
                             "#7a0177"))+
  
  theme_custom()+
  theme(axis.text.x=element_text(angle=80, hjust=1, vjust=1))+
  facet_wrap(~scenarioname)+
  labs(title="Modelled changes in deaths over 20 years",
       subtitle="Cumulative change in alcohol-attributable death rates compared to baseline by deprivation quintile")

dev.off()

datasg %>% filter(Subgroup %in% c("IMDQ1 (least deprived)", "IMDQ2",
                                  "IMDQ3", "IMDQ4",
                                  "IMDQ5 (most deprived)") & 
                    Metric=="Admissions")%>% 
  mutate(Subgroup=factor(Subgroup, levels=c("IMDQ1 (least deprived)", "IMDQ2",
                                            "IMDQ3", "IMDQ4",
                                            "IMDQ5 (most deprived)"))) %>% 
  select(scenarioname, Subgroup, Drinkers, Extreme, Count, Rate, relchange) %>% 
  gt(rowname_col="Subgroup", groupname_col="scenarioname") %>% 
  fmt_number(columns=c(Drinkers, Extreme, Count, Rate), decimals=0, use_seps = TRUE) %>% 
  fmt_percent(columns=relchange, decimals=1) %>% 
  cols_label(Drinkers="Population", Extreme="Baseline", Count="Difference", 
             Rate="Per 100,000", relchange="% Difference") %>% 
  tab_spanner(label="Cumulative change vs. baseline",
              columns=c(Count, Rate, relchange)) %>% 
  tab_options(table.font.names="Calibri") %>% 
  gtsave("Table9.png", path="Outputs/JPEGS")

datasg %>% filter(Subgroup %in% c("IMDQ1 (least deprived)", "IMDQ2",
                                  "IMDQ3", "IMDQ4",
                                  "IMDQ5 (most deprived)") & 
                    Metric=="Deaths")%>% 
  mutate(Subgroup=factor(Subgroup, levels=c("IMDQ1 (least deprived)", "IMDQ2",
                                            "IMDQ3", "IMDQ4",
                                            "IMDQ5 (most deprived)"))) %>% 
  select(scenarioname, Subgroup, Drinkers, Extreme, Count, Rate, relchange) %>% 
  gt(rowname_col="Subgroup", groupname_col="scenarioname") %>% 
  fmt_number(columns=c(Drinkers, Extreme, Count, Rate), decimals=0, use_seps = TRUE) %>% 
  fmt_percent(columns=relchange, decimals=1) %>% 
  cols_label(Drinkers="Population", Extreme="Baseline", Count="Difference", 
             Rate="Per 100,000", relchange="% Difference") %>% 
  tab_spanner(label="Cumulative change vs. baseline",
              columns=c(Count, Rate, relchange)) %>% 
  tab_options(table.font.names="Calibri") %>% 
  gtsave("Table10.png", path="Outputs/JPEGS")

#Costs
agg_png("Outputs/NHSATSFig22.png", units="in", width=9, height=6, res=500)
ggplot(datasg %>% filter(Subgroup=="Population" & 
                           Metric=="Cost"),
       aes(y=scenarioname, x=Count/1000000000, fill=scenarioname))+
  geom_vline(xintercept=0, colour="Grey70")+
  geom_col(show.legend=FALSE)+
  geom_text(aes(label=paste("£", round(Count/1000000000, 1), "bn")),
            hjust=0, nudge_x=0.05, colour="Grey40", size=rel(3))+
  scale_x_continuous(name="Cumulative change over 20 years (£bn)", limits=c(0,6))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("fishualize::Scarus_tricolor")+
  scale_colour_paletteer_d("fishualize::Scarus_tricolor")+
  
  theme_custom()+
  labs(title="Modelled changes in NHS costs over 20 years",
       subtitle="Cumulative change in alcohol-attributable NHS costs compared to baseline")

dev.off()
