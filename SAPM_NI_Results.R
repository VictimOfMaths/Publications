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
library(stringr)
library(snakecase)

options(scipen=999999)

#For MYSTERY REASONS (see https://github.com/rstudio/gt/issues/1077), gt()
#occasionally falls over when saving the tables out, with the error
#Error in s$close() : attempt to apply non-function
#If this happens then run the following and it will magically work again
#(restarting RStudio also works, but is more hassle!)

#f <- chromote::default_chromote_object(); f$close()

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Calibri"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Define some colour palettes
drinkpal <- c("#ffc000", "#00b050", "#7030a0", "#00b0f0", "#ff0000")
drinkerpal <- c("#94d050", "#ffc000", "#c00000")
channelpal <- c("#300060", "#cc99ff")
incomepal <- c("#E69F00", "#56B4E9")
drinkincpal <- c("#619428", "#BCE292", "#BC8F00", "#FFE389", "#8E0000", "#FF3F3F")

#List policy names
Policies <- c("10% price rise", "40p MUP", "45p MUP", "50p MUP", "55p MUP",
              "60p MUP", "65p MUP", "70p MUP", "75p MUP", "80p MUP", 
              "Promotion ban", "Promotion ban + 40p MUP", "Promotion ban + 45p MUP",
              "Promotion ban + 50p MUP", "Promotion ban + 55p MUP",
              "Promotion ban + 60p MUP", "Promotion ban + 65p MUP",
              "Promotion ban + 70p MUP", "Promotion ban + 75p MUP",
              "Promotion ban + 80p MUP")

#Define where the model results live
folder <- "X:/HAR_PR/PR/NI_MUP_2022/Model/Model runs"

#Define where to write graphs & tables out to
outputfolder <- "X:/HAR_PR/PR/NI_MUP_2022/Report/Report Fig + Tabs"

#Define duty rates, assuming cider ABV of 4.5% and wine ABV of 12.5%
#Rates from: https://www.gov.uk/government/publications/rates-and-allowance-excise-duty-alcohol-duty/alcohol-duty-rates-from-24-march-2014
DutyRates <- data.frame(Product=c("Beer", "Wine", "Cider", "Spirits", "RTDs"),
                        DutyPerUnit=c(0.1908, 2.9757/12.5, 0.2874, 0.4038/4.5, 0.2874))

VATRate <- 0.2

#Read in baseline consumption data
Baseline_cons <- read_excel(paste0(folder, "/Extreme scenario/SAPM3_P2C_Results.xlsx"), 
                            range="B2:BV34") %>% 
  #keep only the subgroups we are interested in
  select(c(`...1`, "Population", "Income-1Quintile", "Income-2Quintile",
           "Income-1Quintile-Mod", "Income-2Quintile-Mod", "Income-1Quintile-Haz",
           "Income-2Quintile-Haz", "Income-1Quintile-Harm", "Income-2Quintile-Harm",
           "Mod", "Haz", "Harm"))

##################################################################
#Consumption & Price#
#####################

#Baseline consumption summary
BaseConsSum <- Baseline_cons %>% 
  slice(c(3:5, 11:20, 23:32)) %>% 
  mutate(grouper=c(1,2,3, rep(c(4,5), each=10))) %>% 
  group_by(grouper) %>% 
  summarise(across(.cols=c(2:13), sum), .groups="drop") %>% 
  gather(Group, Value, c(2:13)) %>% 
  spread(grouper, Value) %>% 
  set_names("Group", "Pop", "Weekmean", "DrinkPop", "YearCons", "YearSpend") %>% 
  mutate(AbsRate=1-DrinkPop/Pop,
         DrinkProp=DrinkPop/max(DrinkPop))
  
#Table 1
BaseConsSum %>% 
  filter(Group %in% c("Population", "Mod", "Haz", "Harm", "Income-1Quintile", 
                      "Income-2Quintile")) %>% 
  select(Group, AbsRate, DrinkPop, DrinkProp, YearCons, YearSpend) %>% 
  mutate(Group=case_when(Group=="Population" ~ "All Drinkers",
                         Group=="Mod" ~ "Moderate",
                         Group=="Haz" ~ "Increasing Risk",
                         Group=="Harm" ~ "Higher Risk",
                         Group=="Income-1Quintile" ~ "In Poverty",
                         TRUE ~ "Not In Poverty"),
         Group=factor(Group, levels=c("All Drinkers", "Moderate", 
                                      "Increasing Risk", "Higher Risk", 
                                      "In Poverty", 
                                      "Not In Poverty"))) %>% 
  arrange(Group) %>% 
  #gather(Metric, Value, c(2:5)) %>% 
  gt(rowname_col="Group") %>% 
  tab_row_group("Income Group", rows=c(5,6)) %>% 
  tab_row_group("Drinker Group", rows=c(2:4)) %>% 
  tab_row_group("", rows=1) %>% 
  cols_label(DrinkPop="Drinker population", DrinkProp="Proportion of all drinkers",
             YearCons="Average annual consumption (units)", 
             YearSpend="Average annual spend on alcohol",
             AbsRate="Abstention Rate") %>% 
  fmt_currency(columns=YearSpend, currency="GBP", decimals=0) %>% 
  fmt_number(columns=c(DrinkPop, YearCons), decimals=0) %>% 
  fmt_percent(columns=c(DrinkProp, AbsRate), decimals=1) %>% 
  cols_width(everything() ~ px(120)) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table5-BaselineConsSpendSummary.png"))
  
  #Table 2 Mean prices paid
BasePriceSum <- Baseline_cons %>% 
  slice(c(11:20, 23:32)) %>% 
  mutate(Metric=c(rep(c("Cons", "Spend"), each=10)),
         Channel=if_else(substr(`...1`, 1, 2)=="On", "On-trade", "Off-trade"),
         Drink=word(`...1`, -1)) %>% 
  select(c(`...1`, "Population", "Mod", "Haz", "Harm", "Income-1Quintile", 
                      "Income-2Quintile", "Metric", "Channel", "Drink"))

BasePriceSum %>% 
  group_by(Metric) %>% 
  summarise(across(.cols=c(2:7), sum), .groups="drop") %>% 
  gather(Group, Value, c(2:7)) %>% 
  spread(Metric, Value) %>% 
  mutate(Category="All") %>% 
  bind_rows(
    BasePriceSum %>% 
      group_by(Metric, Channel) %>% 
      summarise(across(.cols=c(2:7), sum), .groups="drop") %>% 
      gather(Group, Value, c(3:8)) %>% 
      spread(Metric, Value) %>% 
      rename("Category"="Channel")) %>% 
  bind_rows(
    BasePriceSum %>% 
      group_by(Metric, Drink) %>% 
      summarise(across(.cols=c(2:7), sum), .groups="drop") %>% 
      gather(Group, Value, c(3:8)) %>% 
      spread(Metric, Value) %>% 
      rename("Category"="Drink")) %>% 
  mutate(MeanPrice=Spend/Cons) %>% 
  select(-c(Cons, Spend)) %>% 
  spread(Category, MeanPrice) %>% 
  mutate(Group=case_when(Group=="Population" ~ "All Drinkers",
                         Group=="Mod" ~ "Moderate",
                         Group=="Haz" ~ "Increasing Risk",
                         Group=="Harm" ~ "Higher Risk",
                         Group=="Income-1Quintile" ~ "In Poverty",
                         TRUE ~ "Not In Poverty"),
         Group=factor(Group, levels=c("All Drinkers", "Moderate", 
                                      "Increasing Risk", "Higher Risk", 
                                      "In Poverty", 
                                      "Not In Poverty"))) %>% 
  arrange(Group) %>% 
  gt(rowname_col="Group") %>% 
  tab_row_group("Income Group", rows=c(5,6)) %>% 
  tab_row_group("Drinker Group", rows=c(2:4)) %>% 
  tab_row_group("", rows=1) %>% 
  tab_spanner(label="Channel", columns=c("On-trade", "Off-trade")) %>% 
  tab_spanner(label="Drink type", columns=c("beer", "cider", "wine", "spirits", "RTDs")) %>% 
  cols_label(beer="Beer", cider="Cider", wine="Wine", spirits="Spirits") %>% 
  fmt_currency(columns=c(2:9), currency="GBP", decimals=2)%>% 
  cols_width(-c(Group) ~ px(80)) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table6-BaselineMeanPrices.png"))
  
#Graph of pop, cons and spending props
agg_png(paste0(outputfolder, "/Fig2-BaselinePopConsSpendProps.png"), units="in", width=8, height=6, res=600)
Baseline_cons %>% 
  slice(n=c(3,5,11:20, 23:32)) %>%
  select(`...1`, "Mod", "Harm", "Haz") %>% 
  mutate(grouper=c("Pop","DrinkPop",rep(c("Cons","Spend"), each=10))) %>% 
  group_by(grouper) %>% 
  summarise(across(c(2:4), sum), .groups="drop") %>% 
  mutate(Abs=c(0, Mod[grouper=="Pop"]-Mod[grouper=="DrinkPop"],0,0)) %>% 
  filter(grouper!="Pop") %>% 
  gather(Group, Value, c(2:5)) %>% 
  spread(grouper, Value) %>% 
  mutate(Cons=Cons*DrinkPop, Spend=Spend*DrinkPop) %>% 
  gather(Metric, Value, c(2:4)) %>% 
  mutate(Group=factor(Group, levels=c("Harm", "Haz", "Mod", "Abs")),
         Metric=factor(Metric, levels=c("DrinkPop", "Cons", "Spend"))) %>%
  group_by(Metric) %>% 
  mutate(total=sum(Value)) %>% 
  ungroup() %>% 
  mutate(Prop=Value/total,
         labels=if_else(Prop==0, "", paste0(round(Prop*100, 0), "%"))) %>% 
  ggplot(aes(x=Metric, y=Prop, fill=Group))+
  geom_col(position="fill")+
  geom_text(aes(label=labels, colour=Group), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_x_discrete(name="", labels=c("Population", "Consumption", "Spending"))+
  scale_y_continuous(name="")+
  scale_fill_manual(values=c(rev(drinkerpal), "#0070c0"),
                    name="", labels=c("Higher risk", "Increasing risk",
                                      "Moderate", "Abstainers"))+
  scale_colour_manual(values=c("White", "Black", "Black", "White"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_blank())

dev.off()

#Graph of consumption by beverage type
agg_png(paste0(outputfolder, "/Fig3-BaselineConsxDrink.png"), units="in", width=8, height=6, res=600)
Baseline_cons %>% 
  slice(n=c(11:20)) %>%
  select(`...1`, "Mod", "Harm", "Haz", "Income-1Quintile", 
         "Income-2Quintile") %>% 
  mutate(Drink=word(`...1`, -1),
         Drink=if_else(Drink=="RTDs", "RTDs", to_upper_camel_case(Drink))) %>% 
  gather(Group, Cons, c(2:6)) %>% 
  group_by(Group, Drink) %>% 
  summarise(Cons=sum(Cons), .groups="drop") %>% 
  group_by(Group) %>% 
  mutate(total=sum(Cons), Prop=Cons/total,
         labels=if_else(Prop<0.03, "", paste0(round(Prop*100, 0), "%"))) %>% 
  ungroup() %>% 
  mutate(Group=case_when(Group=="Mod" ~ "Moderate",
                         Group=="Haz" ~ "Increasing Risk",
                         Group=="Harm" ~ "Higher Risk",
                         Group=="Income-1Quintile" ~ "In Poverty",
                         TRUE ~ "Not In Poverty"),
         Group=factor(Group, levels=c("Moderate", "Increasing Risk", 
                                      "Higher Risk", "In Poverty", 
                                      "Not In Poverty")),
         Drink=factor(Drink, levels=c("Beer", "Cider", "Wine", "Spirits", "RTDs"))) %>% 
  ggplot(aes(x=Group, y=Prop, fill=Drink))+
  geom_col()+
  geom_text(aes(label=labels, colour=Drink), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_fill_manual(values=drinkpal, name="")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Proportion of alcohol consumption")+
  scale_colour_manual(values=c("Black", "Black", "White", "Black", "White"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_blank())

dev.off()

#Graph of consumption by channel
agg_png(paste0(outputfolder, "/Fig4-BaselineConsxChannel.png"), units="in", width=8, height=6, res=600)
Baseline_cons %>% 
  slice(n=c(11:20)) %>%
  select(`...1`, "Mod", "Harm", "Haz", "Income-1Quintile", 
         "Income-2Quintile") %>% 
  mutate(Channel=if_else(substr(`...1`,1,2)=="On", "On-trade", "Off-trade")) %>% 
  gather(Group, Cons, c(2:6)) %>% 
  group_by(Group, Channel) %>% 
  summarise(Cons=sum(Cons), .groups="drop") %>% 
  group_by(Group) %>% 
  mutate(total=sum(Cons), Prop=Cons/total,
         labels=if_else(Prop<0.03, "", paste0(round(Prop*100, 0), "%"))) %>% 
  ungroup() %>% 
  mutate(Group=case_when(Group=="Mod" ~ "Moderate",
                         Group=="Haz" ~ "Increasing Risk",
                         Group=="Harm" ~ "Higher Risk",
                         Group=="Income-1Quintile" ~ "In Poverty",
                         TRUE ~ "Not In Poverty"),
         Group=factor(Group, levels=c("Moderate", "Increasing Risk", 
                                      "Higher Risk", "In Poverty", 
                                      "Not In Poverty")),
         Channel=factor(Channel, levels=c("On-trade", "Off-trade"))) %>% 
  ggplot(aes(x=Group, y=Prop, fill=Channel))+
  geom_col()+
  geom_text(aes(label=labels, colour=Channel), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_fill_manual(values=channelpal, name="")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Proportion of alcohol consumption")+
  scale_colour_manual(values=c("White", "Black"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(),
        axis.text.y=element_blank())

dev.off()

####################################################################
#P2C outcomes

#Read in results from model runs for all policies
for (i in 1:length(Policies)) {

assign(paste0("p_", i), read_excel(paste0(folder, "/", Policies[i], "/SAPM3_P2C_Results.xlsx"), 
                            range="B45:BV66", col_names=FALSE) %>%
  select(c(1,2,51,52,56,57,61,62,66,67,71:73)) %>% 
  set_names(colnames(Baseline_cons)) %>% 
  filter(!is.na(Population)) %>% 
  mutate(Metric=rep(c("Cons", "Spend"), each=10)) %>% 
  group_by(Metric) %>% 
  summarise(across(.cols=c(2:13), sum), .groups="drop") %>% 
  gather(Group, Value, c(2:13)) %>% 
  spread(Metric, Value) %>% 
  mutate(Policy=Policies[i]))
}

P2Cresults <- bind_rows(p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10,
                        p_11, p_12, p_13, p_14, p_15, p_16, p_17, p_18,
                        p_19, p_20)

#Consumption
#Table of absolute impacts
top_cons <- BaseConsSum %>% 
  select(Group, DrinkPop, YearCons) %>% 
  set_names("Group", "Drinker population", "Mean consumption (units/drinker/year)") %>% 
  gather(Policy, Value, c(2,3)) %>% 
  spread(Group, Value) %>% 
  mutate(across(.cols=c(2:13), ~round(.x, 0)))

bind_rows(top_cons, P2Cresults %>% 
  select(-Spend) %>% 
  rename("Value"="Cons") %>% 
  spread(Group, Value)) %>% 
  select(Policy, Population, Mod, Haz, Harm, `Income-1Quintile`, 
         `Income-2Quintile`) %>% 
  gt(rowname_col="Policy") %>% 
  fmt_number(columns=everything(), rows=c(3:22), decimals=1) %>% 
  fmt_number(columns=everything(), rows=c(1,2), decimals=0) %>% 
  tab_row_group("Policy impact", rows=c(3:22)) %>% 
  tab_row_group("Baseline", rows=c(1,2)) %>% 
  tab_spanner(label="Drinker group", columns=c("Mod", "Haz", "Harm")) %>% 
  tab_spanner(label="Income group", columns=c("Income-1Quintile", 
                                              "Income-2Quintile")) %>% 
  cols_label(Population="All Drinkers", Mod="Moderate", Haz="Increasing Risk", 
             Harm="Higher Risk", `Income-1Quintile`="In Poverty",
             `Income-2Quintile`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table13-ConsImpactAbs.png"))
  
#Table of relative impacts
bind_rows(top_cons, P2Cresults %>% 
            select(-Spend) %>% 
            rename("Value"="Cons") %>% 
            spread(Group, Value)) %>% 
  mutate(across(.cols=c(2:13), ~if_else(!Policy %in% c("Drinker population", 
                                                       "Mean consumption (units/drinker/year)"),
                                        .x/.x[Policy=="Mean consumption (units/drinker/year)"],
                                        .x))) %>% 
  select(Policy, Population, Mod, Haz, Harm, `Income-1Quintile`, 
         `Income-2Quintile`) %>% 
  gt(rowname_col="Policy") %>% 
  fmt_percent(columns=everything(), rows=c(3:22), decimals=1) %>% 
  fmt_number(columns=everything(), rows=c(1,2), decimals=0) %>% 
  tab_row_group("Policy impact", rows=c(3:22)) %>% 
  tab_row_group("Baseline", rows=c(1,2)) %>% 
  tab_spanner(label="Drinker group", columns=c("Mod", "Haz", "Harm")) %>% 
  tab_spanner(label="Income group", columns=c("Income-1Quintile", 
                                              "Income-2Quintile")) %>% 
  cols_label(Population="All Drinkers", Mod="Moderate", Haz="Increasing Risk", 
             Harm="Higher Risk", `Income-1Quintile`="In Poverty",
             `Income-2Quintile`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table14-ConsImpactRel.png"))

#Graph of absolute impacts
agg_png(paste0(outputfolder, "/Fig7-ConsImpactxDrinker.png"), units="in", width=8, height=6, res=600)
P2Cresults %>% 
  filter(Group %in% c("Mod", "Haz", "Harm")) %>% 
  mutate(Group=factor(Group, levels=c("Mod", "Haz", "Harm"))) %>% 
  ggplot(aes(x=Cons, y=Policy, fill=Group))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="Grey20")+
  scale_x_continuous(name="Change in mean consumption\n(units per drinker per year)")+
  scale_y_discrete(name="", position="right")+  
  scale_fill_manual(values=drinkerpal, name="", labels=c("Moderate",
                                                         "Increasing Risk",
                                                         "Higher Risk"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")
  
dev.off()

#Table of abs and rel impacts by drinker and income group
bind_rows(top_cons, P2Cresults %>% 
            select(-Spend) %>% 
            rename("Value"="Cons") %>% 
            spread(Group, Value)) %>% 
  select(Policy, `Income-1Quintile-Mod`, `Income-1Quintile-Haz`,
         `Income-1Quintile-Harm`, `Income-2Quintile-Mod`, 
         `Income-2Quintile-Haz`, `Income-2Quintile-Harm`) %>% 
  mutate(across(.cols=c(2:7), 
                ~if_else(!Policy %in% c("Drinker population", 
                                        "Mean consumption (units/drinker/year)"),
                         paste0(round(.x, digits=1), " (",
                               round(100*.x/.x[Policy=="Mean consumption (units/drinker/year)"],
                                     1), "%)"), as.character(.x)))) %>% 
  gt(rowname_col="Policy") %>% 
  tab_row_group("Policy impact (relative change)", rows=c(3:22)) %>% 
  tab_row_group("Baseline", rows=c(1,2)) %>% 
  tab_spanner(label="Moderate", columns=c("Income-1Quintile-Mod", 
                                          "Income-2Quintile-Mod")) %>% 
  tab_spanner(label="Increasing Risk", columns=c("Income-1Quintile-Haz", 
                                          "Income-2Quintile-Haz")) %>% 
  tab_spanner(label="Higher Risk", columns=c("Income-1Quintile-Harm", 
                                          "Income-2Quintile-Harm")) %>% 
  cols_label(`Income-1Quintile-Mod`="In Poverty", `Income-1Quintile-Haz`= "In Poverty",
             `Income-1Quintile-Harm`="In Poverty", `Income-2Quintile-Mod`= "Not In Poverty", 
             `Income-2Quintile-Haz`= "Not In Poverty", `Income-2Quintile-Harm`= "Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(Policy) ~ px(150)) %>% 
  cols_width(Policy ~ px(200)) %>% 
  gtsave(paste0(outputfolder, "/Table15-ConsImpactxDrinkerxInc.png"), vwidth=1600)

#Graph of absolute impacts by income group
agg_png(paste0(outputfolder, "/Fig8-ConsImpactxInc.png"), units="in", width=8, height=6, res=600)
P2Cresults %>% 
  filter(Group %in% c("Income-1Quintile", "Income-2Quintile")) %>% 
  ggplot(aes(x=Cons, y=Policy, fill=Group))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="Grey20")+
  scale_x_continuous(name="Change in mean consumption\n(units per drinker per year)")+
  scale_y_discrete(name="", position="right")+  
  scale_fill_manual(values=incomepal, name="", labels=c("In Poverty",
                                                         "Not In Poverty"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

#Repeat consumption outputs for spending
top_spend <- BaseConsSum %>% 
  select(Group, DrinkPop, YearSpend) %>% 
  set_names("Group", "Drinker population", "Mean annual spend") %>% 
  gather(Policy, Value, c(2,3)) %>% 
  spread(Group, Value) %>% 
  mutate(across(.cols=c(2:13), ~round(.x, 0)))

bind_rows(top_spend, P2Cresults %>% 
            select(-Cons) %>% 
            rename("Value"="Spend") %>% 
            spread(Group, Value)) %>% 
  select(Policy, Population, Mod, Haz, Harm, `Income-1Quintile`, 
         `Income-2Quintile`) %>% 
  gt(rowname_col="Policy") %>% 
  fmt_currency(columns=everything(), rows=c(3:22), currency="GBP", decimals=1) %>% 
  fmt_currency(columns=everything(), rows=2, currency="GBP", decimals=0) %>% 
  fmt_number(columns=everything(), rows=c(1), decimals=0) %>% 
  tab_row_group("Policy impact", rows=c(3:22)) %>% 
  tab_row_group("Baseline", rows=c(1,2)) %>% 
  tab_spanner(label="Drinker group", columns=c("Mod", "Haz", "Harm")) %>% 
  tab_spanner(label="Income group", columns=c("Income-1Quintile", 
                                              "Income-2Quintile")) %>% 
  cols_label(Population="All Drinkers", Mod="Moderate", Haz="Increasing Risk", 
             Harm="Higher Risk", `Income-1Quintile`="In Poverty",
             `Income-2Quintile`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(Policy) ~ px(120)) %>% 
  gtsave(paste0(outputfolder, "/Table16-SpendImpactAbs.png"))

#Table of relative impacts
bind_rows(top_spend, P2Cresults %>% 
            select(-Cons) %>% 
            rename("Value"="Spend") %>% 
            spread(Group, Value)) %>% 
  mutate(across(.cols=c(2:13), ~if_else(!Policy %in% c("Drinker population", 
                                                       "Mean annual spend"),
                                        .x/.x[Policy=="Mean annual spend"],
                                        .x))) %>% 
  select(Policy, Population, Mod, Haz, Harm, `Income-1Quintile`, 
         `Income-2Quintile`) %>% 
  gt(rowname_col="Policy") %>% 
  fmt_percent(columns=everything(), rows=c(3:22), decimals=1) %>% 
  fmt_number(columns=everything(), rows=1, decimals=0) %>% 
  fmt_currency(columns=everything(), rows=2, currency="GBP", decimals=0) %>% 
  tab_row_group("Policy impact", rows=c(3:22)) %>% 
  tab_row_group("Baseline", rows=c(1,2)) %>% 
  tab_spanner(label="Drinker group", columns=c("Mod", "Haz", "Harm")) %>% 
  tab_spanner(label="Income group", columns=c("Income-1Quintile", 
                                              "Income-2Quintile")) %>% 
  cols_label(Population="All Drinkers", Mod="Moderate", Haz="Increasing Risk", 
             Harm="Higher Risk", `Income-1Quintile`="In Poverty",
             `Income-2Quintile`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(Policy) ~ px(120)) %>% 
  gtsave(paste0(outputfolder, "/Table17-SpendImpactRel.png"))

#Graph of absolute impacts
agg_png(paste0(outputfolder, "/Fig11-SpendImpactxDrinker.png"), units="in", width=8, height=6, res=600)
P2Cresults %>% 
  filter(Group %in% c("Mod", "Haz", "Harm")) %>% 
  mutate(Group=factor(Group, levels=c("Mod", "Haz", "Harm"))) %>% 
  ggplot(aes(x=Spend, y=Policy, fill=Group))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="Grey20")+
  scale_x_continuous(name="Change in mean annual spending on alcohol",
                     labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="")+  
  scale_fill_manual(values=drinkerpal, name="", labels=c("Moderate",
                                                         "Increasing Risk",
                                                         "Higher Risk"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

#Table of abs and rel impacts by drinker and income group
bind_rows(top_spend, P2Cresults %>% 
            select(-Cons) %>% 
            rename("Value"="Spend") %>% 
            spread(Group, Value)) %>% 
  select(Policy, `Income-1Quintile-Mod`, `Income-1Quintile-Haz`,
         `Income-1Quintile-Harm`, `Income-2Quintile-Mod`, 
         `Income-2Quintile-Haz`, `Income-2Quintile-Harm`) %>% 
  mutate(across(.cols=c(2:7), 
                ~case_when(
                  !Policy %in% c("Drinker population", 
                                 "Mean annual spend") ~ 
                    paste0("£", round(.x, digits=1), " (", 
                          round(100*.x/.x[Policy=="Mean annual spend"], 1),"%)"),
                  Policy=="Drinker population" ~ as.character(.x),
                  TRUE ~ paste0("£", as.character(.x))))) %>% 
  gt(rowname_col="Policy") %>% 
  tab_row_group("Policy impact (relative change)", rows=c(3:22)) %>% 
  tab_row_group("Baseline", rows=c(1,2)) %>% 
  tab_spanner(label="Moderate", columns=c("Income-1Quintile-Mod", 
                                          "Income-2Quintile-Mod")) %>% 
  tab_spanner(label="Increasing Risk", columns=c("Income-1Quintile-Haz", 
                                                 "Income-2Quintile-Haz")) %>% 
  tab_spanner(label="Higher Risk", columns=c("Income-1Quintile-Harm", 
                                             "Income-2Quintile-Harm")) %>% 
  cols_label(`Income-1Quintile-Mod`="In Poverty", `Income-1Quintile-Haz`= "In Poverty",
             `Income-1Quintile-Harm`="In Poverty", `Income-2Quintile-Mod`= "Not In Poverty", 
             `Income-2Quintile-Haz`= "Not In Poverty", `Income-2Quintile-Harm`= "Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(Policy) ~ px(150)) %>% 
  cols_align(align="right", columns=-c(Policy)) %>% 
  cols_width(Policy ~ px(200)) %>% 
  gtsave(paste0(outputfolder, "/Table18-SpendImpactxDrinkerxInc.png"), vwidth=1600)

#Graph of absolute impacts by income group
agg_png(paste0(outputfolder, "/Fig12-SpendImpactxInc.png"), units="in", width=8, height=6, res=600)
P2Cresults %>% 
  filter(Group %in% c("Income-1Quintile", "Income-2Quintile")) %>% 
  ggplot(aes(x=Spend, y=Policy, fill=Group))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="Grey20")+
  scale_x_continuous(name="Change in annual spending on alcohol",
                     labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="")+  
  scale_fill_manual(values=incomepal, name="", labels=c("In Poverty",
                                                        "Not In Poverty"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

#Proportional consumption reductions by drinker group
agg_png(paste0(outputfolder, "/Fig9-ConsImpactPropxDrinker.png"), units="in", width=8, height=6, res=600)
P2Cresults %>% 
  select(-Spend) %>% 
  merge(top_cons %>% 
  gather(Group, Pop, c(2:13)) %>% 
  filter(Policy=="Drinker population") %>% 
  select(-Policy)) %>% 
  filter(Group %in% c("Mod", "Haz", "Harm")) %>% 
  mutate(AlcVol=Cons*Pop, 
         Group=factor(Group, levels=c("Mod", "Haz", "Harm"))) %>% 
  group_by(Policy) %>% 
  mutate(TotalChange=sum(AlcVol)) %>% 
  ungroup() %>% 
  mutate(ChangeProp=AlcVol/TotalChange) %>% 
  ggplot(aes(x=ChangeProp, y=Policy, fill=Group))+
  geom_col(position="stack")+
  geom_text(aes(label=if_else(ChangeProp<0.03, "", 
                              paste0(round(ChangeProp*100, 0), "%")), colour=Group), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_x_continuous(name="Proportion of total units removed")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=drinkerpal, name="", 
                    labels=c("Moderate", "Increasing risk", "Higher risk"))+
  scale_colour_manual(values=c("Black", "Black", "White"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

dev.off()

#Proportional consumption reductions by income group
agg_png(paste0(outputfolder, "/Fig10-ConsImpactPropxInc.png"), units="in", width=8, height=6, res=600)
P2Cresults %>% 
  select(-Spend) %>% 
  merge(top_cons %>% 
          gather(Group, Pop, c(2:13)) %>% 
          filter(Policy=="Drinker population") %>% 
          select(-Policy)) %>% 
  filter(Group %in% c("Income-1Quintile", "Income-2Quintile")) %>% 
  mutate(AlcVol=Cons*Pop, 
         Group=factor(Group, levels=c("Income-1Quintile", "Income-2Quintile"))) %>% 
  group_by(Policy) %>% 
  mutate(TotalChange=sum(AlcVol)) %>% 
  ungroup() %>% 
  mutate(ChangeProp=AlcVol/TotalChange) %>% 
  ggplot(aes(x=ChangeProp, y=Policy, fill=Group))+
  geom_col(position="stack")+
  geom_text(aes(label=if_else(ChangeProp<0.03, "", 
                              paste0(round(ChangeProp*100, 0), "%"))), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5), colour="Black")+
  scale_x_continuous(name="Proportion of total units removed")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=incomepal, name="", 
                    labels=c("In Poverty", "Not In Poverty"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

dev.off()

##################################################################
#Exchequer and retailer revenue impacts#
########################################

#Read in spending figures for revenue calculations
for (i in 1:length(Policies)) {
  
  assign(paste0("p_s_", i), read_excel(paste0(folder, "/", Policies[i], "/SAPM3_P2C_Results.xlsx"), 
                                     range="B57:C66", col_names=FALSE) %>%
           mutate(Policy=Policies[i]))
}

#Read in associated consumption volumes for revenue calculations
for (i in 1:length(Policies)) {
  
  assign(paste0("p_c_", i), read_excel(paste0(folder, "/", Policies[i], "/SAPM3_P2C_Results.xlsx"), 
                                       range="B45:C54", col_names=FALSE) %>%
           mutate(Policy=Policies[i]))
}

Revenue <- bind_rows(p_s_1, p_s_2, p_s_3, p_s_4, p_s_5, p_s_6, p_s_7, 
                        p_s_8, p_s_9, p_s_10, p_s_11, p_s_12, p_s_13, 
                        p_s_14, p_s_15, p_s_16, p_s_17, p_s_18, p_s_19, 
                        p_s_20) %>% 
  bind_rows(read_excel(paste0(folder, "/Extreme scenario/SAPM3_P2C_Results.xlsx"),
                       range="B25:C34", col_names=FALSE) %>% 
              mutate(Policy="Baseline")) %>% 
  set_names("DrinkCat", "AnnualSpend", "Policy") %>% 
  merge(bind_rows(p_c_1, p_c_2, p_c_3, p_c_4, p_c_5, p_c_6, p_c_7, 
                  p_c_8, p_c_9, p_c_10, p_c_11, p_c_12, p_c_13, 
                  p_c_14, p_c_15, p_c_16, p_c_17, p_c_18, p_c_19, 
                  p_c_20) %>% 
          bind_rows(read_excel(paste0(folder, "/Extreme scenario/SAPM3_P2C_Results.xlsx"),
                               range="B13:C22", col_names=FALSE) %>% 
                      mutate(Policy="Baseline")) %>% 
          set_names("DrinkCat", "AnnualUnits", "Policy")) %>% 
  mutate(channel=if_else(substr(DrinkCat, 1, 2)=="On", "On-trade", "Off-trade"),
         Product=word(DrinkCat, -1),
         Product=if_else(Product=="RTDs", "RTDs", to_upper_camel_case(Product))) %>% 
  merge(DutyRates) %>%
  #Scale numbers up to whole population level
  mutate(AnnualUnits=AnnualUnits*BaseConsSum$Pop[BaseConsSum$Group=="Population"],
         AnnualSpend=AnnualSpend*BaseConsSum$Pop[BaseConsSum$Group=="Population"],
         DutyPaid=AnnualUnits*DutyPerUnit,
         VATPaid=AnnualSpend*VATRate,
         TaxPaid=DutyPaid+VATPaid,
         RetailerRevenue=AnnualSpend-TaxPaid)

RevenueSum <- Revenue %>% 
  group_by(channel, Policy) %>% 
  summarise(TaxPaid=sum(TaxPaid), RetailerRevenue=sum(RetailerRevenue), .groups="drop") %>% 
  bind_rows(Revenue %>% 
              group_by(Policy) %>% 
              summarise(TaxPaid=sum(TaxPaid), RetailerRevenue=sum(RetailerRevenue), .groups="drop") %>% 
              mutate(channel="Total")) 

#Table of revenue impacts
RevenueSum %>% 
  pivot_wider(names_from=channel, values_from=c(TaxPaid, RetailerRevenue)) %>% 
  mutate(across(.cols=c(2:7), 
                ~case_when(
                  Policy != "Baseline" ~ paste0("£", round(.x/1000000, digits=1), "m (", 
                           round(100*.x/.x[Policy=="Baseline"], 1),"%)"),
                  TRUE ~ paste0("£", round(.x/1000000, digits=1), "m"))),
         Policy=if_else(Policy=="Baseline", "Annual receipts", Policy)) %>% 
  gt(rowname_col="Policy") %>% 
  tab_row_group("Policy impact (relative change)", rows=c(1:10, 12:21)) %>% 
  tab_row_group("Baseline", rows=c(11)) %>% 
  tab_spanner(label="Change in tax revenue to government (duty + VAT)", 
              columns=c("TaxPaid_Off-trade", "TaxPaid_On-trade",
                        "TaxPaid_Total")) %>% 
  tab_spanner(label="Change in retailer revenue (after accounting for tax)", 
              columns=c("RetailerRevenue_Off-trade", "RetailerRevenue_On-trade",
                        "RetailerRevenue_Total")) %>% 
  cols_label(`TaxPaid_Off-trade`="Off-trade", `TaxPaid_On-trade`="On-trade",
             `TaxPaid_Total`="Total", `RetailerRevenue_Off-trade`="Off-trade",
             `RetailerRevenue_On-trade`="On-trade", `RetailerRevenue_Total`="Total") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(Policy) ~ px(150)) %>% 
  cols_width(Policy ~ px(200)) %>% 
  gtsave(paste0(outputfolder, "/Table19-RevImpact.png"), vwidth=1600)

#Graph of revenue impacts on the exchequer
agg_png(paste0(outputfolder, "/Fig13-ExchImpact.png"), units="in", width=8, height=6, res=600)
ggplot()+
  geom_vline(xintercept=0, colour="Grey20")+
  geom_col(data=RevenueSum %>% filter(channel!="Total" & Policy!="Baseline"), 
           aes(x=TaxPaid/1000000, y=Policy, fill=channel), position="dodge")+
  geom_point(data=RevenueSum %>% filter(channel=="Total" & Policy!="Baseline"),
             aes(x=TaxPaid/1000000, y=Policy, colour=channel), fill="Yellow", shape=23)+
  scale_x_continuous(name="Change in tax revenue", 
                     labels=label_dollar(prefix="£", suffix="m"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=channelpal, name="")+
  scale_colour_manual(values="Red", name="", labels="Net change")+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

#Graph of revenue impacts on retailers
agg_png(paste0(outputfolder, "/Fig14-RetailImpact.png"), units="in", width=8, height=6, res=600)
ggplot()+
  geom_vline(xintercept=0, colour="Grey20")+
  geom_col(data=RevenueSum %>% filter(channel!="Total" & Policy!="Baseline"), 
           aes(x=RetailerRevenue/1000000, y=Policy, fill=channel), position="dodge")+
  geom_point(data=RevenueSum %>% filter(channel=="Total" & Policy!="Baseline"),
             aes(x=RetailerRevenue/1000000, y=Policy, colour=channel), fill="Yellow", shape=23)+
  scale_x_continuous(name="Change in retailer revenue", 
                     labels=label_dollar(prefix="£", suffix="m"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=channelpal, name="")+
  scale_colour_manual(values="Red", name="", labels="Net change")+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

###################################################################
#Health outcomes#
#################

#Baseline health summary
HealthPop <- BaseConsSum %>% 
  select(Group, Pop) %>% 
  mutate(Group=case_when(
    Group=="Mod" ~ "Moderate", Group=="Haz" ~ "Hazardous", Group=="Harm" ~ "Harmful",
    Group=="Income-1Quintile" ~ "IMDQ1 (least deprived)",
    Group=="Income-2Quintile" ~ "IMDQ2",  Group=="Income-1Quintile-Mod" ~ "IMDQ1 Mod",
    Group=="Income-1Quintile-Mod" ~ "IMDQ1 Mod", Group=="Income-1Quintile-Haz" ~ "IMDQ1 Haz",
    Group=="Income-1Quintile-Harm" ~ "IMDQ1 Harm", Group=="Income-2Quintile-Mod" ~ "IMDQ2 Mod",
    Group=="Income-2Quintile-Haz" ~ "IMDQ2 Haz", Group=="Income-2Quintile-Harm" ~ "IMDQ2 Harm",
    TRUE ~ "Population"))

Baseline_health <- read_excel(paste0(folder, "/Extreme scenario/SAPM3_C2HHealth_Results.xlsx"), 
                            sheet="Summary by subgroups", range="B3:W29") %>% 
  #keep only the subgroups we are interested in
  select(c(`...1`, "Population", "Moderate", "Hazardous", "Harmful",
           "IMDQ1 (least deprived)", "IMDQ2", "IMDQ1 Mod", "IMDQ2 Mod",
           "IMDQ1 Haz", "IMDQ2 Haz", "IMDQ1 Harm", "IMDQ2 Harm")) %>% 
  mutate(Metric=substr(`...1`, 7,8),
         Metric=case_when(
           Metric=="De" ~ "Deaths", Metric=="Si" ~ "Prevalence",
           Metric=="Ad" ~ "Admissions", Metric=="QA" ~ "QALYs",
           TRUE ~ "NHS Costs")) %>% 
  group_by(Metric) %>% 
  summarise(across(.cols=c(2:13), sum), .groups="drop") %>% 
  gather(Group, Value, c(2:13)) %>% 
  spread(Metric, Value) %>% 
  merge(HealthPop)

#Table of baseline health harms
Baseline_health %>% 
  filter(Group %in% c("Population", "Moderate", "Hazardous", "Harmful",
                      "IMDQ1 (least deprived)", "IMDQ2")) %>% 
  select(Group, Admissions, Deaths, `NHS Costs`, Pop) %>% 
  mutate(across(.cols=c(2:4), ~-.x),
         `NHS Costs`=`NHS Costs`/1000000,
         `Admissions per 100,000 drinkers`=Admissions*100000/Pop,
         `Deaths per 100,000 drinkers`=Deaths*100000/Pop, 
         `NHS Costs per 100,000 drinkers`=`NHS Costs`*100000/Pop) %>% 
  select(-Pop) %>% 
  gather(Metric, Value, c(2:7)) %>% 
  spread(Group, Value) %>% 
  gt(rowname_col="Metric") %>%
  cols_move_to_start(columns=c(Population, Moderate, Hazardous, Harmful)) %>% 
  fmt_currency(columns=everything(), rows=c(5,6), currency="GBP", decimals=1) %>% 
  fmt_number(columns=everything(), rows=c(1:4), decimals=0) %>% 
  tab_row_group("Annual NHS Costs Due To Alcohol (£millions)", rows=c(5,6)) %>% 
  tab_row_group("Annual Alcohol-Attributable Hospital Admissions", rows=c(1,2)) %>% 
  tab_row_group("Annual Alcohol-Attributable Deaths", rows=c(3,4)) %>% 
  tab_spanner(label="Income group", columns=c("IMDQ1 (least deprived)", 
                                              "IMDQ2")) %>% 
  tab_spanner(label="Drinker group", columns=c("Moderate", "Hazardous", "Harmful")) %>% 
  cols_label(Population="All Drinkers", `IMDQ1 (least deprived)`="In Poverty",
             IMDQ2="Not In Poverty", Hazardous="Increasing Risk",
             Harmful="Higher Risk") %>% 
  tab_footnote(
    footnote = "Note that negative numbers reflect the fact that in SAPM alcohol consumption in moderate drinkers is estimated to be overall protective, i.e. there are fewer deaths and hospital admissions than if nobody drank. This arises from conservative assumptions in the model, although the evidence remains uncertain whether the effect is genuine. See the discussion section for further details",
    locations = cells_body(columns = Moderate), placement="right") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(Metric) ~ px(120)) %>% 
  gtsave(paste0(outputfolder, "/Table7-BaselineHarms.png"))

#Graph of deaths attributable to alcohol
agg_png(paste0(outputfolder, "/Fig5-BaselineDeathsxDrinkerxInc.png"), units="in", width=8, height=6, res=600)
Baseline_health %>% 
  filter(Group %in% c("IMDQ1 Mod", "IMDQ2 Mod", "IMDQ1 Haz", "IMDQ2 Haz", 
                      "IMDQ1 Harm", "IMDQ2 Harm")) %>% 
  select(Group, Deaths, Pop) %>% 
  mutate(DeathRate=-Deaths*100000/Pop,
         Group=factor(Group, levels=c("IMDQ1 Mod", "IMDQ2 Mod", "IMDQ1 Haz", "IMDQ2 Haz", 
                                      "IMDQ1 Harm", "IMDQ2 Harm"))) %>%  
  ggplot(aes(x=Group, y=DeathRate, fill=Group))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_col(show.legend=FALSE)+
  scale_y_continuous(name="Annual alcohol-attributable deaths\nper 100,000 drinkers")+
  scale_x_discrete(name="", labels=c("Moderate\nIn Poverty", "Moderate\nNot In Poverty",
                                     "Increasing Risk\nIn Poverty", "Increasing Risk\nNot In Poverty",
                                     "Higher Risk\nIn Poverty", "Higher Risk\nNot In Poverty"))+
  scale_fill_manual(values=drinkincpal)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey90"), axis.line.x=element_blank())

dev.off()

#Baseline health harms by condition
Baseline_health_cond <- read_excel(paste0(folder, "/Extreme scenario/SAPM3_C2HHealth_Results.xlsx"), 
                              sheet="Summary by health conditions", range="B4:I48", col_names=FALSE) %>% 
  select(c(`...1`, `...2`, `...8`)) %>% 
  set_names("ConditionNo", "Deaths", "Admissions") %>% 
  mutate(ConditionNo=as.numeric(substr(ConditionNo, 18,19))) %>% 
  merge(read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Health/ConditionLookup.xlsx",
                              range="A1:C46")) %>% 
  group_by(ConditionGroup) %>% 
  summarise(Deaths=-sum(Deaths), Admissions=-sum(Admissions), .groups="drop") %>% 
  mutate(ConditionGroup=factor(ConditionGroup, levels=c("Liver disease", "Mental and behavioural disorders due to use of alcohol", 
                                                        "Alcohol poisoning", "Other wholly alcohol-attributable conditions",
                                                        "Cancers", "Hypertension", "Stroke", "Other cardiovascular disease",
                                                        "Diabetes", "Other chronic conditions", "Road traffic accidents",
                                                        "Falls", "Other injuries"))) %>% 
  arrange(ConditionGroup)

#Table
Baseline_health_cond %>% 
  gt(rowname_col="ConditionGroup") %>% 
  tab_spanner(label="Annual alcohol-attributable", columns=c(2,3)) %>% 
  tab_row_group("Injuries", rows=c(11:13)) %>% 
  tab_row_group("Chronic conditions", rows=c(5:10)) %>% 
  tab_row_group("Wholly alcohol-attributable", rows=c(1:4)) %>% 
  fmt_number(columns=c(2,3), decimals=0) %>% 
  summary_rows(columns=c(2,3), fns=list(Total="sum"), 
               formatter=fmt_number, decimals=0) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(ConditionGroup) ~ px(120)) %>% 
  gtsave(paste0(outputfolder, "/Table8-BaselineHarmsxCondition.png"))

#Graph
agg_png(paste0(outputfolder, "/Fig6-BaselineHarmPropxCondition.png"), units="in", width=8, height=6, res=600)
Baseline_health_cond %>% 
  filter(!ConditionGroup %in% c("Diabetes", "Other cardiovascular disease")) %>% 
  gather(Outcome, Value, c(2,3)) %>% 
  mutate(Outcome=factor(Outcome, levels=c("Deaths", "Admissions"))) %>% 
  ggplot(aes(x=Outcome, y=Value, fill=ConditionGroup))+
  geom_col(position="fill")+
  scale_x_discrete(name="")+
  scale_y_continuous(labels=label_percent(accuracy=1), name="Proportion of annual total")+
  scale_fill_manual(values=c("#bdd7e7", "#6baed6", "#3182bd", "#08519c",
                            "#fcae91", "#fb6a4a", "#de2d26", "#a50f15",
                            "#bae4b3", "#74c476", "#238b45"), name="")+
  theme_custom()+
  theme(axis.line=element_blank())

dev.off()

#Proportion of all deaths
Baseline_health %>% 
  select(Group, Deaths) %>% 
  filter(Group %in% c("IMDQ1 (least deprived)", "IMDQ2", "Population")) %>% 
  merge(read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Health/MUPA mortality data 2017-19.xlsx",
                   sheet="Counts", range="BA2:BB5")) %>% 
  mutate(Prop=-Deaths/AllDeaths,
         Group=case_when(
           Group=="IMDQ1 (least deprived)" ~ "In Poverty",
           Group=="IMDQ2" ~ "Not In Poverty", TRUE ~ "Population"),
         Group=factor(Group, levels=c("Population", "In Poverty", "Not In Poverty"))) %>% 
  arrange(Group) %>% 
  select(Group, Prop) %>% 
  gt(rowname_col="Group") %>% 
  fmt_percent(columns=Prop, decimals=1) %>% 
  cols_label(Prop="Proportion of all deaths which are attributable to alcohol") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(Group) ~ px(180)) %>% 
  gtsave(paste0(outputfolder, "/Table9-BaselineDeathProp.png"))

#Read in health impact results from model runs for all policies
for (i in 1:length(Policies)) {
  
  assign(paste0("h_", i), read_excel(paste0(folder, "/", Policies[i], "/SAPM3_C2HHealth_Results.xlsx"), 
                                     sheet="Summary by subgroups",
                                     range="B4:W81", col_names=FALSE) %>%
           select(c(1:7, 11,12, 16, 17, 21, 22)) %>% 
           set_names("Outcome", "Population", "Moderate", "Harmful", "Hazardous", 
                     "IMDQ1 (least deprived)", "IMDQ2", "IMDQ1 Mod", "IMDQ2 Mod",
                     "IMDQ1 Haz", "IMDQ2 Haz", "IMDQ1 Harm", "IMDQ2 Harm") %>% 
           mutate(Policy=Policies[i]))
}

Healthresults <- bind_rows(h_1, h_2, h_3, h_4, h_5, h_6, h_7, h_8, h_9, h_10,
                        h_11, h_12, h_13, h_14, h_15, h_16, h_17, h_18,
                        h_19, h_20) %>% 
  mutate(Period=case_when(
    substr(Outcome,1,5)=="Year1" ~ "Year 1",
    substr(Outcome,1,4)=="Full" ~ "Year 20",
    TRUE ~ "Cumulative"),
    Measure=case_when(
      grepl("Death", Outcome) ~ "Deaths",
      grepl("Cost", Outcome) ~ "Costs",
      grepl("Sick", Outcome) ~ "Prevalence",
      grepl("Adm", Outcome) ~ "Admissions",
      grepl("QALY", Outcome) ~ "QALYs")) %>% 
  group_by(Measure, Period, Policy) %>% 
  summarise(across(.cols=c(2:13), ~sum(.x)), .groups="drop")

#Policy impacts on mortality
#Table of absolute numbers
Healthresults %>% 
  filter(Measure=="Deaths" & Period=="Year 20") %>% 
  bind_rows(Baseline_health %>% 
              select(Group, Deaths) %>% 
              mutate(Deaths=-Deaths, Policy="Annual alcohol-attributable deaths") %>% 
              spread(Group, Deaths)) %>% 
  select(c(3:9)) %>% 
  gt(rowname_col="Policy") %>% 
  fmt_number(columns=everything(), decimals=0) %>% 
  tab_row_group("Policy impact", rows=c(1:20)) %>% 
  tab_row_group("Baseline", rows=21) %>% 
  tab_spanner(label="Drinker group", columns=c("Moderate", "Hazardous", "Harmful")) %>% 
  tab_spanner(label="Income group", columns=c("IMDQ1 (least deprived)", 
                                              "IMDQ2")) %>% 
  cols_label(Population="All Drinkers", Hazardous="Increasing Risk", 
             Harmful="Higher Risk", `IMDQ1 (least deprived)`="In Poverty",
             `IMDQ2`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table20-DeathImpactAbs.png"))
  
#Table of rates & % changes
Healthresults %>% 
  filter(Measure=="Deaths" & Period=="Year 20") %>% 
  bind_rows(Baseline_health %>% 
              select(Group, Deaths) %>% 
              mutate(Deaths=-Deaths, Policy="Annual alcohol-attributable deaths per 100,000 drinkers") %>% 
              spread(Group, Deaths)) %>% 
  select(c(3:9)) %>% 
  gather(Group, Value, c(2:7)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>% 
  select(Policy, Rate, Group) %>% 
  spread(Group, Rate) %>% 
  select(Policy, Population, Moderate, Hazardous, Harmful, `IMDQ1 (least deprived)`, 
         IMDQ2) %>% 
  mutate(across(.cols=c(2:7), 
                ~if_else(Policy != "Annual alcohol-attributable deaths per 100,000 drinkers", paste0(round(.x, digits=1), " (", round(100*.x/.x[Policy=="Annual alcohol-attributable deaths per 100,000 drinkers"], 1),"%)"),
    as.character(round(.x, digits=1))))) %>% 
  gt(rowname_col="Policy") %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group("Baseline", rows=11) %>% 
  tab_spanner(label="Drinker group", columns=c("Moderate", "Hazardous", "Harmful")) %>% 
  tab_spanner(label="Income group", columns=c("IMDQ1 (least deprived)", 
                                              "IMDQ2")) %>% 
  cols_label(Population="All Drinkers", Hazardous="Increasing Risk", 
             Harmful="Higher Risk", `IMDQ1 (least deprived)`="In Poverty",
             `IMDQ2`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(columns = -c(Policy) ~ px(140)) %>% 
  cols_width(columns = Policy ~ px(240)) %>% 
  gtsave(paste0(outputfolder, "/Table21-DeathImpactRates.png"), vwidth=1600)

#Graph of policy impacts by drinker group
agg_png(paste0(outputfolder, "/Fig15-DeathImpactxDrinker.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Deaths" & Period=="Year 20") %>% 
  select(c(3, 5:7)) %>% 
  gather(Group, Value, c(2:4)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop,
         Group=factor(Group, levels=c("Moderate", "Hazardous", "Harmful"))) %>%
  ggplot(aes(x=Rate, y=Policy, fill=Group))+
  geom_vline(xintercept=0, colour="Grey20")+
  geom_col(position="dodge")+
  scale_x_continuous(name="Change in annual deaths per 100,000 drinkers")+
  scale_y_discrete(name="", position="right")+  
  scale_fill_manual(values=drinkerpal, name="", labels=c("Moderate",
                                                         "Increasing Risk",
                                                         "Higher Risk"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

agg_png(paste0(outputfolder, "/Fig16-DeathImpactxInc.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Deaths" & Period=="Year 20") %>% 
  select(c(3, 8,9)) %>% 
  gather(Group, Value, c(2:3)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>%
  ggplot(aes(x=Rate, y=Policy, fill=Group))+
  geom_vline(xintercept=0, colour="Grey20")+
  geom_col(position="dodge")+
  scale_x_continuous(name="Change in annual deaths per 100,000 drinkers")+
  scale_y_discrete(name="", position="right")+  
  scale_fill_manual(values=incomepal, name="", labels=c("In Poverty",
                                                         "Not In Poverty"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

#Proportional mortality reductions by drinker group
agg_png(paste0(outputfolder, "/Figx1-DeathImpactPropxDrinker.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Deaths" & Period=="Year 20") %>% 
  select(c(3, 5:7)) %>% 
  gather(Group, Value, c(2:4)) %>% 
  group_by(Policy) %>% 
  mutate(Total=sum(Value),
         Group=factor(Group, levels=c("Moderate", "Hazardous", "Harmful"))) %>% 
  ungroup() %>% 
  mutate(ChangeProp=Value/Total) %>% 
  ggplot(aes(x=ChangeProp, y=Policy, fill=Group))+
  geom_col(position="stack")+
  geom_text(aes(label=if_else(ChangeProp<0.03, "", 
                              paste0(round(ChangeProp*100, 0), "%")), colour=Group), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_x_continuous(name="Proportion of all deaths averted")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=drinkerpal, name="", 
                    labels=c("Moderate", "Increasing risk", "Higher risk"))+
  scale_colour_manual(values=c("Black", "Black", "White"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

dev.off()

#Proportional mortality reductions by income group
agg_png(paste0(outputfolder, "/Figx2-DeathImpactPropxInc.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Deaths" & Period=="Year 20") %>% 
  select(c(3, 8,9)) %>% 
  gather(Group, Value, c(2:3)) %>% 
  group_by(Policy) %>% 
  mutate(Total=sum(Value)) %>% 
  ungroup() %>% 
  mutate(ChangeProp=Value/Total) %>% 
  ggplot(aes(x=ChangeProp, y=Policy, fill=Group))+
  geom_col(position="stack")+
  geom_text(aes(label=if_else(ChangeProp<0.03, "", 
                              paste0(round(ChangeProp*100, 0), "%"))), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_x_continuous(name="Proportion of all deaths averted")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=incomepal, name="", 
                    labels=c("In Poverty", "Not In Poverty"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

dev.off()

#Table of rates & % changes by drinker & income group
Healthresults %>% 
  filter(Measure=="Deaths" & Period=="Year 20") %>% 
  bind_rows(Baseline_health %>% 
              select(Group, Deaths) %>% 
              mutate(Deaths=-Deaths, Policy="Annual alcohol-attributable deaths per 100,000 drinkers") %>% 
              spread(Group, Deaths)) %>% 
  select(c(3, 10:15)) %>% 
  gather(Group, Value, c(2:7)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>% 
  select(Policy, Rate, Group) %>% 
  spread(Group, Rate) %>% 
  select(Policy, `IMDQ1 Mod`, `IMDQ2 Mod`, `IMDQ1 Haz`, `IMDQ2 Haz`, `IMDQ1 Harm`, 
         `IMDQ2 Harm`) %>% 
  mutate(across(.cols=c(2:7), 
                ~if_else(Policy != "Annual alcohol-attributable deaths per 100,000 drinkers", paste0(round(.x, digits=1), " (", round(100*.x/.x[Policy=="Annual alcohol-attributable deaths per 100,000 drinkers"], 1),"%)"),
                         as.character(round(.x, digits=1))))) %>% 
  gt(rowname_col="Policy") %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group("Baseline", rows=11) %>% 
  tab_spanner(label="Moderate", columns=c("IMDQ1 Mod", "IMDQ2 Mod")) %>% 
  tab_spanner(label="Increasing Risk", columns=c("IMDQ1 Haz", "IMDQ2 Haz")) %>% 
  tab_spanner(label="Higher Risk", columns=c("IMDQ1 Harm", "IMDQ2 Harm")) %>% 
  cols_label(`IMDQ1 Mod`="In Poverty", `IMDQ2 Mod`= "In Poverty",
             `IMDQ1 Haz`="In Poverty", `IMDQ2 Haz`= "Not In Poverty", 
             `IMDQ1 Harm`= "In Poverty", `IMDQ2 Harm`= "Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(columns = -c(Policy) ~ px(140)) %>% 
  cols_width(columns = Policy ~ px(240)) %>% 
  gtsave(paste0(outputfolder, "/Table22-DeathImpactxDrinkerxInc.png"), vwidth=1600)

#Hospital admissions
#Table of absolute numbers
Healthresults %>% 
  filter(Measure=="Admissions" & Period=="Year 20") %>% 
  bind_rows(Baseline_health %>% 
              select(Group, Admissions) %>% 
              mutate(Admissions=-Admissions, Policy="Annual alcohol-attributable admissions") %>% 
              spread(Group, Admissions)) %>% 
  select(c(3:9)) %>% 
  gt(rowname_col="Policy") %>% 
  fmt_number(columns=everything(), decimals=0) %>% 
  tab_row_group("Policy impact", rows=c(1:20)) %>% 
  tab_row_group("Baseline", rows=21) %>% 
  tab_spanner(label="Drinker group", columns=c("Moderate", "Hazardous", "Harmful")) %>% 
  tab_spanner(label="Income group", columns=c("IMDQ1 (least deprived)", 
                                              "IMDQ2")) %>% 
  cols_label(Population="All Drinkers", Hazardous="Increasing Risk", 
             Harmful="Higher Risk", `IMDQ1 (least deprived)`="In Poverty",
             `IMDQ2`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table23-AdmImpactAbs.png"))

#Table of rates & % changes
Healthresults %>% 
  filter(Measure=="Admissions" & Period=="Year 20") %>% 
  bind_rows(Baseline_health %>% 
              select(Group, Admissions) %>% 
              mutate(Admissions=-Admissions, Policy="Annual alcohol-attributable admissions per 100,000 drinkers") %>% 
              spread(Group, Admissions)) %>% 
  select(c(3:9)) %>% 
  gather(Group, Value, c(2:7)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>% 
  select(Policy, Rate, Group) %>% 
  spread(Group, Rate) %>% 
  select(Policy, Population, Moderate, Hazardous, Harmful, `IMDQ1 (least deprived)`, 
         IMDQ2) %>% 
  mutate(across(.cols=c(2:7), 
                ~if_else(Policy != "Annual alcohol-attributable admissions per 100,000 drinkers", paste0(round(.x, digits=1), " (", round(100*.x/.x[Policy=="Annual alcohol-attributable admissions per 100,000 drinkers"], 1),"%)"),
                         as.character(round(.x, digits=1))))) %>% 
  gt(rowname_col="Policy") %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group("Baseline", rows=11) %>% 
  tab_spanner(label="Drinker group", columns=c("Moderate", "Hazardous", "Harmful")) %>% 
  tab_spanner(label="Income group", columns=c("IMDQ1 (least deprived)", 
                                              "IMDQ2")) %>% 
  cols_label(Population="All Drinkers", Hazardous="Increasing Risk", 
             Harmful="Higher Risk", `IMDQ1 (least deprived)`="In Poverty",
             `IMDQ2`="Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(columns = -c(Policy) ~ px(140)) %>% 
  cols_width(columns = Policy ~ px(240)) %>% 
  gtsave(paste0(outputfolder, "/Table24-AdmImpactRates.png"), vwidth=1600)

#Graph of policy impacts by drinker group
agg_png(paste0(outputfolder, "/Fig17-AdmImpactxDrinker.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Admissions" & Period=="Year 20") %>% 
  select(c(3, 5:7)) %>% 
  gather(Group, Value, c(2:4)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop,
         Group=factor(Group, levels=c("Moderate", "Hazardous", "Harmful"))) %>%
  ggplot(aes(x=Rate, y=Policy, fill=Group))+
  geom_vline(xintercept=0, colour="Grey20")+
  geom_col(position="dodge")+
  scale_x_continuous(name="Change in annual admissions per 100,000 drinkers")+
  scale_y_discrete(name="", position="right")+  
  scale_fill_manual(values=drinkerpal, name="", labels=c("Moderate",
                                                         "Increasing Risk",
                                                         "Higher Risk"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

agg_png(paste0(outputfolder, "/Fig18-AdmImpactxInc.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Admissions" & Period=="Year 20") %>% 
  select(c(3, 8,9)) %>% 
  gather(Group, Value, c(2:3)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>%
  ggplot(aes(x=Rate, y=Policy, fill=Group))+
  geom_vline(xintercept=0, colour="Grey20")+
  geom_col(position="dodge")+
  scale_x_continuous(name="Change in annual admissions per 100,000 drinkers")+
  scale_y_discrete(name="", position="right")+  
  scale_fill_manual(values=incomepal, name="", labels=c("In Poverty",
                                                        "Not In Poverty"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

#Proportional admission reductions by drinker group
agg_png(paste0(outputfolder, "/Fig19-AdmImpactPropxDrinker.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Admissions" & Period=="Year 20") %>% 
  select(c(3, 5:7)) %>% 
  gather(Group, Value, c(2:4)) %>% 
  group_by(Policy) %>% 
  mutate(Total=sum(Value),
         Group=factor(Group, levels=c("Moderate", "Hazardous", "Harmful"))) %>% 
  ungroup() %>% 
  mutate(ChangeProp=Value/Total) %>% 
  ggplot(aes(x=ChangeProp, y=Policy, fill=Group))+
  geom_col(position="stack")+
  geom_text(aes(label=if_else(ChangeProp<0.03, "", 
                              paste0(round(ChangeProp*100, 0), "%")), colour=Group), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_x_continuous(name="Proportion of all admissions averted")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=drinkerpal, name="", 
                    labels=c("Moderate", "Increasing risk", "Higher risk"))+
  scale_colour_manual(values=c("Black", "Black", "White"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

dev.off()

#Proportional mortality reductions by income group
agg_png(paste0(outputfolder, "/Fig20-AdmImpactPropxInc.png"), units="in", width=8, height=6, res=600)
Healthresults %>% 
  filter(Measure=="Admissions" & Period=="Year 20") %>% 
  select(c(3, 8,9)) %>% 
  gather(Group, Value, c(2:3)) %>% 
  group_by(Policy) %>% 
  mutate(Total=sum(Value)) %>% 
  ungroup() %>% 
  mutate(ChangeProp=Value/Total) %>% 
  ggplot(aes(x=ChangeProp, y=Policy, fill=Group))+
  geom_col(position="stack")+
  geom_text(aes(label=if_else(ChangeProp<0.03, "", 
                              paste0(round(ChangeProp*100, 0), "%"))), show.legend = FALSE,
            position=position_stack(vjust=0.5), family="Lato", size=rel(3.5))+
  scale_x_continuous(name="Proportion of all admissions averted")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=incomepal, name="", 
                    labels=c("In Poverty", "Not In Poverty"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

dev.off()

#Table of rates & % changes by drinker & income group
Healthresults %>% 
  filter(Measure=="Admissions" & Period=="Year 20") %>% 
  bind_rows(Baseline_health %>% 
              select(Group, Admissions) %>% 
              mutate(Admissions=-Admissions, Policy="Annual alcohol-attributable admissions per 100,000 drinkers") %>% 
              spread(Group, Admissions)) %>% 
  select(c(3, 10:15)) %>% 
  gather(Group, Value, c(2:7)) %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>% 
  select(Policy, Rate, Group) %>% 
  spread(Group, Rate) %>% 
  select(Policy, `IMDQ1 Mod`, `IMDQ2 Mod`, `IMDQ1 Haz`, `IMDQ2 Haz`, `IMDQ1 Harm`, 
         `IMDQ2 Harm`) %>% 
  mutate(across(.cols=c(2:7), 
                ~if_else(Policy != "Annual alcohol-attributable admissions per 100,000 drinkers", paste0(round(.x, digits=1), " (", round(100*.x/.x[Policy=="Annual alcohol-attributable admissions per 100,000 drinkers"], 1),"%)"),
                         as.character(round(.x, digits=1))))) %>% 
  gt(rowname_col="Policy") %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group("Baseline", rows=11) %>% 
  tab_spanner(label="Moderate", columns=c("IMDQ1 Mod", "IMDQ2 Mod")) %>% 
  tab_spanner(label="Increasing Risk", columns=c("IMDQ1 Haz", "IMDQ2 Haz")) %>% 
  tab_spanner(label="Higher Risk", columns=c("IMDQ1 Harm", "IMDQ2 Harm")) %>% 
  cols_label(`IMDQ1 Mod`="In Poverty", `IMDQ2 Mod`= "In Poverty",
             `IMDQ1 Haz`="In Poverty", `IMDQ2 Haz`= "Not In Poverty", 
             `IMDQ1 Harm`= "In Poverty", `IMDQ2 Harm`= "Not In Poverty") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(columns = -c(Policy) ~ px(140)) %>% 
  cols_width(columns = Policy ~ px(240)) %>% 
  gtsave(paste0(outputfolder, "/Table25-AdmImpactDrinkerxInc.png"), vwidth=1600)

#Distribution of harm reduction over time for one examplar policy only
exemplar <- "50p MUP"

exemplardata <- read_excel(paste0(folder, "/", exemplar, "/SAPM3_C2HHealth_Results.xlsx"), 
                           sheet="Summary by health conditions",
                           range="B4:BN48", col_names=FALSE) %>% 
  select(1:21, 46:65) %>% 
  set_names(c("ConditionNo", paste0(rep(c("Deaths", "Admissions"), each=20), "_", 
                                  rep(1:20, times=2)))) %>% 
  pivot_longer(cols=c(2:41), names_to=c("Metric", "Year"), names_sep="_", 
               values_to="Value") %>% 
  mutate(ConditionNo=as.numeric(substr(ConditionNo, 18,19))) %>% 
  merge(read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Health/ConditionLookup.xlsx",
                   range="A1:C46")) %>% 
  group_by(Year, Metric, ConditionGroup) %>% 
  summarise(Value=sum(Value), .groups="drop") %>% 
  mutate(Year=as.numeric(Year), 
         Metric=factor(Metric, levels=c("Deaths", "Admissions")),
         ConditionGroup=factor(ConditionGroup, levels=c("Liver disease", "Mental and behavioural disorders due to use of alcohol", 
                                                        "Alcohol poisoning", "Other wholly alcohol-attributable conditions",
                                                        "Cancers", "Hypertension", "Stroke", "Other cardiovascular disease","Other chronic conditions", 
                                                        "Road traffic accidents", "Falls", "Other injuries", 
                                                        "Diabetes"))) %>% 
  arrange(ConditionGroup)

#Table of full effect outcomes by condition group
exemplardata %>% 
  filter(Year==20) %>% 
  select(-Year) %>% 
  merge(Baseline_health_cond %>% 
          gather(Metric, Baseline, c(2:3))) %>% 
  mutate(text=paste0(round(Value, 0), " (", round(Value*100/Baseline, 1), "%)")) %>% 
  select(ConditionGroup, Metric, text) %>% 
  spread(Metric, text) %>% 
  mutate(ConditionGroup=factor(ConditionGroup, levels=c("Liver disease", "Mental and behavioural disorders due to use of alcohol", 
                                                        "Alcohol poisoning", "Other wholly alcohol-attributable conditions",
                                                        "Cancers", "Hypertension", "Stroke", "Other cardiovascular disease",
                                                        "Diabetes", "Other chronic conditions", "Road traffic accidents",
                                                        "Falls", "Other injuries"))) %>% 
  arrange(ConditionGroup) %>% 
  gt(rowname_col="ConditionGroup") %>% 
  tab_spanner(label="Annual change in alcohol-attributable", columns=c(2,3)) %>% 
  tab_row_group("Injuries", rows=c(11:13)) %>% 
  tab_row_group("Chronic conditions", rows=c(5:10)) %>% 
  tab_row_group("Wholly alcohol-attributable", rows=c(1:4)) %>% 
  tab_footnote(
    footnote = "Note that these conditions are estimated to have negative alcohol-attributable baseline deaths and admissions (see Table 5) and therefore absolute *increases* represent *negative* relative changes",
    locations = cells_stub(rows=c(8,9)), placement="right") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(-c(ConditionGroup) ~ px(120)) %>% 
  cols_width(ConditionGroup ~ px(400)) %>% 
  gtsave(paste0(outputfolder, "/Table26-HarmImpactsxCond.png"))

#Graph of outcomes over time by condition group
agg_png(paste0(outputfolder, "/Fig21-HarmImpactsxCond.png"), units="in", width=12, height=6, res=600)
ggplot(exemplardata, aes(x=Year, y=Value, fill=ConditionGroup))+
  geom_col(position="stack")+
  geom_hline(yintercept=0, colour="Grey20")+
  scale_x_continuous(name="Years since policy introduction")+
  scale_y_continuous(name="Annual change from baseline")+
  scale_fill_manual(values=c("#bdd7e7", "#6baed6", "#3182bd", "#08519c",
                                      "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26",
                                      "#a50f15", "#bae4b3", "#74c476", "#238b45", 
                                      "grey40"), name="",
                                      guide = guide_legend(reverse = TRUE))+
  facet_wrap(~Metric, scales="free_y")+
  theme_custom()

dev.off()

#NHS cost impacts
Healthresults %>% 
  filter(Measure=="Costs") %>% 
  select(Period, Policy, Population) %>% 
  mutate(Population=Population/1000000, 
         Period=factor(Period, levels=c("Year 1", "Year 20", "Cumulative"))) %>%
  arrange(Period) %>% 
  spread(Period, Population) %>% 
  gt(rowname_col="Policy") %>% 
  fmt_currency(columns=everything(), currency="GBP", decimals=1) %>% 
  tab_spanner(label="Change in NHS costs due to alcohol (£millions)",
              columns=everything()) %>% 
  cols_label(Cumulative="Cumulative over 20 years") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(columns = -c(Policy) ~ px(140)) %>% 
  gtsave(paste0(outputfolder, "/Table27-CostImpacts.png"))

###################################################################
#Crime outcomes#
################

#Baseline crime volumes
Baseline_crime_offence <- read_excel(paste0(folder, "/Extreme scenario/SAPM3_C2HCrime_Results.xlsx"), 
                              sheet="Summary by crime conditions", range="B3:C21") %>% 
  mutate(Key=as.numeric(substr(`Crime Table Difference`, 17,18))) %>% 
  merge(read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Crime/Crime Inputs Full.xlsm",
                   sheet="Costs", range="E1:G19")) 

Baseline_crime_offence %>% 
  select(`Year 1`, Field1, Group) %>% 
  set_names("Offences", "UnitCost", "Category") %>% 
  mutate(Offences=-Offences, Cost=Offences*UnitCost, Cost=Cost/1000000) %>% 
  group_by(Category) %>% 
  summarise(Offences=sum(Offences), Cost=sum(Cost), .groups="drop") %>% 
  gt(rowname_col="Category") %>% 
  fmt_number(columns=Offences, decimals=0) %>% 
  fmt_currency(columns=Cost, currency="GBP", decimals=1) %>% 
  grand_summary_rows(columns=Offences, fns=list(Total=~sum(.,na.rm=TRUE)),
               fmt = list(~ fmt_number(., decimals=0))) %>% 
  grand_summary_rows(columns=Cost, fns=list(Total=~sum(.,na.rm=TRUE)),
                   fmt = list(~ fmt_currency(., decimals=1, currency="GBP"))) %>% 
  cols_label(Offences="Alcohol-attributable offences", Cost="Annual cost (£m)") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table10-BaselineCrimesxOffence.png"))

Baseline_crime <- read_excel(paste0(folder, "/Extreme scenario/SAPM3_C2HCrime_Results.xlsx"), 
                             sheet="Summary by subgroups", range="B3:F13") %>% 
  mutate(Metric=rep(c("Offences", "Cost"), each=5)) %>% 
  group_by(Metric) %>% 
  summarise(across(.cols=c(2:5), ~sum(.x)), .groups="drop")

#Baseline crimes by drinker group
Baseline_crime %>% 
  filter(Metric=="Offences") %>% 
  gather(Group, `Alcohol-Attributable Offences`, c(2:5)) %>% 
  mutate(`Alcohol-Attributable Offences`=-`Alcohol-Attributable Offences`) %>% 
  merge(HealthPop) %>% 
  mutate(`Rate per 100,000 drinkers`=`Alcohol-Attributable Offences`*100000/Pop) %>% 
  select(-c(Pop, Metric)) %>% 
  gather(Metric, Value, c(2:3)) %>% 
  spread(Group, Value) %>% 
  gt(rowname_col="Metric") %>% 
  cols_move(Moderate, Population) %>% 
  cols_move(Hazardous, Moderate) %>% 
  cols_move(Harmful, Hazardous) %>% 
  tab_spanner(label="Drinker Group", columns=c(Moderate, Hazardous, Harmful)) %>% 
  fmt_number(columns=everything(), decimals=0) %>% 
  cols_label(Hazardous="Increasing Risk", Harmful="Higher Risk") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table11-BaselineCrimesxDrinker.png"))

#Read in policy impacts
for (i in 1:length(Policies)) {
  
  assign(paste0("cr_", i), read_excel(paste0(folder, "/", Policies[i], "/SAPM3_C2HCrime_Results.xlsx"), 
                                     sheet="Summary by subgroups",
                                     range="B3:F13") %>%
           mutate(Policy=Policies[i]))
}

Crimeresults <- bind_rows(cr_1, cr_2, cr_3, cr_4, cr_5, cr_6, cr_7, cr_8, cr_9, 
                           cr_10, cr_11, cr_12, cr_13, cr_14, cr_15, cr_16, cr_17, 
                           cr_18, cr_19, cr_20) %>% 
  mutate(Metric=rep(c("Offences", "Cost"), each=5, times=length(Policies))) %>% 
  group_by(Policy, Metric) %>% 
  summarise(across(.cols=c(2:5), ~sum(.x)), .groups="drop")

#Table of absolute crime impacts
Crimeresults %>% 
  bind_rows(Baseline_crime %>% mutate(Policy="Baseline", Population=-Population)) %>% 
  gather(Group, Value, c(3:6)) %>% 
  spread(Metric, Value) %>% 
  filter(Group=="Population") %>% 
  select(-Group) %>% 
  ungroup() %>% 
  mutate(Cost=Cost/1000000,
         across(.cols=c(2,3), ~if_else(Policy!="Baseline",
                                       paste0(round(.x, 0)," (", round(.x*100/.x[Policy=="Baseline"],1), "%)"),
                                       as.character(round(.x,0)))),
         Cost=if_else(substr(Cost, 1, 1)=="-", paste0("-£", substr(Cost,2,99)),
                      paste0("£", Cost))) %>% 
  gt(rowname_col="Policy") %>% 
  cols_move(Cost, Offences) %>% 
  cols_align(columns="Cost", align="right") %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group(" ", rows=11) %>% 
  cols_label(Offences="Alcohol-attributable offences", Cost="Annual cost (£m)") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table28-CrimeImpactsAbs.png"))
  
#Table of impacts on crime rates
Crimeresults %>% 
  bind_rows(Baseline_crime %>% mutate(Policy="Annual alcohol-attributable offences per 100,000 drinkers")) %>% 
  gather(Group, Value, c(3:6)) %>% 
  spread(Metric, Value) %>% 
  select(-Cost) %>% 
  merge(HealthPop) %>% 
  mutate(Offences=if_else(Policy=="Annual alcohol-attributable offences per 100,000 drinkers",
                          -Offences, Offences),
    OffenceRate=Offences*100000/Pop) %>% 
  select(Policy, Group, OffenceRate) %>% 
  mutate(OffenceRate=if_else(Policy=="Baseline", -OffenceRate, OffenceRate)) %>% 
  spread(Group, OffenceRate) %>% 
  gt(rowname_col="Policy") %>% 
  cols_move(Moderate, Population) %>% 
  cols_move(Hazardous, Moderate) %>% 
  cols_move(Harmful, Hazardous) %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group("Baseline", rows=11) %>% 
  fmt_number(columns=everything(), decimals=0) %>% 
  tab_spanner(label="Drinker Group", columns=c(Moderate, Hazardous, Harmful)) %>% 
  cols_label(Population="All Drinkers", Hazardous="Increasing Risk",
             Harmful="Higher Risk") %>% 
  cols_width(columns = Policy ~ px(240)) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table29-CrimeImpactsxDrinker.png"))

#Graph of impacts on crime rates
agg_png(paste0(outputfolder, "/Fig22-CrimeImpactxDrinker.png"), units="in", width=8, height=6, res=600)
Crimeresults %>% 
  filter(Metric!="Cost") %>% 
  gather(Group, Value, c(3:6)) %>% 
  filter(Group!="Population") %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>% 
  select(Group, Policy, Rate) %>% 
  mutate(Group=factor(Group, levels=c("Moderate", "Hazardous", "Harmful"))) %>% 
  ggplot(aes(x=Rate, y=Policy, fill=Group))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="grey20")+
  scale_x_continuous(name="Change in annual offences per 100,000 drinkers")+
  scale_y_discrete(name="", position="right")+
  scale_fill_manual(values=drinkerpal, name="", labels=c("Moderate", "Increasing Risk", "Higher Risk"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

###################################################################
#Workplace outcomes#
####################

#Table of key workplace model parameters
read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Workplace/LFS Outputs_new.xlsm",
                         sheet="LFS outputs", range="B26:H35") %>% 
  rename(Age=`...1`) %>% 
  mutate(Age=if_else(is.na(Age), "Population", Age)) %>% 
  gt(rowname_col="Age") %>% 
  tab_row_group("Female", rows=c(5:8)) %>% 
  tab_row_group("Male", rows=c(1:4)) %>% 
  fmt_number(columns=2, decimals=0) %>% 
  fmt_number(columns=4, decimals=2) %>% 
  fmt_percent(columns=c(3,5), decimals=1) %>% 
  fmt_percent(columns=7, decimals=0) %>% 
  fmt_currency(columns=6, currency="GBP", decimals=0) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table4-WorkplaceInputs.png"))

#Baseline alcohol-attributable absence
Baseline_work <- read_excel(paste0(folder, "/Extreme scenario/SAPM3_C2HWorkplace_Results.xlsx"), 
                            range="C3:F5") %>% 
  mutate(Metric=c("Absences", "Cost")) 

#Table
Baseline_work %>% 
  gather(Group, `Alcohol-Attributable Absences (Days)`, c(1:4)) %>% 
  merge(HealthPop) %>% 
  filter(Metric!="Cost") %>% 
  mutate(`Alcohol-Attributable Absences (Days)`=-`Alcohol-Attributable Absences (Days)`, 
         `Rate per 100,000 drinkers`=`Alcohol-Attributable Absences (Days)`*100000/Pop,
         Group=factor(Group, levels=c("Population", "Moderate", "Hazardous", "Harmful"))) %>% 
  select(-c(Pop, Metric)) %>% 
  arrange(Group) %>% 
  gather(Metric, `Alcohol-Attributable Absences (Days)`, c(2,3)) %>% 
  spread(Group, `Alcohol-Attributable Absences (Days)`) %>% 
  gt(rowname_col="Metric") %>% 
  fmt_number(columns=everything(), decimals=0) %>% 
  tab_spanner(label="Drinker Group", columns=c(Moderate, Hazardous, Harmful)) %>% 
  cols_label(Population="All Drinkers", Hazardous="Increasing Risk",
             Harmful="Higher Risk") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table12-BaselineWorkplace.png"))
  
#Pull out cost
Baseline_WorkCost <- Baseline_work %>% 
  filter(Metric=="Cost") %>% 
  select(Population) %>% 
  mutate(Population=-Population/1000000) %>% 
  as.numeric()

#Read in policy impacts
for (i in 1:length(Policies)) {
  
  assign(paste0("w_", i), read_excel(paste0(folder, "/", Policies[i], "/SAPM3_C2HWorkplace_Results.xlsx"), 
                                      range="C3:F5") %>%
           mutate(Policy=Policies[i]))
}

Workresults <- bind_rows(w_1, w_2, w_3, w_4, w_5, w_6, w_7, w_8, w_9, 
                          w_10, w_11, w_12, w_13, w_14, w_15, w_16, w_17, 
                          w_18, w_19, w_20) %>% 
  mutate(Metric=rep(c("Absences", "Cost"), times=length(Policies)))

#Table of absolute workplace impacts
Workresults %>% 
  bind_rows(Baseline_work %>% mutate(Policy="Baseline", Population=-Population)) %>% 
  gather(Group, Value, c(1:4)) %>% 
  spread(Metric, Value) %>% 
  filter(Group=="Population") %>% 
  select(-Group) %>% 
  ungroup() %>% 
  mutate(Cost=Cost/1000000,
         Cost=if_else(Policy!="Baseline", paste0(round(Cost, 1)," (", round(Cost*100/Cost[Policy=="Baseline"],1), "%)"),
                                       as.character(round(Cost,1))),
         Cost=if_else(substr(Cost, 1, 1)=="-", paste0("-£", substr(Cost,2,99)),
                      paste0("£", Cost)),
         Absences=if_else(Policy!="Baseline", paste0(round(Absences, 0)," (", round(Absences*100/Absences[Policy=="Baseline"],1), "%)"),
                          as.character(round(Absences,0)))) %>% 
  gt(rowname_col="Policy") %>% 
  cols_align(columns="Cost", align="right") %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group(" ", rows=11) %>% 
  cols_label(Absences="Alcohol-attributable absences (days)", Cost="Annual cost (£m)") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table30-WorkplaceImpactsAbs.png"))

#Table of impacts on workplace absence rates by drinker
Workresults %>% 
  bind_rows(Baseline_work %>% mutate(Policy="Annual alcohol-attributable absences (days) per 100,000 drinkers")) %>% 
  gather(Group, Value, c(1:4)) %>% 
  spread(Metric, Value) %>% 
  select(-Cost) %>% 
  merge(HealthPop) %>% 
  mutate(AbsenceRate=Absences*100000/Pop) %>% 
  select(Policy, Group, AbsenceRate) %>% 
  mutate(AbsenceRate=if_else(Policy=="Annual alcohol-attributable absences (days) per 100,000 drinkers", -AbsenceRate, AbsenceRate)) %>% 
  spread(Group, AbsenceRate) %>% 
  gt(rowname_col="Policy") %>% 
  cols_move(Moderate, Population) %>% 
  cols_move(Hazardous, Moderate) %>% 
  cols_move(Harmful, Hazardous) %>% 
  tab_row_group("Policy impact", rows=c(1:10, 12:21)) %>% 
  tab_row_group("Baseline", rows=11) %>% 
  fmt_number(columns=everything(), decimals=0) %>% 
  tab_spanner(label="Drinker Group", columns=c(Moderate, Hazardous, Harmful)) %>% 
  cols_label(Population="All Drinkers", Hazardous="Increasing Risk",
             Harmful="Higher Risk") %>% 
  cols_width(columns = Policy ~ px(240)) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table31-WorkplaceImpactsxDrinker.png"))

#Graph of impacts on crime rates
agg_png(paste0(outputfolder, "/Fig23-WorkplaceImpactsxDrinker.png"), units="in", width=8, height=6, res=600)
Workresults %>% 
  filter(Metric!="Cost") %>% 
  gather(Group, Value, c(1:4)) %>% 
  filter(Group!="Population") %>% 
  merge(HealthPop) %>% 
  mutate(Rate=Value*100000/Pop) %>% 
  select(Group, Policy, Rate) %>% 
  mutate(Group=factor(Group, levels=c("Moderate", "Hazardous", "Harmful"))) %>% 
  ggplot(aes(x=Rate, y=Policy, fill=Group))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="grey20")+
  scale_x_continuous(name="Change in annual workplace absence days\nper 100,000 drinkers")+
  scale_y_discrete(name="", position="right")+
  scale_fill_manual(values=drinkerpal, name="", labels=c("Moderate", "Increasing Risk", "Higher Risk"))+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.line.y=element_blank(), legend.position="top")

dev.off()

############################################
#Miscellaneous input data graphs

#Price distributions from Nielsen
agg_png(paste0(outputfolder, "/Fig1-NielsenPriceDists.png"), units="in", width=7, height=5, res=600)
read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Price/NielsenForCalibration.xlsx",
           sheet="Outputs", range="L1:Q16") %>%
  set_names("priceband", "Beer", "Cider", "Wine", "Spirits", "RTDs") %>% 
  gather(Drink, Proportion, c(2:6)) %>% 
  filter(priceband<0.9) %>% 
  mutate(Drink=factor(Drink, levels=c("Beer", "Cider", "Wine", "Spirits", "RTDs"))) %>% 
  ggplot(aes(x=priceband, y=Proportion/100, colour=Drink))+
  geom_line()+
  geom_text_repel(data=. %>% filter(priceband==0.825), 
                  aes(label=Drink), family="Lato", xlim=c(0.83, NA))+
  scale_x_continuous(limits=c(0.15, 0.9), name="Price per unit",
                     labels=label_dollar(prefix="£"))+
  scale_y_continuous(name="Proportion of alcohol sold",
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=drinkpal)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey90"))+
  guides(color = "none")
  
  dev.off()
  
  #Promo data from Nielsen
read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Price/NielsenDiscounts.xlsx",
             sheet="On Promo %", range="B108:D114") %>% 
  gt(rowname_col="Drink") %>% 
  fmt_percent(columns=c(2,3), decimals=1) %>% 
  cols_width(columns = -Drink ~ px(180)) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table1-PricePromo.png"))

#Modelled health conditions
read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Health/MUPA mortality data 2017-19.xlsx",
           sheet="Example data table", range="A3:B48") %>% 
  set_names("Health Condition", "ICD10 code(s)") %>% 
  gt() %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table2-HealthConditionList.png"))

#Modelled criminal offences and baseline volumes
read_excel("X:/HAR_PR/PR/NI_MUP_2022/Data/Crime/Crime Inputs Full Revised.xlsm",
           sheet="Volumes", range="A2:F20") %>% 
  mutate(EstVol=Mean*Multiplier) %>% 
  select(Offence, Mean, Multiplier, EstVol) %>% 
  gt(rowname_col="Offence") %>% 
  tab_row_group("Violent crimes", rows=c(1:4, 18)) %>% 
  tab_row_group("Theft", rows=c(8:16)) %>% 
  tab_row_group("Sexual offences", rows=17) %>% 
  tab_row_group("Robbery", rows=c(6,7)) %>% 
  tab_row_group("Criminal damage", rows=5) %>% 
  fmt_number(columns=c(2,4), decimals=0) %>% 
  fmt_number(columns=c(3), decimals=1) %>% 
  cols_label(Mean="Recorded Offences", EstVol="Estimated Total Offences") %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  cols_width(columns = Offence ~ px(300)) %>% 
  gtsave(paste0(outputfolder, "/Table3-CrimeOffences.png"))

####################################################
#Sensitivity Analyses

SAPolicies <- c("50p MUP", "50p MUP Sousa", "50p MUP Pryce", "Promotion ban",
                "Promotion ban Sousa", "Promotion ban Pryce")

#Read in consumption and spending impacts
for (i in 1:length(SAPolicies)) {
  
  assign(paste0("SA_p_", i), read_excel(paste0(folder, "/", SAPolicies[i], "/SAPM3_P2C_Results.xlsx"), 
                                     range="B45:BV66", col_names=FALSE) %>%
           select(c(1,2,51,52,56,57,61,62,66,67,71:73)) %>% 
           set_names(colnames(Baseline_cons)) %>% 
           filter(!is.na(Population)) %>% 
           mutate(Metric=rep(c("Cons", "Spend"), each=10)) %>% 
           group_by(Metric) %>% 
           summarise(across(.cols=c(2:13), sum), .groups="drop") %>% 
           gather(Group, Value, c(2:13)) %>% 
           spread(Metric, Value) %>% 
           mutate(Policy=SAPolicies[i]))
}

SA_cons <- bind_rows(SA_p_1, SA_p_2, SA_p_3, SA_p_4, SA_p_5, SA_p_6) %>% 
  mutate(Group=case_when(
    Group=="Mod" ~ "Moderate", Group=="Haz" ~ "Hazardous", Group=="Harm" ~ "Harmful",
    Group=="Income-1Quintile" ~ "IMDQ1 (least deprived)",
    Group=="Income-2Quintile" ~ "IMDQ2",
    Group=="Income-1Quintile-Mod" ~ "IMDQ1 - Mod",
    Group=="Income-2Quintile-Mod" ~ "IMDQ2 - Mod",
    Group=="Income-1Quintile-Haz" ~ "IMDQ1 - Haz",
    Group=="Income-2Quintile-Haz" ~ "IMDQ2 - Haz",
    Group=="Income-1Quintile-Harm" ~ "IMDQ1 - Harm",
    Group=="Income-2Quintile-Harm" ~ "IMDQ2 - Harm", TRUE ~ Group))

#Read in health impacts
for (i in 1:length(SAPolicies)) {
  
  assign(paste0("SA_h_", i), read_excel(paste0(folder, "/", SAPolicies[i], "/SAPM3_C2HHealth_Results.xlsx"), 
                                     sheet="Summary by subgroups",
                                     range="B4:W81", col_names=FALSE) %>%
           select(c(1:7, 11,12, 16, 17, 21, 22)) %>% 
           set_names("Outcome", "Population", "Moderate", "Harmful", "Hazardous", 
                     "IMDQ1 (least deprived)", "IMDQ2", "IMDQ1 - Mod", "IMDQ2 - Mod",
                     "IMDQ1 - Haz", "IMDQ2 - Haz", "IMDQ1 - Harm", "IMDQ2 - Harm") %>% 
           mutate(Policy=SAPolicies[i]))
}

SA_health <- bind_rows(SA_h_1, SA_h_2, SA_h_3, SA_h_4, SA_h_5, SA_h_6) %>% 
  filter(substr(Outcome, 1, 4)=="Full") %>% 
  mutate(Measure=case_when(
      grepl("Death", Outcome) ~ "Deaths",
      grepl("Cost", Outcome) ~ "Costs",
      grepl("Sick", Outcome) ~ "Prevalence",
      grepl("Adm", Outcome) ~ "Admissions",
      grepl("QALY", Outcome) ~ "QALYs")) %>% 
  group_by(Measure, Policy) %>% 
  summarise(across(.cols=c(2:13), ~sum(.x)), .groups="drop") %>% 
  gather(Group, Value, c(3:14)) %>% 
  spread(Measure, Value)

#Read in crime impacts
for (i in 1:length(SAPolicies)) {
  
  assign(paste0("SA_cr_", i), read_excel(paste0(folder, "/", SAPolicies[i], "/SAPM3_C2HCrime_Results.xlsx"), 
                                      sheet="Summary by subgroups",
                                      range="B3:F13") %>%
           mutate(Policy=SAPolicies[i]))
}

SA_crime <- bind_rows(SA_cr_1, SA_cr_2, SA_cr_3, SA_cr_4, SA_cr_5, SA_cr_6) %>% 
  mutate(Metric=rep(c("Offences", "Cost"), each=5, times=length(SAPolicies))) %>% 
  group_by(Policy, Metric) %>% 
  summarise(across(.cols=c(2:5), ~sum(.x)), .groups="drop") %>% 
  gather(Group, Value, c(3:6)) %>% 
  spread(Metric, Value)

#Read in workplace impacts
for (i in 1:length(SAPolicies)) {
  
  assign(paste0("SA_w_", i), read_excel(paste0(folder, "/", SAPolicies[i], "/SAPM3_C2HWorkplace_Results.xlsx"), 
                                     range="C3:F5") %>%
           mutate(Policy=SAPolicies[i]))
}

SA_work <- bind_rows(SA_w_1, SA_w_2, SA_w_3, SA_w_4, SA_w_5, SA_w_6) %>% 
  mutate(Metric=rep(c("Absences", "Cost"), times=length(SAPolicies))) %>% 
  gather(Group, Value, c(1:4)) %>% 
  spread(Metric, Value)

SA_data <- merge(SA_cons, SA_health) %>% 
  merge(SA_crime %>% rename("CrimeCosts"="Cost"), all.x=TRUE) %>% 
  merge(SA_work %>% rename("WorkCosts"="Cost"), all.x=TRUE) %>% 
  mutate(PolicyGroup=if_else(substr(Policy, 1, 3)=="50p", "MUP", "Promotion Ban"),
         Scenario=case_when(
           word(Policy, -1)=="Pryce" ~ "Pryce elasticities",
           word(Policy, -1)=="Sousa" ~ "Sousa elasticities",
           TRUE ~ "Baseline"),
         across(.cols=c("Costs", "CrimeCosts", "WorkCosts"), ~.x/1000000))

#Population level analysis
agg_png(paste0(outputfolder, "/Fig24-SASummary.png"), units="in", width=10, height=7, res=600)
SA_data %>% 
  filter(Group=="Population") %>% 
  gather(Metric, Value, c(3:13)) %>% 
  filter(! Metric %in% c("Prevalence", "QALYs")) %>% 
  mutate(Metric=case_when(
    Metric=="Absences" ~ "Workplace absence days", Metric=="Admissions" ~ "Hospital admissions",
    Metric=="Cons" ~ "Mean consumption", Metric=="Costs" ~ "Hospital costs (£m)",
    Metric=="CrimeCosts" ~ "Crime costs (£m)", Metric=="Deaths" ~ "Deaths",
    Metric=="Offences" ~ "Criminal offences", Metric=="Spend" ~ "Spending on alcohol",
    Metric=="WorkCosts" ~ "Workplace costs (£m)"),
    Metric=factor(Metric, levels=c("Mean consumption", "Spending on alcohol", 
                                   "Deaths", "Hospital admissions", "Hospital costs (£m)",
                                   "Criminal offences", "Crime costs (£m)",
                                   "Workplace absence days", "Workplace costs (£m)"))) %>% 
  ggplot(aes(x=Value, y=PolicyGroup, fill=fct_rev(Scenario)))+
  geom_vline(xintercept=0, colour="grey20")+
  geom_col(position="dodge")+
  scale_x_continuous(name="Estimated policy impact")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="Scenario",
                         guide=guide_legend(reverse=TRUE))+
  facet_wrap(~Metric, scales="free_x")+
  theme_custom()

dev.off()

SA_data %>% 
  filter(Group=="Population") %>% 
  gather(Metric, Value, c(3:13)) %>% 
  filter(! Metric %in% c("Prevalence", "QALYs")) %>% 
  mutate(Metric=case_when(
    Metric=="Absences" ~ "Annual workplace absence days", Metric=="Admissions" ~ "Annual hospital admissions (full effect)",
    Metric=="Cons" ~ "Mean consumption (units/drinker/year)", Metric=="Costs" ~ "Annual hospital costs (full effect) (£m)",
    Metric=="CrimeCosts" ~ "Annual crime costs (£m)", Metric=="Deaths" ~ "Annual deaths (full effect)",
    Metric=="Offences" ~ "Annual criminal offences", Metric=="Spend" ~ "Mean annual spending on alcohol",
    Metric=="WorkCosts" ~ "Annual workplace costs (£m)"),
    Metric=factor(Metric, levels=c("Mean consumption (units/drinker/year)", 
                                   "Mean annual spending on alcohol", 
                                   "Annual deaths (full effect)", 
                                   "Annual hospital admissions (full effect)", 
                                   "Annual hospital costs (full effect) (£m)",
                                   "Annual criminal offences", "Annual crime costs (£m)",
                                   "Annual workplace absence days", 
                                   "Annual workplace costs (£m)"))) %>% 
  select(-c(Group, Policy)) %>% 
  spread(Scenario, Value) %>% 
  arrange(Metric) %>% 
  gt(rowname_col="PolicyGroup", groupname_col="Metric") %>% 
  tab_spanner(columns=c("Baseline":"Sousa elasticities"), label="Change in outcome") %>% 
  fmt_currency(columns=c("Baseline":"Sousa elasticities"),
               rows=c(3,4,9,10,13,14,17,18), currency="GBP", decimals=1) %>% 
  fmt_number(columns=c("Baseline":"Sousa elasticities"),
              rows=c(1,2), decimals=1) %>% 
  fmt_number(columns=c("Baseline":"Sousa elasticities"),
              rows=c(5:8,11,12,15,16), decimals=0) %>% 
  opt_stylize(style=6, color="cyan") %>% 
  opt_table_font(font="Lato") %>% 
  gtsave(paste0(outputfolder, "/Table32-SASummary.png"))

SA_data %>% 
  filter(Group %in% c("Moderate", "Hazardous", "Harmful")) %>% 
  select(Group, PolicyGroup, Scenario, Cons, Deaths) %>% 
  mutate(Group=factor(Group, levels=c("Moderate", "Hazardous", "Harmful"))) %>% 
  ggplot(aes(x=Cons, y=PolicyGroup, fill=fct_rev(Scenario)))+
  geom_vline(xintercept=0, colour="grey30")+
  geom_col(position="dodge")+
  scale_x_continuous(name="Change in mean consumption\n(units/drinker/year)")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="Scenario",
                         guide=guide_legend(reverse=TRUE))+
  facet_wrap(~Group, scales="free_x")+
  theme_custom()



