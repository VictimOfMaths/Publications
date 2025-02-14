rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ragg)
library(scales)
library(ggrepel)
library(RcppRoll)

#Set common font for all plots
font <- "Lato"

options(scipen=9999999)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          panel.grid.major.y=element_line(colour="grey95"),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Read in CPI figures from https://data.cso.ie/table/CPM16
CPIdata <- read.csv("Data/ROI Affordability/ROICPIJan25.csv") %>% 
  dplyr::select(2,3,5) %>% 
  set_names(c("Date", "Index", "Value")) %>% 
  #Remove all items as want longer time series with more recent data
  filter(Index!="All items") %>% 
  #Combine with overall alcohol CPI figures from https://data.cso.ie/table/CPM03
  bind_rows(read.csv("Data/ROI Affordability/ROICPIAlcoholJan25.csv") %>% 
              dplyr::select(2,3,5) %>% 
              set_names(c("Date", "Index", "Value")),
            #Combine with overall CPI that is more up to date than in the disaggregated data https://data.cso.ie/table/CPM01
            read.csv("Data/ROI Affordability/ROICPIOverallJan25.csv") %>% 
              dplyr::select(2,3,5) %>% 
              set_names(c("Date", "Index", "Value"))) %>% 
  mutate(Date=as.Date(paste0("01", Date), "%d %Y %B")) %>% 
  filter(Date>=as.Date("2003-01-01")) %>% 
  #rebase with oldest data as 100
  group_by(Index) %>% 
  mutate(Value=Value/Value[Date==as.Date("2003-01-01")]) %>% 
  ungroup() %>% 
  mutate(Index=case_when(
    Index=="Alcohol" ~ "All alcohol",
    Index=="Alcoholic beverages - off licences" ~ "Off-trade alcohol",
    Index=="Licensed premises" ~ "On-trade alcohol",
    Index=="Beer - licensed premises" ~ "On-trade beer",
    Index=="Spirits - licensed premises" ~ "On-trade spirits",
    Index=="Wine - licensed premises" ~ "On-trade wine",
    Index=="Beer - off licences" ~ "Off-trade beer",
    Index=="Spirits - off licences" ~ "Off-trade spirits",
    Index=="Wine - off licences" ~ "Off-trade wine",
    TRUE ~ "All items"),
    Channel=case_when(
      substr(Index,1,2)=="On" ~ "On-trade",
      substr(Index,1,3)=="Off" ~ "Off-trade"),
    Beverage=case_when(
      Channel=="On-trade" ~ substr(Index, 10,20),
      Channel=="Off-trade" ~ substr(Index, 11, 21)))

#Plot CPI figures
#Alcohol vs. all-item
agg_png("Outputs/ROIInflationAllAlc.png", units="in", width=8, height=6, res=800)
ggplot(CPIdata %>% filter(Index %in% c("All items", "All alcohol")),
       aes(x=Date, y=Value, colour=Index, linetype=Index))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line( show.legend=FALSE)+
  geom_text_repel(data=. %>% filter(Date==max(Date)),
                  aes(label = Index), family = "Lato", fontface = "bold", 
                  direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2023-05-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("2003-01-01"), as.Date("2027-12-01")),
               breaks=as.Date(c("2003-01-01", "2008-01-01", "2013-01-01", "2018-01-01",
                                "2023-01-01")), labels=c("2003", "2008", "2013", "2018", "2023"))+
  scale_y_continuous(trans="log", name="CPI prices relative to January 2003\n(log scale)",
                     breaks=c(1, 1.1, 1.2, 1.3, 1.4), labels=c("No change", "+10%", "+20%", "+30%", "+40%"))+
  scale_colour_manual(values=c("#0099D5", "black"))+
  scale_linetype_manual(values=c(1,2))+
  theme_custom()+
  labs(title="Alcohol prices in Ireland have kept pace with inflation",
       subtitle="CPI prices for alcohol compared to overall inflation relative to January 2003",
       caption="Data from CSO")+
  theme(axis.line.x=element_blank())

dev.off()

#On vs off-trade
agg_png("Outputs/ROIInflationxChannel.png", units="in", width=8, height=6, res=800)
ggplot(CPIdata %>% filter(Index %in% c("On-trade alcohol", "Off-trade alcohol")),
       aes(x=Date, y=Value, colour=Index))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line( show.legend=FALSE)+
  geom_text_repel(data=. %>% filter(Date==max(Date)),
                  aes(label = Index), family = "Lato", fontface = "bold", 
                  direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2024-01-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  geom_vline(xintercept=as.Date("2022-01-01"), colour="grey70", linetype=2)+
  scale_x_date(name="", limits=c(as.Date("2003-01-01"), as.Date("2027-12-01")),
               breaks=as.Date(c("2003-01-01", "2008-01-01", "2013-01-01", "2018-01-01",
                                "2023-01-01")), labels=c("2003", "2008", "2013", "2018", "2023"))+  
  scale_y_continuous(trans="log", name="CPI prices relative to January 2003\n(log scale)",
                     breaks=c(0.8, 1, 1.2, 1.4, 1.6),
                     labels=c("-20%", "No change", "+20%", "+40%", "+60%"))+
  scale_colour_manual(values=c("#41ead4", "#ff206e"))+
  theme_custom()+
  labs(title="The price of shop-bought alcohol is the same as it was 20 years ago",
       subtitle="CPI prices for on- and off-trade alcohol relative to January 2003",
       caption="Data from CSO")+
  theme(axis.line.x=element_blank())+
  annotate("text", x=as.Date("2022-06-01"), y=1.19, angle=90, colour="grey70", 
           label="MUP introduced", family="Lato")

dev.off()

#Beverage-specific panels
agg_png("Outputs/ROIInflationxChannelxBev.png", units="in", width=10, height=6, res=800)
ggplot(CPIdata %>% filter(!Index %in% c("On-trade alcohol", "Off-trade alcohol",
                                        "All alcohol", "All items")),
       aes(x=Date, y=Value, colour=Beverage))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="CPI prices relative to January 2003\n(log scale)",
                     breaks=c(0.8, 1, 1.2, 1.4, 1.6),
                     labels=c("-20%", "No change", "+20%", "+40%", "+60%"))+
  scale_colour_manual(name="", labels=c("Beer", "Spirits", "Wine"),
                      values=c("#F7AA14", "#0099D5", "#C70E7B"))+
  geom_vline(xintercept=as.Date("2022-01-01"), colour="grey70", linetype=2)+
  facet_wrap(~Channel)+
  theme_custom()+
  labs(title="Prices have risen more for spirits and beer than wine",
       subtitle="CPI prices for on- and off-trade alcohol relative to January 2003",
       caption="Data from CSO")+
  theme(axis.line.x=element_blank())

dev.off()

#Download disposable income data from EU-SILC
#Data cobbled together from https://data.cso.ie/table/SIA12 (2004-19)
#and https://www.cso.ie/en/releasesandpublications/ep/p-silc/surveyonincomeandlivingconditionssilc2022/householdincome/ table 2.1c
SILCdata <- read.csv("Data/ROI Affordability/SILCIncomes.csv") %>% 
  dplyr::select(Year, VALUE) %>% 
  rename("Income"="VALUE") %>% 
  #Download annual CPI inflation data from https://data.cso.ie/table/CPA01
  merge(read.csv("Data/ROI Affordability/ROICPIAnnual.csv") %>% 
          dplyr::select(Year, VALUE) %>% 
          rename("CPI"="VALUE")) %>% 
  #Bring in annual alcohol CPI inflation from https://data.cso.ie/table/CPA02
  merge(read.csv("Data/ROI Affordability/ROICPIAnnualAlcohol.csv") %>% 
          dplyr::select(Year, VALUE) %>% 
          rename("Alcohol"="VALUE")) %>% 
  #No annual series available from sub-indices, so take straight average of
  #monthly figures to approximate annual values
  merge(CPIdata %>% filter(!is.na(Channel)) %>% 
          mutate(Year=year(Date)) %>% 
          group_by(Index, Year) %>% 
          summarise(VALUE=mean(Value), .groups="drop") %>% 
          spread(Index, VALUE)) %>% 
  gather(Index, Value, c(4:12)) %>% 
  #Rebase everything to 2004
  group_by(Index) %>% 
  mutate(Income=Income*100/Income[Year==2004],
         CPI=CPI*100/CPI[Year==2004],
         Value=Value*100/Value[Year==2004]) %>% 
  ungroup() %>% 
  mutate(RAPI=Value*100/CPI,
         Affordability=Income*100/RAPI,
         Channel=case_when(
           substr(Index,1,2)=="On" ~ "On-trade",
           substr(Index,1,3)=="Off" ~ "Off-trade"),
         Beverage=case_when(
           Channel=="On-trade" ~ substr(Index, 10,20),
           Channel=="Off-trade" ~ substr(Index, 11, 21)))

#Repeat the inflation graphs using affordability
#Overall and by channel
agg_png("Outputs/ROIAffordabilityxChannel.png", units="in", width=8, height=6, res=800)
ggplot(SILCdata %>% filter(Index %in% c("Alcohol", "On-trade alcohol", "Off-trade alcohol")),
       aes(x=Year, y=Affordability/100, colour=Index))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=. %>% filter(Year==max(Year)),
                  aes(label = Index), family = "Lato", fontface = "bold", 
                  direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(2023.1, NA), show.legend=FALSE, segment.color = NA)+
  scale_x_continuous(name="", limits=c(2004, 2027))+
  scale_y_continuous(name="Change in alcohol affordability since 2004", trans="log",
                     breaks=c(0.75,1,1.25,1.5,1.75),
                     labels=c("-25%", "No change", "+25%", "+50%", "+75%"),
                     limits=c(0.75, 1.9))+
  scale_colour_manual(values=c("black", "#41ead4", "#ff206e"))+
  theme_custom()+
  labs(title="Off-trade alcohol has become much more affordable in the last decade",
       subtitle="Alcohol affordability in Ireland since 2004 (higher = more affordable). Affordability is calculated as\nthe ratio of median household disposable income to the relative price of alcohol vs. overall CPI inflation",
       caption="Data from CSO")+
  theme(axis.line.x=element_blank())

dev.off()

#Beverage-specific panels
agg_png("Outputs/ROIAffordabilityxChannelxBev.png", units="in", width=10, height=6, res=800)
ggplot(SILCdata %>% filter(!Index %in% c("On-trade alcohol", "Off-trade alcohol",
                                        "Alcohol")),
       aes(x=Year, y=Affordability/100, colour=Beverage))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Change in alcohol affordability since 2004", trans="log",
                     breaks=c(0.75,1,1.25,1.5,2),
                     labels=c("-25%", "No change", "+25%", "+50%", "+100%"),
                     limits=c(0.75, 2.2))+
  scale_colour_manual(name="", labels=c("Beer", "Spirits", "Wine"),
                      values=c("#F7AA14", "#0099D5", "#C70E7B"))+
  facet_wrap(~Channel)+
  theme_custom()+
  labs(title="Wine has become a lot more affordable",
       subtitle="Alcohol affordability in Ireland since 2004 (higher = more affordable). Affordability is calculated as\nthe ratio of median household disposable income to the relative price of alcohol vs. overall CPI inflation",
       caption="Data from CSO")+
  theme(axis.line.x=element_blank())

dev.off()

#Time series of alcohol duty rates
#Current rates from: https://www.revenue.ie/en/companies-and-charities/excise-and-licences/excise-duty-rates/alcohol-products-tax.aspx
#Older rates from: https://www.drugsandalcohol.ie/19131/
DutyRates <- tibble(Date=seq.Date(from=as.Date("1994-01-01"), 
                                  to=as.Date("2024-12-01"),by="month"),
                    Beer=case_when(
                      Date>=as.Date("2013-10-01") ~ 22.55,
                      Date>=as.Date("2009-12-01") ~ 15.71,
                      Date>=as.Date("1994-10-01") ~ 19.87),
                    Cider=case_when(
                      Date>=as.Date("2013-10-01") ~ 94.46,
                      Date>=as.Date("2009-12-01") ~ 65.86,
                      Date>=as.Date("1994-10-01") ~ 44.48),
                    Spirits=case_when(
                      Date>=as.Date("2013-10-01") ~ 42.57,
                      Date>=as.Date("2009-12-01") ~ 31.13,
                      Date>=as.Date("1994-10-01") ~ 27.72),
                    Wine=case_when(
                      Date>=as.Date("2013-10-01") ~ 424.84,
                      Date>=as.Date("2009-12-01") ~ 262.24,
                      Date>=as.Date("1994-10-01") ~ 273.00)) %>% 
  #Convert all rates to rates per std. drink, assuming wine ABV of 12.5%
  #and cider ABV of 4%
  mutate(Beer=Beer*(10/8)/100,
         Cider=Cider*(10/8)/(0.04*10000),
         Spirits=Spirits*(10/8)/100,
         Wine=Wine*(10/8)/(0.125*10000)) %>% 
  merge(read.csv("Data/ROI Affordability/ROICPIOverallJan25.csv") %>% 
          dplyr::select(2,5) %>% 
          set_names(c("Date", "CPI")) %>% 
          mutate(Date=as.Date(paste0("01", Date), "%d %Y %B"))) %>% 
  #Add real-terms in current prices
  gather(Beverage, DutyRate, c(2:5)) %>% 
  group_by(Beverage) %>% 
  mutate(inflator=CPI[Date==max(Date)]/CPI,
         DutyRate_adj=DutyRate*inflator) %>% 
  ungroup()

#Cash terms plot
agg_png("Outputs/ROIDutyRatesCash.png", units="in", width=8, height=6, res=800)
ggplot(DutyRates, aes(x=Date, y=DutyRate, colour=Beverage))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Duty payable per std. drink",
                     labels=label_dollar(prefix="€"), limits=c(0,NA))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"),
                      name="")+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="Alcohol duty rates are higher than they were 20 years ago",
       subtitle="Duty rates per standard drink (10g ethanol)",
       caption="Data from Revenue.ie and HRB")

dev.off()

agg_png("Outputs/ROIDutyRatesReal.png", units="in", width=8, height=6, res=800)
ggplot(DutyRates, aes(x=Date, y=DutyRate_adj, colour=Beverage))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Duty payable per std. drink (2024 prices)",
                     labels=label_dollar(prefix="€"), limits=c(0,NA))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"),
                      name="")+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="Inflation-adjusted duty rates have fallen sharply since 2020",
       subtitle="Real-terms duty rates per standard drink (10g ethanol) in 2024 prices",
       caption="Data from CSO, Revenue.ie and HRB")

dev.off()

#Write the data out
CPIdata %>% 
  select(Date, Index, Value) %>% 
  spread(Index, Value) %>% 
  write.csv("Data/ROI Affordability/OutputsCPIData.csv", row.names=FALSE)

DutyRates %>% 
  select(Date, Beverage, DutyRate, DutyRate_adj) %>% 
  set_names(c("Date", "Beverage", "CashTerms", "RealTerms")) %>% 
  pivot_wider(names_from=Beverage, values_from=c("CashTerms", "RealTerms")) %>% 
  filter(Date>=as.Date("1996-11-01")) %>% 
  write.csv("Data/ROI Affordability/OutputsDutyRateData.csv", row.names=FALSE)

SILCdata %>% 
  select(-c(Channel, Beverage, RAPI)) %>% 
  set_names(c("Year", "Income", "AllItemCPI", "Product", "SpecificCPI", "Affordability")) %>% 
  pivot_wider(names_from=Product, values_from=c(Income, AllItemCPI, SpecificCPI, Affordability),
              names_glue = "{Product}_{.value}") %>% 
  write.csv("Data/ROI Affordability/OutputsAffordabilityData.csv", row.names=FALSE)

#############################
#Analysis of sales volumes from Revenue.ie data
#Read in data from Revenue.ie
temp <- tempfile()
source <- "https://www.revenue.ie/en/corporate/documents/statistics/excise/quarterly-alcohol-breakdown.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read.csv(temp)

finaldata <- rawdata %>% 
  #Convert to litres of ethanol using standard ABV assumptions
  mutate(AlcVol=case_when(
    type_of_alcohol=="Wine" ~ net_duty_paid_quantities*0.125,
    type_of_alcohol=="Cider" ~ net_duty_paid_quantities*0.04,
    TRUE ~ net_duty_paid_quantities),
    Date=as.Date(paste0(year, "-", case_when(
      quarter=="Q1" ~ "01-01",
      quarter=="Q2" ~ "04-01",
      quarter=="Q3" ~ "07-01",
      TRUE ~ "10-01")))) %>% 
  arrange(Date) %>% 
  group_by(type_of_alcohol) %>% 
  mutate(Index=1:(nrow(.)/4)) %>% 
  ungroup() %>% 
  dplyr::select(Date, quarter, Index, type_of_alcohol, AlcVol) %>% 
  spread(type_of_alcohol, AlcVol) %>% 
  mutate(Total=Beer+Cider+Wine+Spirits,
         BeerProp=Beer/Total,
         CiderProp=Cider/Total,
         WineProp=Wine/Total,
         SpiritsProp=Spirits/Total)

#Read in population data from https://data.cso.ie/table/PEA01
#Pop <- read.csv("Data/ROI Affordability/ROIPopulation.csv")
Pop <- read.csv("Data/ROI Affordability/ROIPopulation15Plus.csv")

percapdata <- finaldata %>% 
  mutate(Year=year(Date)) %>% 
  merge(Pop %>% mutate(quarter="Q2") %>% 
          select(c(Year, quarter, VALUE)), by=c("Year", "quarter"), all.x=TRUE) %>% 
  mutate(Pop=zoo::na.approx(VALUE, maxgap=4, rule=2)*1000) %>% 
  gather(Bev, AlcVol, c(5:9)) %>% 
  mutate(PCC=AlcVol/Pop)

agg_png("Outputs/ROIRevenueAlcVolRollOverall.png", units="in", height=6, width=8, res=800)
finaldata %>%
  dplyr::select(Date, Total) %>% 
  mutate(AlcRoll=4*roll_mean(Total, n=4, align="right", fill=NA)) %>% 
  ggplot(aes(x=Date, y=AlcRoll/1000000))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(colour="tomato")+
  #geom_point(aes(y=Total/1000000), shape=21, colour="grey70")+
  scale_x_date(name="", limits=c(as.Date("2008-01-01"), NA_Date_))+
  scale_y_continuous(name="Total litres of alcohol cleared for sale (millions)\n(rolling 12-month average)")+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="The total volume of alcohol sold in Ireland has remained fairly stable",
       subtitle="Rolling 12-month average of total alcohol cleared for sale in Ireland.\nFigures for wine and cider are estimated from total product volumes.",
       caption="Data from Revenue")

dev.off()

agg_png("Outputs/ROIRevenueAlcVolRollOverallPerCapita.png", units="in", height=6, width=8, res=800)
percapdata %>%
  filter(Bev=="Total") %>% 
  mutate(AlcRoll=4*roll_mean(PCC, n=4, align="right", fill=NA)) %>% 
  ggplot(aes(x=Date, y=AlcRoll))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(colour="tomato")+
  #geom_point(aes(y=PCC), shape=21, colour="grey70")+
  scale_x_date(name="", limits=c(as.Date("2008-01-01"), NA_Date_))+
  scale_y_continuous(name="Total litres of alcohol cleared for sale per capita\n(rolling 12-month average)")+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="The total volume of alcohol sold per adult in Ireland has fallen",
       subtitle="Rolling 12-month average of total alcohol cleared for sale in Ireland per adult (aged 15+)\nFigures for wine and cider are estimated from total product volumes.\n",
       caption="Data from Revenue and CSO")

dev.off()

agg_png("Outputs/ROIRevenueAlcVol.png", units="in", height=6, width=8, res=800)
finaldata %>%
  gather(Bev, AlcVol, c(4:7)) %>% 
  ggplot(aes(x=Date, y=AlcVol/1000000, colour=Bev))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_date(name="", limits=c(as.Date("2008-01-01"), NA_Date_))+
  scale_y_continuous(name="Total litres of alcohol cleared for sale (millions)\n(rolling 12-month average)")+
  scale_colour_manual(name="", values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"))+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="Beer sales fell during the pandemic, wine sales rose",
       subtitle="Rolling 12-month average of total alcohol cleared for sale in Ireland\nFigures for wine and cider are estimated from total product volumes.",
       caption="Data from Revenue")

dev.off()

agg_png("Outputs/ROIRevenueAlcVolRoll.png", units="in", height=6, width=8, res=800)
finaldata %>%
  gather(Bev, AlcVol, c(4:7)) %>% 
  group_by(Bev) %>% 
  mutate(AlcRoll=roll_mean(AlcVol, n=4, align="right", fill=NA)) %>% 
  ggplot(aes(x=Date, y=AlcRoll/1000000, colour=Bev))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_date(name="", limits=c(as.Date("2008-01-01"), NA_Date_))+
  scale_y_continuous(name="Total litres of alcohol cleared for sale (millions)\n(rolling 12-month average)")+
  scale_colour_manual(name="", values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"))+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="Beer sales fell during the pandemic, wine sales rose",
       subtitle="Rolling 12-month average of alcohol cleared for sale in Ireland\nFigures for wine and cider are estimated from total product volumes.",
       caption="Data from Revenue")

dev.off()

agg_png("Outputs/ROIRevenueAlcVolRollPerCapita.png", units="in", height=6, width=8, res=800)
percapdata %>%
  filter(Bev!="Total") %>% 
  group_by(Bev) %>% 
  mutate(AlcRoll=4*roll_mean(PCC, n=4, align="right", fill=NA)) %>% 
  ggplot(aes(x=Date, y=AlcRoll, colour=Bev))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_date(name="", limits=c(as.Date("2008-01-01"), NA_Date_))+
  scale_y_continuous(name="Total litres of alcohol cleared for sale per capita\n(rolling 12-month average)")+
  scale_colour_manual(name="", values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"))+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="Beer sales fell during the pandemic, wine sales rose",
       subtitle="Rolling 12-month average of alcohol cleared for sale in Ireland per adult (aged 15+).\nFigures for wine and cider are estimated from total product volumes.",
       caption="Data from Revenue and CSO")

dev.off()


