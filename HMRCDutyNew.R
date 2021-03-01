rm(list=ls())

library(tidyverse)
library(readODS)
library(curl)
library(lubridate)
library(paletteer)
library(cowplot)
library(RcppRoll)
library(ggtext)

#Read in data from HMRC Alcohol Bulletin https://www.gov.uk/government/statistics/alcohol-bulletin
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/964841/2021_Jan_Alc_Tabs.ods"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

###########
#Wine data#
###########
raw.wine <- read_ods(temp, sheet="Wine_statistics", range="A51:J312", col_names=FALSE)[,c(1:4,8,9)] 

colnames(raw.wine) <- c("Month", "Still", "Sparkling", "Fortified", "Total", 
                        "Receipts")

wine <- raw.wine %>% 
  mutate(Product="Wine", Product2="Wine") %>% 
  pivot_longer(cols=c(2:5), names_to="Source", values_to="Clearances.Product") %>% 
  #Make some ABV assumptions to get to alcohol volumes
  mutate("Clearances.Alcohol"=case_when(
    Source=="Still" ~ Clearances.Product*0.125,
    Source=="Sparkling" ~ Clearances.Product*0.125,
    Source=="Fortified" ~ Clearances.Product*0.17
  ))

#Estimate total alcohol clearances
temp.wine <- wine %>% 
  filter(Source!="Total") %>% 
  group_by(Month) %>% 
  summarise(Clearances.Alcohol=sum(Clearances.Alcohol)) %>% 
  mutate(Source="Total")

wine <- merge(wine, temp.wine, by=c("Source", "Month"), all=TRUE) %>% 
  mutate(Clearances.Alcohol=coalesce(Clearances.Alcohol.x, Clearances.Alcohol.y)) %>% 
  select(-c("Clearances.Alcohol.x", "Clearances.Alcohol.y")) %>% 
  #Put into common units - receipts in £m, volumes (product and alcohol) in millions of litres
  mutate(Clearances.Product=Clearances.Product*100/1000000, Clearances.Alcohol=Clearances.Alcohol*100/1000000)

################
#Made wine data#
################
raw.madewine <- read_ods(temp, sheet="Made_wine_statistics", range="A51:K312", col_names=FALSE)[,c(1:4,9)]

colnames(raw.madewine) <- c("Month", "Low", "Still_High", "Sparkling_High", "Total")

madewine <- raw.madewine %>% 
  mutate(Product="Wine", Product2="MadeWine") %>% 
  pivot_longer(cols=c(2:5), names_to="Source", values_to="Clearances.Product") %>% 
  #Make some ABV assumptions to get to alcohol volumes
  mutate("Clearances.Alcohol"=case_when(
    Source=="Low" ~ Clearances.Product*0.045,
    Source=="Still_High" ~ Clearances.Product*0.125,
    Source=="Sparkling_High" ~ Clearances.Product*0.125
  ))

#Estimate total alcohol clearances
temp.madewine <- madewine %>% 
  filter(Source!="Total") %>% 
  group_by(Month) %>% 
  summarise(Clearances.Alcohol=sum(Clearances.Alcohol)) %>% 
  mutate(Source="Total")

madewine <- merge(madewine, temp.madewine, by=c("Source", "Month"), all=TRUE) %>% 
  mutate(Clearances.Alcohol=coalesce(Clearances.Alcohol.x, Clearances.Alcohol.y)) %>% 
  select(-c("Clearances.Alcohol.x", "Clearances.Alcohol.y")) %>% 
  #Put into common units - receipts in £m, volumes (product and alcohol) in millions of litres
  mutate(Clearances.Product=Clearances.Product*100/1000000, Clearances.Alcohol=Clearances.Alcohol*100/1000000)

##############
#Spirits data#
##############
raw.spirits <- read_ods(temp, sheet="Spirits_statistics", range="A53:J314", col_names=FALSE)[,c(1,3,4,6:9)]

colnames(raw.spirits) <- c("Month", "MaltWhisky", "OtherWhisky", "RTDs", "Other", "Total",
                          "Receipts")

spirits <- raw.spirits %>% 
  mutate(Product="Spirits", Product2="Spirits") %>% 
  pivot_longer(cols=c(2:6), names_to="Source", values_to="Clearances.Alcohol") %>% 
  #Make some ABV assumptions to get to product volumes
  mutate("Clearances.Product"=case_when(
    Source=="MaltWhisky" ~ Clearances.Alcohol/0.4,
    Source=="OtherWhisky" ~ Clearances.Alcohol/0.4,
    Source=="RTDs" ~ Clearances.Alcohol/0.045,
    Source=="Other" ~ Clearances.Alcohol/0.375
  ))

#Estimate total product volumes
temp.spirits <- spirits %>% 
  filter(Source!="Total") %>% 
  group_by(Month) %>% 
  summarise(Clearances.Product=sum(Clearances.Product)) %>% 
  mutate(Source="Total")

spirits <- merge(spirits, temp.spirits, by=c("Source", "Month"), all=TRUE) %>% 
  mutate(Clearances.Product=coalesce(Clearances.Product.x, Clearances.Product.y)) %>% 
  select(-c("Clearances.Product.x", "Clearances.Product.y")) %>% 
  #Put into common units - receipts in £m, volumes (product and alcohol) in litres
  mutate(Clearances.Product=Clearances.Product*100/1000000, Clearances.Alcohol=Clearances.Alcohol*100/1000000)

###################
#Beer & Cider data#
###################
raw.beercider <- read_ods(temp, sheet="Beer_and_cider_statistics", range="A51:K312", col_names=FALSE)
beer <- raw.beercider[,c(1:3,6,7,9)]
cider <- raw.beercider[,c(1,8,10)]

colnames(beer) <- c("Month", "Production.Product", "Production.Alcohol", "Clearances.Product",
                        "Clearances.Alcohol", "Receipts")

colnames(cider) <- c("Month", "Clearances.Product", "Receipts")

beer <- beer %>% 
  mutate(Production.ABV=Production.Alcohol/Production.Product,
         Clearances.ABV=Clearances.Alcohol/Clearances.Product,
         Source="Total", Clearances.Product=Clearances.Product*100*1000/1000000,
         Clearances.Alcohol=Clearances.Alcohol*100*1000/1000000,
         Product="Beer", Product2="Beer")

cider <- cider %>% 
  mutate(Clearances.Alcohol=Clearances.Product*0.045,
         Source="Total", Product="Cider", Product2="Cider",
         Clearances.Product=Clearances.Product*100*1000/1000000, 
         Clearances.Alcohol=Clearances.Alcohol*100*1000/1000000)

#Stick together for analysis
data2 <- bind_rows(wine, madewine, spirits, beer, cider) %>% 
  mutate(date=as.Date(paste0("01-",substr(Month,1,6)), format="%d-%b-%y"))

#Inflate receipts
#Read in RPI data - need to edit the to and from months/years in the URL
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/czeq/mm23&series=&fromMonth=01&fromYear=1950&toMonth=01&toYear=2021&frequency=months"
temp2 <- curl_download(url=source, destfile=temp2, quiet=FALSE, mode="wb")

RPIdata <- read.csv(temp2)[-c(1:5),]
colnames(RPIdata) <- c("date", "RPI")

RPIdata <- RPIdata %>% 
  mutate(date=as.Date(paste0(date, " 1"), "%Y %b %d"),
         RPI=(as.numeric(as.character(RPI))+100)/100,
         index=cumprod(RPI),
         inflator=index[length(index)]/index)

data2 <- merge(data2, RPIdata, by="date") %>% 
  #Inflate to Jan 2021 prices
  mutate(Receipts.Adj=Receipts*inflator)

#Compress to shorter data for main analysis
data <- data2 %>% 
  filter(Source=="Total") %>% 
  group_by(date, Product) %>% 
  summarise(Clearances.Product=sum(Clearances.Product),
            Clearances.Alcohol=sum(Clearances.Alcohol),
            Receipts=sum(Receipts, na.rm=TRUE), Receipts.Adj=sum(Receipts.Adj, na.rm=TRUE))

#Add in all alcohol
data <- data %>% 
  group_by(date) %>% 
  summarise(Clearances.Product=sum(Clearances.Product),
            Clearances.Alcohol=sum(Clearances.Alcohol),
            Receipts=sum(Receipts), Receipts.Adj=sum(Receipts.Adj)) %>% 
  mutate(Product="Total") %>% 
  bind_rows(data)

#2020 duty takes vs. 5-year ranges
AlcVol <- data %>%
  filter(date>=as.Date("2015-01-01") & date<as.Date("2020-01-01")) %>%
  group_by(date, Product) %>%
  summarise(Clearances.Product=sum(Clearances.Product),
            Clearances.Alcohol=sum(Clearances.Alcohol),
            Receipts=sum(Receipts), Receipts.Adj=sum(Receipts.Adj)) %>%
  mutate(yearmonth=month(date)) %>%
  group_by(yearmonth, Product) %>%
  summarise(alcvolmax=max(Clearances.Alcohol), alcvolmin=min(Clearances.Alcohol), alcvolmean=mean(Clearances.Alcohol),
            prodvolmax=max(Clearances.Product), prodvolmin=min(Clearances.Product), prodvolmean=mean(Clearances.Product),
            revmax=max(Receipts.Adj), revmin=min(Receipts.Adj), revmean=mean(Receipts.Adj)) %>% 
  ungroup()

AlcVol.2020 <- data %>%
  filter(date>=as.Date("2020-01-01") & date<as.Date("2021-01-01")) %>%
  mutate(yearmonth=month(date)) %>%
  group_by(yearmonth, Product) %>%
  summarise(Clearances.Product=sum(Clearances.Product),
            Clearances.Alcohol=sum(Clearances.Alcohol),
            Receipts=sum(Receipts), Receipts.Adj=sum(Receipts.Adj)) %>% 
  ungroup()

AlcVol <- merge(AlcVol, AlcVol.2020, by=c("yearmonth", "Product"), all.x=TRUE)

#Calculate overall changes from historic values
loss <- AlcVol %>% 
  mutate(alcvolchange=Clearances.Alcohol-alcvolmean,
         prodvolchange=Clearances.Product-prodvolmean,
         revchange=Receipts.Adj-revmean) %>% 
  group_by(Product) %>% 
  summarise(alcvolchange=sum(alcvolchange, na.rm=TRUE), alchist=sum(alcvolmean),
            alcvolprop=alcvolchange/alchist, prodvolchange=sum(prodvolchange, na.rm=TRUE),
            prodhist=sum(prodvolmean), prodvolprop=prodvolchange/prodhist,
            revchange=sum(revchange, na.rm=TRUE), revhist=sum(revmean),
            revprop=revchange/revhist) %>% 
  mutate(vollabs=if_else(alcvolchange>0, paste0("+", round(alcvolchange, 1), " million litres (+",
                                                round(alcvolprop*100, 1), "%)"),
                         paste0(round(alcvolchange, 1), " million litres (",
                                round(alcvolprop*100, 1), "%)")),
         prodlabs=if_else(prodvolchange>0, paste0("+", round(prodvolchange, 1), " million litres (+",
                                                  round(prodvolprop*100, 1), "%)"),
                          paste(round(prodvolchange, 1), " million litres (",
                                round(prodvolprop*100, 1), "%)")),
         revlabs=if_else(revchange>0, paste0("+£", round(revchange, 1), " million (+",
                                                 round(revprop*100, 1), "%) revenue from alcohol duty"),
                         paste("£", round(revchange, 1), " million (",
                               round(revprop*100, 1), "%) revenue from alcohol duty")))

#Visualise
#Totals
tiff("Outputs/HMRCClearancesExcess.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol, ymax=alcvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  annotate("text", x=7, y=53, colour="Red", label="2020")+
  annotate("text", x=3.5, y=54, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=44, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=3.5, y=52.5, xend=3.2, yend=49.5), colour="Skyblue4", curvature=-0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=45.5, xend=10, yend=48), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", x=6, y=20, colour="Red", label=paste0("+", round(loss[4,2], 1), " million litres (+", 
                                                         round(loss[4,4]*100, 1), 
                                                         "%) of alcohol cleared in 2020\ncompared to the 2015-19 average"))+
  labs(title="Total ethanol clearances reported by HMRC",
       subtitle="Data for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#This one is less meaningful since it's adding beer volumes to spirits volumes etc.
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=prodvolmin, ymax=prodvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=prodvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Product, ymax=prodvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Product), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly alcoholic product clearances\n(millions of litres)", limits=c(0,NA))+
  theme_classic()+
  annotate("text", x=9, y=650, colour="Red", label="2020")+
  annotate("text", x=2.2, y=670, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=590, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=2.2, y=650, xend=2.6, yend=600), colour="Skyblue4", curvature=0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=605, xend=10.3, yend=640), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", x=5, y=250, colour="Red", label=paste0(round(loss[4,5], 1), " million litres (", 
                                                         round(loss[4,7]*100, 1), 
                                                         "%) of alcoholic products cleared in 2020\ncompared to the 2015-19 average"))+
  labs(title="Total alcohol clearances (product volumes) reported by HMRC",
       subtitle="Data for Spirits is estimated based on ABV assumptions")


tiff("Outputs/HMRCRevenueExcess.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=revmin, ymax=revmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=revmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Receipts.Adj, ymax=revmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Receipts.Adj), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="HMRC monthly duty receipts\n(£millions)", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  annotate("text", x=6, y=1350, colour="Red", label="2020")+
  annotate("text", x=2.2, y=1150, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=900, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=2.2, y=1110, xend=2.5, yend=1000), colour="Skyblue4", curvature=0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=980, xend=10.4, yend=1060), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", x=6, y=400, colour="Red", label=paste0("£", round(loss[4,8], 1), "million (", 
                                                         round(loss[4,10]*100, 1), 
                                                         "%) revenue from alcohol duty collected in 2020\ncompared to the 2015-19 average"))+
  labs(title="Total HMRC revenue from alcohol duty",
       subtitle="Adjusted to January 2021 prices using the Retail Prices Index", 
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#By product
tiff("Outputs/HMRCClearanceExcessxProd.tiff", units="in", width=10, height=8, res=500)
ggplot(subset(AlcVol, Product!="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol, ymax=alcvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol), colour="Red")+
  geom_text(data=subset(loss, Product!="Total"), aes(x=c(6,6,6,6), y=c(10,7,15,9), label=vollabs), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  facet_wrap(~Product)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Total ethanol clearances reported by HMRC",
       subtitle="Data for Cider and Wine is estimated from product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

ggplot(subset(AlcVol, Product!="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=prodvolmin, ymax=prodvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=prodvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Product, ymax=prodvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Product), colour="Red")+
  geom_text(data=subset(loss, Product!="Total"), aes(x=c(6,6,6,6), y=c(200,150,100,230), label=prodlabs), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly alcoholic product clearances\n(millions of litres)", limits=c(0,NA))+
  facet_wrap(~Product)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Total alcohol clearances (product volumes) reported by HMRC",
       subtitle="Data for Spirits is estimated based on ABV assumptions")

tiff("Outputs/HMRCRevenueExcessxProd.tiff", units="in", width=10, height=8, res=500)
ggplot(subset(AlcVol, Product!="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=revmin, ymax=revmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=revmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Receipts.Adj, ymax=revmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Receipts.Adj), colour="Red")+
  geom_text(data=subset(loss, Product!="Total"), aes(x=c(6,6,6,6), y=c(600,100,460,200), label=revlabs), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="HMRC monthly duty receipts\n(£millions)", limits=c(0,NA))+
  facet_wrap(~Product)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Total HMRC revenue from alcohol duty",
       subtitle="Adjusted to January 2021 prices using the Retail Prices Index",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Generate rolling averages in long-term data
data <- data %>% 
  group_by(Product) %>% 
  mutate(Clearances.Product_roll=roll_mean(Clearances.Product, 12, align="right", fill=NA),
         Clearances.Alcohol_roll=roll_mean(Clearances.Alcohol, 12, align="right", fill=NA),
         Receipts_roll=roll_mean(Receipts, 12, align="right", fill=NA),
         Receipts.Adj_roll=roll_mean(Receipts.Adj, 12, align="right", fill=NA)) %>% 
  ungroup()

#Plot cash vs. real terms overall duty revenue
tiff("Outputs/HMRCRevenueTotal.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, Product=="Total" & date<as.Date("2020-01-01")))+
  geom_line(aes(x=date, y=Receipts.Adj_roll), colour="#40A0D8")+
  geom_line(aes(x=date, y=Receipts_roll), colour="#F89088")+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly HMRC alcohol duty receipts (£m)")+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="In real terms, duty revenue changed little in the last decade",
       subtitle="Rolling 12-month average of HMRC alcohol duty revenue <span style='color:#F89088;'>before</span> and <span style='color:#40A0D8;'>after</span> adjusting for inflation",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearancesxProd.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, Product!="Total" & date>=as.Date("2010-01-01")))+
  geom_line(aes(x=date, y=Clearances.Alcohol_roll, colour=Product))+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly alcohol clearances (millions of litres of ethanol)")+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0", "#7030a0"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Longer-term trends in alcohol clearances by product",
       subtitle="Rolling 12-month average of total alcohol cleared by HMRC.\nData for cider and wine is estimated from reported product volumes assuming no change in ABVs over time",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#On-off data from NHS Health Scotland 
#http://www.healthscotland.scot/media/2585/mesas-monitoring-report-2019-alcohol-sales.xlsx

split <- data.frame(prod=rep(c("Beer", "Cider", "Wine", "Spirits"), times=2), 
                    channel=rep(c("On-trade", "Off-trade"), each=4),
                    vol=c(82.4, 13.4, 18.9, 21.5, 95.2, 27.7, 126.6, 97.7),
                    prop=c("46%", "33%", "13%", "18%", "54%", "67%", "87%", "82%"))

tiff("Outputs/MESAS2018PrefVector.tiff", units="in", width=8, height=6, res=300)
ggplot(split, aes(x=prod, y=vol, fill=channel))+
  geom_bar(stat="identity", position="stack", show.legend=FALSE)+
  geom_text(aes(label=prop), position=position_stack(vjust=0.5))+
  scale_fill_paletteer_d("LaCroixColoR::PeachPear")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Millions of litres of alcohol sold per year")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The British drink beer in pubs, but wine and spirits at home",
       subtitle="Total alcohol sales in 2018 by volume of pure alcohol in the <span style='color:#E9A17C;'>on-trade</span> and the <span style='color:#FF3200;'>off-trade",
       caption="Data from NHS Health Scotland | Plot by @VictimOfMaths")
dev.off()
