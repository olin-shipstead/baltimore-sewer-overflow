# Olin Shipstead
# EHE Final Lab
# 6 May 2020

library(tidyverse)
library(lubridate)
library(stringr)
library(RColorBrewer)
theme_set(theme_classic(base_size=15))
setwd("../EHE Lab/Final Lab")

sso <- read.csv("raw_data.csv", header=T, stringsAsFactors = F)
# 6219 obs
balti <- sso %>% 
    mutate(Municipality.Facility = tolower(Municipality.Facility)) %>%
    filter(grepl("baltimore", Municipality.Facility)) %>%
    mutate(
        Date.Discovered = mdy(Date.Discovered),
        Year = year(Date.Discovered),
        Month = month(Date.Discovered),
        Cause = tolower(Cause),
        Duration = as.numeric(Duration...Days)*24 + as.numeric(Duration...Hours) + as.numeric(Duration...Minutes)/60, 
        Volume = as.numeric(Quantity.in.Gallons..Estimated.)
    ) %>%
    filter(!is.na(Volume)) %>%
    mutate(Cause = ifelse(grepl("precip", Cause) | grepl("rain",Cause),"precipitation",Cause))

years <- data.frame(balti %>% group_by(Year) %>% summarize(TotalVolume = sum(Volume)) %>% mutate(Year = as.factor(Year)), balti %>% group_by(Year) %>% summarize(Count = length(Volume)) %>% select(Count))
ggplot(years) + 
    geom_col(aes(x = Year, y = Count), size = 1.25, color = "steelblue4", fill = "white") +
    geom_line(aes(x = Year, y = TotalVolume/1e5), size = 1.5, color="steelblue2", group = 1)+ 
    scale_y_continuous(sec.axis = sec_axis(~.*1e5, name = "SSO Volume (gal)"))+
    labs(title="SSO Frequency by Year", x="", y="Number of reports")+
    scale_x_discrete(guide = guide_axis(angle = 45))+
    theme(axis.title.y = element_text(color = "steelblue4"),
          axis.title.y.right = element_text(color = "steelblue2"))


# bar plot by year
ggplot(data=balti,aes(x=as.factor(Year))) +
    geom_bar(stat="count", aes(fill=after_stat(count)))+
    labs(title="SSO Frequency by Year", x="", y="Number of reports")+
    scale_x_discrete(guide = guide_axis(angle = 45))+
    theme(legend.position = "none")

# causes
n=7
(causes_n <- balti %>% group_by(Cause) %>% summarize(CauseTotal = sum(Volume)) %>% arrange(-CauseTotal) %>% head(n))
(causes_all <- balti %>% 
    mutate(Cause = ifelse(Cause %in% causes_n$Cause, Cause, "other")) %>%
    group_by(Cause) %>% summarize(CauseTotal = sum(Volume)) %>% arrange(-CauseTotal))
causes_all$Cause <- factor(causes_all$Cause, levels = causes_all$Cause, ordered = T)

# pie chart of volume by cause
ggplot(causes_all, aes(x="", y=CauseTotal, fill=Cause)) +
    geom_bar(stat="identity",color="white") +
    coord_polar("y", direction = -1)+
    theme_void(base_size = 15)+
    scale_fill_brewer(palette = "RdYlBu", direction = -1)+
    labs(title="Causes of SSOs (by volume)")+ 
    theme(legend.title = element_blank())

# same but bar chart
ggplot(causes_all, aes(x=Cause, y=CauseTotal, fill=Cause)) + 
    geom_bar(stat="identity",color="white") + 
    scale_fill_brewer(palette = "RdYlBu", direction = -1)+ 
    labs(title="Breakdown of SSO Volume", x="", y="Volume (gal)")+ 
    theme(axis.text.x.bottom = element_blank())

# monthly sso and precipitation timeseries
precip <- read.csv("bwiprecip.csv")
colnames(precip) <- c("Year",1:12)
balt_prec <- precip %>%
    pivot_longer(cols = colnames(precip)[2:13], names_to = "Month", values_to = "Precipitation") %>%
    mutate(Year=as.integer(Year),
           Month=as.integer(Month))

balt_sso <- balti %>%
    group_by(Year, Month) %>%
    summarise(SSO = sum(Volume)) %>%
    mutate(Month = as.numeric(Month))

both <- balt_prec %>%
    inner_join(balt_sso) %>%
    mutate(YearMonth = ymd(paste(Year,Month,1,sep="-")))

cor(both$Precipitation, log10(both$SSO))
plot(log10(SSO)~Precipitation, data=both)
lm <- lm(log10(SSO)~Precipitation, data=both)
summary(lm)
abline(a = c(lm$coefficients),col="red")
# > 10^lm$coefficients
# (Intercept) Precipitation 
# 38070.059376      1.516772

# > 10^sum(lm$coefficients)-10^lm$coefficients[1]
# (Intercept) 
# 19673.54
# every 1 inch increase in precipitation corresponds to a 19,673 unit increase in SSO gallons

ggplot(both, aes(x=Precipitation, y=SSO))+
    scale_y_log10()+
    geom_smooth(formula = y~x, method = "lm", color="black")+
    geom_point(color="steelblue4")+
    geom_text(aes(x=13), y=4, label="y = 4.581 + 0.181x\nR^2 = 0.183\nSE = 0.966", color = "steelblue4", size=5)+
    labs(x="Precipitation (in)", y="log10 (SSO volume)", title="SSO volume vs precipitation regression")

ggplot(data=both %>%
           filter(Year > 2011 & Year < 2018), 
       aes(x=YearMonth)) +
    geom_line(aes(y=SSO),color="steelblue4", size=1.25)+
    geom_line(aes(y=Precipitation*1e6), color="steelblue2", size=1.25)+
    scale_y_continuous(name = "SSO volume (gal)",
                       sec.axis = sec_axis(~./1e6, name="Precipitation (in)"), 
                       limits = c(0,NA))+
    theme(axis.title.y = element_text(color = "steelblue4"),
          axis.title.y.right = element_text(color = "steelblue2"))+
    geom_text(aes(x=ymd("2013-08-10"),label="Broken force main \nat Patapsco WWTP\n(55M gallons)"), y=4.85e7, color="steelblue4", size=4.75)+
    labs(title="SSO volume and precipitation", x="")



balti <- balti %>%
    mutate(Collection.System = tolower(Collection.System)) %>%
    mutate(Collection.System = ifelse(grepl("baltimore", Collection.System),"baltimore dpw",Collection.System)) %>%
    mutate(Collection.System = ifelse(Collection.System == "", "(blank)",Collection.System))

# collection 
n=4
(collection_n <- balti %>% group_by(Collection.System) %>% summarize(Collection.SystemTotal = sum(Volume)) %>% arrange(-Collection.SystemTotal) %>% head(n))
(collection_all <- balti %>% 
        mutate(Collection.System = ifelse(Collection.System %in% collection_n$Collection.System, Collection.System, "other")) %>%
        group_by(Collection.System) %>% summarize(Collection.SystemTotal = sum(Volume)) %>% arrange(-Collection.SystemTotal))
collection_all$Collection.System <- factor(collection_all$Collection.System, levels = collection_all$Collection.System, ordered = T)

# pie chart of volume by Collection.System
ggplot(collection_all, aes(x="", y=Collection.SystemTotal, fill=Collection.System)) +
    geom_bar(stat="identity",color="white") +
    coord_polar("y", direction = -1)+
    theme_void(base_size = 15)+
    theme(legend.title =  element_blank())+
    scale_fill_brewer(palette = "RdBu", direction = -1)+
    labs(title="Where SSOs are collected")

# same but bar chart
ggplot(collection_all, aes(x=Collection.System, y=Collection.SystemTotal, fill=Collection.System)) + 
    geom_bar(stat="identity",color="white") + 
    scale_fill_brewer(palette = "RdBu", direction = -1)+ 
    labs(title="Breakdown of SSO Volume by \nCollection System", x="", y="Volume (gal)")+ 
    theme(axis.text.x.bottom = element_blank(), legend.title = element_blank())





