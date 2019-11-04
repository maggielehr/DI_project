library(dplyr)

setwd("/Users/maggielehr/Documents/Grad-School/Job Hunt/DC/bank_data/")
d2019 <- read.csv("ALL_2019.csv")
d2018 <- read.csv("ALL_2018.csv")
d2017 <- read.csv("ALL_2017.csv")
d2016 <- read.csv("ALL_2016.csv")
d2015 <- read.csv("ALL_2015.csv")
d2014 <- read.csv("ALL_2014.csv")
d2013 <- read.csv("ALL_2013.csv")
d2012 <- read.csv("ALL_2012.csv")
d2011 <- read.csv("ALL_2011.csv")
d2010 <- read.csv("ALL_2010.csv")
census_pop <- read.csv("ACS_17_5YR_B01003_with_ann.csv", header = TRUE)
census_pop <- census_pop[,c(2,4)]
county_data <- read.csv("NCHSURCodes2013.csv")
county_data$County.2012.pop <- as.numeric(as.character(county_data$County.2012.pop))
county_data$County.name <- gsub(" County", "", county_data$County.name) 
county_data$County.name <- gsub(" Borough", "", county_data$County.name) 
county_data$County.name <- gsub(" Census Area", "", county_data$County.name) 
county_data$County.name <- gsub(" Municipality", "", county_data$County.name) 
county_data$county <- paste(county_data$State.Abr.,county_data$County.name)
county_data <- county_data[,c(6,7,11)] 

dt <- rbind(d2019, d2018)
dt <- rbind(dt, d2017)
c18 <- colnames(d2018)
c16 <- colnames(d2016)
c18 == c16

d2019$county <- paste(d2019$STALPBR,d2019$CNTYNAMB)
branch_by_county <- as.data.frame(table(d2019$statecol))
names(branch_by_county)[1]<-"county"
names(branch_by_county)[2]<-"branch_num"
branch_by_county$county <- as.character(branch_by_county$county)

branch_pop <- county_data %>% left_join(branch_by_county)
branch_pop$branch_num[is.na(branch_pop$branch_num)] <- 0
#branch_pop$popcode_name <- ordered(branch_pop$popcode_name, levels = c("Large_metro", "large_fringe_metro", "Medium_metro", "Small_metro", "Micropolitan", "Noncore"))
branch_pop$popcode_name <- NA
branch_pop$popcode_name[branch_pop$X2013.code == 1] <- "Large metro"
branch_pop$popcode_name[branch_pop$X2013.code == 2] <- "Large fringe metro"
branch_pop$popcode_name[branch_pop$X2013.code == 3] <- "Medium metro"
branch_pop$popcode_name[branch_pop$X2013.code == 4] <- "Small metro"
branch_pop$popcode_name[branch_pop$X2013.code == 5] <- "Micropolitan"
branch_pop$popcode_name[branch_pop$X2013.code == 6] <- "Noncore"
branch_pop$popcode_name <- ordered(branch_pop$popcode_name, levels = c("Large metro", "Large fringe metro", "Medium metro", "Small metro", "Micropolitan", "Noncore"))

branch_pop$pop_thousand <- branch_pop$County.2012.pop / 1000
branch_pop$bpt <- branch_pop$branch_num / branch_pop$pop_thousand

bpt <- ggplot(branch_pop, aes(x=bpt, fill=popcode_name)) +
  geom_density(alpha = .3) +
  xlim(0,2.2) + 
  labs(x="Banks Per 1,000 Adults", fill="Zip Code Densely \n Populated", title="Commercial Bank Access for Each Zip Code")

d2019 <- d2019 %>% left_join(branch_pop)

#established data 
d2019$year_est <- format(as.Date(d2019$SIMS_ESTABLISHED_DATE, format="%m/%d/%Y"),"%Y")
d2019$year_est <- as.numeric(as.character(d2019$year_est))
d2019_noNA <- subset( d2019, popcode_name != "NA")
estab <- ggplot(d2019_noNA, aes(x=popcode_name, y=year_est)) +
  geom_boxplot() +
  labs(y="Year Branch Established", x="NCHS Urban-Rural Classification Scheme", title="Branch Establishment Year")

plot_grid(bpt, estab)

table
specialization <- as.data.frame(table(d2019_noNA$SPECGRP))
specialization_byclass <- as.data.frame(table(d2019_noNA$SPECGRP, d2019_noNA$popcode_name))
specialization_byclass <- specialization_byclass %>% spread(Var2, Freq)
specialization_byclass[, -1] <- lapply(specialization_byclass[ , -1], function(x) x/sum(x, na.rm=TRUE) )
#2 and 4 most frequent

d2010_deposits <- d2010[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2010_deposits$year <- 2010
d2011_deposits <- d2011[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2011_deposits$year <- 2011
d2012_deposits <- d2012[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2012_deposits$year <- 2012
d2013_deposits <- d2013[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2013_deposits$year <- 2013
d2014_deposits <- d2014[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2014_deposits$year <- 2014
d2015_deposits <- d2015[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2015_deposits$year <- 2015
d2016_deposits <- d2016[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2016_deposits$year <- 2016
d2017_deposits <- d2017[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2017_deposits$year <- 2017
d2018_deposits <- d2018[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2018_deposits$year <- 2018
d2019_deposits <- d2019[,c("STALPBR", "CNTYNAMB", "SPECGRP", "DEPDOM")]
d2019_deposits$year <- 2019

deposits <- rbind(d2010_deposits, d2010_deposits, d2011_deposits, d2012_deposits, d2013_deposits, d2014_deposits, d2015_deposits, d2016_deposits, d2017_deposits, d2018_deposits, d2019_deposits)
deposits$county <- paste(deposits$STALPBR,deposits$CNTYNAMB)
deposits$DEPDOM <- as.character(deposits$DEPDOM)
deposits$DEPDOM <- gsub("\\,", "", deposits$DEPDOM) 
deposits$DEPDOM <- as.numeric(deposits$DEPDOM)
county_deposits <- deposits %>%
  group_by(county, factor(SPECGRP), factor(year)) %>%
  summarise(sumdep = sum(DEPDOM))
library(data.table)
county_deposits <- setDT(deposits)[, lapply(.SD, sum), by = .(county, factor(SPECGRP), factor(year))] 
county_deposits <- aggregate(list(deposits$DEPDOM), by = list(deposits$year, deposits$county, deposits$SPECGRP), sum)
names(county_deposits)[4]<-"deposits"
names(county_deposits)[1]<-"year"
names(county_deposits)[2]<-"county"
names(county_deposits)[3]<-"spec_group"

county_deposits <- county_deposits %>% left_join(county_data)
commercial_lending_deposit <- subset(county_deposits, spec_group == 4)
str(commercial_lending_deposit)
commercial_lending_deposit <- aggregate(list(commercial_lending_deposit$deposits), by = list(factor(commercial_lending_deposit$year), factor(commercial_lending_deposit$X2013.code)), sum)
names(commercial_lending_deposit)[3]<-"deposits"
names(commercial_lending_deposit)[1]<-"year"
names(commercial_lending_deposit)[2] <- "NCHS"
commercial_lending_deposit$popcode_name <- NA
commercial_lending_deposit$popcode_name[commercial_lending_deposit$NCHS == 1] <- "Large metro"
commercial_lending_deposit$popcode_name[commercial_lending_deposit$NCHS == 2] <- "Large fringe metro"
commercial_lending_deposit$popcode_name[commercial_lending_deposit$NCHS == 3] <- "Medium metro"
commercial_lending_deposit$popcode_name[commercial_lending_deposit$NCHS == 4] <- "Small metro"
commercial_lending_deposit$popcode_name[commercial_lending_deposit$NCHS == 5] <- "Micropolitan"
commercial_lending_deposit$popcode_name[commercial_lending_deposit$NCHS == 6] <- "Noncore"
commercial_lending_deposit$popcode_name <- ordered(commercial_lending_deposit$popcode_name, levels = c("Large metro", "Large fringe metro", "Medium metro", "Small metro", "Micropolitan", "Noncore"))


p<-ggplot(commercial_lending_deposit, aes(x=year, y=deposits, group=popcode_name)) +
  geom_line(aes(color=popcode_name))+
  geom_point(aes(color=popcode_name)) + 
  labs(y="Deposits", x="Year", color = "NCHS Urban-Rural \n Classification Scheme", title="Commercial Loan Deposits Over Time")
p




