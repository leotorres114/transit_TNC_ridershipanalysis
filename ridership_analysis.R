######
library(tidyverse)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)
library(rJava)
library(OpenStreetMap)
library(tidycensus)
library(shinyjs)
library(pastecs)
library(broom)
library(ggfortify)
library(gvlma)
library(nortest)
install.packages("vcd")
install.packages("MASS")
library(vcd)
library(MASS)
install.packages("gridExtra")
library(gridExtra)
library(scales)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggthemes")
library(ggthemes)
census_api_key("64e66316a4cbce9b2a8a4d06b5f6fb3c47d1acdf")


######
#data import
tnc_trips <- read_csv("Data/tnctrips.csv")
cta_trips <- read_csv("Data/CTARidership2019.csv")
weather <- read_csv("Data/WeatherChicago2019.csv")
temp <- read_csv("Data/temp.csv")
cta_stations <- st_read("Data/CTA_RailStations.shp")
ex <- st_read("Data/CMAP_ONTO2050_ForecastbyLAZ.shp")
table_code <- load_variables(2010, "sf1", cache = TRUE)
vars <- c("P001001")
cookcounty_tracts <- get_decennial(geography = "tract", 
                             variables=vars, year = 2010, 
                             state = "Illinois", county = "Cook", 
                             output = "wide", geometry = TRUE)

#data cleaning

cta_trips$date <- mdy(cta_trips$date)
tnc_trips$DATE <- mdy(tnc_trips$DATE)
tnc_trips <- tnc_trips %>% rename(DATE = `Trip Start Timestamp`, 
                                  tnc_trip_distance = `Trip Miles`,
                                  tnc_fare = Fare, 
                                  total_tnctrips = `Trips Pooled`)
cta_trips_clean <- cta_trips %>% filter(date >= "2019-01-01", date <= "2019-12-31") %>% 
  group_by(date) %>% 
  summarise(dailytrips = sum(rides))

cta_trips_clean <- cta_trips_clean %>% rename(DATE = date, 
                                              total_ctatrips = dailytrips)
temp <- temp[-c(1:2)]
weather <- weather[-c(1:2, 4, 7:17)]
weather <- weather %>% filter(NAME == "CHICAGO MIDWAY AIRPORT 3 SW, IL US") %>% 
  inner_join(temp, weather, by = c("DATE"="DATE"))
tidy_tripweather <- inner_join(tnc_trips, weather, by = c("DATE"="DATE")) %>% 
  inner_join(cta_trips_clean, weather, by = c("DATE"="DATE"))

#graphing
options(scipen = 10000)
#distribution of TNC trips with each explanatory variable
a <- ggplot(tidy_tripweather, aes(x=PRCP, y=total_tnctrips))+geom_histogram(stat = "identity") + 
  xlim(c(-.01,1.5)) + ylim(c(0,6000000)) +
  labs(title="Precipitation and TNC Trips", x="Precipitation (inches)", y="Total TNC Trips") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))
b <- ggplot(tidy_tripweather, aes(x=SNOW, y=total_tnctrips))+geom_histogram(stat = "identity") + 
  xlim(c(-.1,2)) +
  labs(title="Snow and TNC Trips", x="Snow (inches)", y="Total TNC Trips") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))
c <- ggplot(tidy_tripweather, aes(x=TMAX, y=total_tnctrips))+geom_histogram(stat = "identity")+
  labs(title="Temperature and TNC Trips", x="Temperature (F)", y="Total TNC Trips") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))

#distribution of CTA trips with each explanatory variable
d <- ggplot(tidy_tripweather, aes(x=PRCP, y=total_ctatrips))+geom_histogram(stat = "identity") + 
  xlim(c(-.01,1.5)) + ylim(c(0,6000000))+
  labs(title="Precipitation and 'L' Trips", x="Precipitation (inches)", y="Total 'L' Trips") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))
e <- ggplot(tidy_tripweather, aes(x=SNOW, y=total_ctatrips))+geom_histogram(stat = "identity") + 
  xlim(c(-.1,2))+
  labs(title="Snowfall and 'L' Trips", x="Snow (inches)", y="Total 'L' Trips") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))
f <- ggplot(tidy_tripweather, aes(x=TMAX, y=total_ctatrips))+geom_histogram(stat = "identity") +
  labs(title="Temperature and 'L' Trips", x="Temperature (F)", y="Total 'L' Trips") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))

#daily CTA ridership
g <- ggplot(tidy_tripweather, aes(x=DATE, y=(total_ctatrips/1000)))+geom_line() +
  labs(title="Daily 'L' Trips in 2019", x="Date", y="Total 'L' Trips (in thousands)") + 
  theme_economist()
g + expand_limits(y=0) + scale_y_continuous(limits = c(0,800)) +
  theme(axis.title.y = element_text(vjust = 3))

#daily TNC ridership
h <- ggplot(tidy_tripweather, aes(x=DATE, y=(total_tnctrips/1000)))+geom_line() +
  labs(title="Daily TNC Trips in 2019", x="Date", y="Total TNC Trips (in thousands)") + 
  theme_economist()
h + expand_limits(y=0) + scale_y_continuous(limits = c(0,650)) +
 theme(axis.title.y = element_text(vjust = 3))

#daily temperature
i <- ggplot(tidy_tripweather, aes(x=DATE, y=TMAX))+geom_line() +
  labs(title="Daily Temperature Highs in 2019", x="Date", y="Temperature (F)") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))

#daily precipitation
j <- ggplot(tidy_tripweather, aes(x=DATE, y=PRCP))+geom_line() +
  labs(title="Daily Precipitation in 2019", x="Date", y="Precipitations (inches)") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))

#daily snowfall
k <- ggplot(tidy_tripweather, aes(x=DATE, y=SNOW))+geom_line() +
  labs(title="Daily Snowfall in 2019", x="Date", y="Snowfall (inches)") + 
  theme_economist() + theme(axis.title.y = element_text(vjust = 3))

#check if variance = mean 
mean(tidy_tripweather$total_ctatrips)
var(tidy_tripweather$total_ctatrips)
mean(tidy_tripweather$total_tnctrips)
var(tidy_tripweather$total_tnctrips)

#negative binomial model 

tnc_model <- glm.nb(total_tnctrips~PRCP+SNOW+TMAX, data = tidy_tripweather) #model tnc trips
summary(tnc_model)

cta_model <- glm.nb(total_ctatrips~PRCP+SNOW+TMAX, data = tidy_tripweather) #model CTA trips
summary(cta_model)

#predict values of negative binomial 
tidy_tripweather$tnc_pred <- predict(tnc_model, type = "response")
tidy_tripweather$cta_pred <- predict(cta_model, type = "response")

#graph monthly average of predicted and true values of negative binomial
l <- tidy_tripweather %>% group_by(month=floor_date(DATE, "month")) %>% 
  summarise(cta_monthly=mean(total_ctatrips), cta_monthly_pred=mean(cta_pred)) %>%
  ggplot(aes(x=month)) + 
  geom_line(aes(y=cta_monthly, colour="Actual")) +
  geom_line(aes(y=cta_monthly_pred, colour = "Predicted"))+ 
  labs(x="Date", y="Total Trips", color = element_blank()) + 
  theme_economist() + scale_colour_manual(values = c("Actual"="gray",
                                                     "Predicted"="darkred")) +
  theme(axis.title.y = element_text(vjust = 3), 
                            axis.title.x = element_text(vjust = -3),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.x = element_blank())

m <- tidy_tripweather %>% group_by(month=floor_date(DATE, "month")) %>% 
  summarise(tnc_monthly=mean(total_tnctrips), tnc_monthly_pred=mean(tnc_pred)) %>%
  ggplot(aes(x=month)) + 
  geom_line(aes(y=tnc_monthly, colour = "Actual")) +
  geom_line(aes(y=tnc_monthly_pred, colour = "Predicted"))+ 
  labs(x="Date", y="Total Trips", color = element_blank()) + 
  theme_economist() + scale_colour_manual(values = c("Actual"="gray",
                                                     "Predicted"="darkred")) +
  theme(axis.title.y = element_text(vjust = 3), 
        axis.title.x = element_text(vjust = -3),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank())

#Poisson model 

tnc_model_pssn <- glm(total_tnctrips~PRCP+SNOW+TMAX, family = "poisson", data = tidy_tripweather) #model tnc trips
summary(tnc_model_pssn)

cta_model_pssn <- glm(total_ctatrips~PRCP+SNOW+TMAX, family = "poisson", data = tidy_tripweather) #model CTA trips
summary(cta_model_pssn)

#predict values of Poisson
tidy_tripweather$tnc_pred_pssn <- predict(tnc_model_pssn, type = "response")
tidy_tripweather$cta_pred_pssn <- predict(cta_model_pssn, type = "response")

#graph monthly average of predicted and true values of Poisson
n <- tidy_tripweather %>% group_by(month=floor_date(DATE, "month")) %>% 
  summarise(cta_monthly=mean(total_ctatrips), cta_monthly_pred=mean(cta_pred_pssn)) %>% 
  ggplot(aes(x=month)) + 
  geom_line(aes(y=cta_monthly, colour = "Actual")) +
  geom_line(aes(y=cta_monthly_pred, colour = "Predicted")) + 
  labs(x="Date", y="Total Trips", color = element_blank()) + 
  theme_economist() + scale_colour_manual(values = c("Actual"="gray",
                                                     "Predicted"="darkred")) +
  theme(axis.title.y = element_text(vjust = 3), 
        axis.title.x = element_text(vjust = -3),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank())

o <- tidy_tripweather %>% group_by(month=floor_date(DATE, "month")) %>% 
  summarise(tnc_monthly=mean(total_tnctrips), tnc_monthly_pred=mean(tnc_pred_pssn)) %>% 
  ggplot(aes(x=month)) + 
  geom_line(aes(y=tnc_monthly, colour = "Actual")) +
  geom_line(aes(y=tnc_monthly_pred, colour = "Predicted")) + 
  labs(x="Date", y="Total Trips", color = element_blank()) + 
  theme_economist() + scale_colour_manual(values = c("Actual"="gray",
                                                       "Predicted"="darkred")) +
  theme(axis.title.y = element_text(vjust = 3), 
        axis.title.x = element_text(vjust = -3),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank())

#show all actual vs predicted plots
ggarrange(l, m, n, o, common.legend = TRUE, legend = "bottom", 
          labels = c("A", "B", "C", "D"))

#goodness of fit test using chi-square
1 - pchisq(summary(tnc_model)$deviance, summary(tnc_model)$df.residual)
1 - pchisq(summary(tnc_model_pssn)$deviance, summary(tnc_model_pssn)$df.residual)

1 - pchisq(summary(cta_model)$deviance, summary(cta_model)$df.residual)
1 - pchisq(summary(cta_model_pssn)$deviance, summary(cta_model_pssn)$df.residual)