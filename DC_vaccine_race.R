# The Great Race to Vaccinate
setwd("C:/Users/sondr/Dropbox/Economist - DC - Covid-19 Vaccine Race")

library(tidyverse)
library(data.table)
library(countrycode)

# Import daily data for countries from Our World In Data
country_daily_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>%
  mutate(date = as.Date(date),
         country = location,
         iso3c = iso_code,
         region = countrycode(iso3c, origin="iso3c",destination="un.region.name"),
         subregion = countrycode(iso3c, origin="iso3c",destination="un.regionsub.name"),
         daily_covid_deaths = new_deaths_smoothed,
         daily_covid_deaths_per_100k = (daily_covid_deaths / population) * 100000,
         daily_covid_cases = new_cases_smoothed,
         daily_covid_cases_per_100k = (daily_covid_cases / population) * 100000,
         daily_tests = new_tests_smoothed,
         daily_tests_per_100k = (daily_tests / population) * 100000,
         daily_positive_rate = positive_rate * 100,
         daily_vaccinations = new_vaccinations_smoothed,
         daily_vaccinations_per_100k = (daily_vaccinations / population) * 100000,
         vaccinated_pct = people_vaccinated_per_hundred,
         fully_vaccinated_pct = people_fully_vaccinated_per_hundred) %>%
  filter(date >= as.Date("2020-01-01"),
         !str_detect(iso3c,"OWID")) %>%
  group_by(iso3c) %>%
  fill(daily_tests_per_100k,daily_positive_rate)  %>%
  dplyr::select(date,country,iso3c,region,subregion,population,
                hospital_beds_per_thousand,
                population_density,
                median_age,aged_65_older,aged_70_older,life_expectancy,
                daily_covid_deaths,daily_covid_deaths_per_100k,
                daily_covid_cases,daily_covid_cases_per_100k,
                daily_tests,daily_tests_per_100k,daily_positive_rate,
                daily_vaccinations,
                daily_vaccinations_per_100k,
                vaccinated_pct,
                fully_vaccinated_pct)

country_daily_data$EU <- ifelse(country_daily_data$iso3c %in% 
                                  c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"), "EU", "not EU")

# Send to convenient dataframe
pdat <- data.frame(country_daily_data)

library(readr)
hmm <- pdat[pdat$date == as.Date("2021-06-18") & pdat$EU == "EU", c("country", "vaccinated_pct")]
hmm <- hmm[hmm$country != "Malta", ]
sd(hmm$vaccinated_pct, na.rm = T)
write_csv( pdat[pdat$date == as.Date("2021-06-20") & pdat$EU == "EU", c("country", "vaccinated_pct", "iso3c")], "EU.csv")

# Get continent
pdat$continent <- countrycode(pdat$iso3c, "iso3c", "continent")
pdat$continent[pdat$iso3c %in% c("USA", "CAN")] <- "North America"

# Restrict to North America and Europe (and excluding UK dependencies)
pdat <- pdat[pdat$continent %in% c("Europe", "North America") & pdat$date >= as.Date("2020-12-01") & !pdat$country %in% c("Gibraltar", "Isle of Man", "Guernsey", "Jersey"), ]

pdat$group <- pdat$country
pdat$group[pdat$continent == "Europe" & pdat$EU == "EU"] <- "European Union (average)"
pdat$group[pdat$continent == "Europe" & pdat$EU != "EU"] <- "Other Europe"
pdat$group[pdat$iso3c == "GBR"] <- "Britain"

pdat <- pdat[pdat$group != "Other Europe", ]
pdat <- pdat[pdat$date <= Sys.Date() - 3, ] # to account for reporting lags

# Make 7-day averages:
pdat <- pdat[order(pdat$date), ]
for(i in c("daily_vaccinations", "daily_vaccinations_per_100k")){
  pdat[, i] <- ave(pdat[, i], pdat$iso3c, FUN = function(x){
    temp <- x
    for(i in 1:length(x)){
      temp[i] <- mean(x[max(c(1, i-3)):min(c(length(x), i+3))], na.rm = T)
    }
    temp
  })
}

# Make population-weighted averages
pdat$daily_vaccinations_per_100k_region_average <- 100000*ave(pdat$daily_vaccinations, paste0(pdat$date, pdat$group), FUN = function(x) sum(x, na.rm = T)) / ave(pdat$population, paste0(pdat$date, pdat$group), FUN = function(x) sum(x, na.rm = T))

for(i in unique(pdat$group)){
  for(t in unique(pdat$date)){
pdat$vaccinated_pct_region_average[pdat$group == i & pdat$date == t] <- sum(pdat$vaccinated_pct[pdat$group == i & pdat$date == t]*pdat$population[pdat$group == i & pdat$date == t], na.rm = T)/sum(pdat$population[pdat$group == i & pdat$date == t], na.rm = T)
pdat$fully_vaccinated_pct_region_average[pdat$group == i & pdat$date == t] <- sum(pdat$fully_vaccinated_pct[pdat$group == i & pdat$date == t]*pdat$population[pdat$group == i & pdat$date == t], na.rm = T)/sum(pdat$population[pdat$group == i & pdat$date == t], na.rm = T)
pdat$daily_vaccinations_region_average[pdat$group == i & pdat$date == t] <- sum(pdat$daily_vaccinations[pdat$group == i & pdat$date == t]*pdat$population[pdat$group == i & pdat$date == t], na.rm = T)/sum(pdat$population[pdat$group == i & pdat$date == t], na.rm = T)
  }
}

# Part 1 : Prediction ------------------------------------------------------------------------------------------------------------------------

# Generate Separate model data frame
model_df <- pdat[pdat$continent %in% c("North America", "Europe"), ] 
model_df <- model_df[order(model_df$date), ]
model_df$total_doses <- ave(model_df$daily_vaccinations, 
                            model_df$iso3c, 
                            FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))
model_df$total_doses_lagged_1_day <- ave(model_df$total_doses, model_df$iso3c,
                                         FUN = function(x){
                                           c(NA, x)[1:length(x)]
                                         })

# Fit Poisson model with cubic splines
library(splines)
my_model <- glm(daily_vaccinations ~ country*bs(date,3)
               + total_doses_lagged_1_day/population - population
               ,
               data = model_df, family = "poisson")


# Cycle through days, cast forward:
model_df$prediction <- model_df$daily_vaccinations
for(i in 1:31){
  temp <- model_df[model_df$date == max(model_df$date), ]
  temp$date <- temp$date + 1
  temp$total_doses_lagged_1_day <- temp$total_doses
  temp$prediction <- unlist(predict(newdata = temp, my_model, type = "response"))
  temp$daily_vaccinations <- temp$prediction
  model_df$total_doses <- ave(model_df$daily_vaccinations,
                              model_df$iso3c,
                              FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))
  model_df <- rbind(model_df, temp)
}

# Remove latest day, which was strange
model_df <- model_df[model_df$date < max(model_df$date), ]

# Add the EU as a population-averaged unit:
temp <- model_df[model_df$country == "Germany", ]
temp$country <- "European Union (average)"
temp[, 3:ncol(temp)] <- NA
for(i in unique(model_df$date)){
    temp$total_doses[temp$date == i] <- sum(model_df$total_doses[model_df$EU == "EU" & model_df$date == i], na.rm = T)
   temp$population[temp$date == i] <- sum(model_df$population[model_df$EU == "EU" & model_df$date == i], na.rm = T)
}
temp <- temp[!is.na(temp$population), ]
model_df <- rbind(model_df, temp)

model_df$rate <- model_df$total_doses/model_df$population


# plot
ggplot(model_df[model_df$country %in% c("United States", 
                                        "United Kingdom", "Germany", 
                                        "European Union (average)", 
                                        "France", "Italy", "Canada", "Spain"
) & model_df$date <= Sys.Date(), ], 
aes(x=date, y=total_doses/population, col = country))+
  geom_line()+
  geom_line(data = model_df[model_df$country %in% c("United States", "United Kingdom", "Germany", "European Union (average)", "France", "Italy", "Canada", "Spain") & model_df$date >= Sys.Date(), ], aes(linetype = "projected"), linetype = 2)+
  theme_minimal()+
  theme(legend.title = element_blank())+xlab("Sources: Our World in Data, The Economist\n *explanation on dotted line = 'Projected by The Economist based on model including country's cumulative doses administered to date'\nLabel: 'Given current trends, The EU will overtake America in vaccinations administered per person on July 14th" )+ylab("")+ggtitle("Unflatten the curve\nTotal covid-19 vaccinations administered, per person, December 2020 to July 2021")

# Save the csv for visualizers to work with:
write_csv(model_df, "plot_1.csv")

# Part 2 : Straighforward summary statistics ------------------------------------------------------------------------------------------------------------------------
# FOr the next few plots, generate the EU as a separate country

# Generate a EU as a country
temp <- pdat[pdat$country == "Germany", ]
temp$country <- "European Union (average)"
temp$daily_vaccinations_per_100k <- temp$daily_vaccinations_per_100k_region_average
temp$vaccinated_pct_region_average <- temp$vaccinated_pct_region_average
temp$fully_vaccinated_pct_region_average <- temp$fully_vaccinated_pct_region_average
temp$iso3c <- "EU"

pdat <- rbind(pdat, temp)

pdat <- pdat[pdat$country %in% c("France", "Germany", "Italy", "Spain", "Canada", "United Kingdom", "United States", "European Union (average)"), ]
write_csv(pdat, "plot_2_3.csv")

# Part 3 : Plots ------------------------------------------------------------------------------------------------------------------------

# Plot 1
library(readr)
library(ggplot2)
model_df <- read_csv("plot_1.csv")
ggplot(model_df[model_df$country %in% c("United States", 
                                        "United Kingdom", "Germany", 
                                        "European Union (average)", 
                                        "France", "Italy", "Canada", "Spain"
) & model_df$date <= Sys.Date(), ], 
aes(x=date, y=total_doses/population, col = country))+
  geom_line()+
  geom_line(data = model_df[model_df$country %in% c("United States", "United Kingdom", "Germany", "European Union (average)", "France", "Italy", "Canada", "Spain") & model_df$date >= Sys.Date(), ], aes(linetype = "projected"), linetype = 2)+
  theme_minimal()+
  theme(legend.title = element_blank())+xlab("Sources: Our World in Data, The Economist\n *explanation on dotted line = 'Projected by The Economist based on model including country's cumulative doses administered to date'\nLabel: 'Given current trends, The EU will overtake America in vaccinations administered per person on July 14th" )+ylab("")+ggtitle("Unflatten the curve\nTotal covid-19 vaccinations administered, per person, December 2020 to July 2021")
ggsave("Unflatten.png", width = 8, height = 5)

# Plot 2:
library(readr)
pdat <- read_csv("plot_2_3.csv")
pdat$country[pdat$country == "European Union (average)"] <- "European Union"
ggplot(pdat, 
       aes(x=date, y=daily_vaccinations_per_100k/1000, col = country))+
  geom_line()+
  theme_minimal()+theme(legend.title = element_blank())+
  ylab("")+xlab("")+ggtitle("Jab sessions\nPopulation vaccinated per day, %, 7-day average")+xlab("Source: Our World in Data")
ggsave("jab_sessions.png", width = 8, height = 5)

# Plot 3:
library(readr)
pdat <- read_csv("plot_2_3.csv")
ggplot(pdat[pdat$country %in% c("United Kingdom", "United States", "European Union (average)"), ], 
       aes(x=date))+
  geom_area(aes(y=vaccinated_pct, fill = "First dose"))+
  geom_area(aes(y=fully_vaccinated_pct, fill = "Fully vaccinated"))+
  theme_minimal()+theme(legend.title = element_blank())+
  theme(legend.position = "right")+
  ylab("")+xlab("")+ggtitle("Giant steps\nVaccination rates, %")+facet_grid(.~country)+xlab("Source: Our World in Data")
ggsave("giant_steps.png", width = 8, height = 3)

