
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

