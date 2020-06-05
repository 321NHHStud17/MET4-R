# Load necessary libraries. If you do not have these installed,  
# you may need to run "install.library("libraryname")" where you 
# replace libraryname with the library you want to install:  
library(tidyverse) 
library(lubridate) 
library(dplyr)
library(gridExtra)
library(kableExtra)

# Load the avinor-data. -----
load("avinor.Rdata") 

df_total <-   
  df %>% # The raw data frame from Avinor   
  filter(     
    # Drop all flights that have not yet departed     
    code == "D") %>%
  mutate(     
    # Create an indicator variable, equal to 1 if the flight     
    # is international     
    international = case_when(dom_int == "D" ~ 0, TRUE ~ 1),
    # Recode the departure time into a date     
    dep_date = as.Date(schedule_time)   
    ) %>%   
  filter(     
    # Drop all obs. before 19th of Jan, due to a data hiccup       
    # at this date.      
    dep_date > as.Date('2020-01-19')) %>%    
  group_by(     
    # Group the data frame by each date     
    dep_date) %>%    
  summarise(     
    # Count the total number of flights each day     
    flights = n(),     
    # Count the number of international flights each day     
    flights_int = n() * sum(international) / n(),     
    # Count the number of domestic flights each day     
    flights_dom = n() * sum(1-international) / n()) %>%    
  arrange(     
    # Ensure the data frame is sorted by date     
    dep_date) %>%    
  mutate(     
    # Cumulative number of daily flights     
    cumflights = cumsum(flights),     
    # Cumulative number of daily international flights     
    cumflights_int = cumsum(flights_int),     
    # Cumulative number of daily domestic flights     
    cumflights_dom = cumsum(flights_dom),     
    # Create a factor variable, indicating the day of week     
    day_of_week = factor(lubridate::wday(dep_date)),     
    # Calculate a trend variable, increasing by 1 each day.      
    trend = 1,     
    trend = cumsum(trend)) 
  
  
df_airline <-   
  df %>% # The original data set   
  filter(     
    # Drop all flights that have not yet departed     
    code == "D") %>%    
  mutate(     
    #Create an indicator variable, equal to 1 if the flight     
    # is international     
    international = case_when(dom_int == "D" ~ 0, TRUE ~ 1),  
    # Recode the departure time into a date     
    dep_date = as.Date(schedule_time)   
    ) %>%   
  filter(     
    # Drop all obs. before 19th of Jan, due to a data hiccup on the      
    # 19th.      
    dep_date > as.Date('2020-01-19')) %>%  
  group_by(     
    # Group the data frame by each date and airline     
    dep_date, airline) %>%    
  summarise(     
    # Count the total number of flights each day     
    flights = n(),     
    # Count the number of international flights each day     
    flights_int = n() * sum(international) / n(),     
    # Count the number of domestic flights each day     
    flights_dom = n() * sum(1-international) / n()) %>%    
  ungroup(     
    # Remove the grouping from the data set   
    ) %>%    
  full_join(     
    # Some airlines might not have any flights every day. If so, they     
    # will be missing entirely from the data frame. I would prefer to have      
    # such days as a row in the data frame, with the number of flights     
    # equal to zero. So, expand the data set with all the missing      
    # date/airline combinations     
    expand.grid(       
      dep_date = unique(.$dep_date),       
      airline = unique(.$airline))) %>%    
  mutate(     
    # Set the flights data to zero for all those we have      
    # just filled in     
    flights = replace_na(flights, 0),     
    flights_int = replace_na(flights_int, 0),     
    flights_dom = replace_na(flights_dom, 0)) %>%    
  arrange(     
    # Sort the data frame by airline and dep. date     
    airline,dep_date) %>%    
  group_by(     
    # Group by airline     
    airline) %>%    
  mutate(     
    # Create cumulative variables, day of week and a trend.      
    cumflights = cumsum(flights),     
    cumflights_int = cumsum(flights_int),     
    cumflights_dom = cumsum(flights_dom),     
    day_of_week = factor(lubridate::wday(dep_date)),     
    trend = 1,     
    trend = cumsum(trend)) %>%    
  mutate(     
    # There are many airlines with very few flights.      
    # Find the total number of flights for each airline.      
    max_cum_flights = max(cumflights)) %>%    
  filter(     
    # Remove all airlines with less than 100 flights.     
    max_cum_flights > 100) %>%    
  ungroup()  
    
  # Save the cleaned data frames in a new file 
save(   
  df_total,   
  df_airline,   
  file = "avinor_cleaned.Rdata") 

# Oppgave 1 ----
library(ggplot2)
library(scales)

df_total %>%
  group_by(day_of_week) %>%
  summarise(`Gj.snitt` = round(mean(flights),digits = 0),
            `St.avvik` = round(sd(flights), digits = 0),
            `Min` = min(flights),
            `25. Pctl` = round(quantile(flights, .25), digits = 0),
            `Median` = round(median(flights), digits = 0),
            `75. Pctl` = round(quantile(flights, .75), digits = 0),
            `Max` = max(flights)) %>%
  stargazer::stargazer(type = "html", summary = FALSE, digits = 0, out = "desk.html",
                       title = "Deskriptiv statistikk for totale daglige flyavganger",
                       column.sep.width = "1000 pt")

a1 <- ggplot(df_total, aes(x = dep_date, y = flights)) +
  theme_classic() +
  geom_line(col = "darkblue",
            size = 1) +
  geom_point(size = 1.3,
             col = "darkblue") +
  labs(title = "Totale antall flygninger per dag",
       x = "Dato", 
       y = "Antall flygninger") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 18),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_x_date(labels = date_format("%d-%m-%y"))

a1

a2 <- ggplot(df_total, aes(x = dep_date, y = cumflights)) +
  theme_classic() +
  geom_line(col = "darkblue",
            size = 1) +
  labs(title = "Kumulative daglige flygninger",
       x = "Dato",
       y = "Antall flygnigner") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 18),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_x_date(labels = date_format("%d-%m-%y"))  
  

grid.arrange(a1, a2, nrow = 1)
myts <- ts(df_total$flights, start = c(1, 1), end = c(7, 7), frequency = 7)

myts_2 <- ts(df_total$flights, start = c(1, 1) , end = c(11, 7), frequency = 7)

library(forecast)

dekomponert <- stl(myts, s.window = "periodic")

autoplot(dekomponert)

prediksjon <- forecast(dekomponert, h = 28, level = c(.9999))
prediksjon2 <- forecast(best_aic_pred_pre, h = 28, level = .9999)

# Leter etter helt ekstreme utslag, derfor så tynt intervall. Leter ikke etter noe som 
# skjer hver 20. dag.

plot(prediksjon, type = "l", 
     xlim = c(5, 12), col = "darkblue",
     bty = "l", ylim = c(0, 1200),
     xlab = "Ukenummer i datasettet",
     ylab = "Antall flygnigner",
     main = "Observert mot predikert antall flygnigner",
     sub = "Blå er predikert. Rød er observert.")
lines(myts_2, type = "b", pch = 20, col = "red")
abline(v = 9, lty = 2, col = "cornflowerblue")

prediksjon[["mean"]]
prediksjon[["lower"]]
myts_2
# Mandag 16.03.20 er observert som 601 flygninger, 99.99% nedre intervall gir 808 flygninger. 
# Klart at denne er "unormal". Søndag 15.03.20 er observert som 683 flygninger, 99.99% nedre
# er 655 flygninger. Innenfor.

df_total_pre %>%
  group_by(day_of_week) %>%
  summarise(`Mean Flights` = round(mean(flights),digits = 2),
            `Mean Flights Dom` = round(mean(flights_dom), digits = 2),
            `Mean Flights Int` = round(mean(flights_int), digits = 2)) %>%
  stargazer::stargazer(type = "text", summary = FALSE, digits = 2)

df_total_post %>%
  group_by(day_of_week) %>%
  summarise(`Mean Flights` = round(mean(flights),digits = 2),
            `Mean Flights Dom` = round(mean(flights_dom), digits = 2),
            `Mean Flights Int` = round(mean(flights_int), digits = 2)) %>%
  stargazer::stargazer(type = "text", summary = FALSE, digits = 2)

# Oppgave 2 ----

df_total_pre <- df_total %>% 
  filter(dep_date <= '2020-03-15') 


df_total_post <- df_total %>% 
  filter(dep_date > '2020-03-15') 

# https://flysmart24.no/2020/02/10/hundrevis-av-flyavganger-innstilt-pa-grunn-av-uvaeret/
# Uvær 10. februar


# Reg1 - Flygninger totalt per dag
reg1 <- lm(flights ~ day_of_week,
           data = df_total_pre)
# Argumentere for trend ut, kort periode vi har data.
# Kan være trend kun for våren, store sesongvariasjoner i flytrafikken. 
# Trenden blir ganske "aggresiv".
library(stargazer)

stargazer(reg1, type = "text")

# Diagnoseplott
layout(matrix(c(1, 1, 1, 1), nrow = 2, byrow = FALSE))

p1 <- ggplot(reg1, aes(x = reg1$fitted.values, y = reg1$residuals, col = reg1$model$day_of_week)) + 
  theme_classic() +
  geom_point() +
  geom_hline(yintercept = 0,
             color = "darkblue") + 
  labs(title = "Residualer etter prediksjon",
       x = "Predikert antall flygninger",
       y = "Residualer") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_color_discrete(name = "Ukedag",
                       labels = c("Søndag", "Mandag", "Tirsdag", "Onsdag", "Torsdag", "Fredag", "Lørdag"))

#acf(reg1$residuals, main = "Autokorrelasjonsplott",
#   xlab = "Antall observasjoner tilbake", ylab = "Autokorrelasjonsfaktor",
#   col = "darkblue", ci.col = "red", bty = "l")


conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(reg1$residuals))
bacf <- acf(reg1$residuals, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

acf(reg1$residuals, ci = .999)

p2 <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  theme_classic() +
  geom_hline(aes(yintercept = 0), col = "white") +
  geom_segment(mapping = aes(xend = lag, yend = 0), col = "darkblue",
               size = 1) +
  geom_hline(aes(yintercept = ciline), col = "red",
             linetype = "dashed") +
  geom_hline(aes(yintercept = -ciline), col = "red",
             linetype = "dashed") +
  labs(title = "Autokorrelasjonsplott",
       x = "Lag",
       y = "Autokorrelasjonsfaktor") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) 

p2
p3 <- ggplot(reg1, aes(x = reg1$residuals)) +
  theme_classic() +
  geom_histogram(fill = "darkblue",
                 color = "white") +
  labs(title = "Fordeling av residualer",
       x = "Residualer",
       y = "Frekvens") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) 

p4 <- ggplot(reg1, aes(sample = reg1$residuals)) +
  theme_classic() +
  geom_qq(color = "darkblue", pch = 1) +
  geom_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot av residualene",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))


grid.arrange(p1, p2, p3, p4, nrow = 2)
# Reg2 - Kumulative flygninger totalt
reg2 <- lm(cumflights ~ day_of_week + trend,
           data = df_total_pre)

stargazer(reg2, type = "text")

ggplot(reg2, aes(x = reg2$fitted.values, y = reg2$residuals, col = reg2$model$day_of_week)) + 
  theme_classic() +
  geom_point() +
  geom_hline(yintercept = 0,
             color = "darkblue") + 
  labs(title = "Residualer ved predikerte flygninger",
       x = "Predikert antall flygninger",
       y = "Residualer") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 18),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_color_discrete(name = "Ukedag",
                       labels = c("Søndag", "Mandag", "Tirsdag", "Onsdag", "Torsdag", "Fredag", "Lørdag"))

plot(reg2$residuals, type = "b",
     ylab = "Residualer", xlab = "Observasjon nummer",
     main = "Residualer mot indeks", col = "darkblue",
     pch = 1, bty = "l")
abline(h = 0, col = "red", lty = 2)

acf(reg2$residuals, main = "Autokorrelasjonsplott",
    xlab = "Antall observasjoner tilbake", ylab = "Autokorrelasjonsfaktor",
    col = "darkblue", ci.col = "red", bty = "l")

ggplot(reg2, aes(x = reg2$residuals)) +
  theme_classic() +
  geom_histogram(fill = "darkblue",
                 color = "white") +
  labs(title = "Fordeling av residualer",
       x = "Residualer",
       y = "Frekvens") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 18),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

ggplot(reg2, aes(sample = reg2$residuals)) +
  theme_bw() +
  geom_qq(color = "darkblue", pch = 1) +
  geom_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot av residualene",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 18),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# Reg3 - Innenlandsflygninger per dag
reg3 <- lm(flights_dom ~ day_of_week,
           data = df_total_pre)

ggplot(reg3, aes(x = reg3$fitted.values, y = reg3$residuals, col = reg3$model$day_of_week)) + 
  theme_bw() +
  geom_point(
    pch = 1) +
  geom_hline(yintercept = 0,
             color = "#84216B") + 
  labs(title = "Residualer ved predikerte flygninger",
       x = "Predikert antall flygninger",
       y = "Residualer") +
  theme(plot.title = element_text(hjust = .5)) 

plot(reg3$residuals, type = "b",
     ylab = "Residualer", xlab = "Observasjon nummer",
     main = "Residualer mot indeks", col = "#84216B",
     pch = 1, bty = "l")
abline(h = 0, col = "#42B8B2", lty = 2)

acf(reg3$residuals, main = "Autokorrelasjonsplott",
    xlab = "Antall observasjoner tilbake", ylab = "Autokorrelasjonsfaktor",
    col = "#84216B", ci.col = "#42B8B2", bty = "l")

ggplot(reg3, aes(x = reg3$residuals)) +
  theme_bw() +
  geom_histogram(fill = "#84216B",
                 color = "white") +
  labs(title = "Fordeling av residualer",
       x = "Residualer",
       y = "Frekvens") +
  theme(plot.title = element_text(hjust = .5)) 

ggplot(reg3, aes(sample = reg3$residuals)) +
  theme_bw() +
  geom_qq(color = "#84216B", pch = 1) +
  geom_qq_line(color = "#42B8B2", linetype = "dashed") +
  labs(title = "Q-Q Plot av residualene",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = .5))

# Reg4 - Kumulative innenlandsflygninger 
reg4 <- lm(cumflights_dom ~ day_of_week + trend,
           data = df_total_pre)

ggplot(reg4, aes(x = reg4$fitted.values, y = reg4$residuals, col = reg4$model$day_of_week)) + 
  theme_bw() +
  geom_point(
    pch = 1) +
  geom_hline(yintercept = 0,
             color = "#84216B") + 
  labs(title = "Residualer ved predikerte flygninger",
       x = "Predikert antall flygninger",
       y = "Residualer") +
  theme(plot.title = element_text(hjust = .5)) 

plot(reg4$residuals, type = "b",
     ylab = "Residualer", xlab = "Observasjon nummer",
     main = "Residualer mot indeks", col = "#84216B",
     pch = 1, bty = "l")
abline(h = 0, col = "#42B8B2", lty = 2)

acf(reg4$residuals, main = "Autokorrelasjonsplott",
    xlab = "Antall observasjoner tilbake", ylab = "Autokorrelasjonsfaktor",
    col = "#84216B", ci.col = "#42B8B2", bty = "l")

ggplot(reg4, aes(x = reg4$residuals)) +
  theme_bw() +
  geom_histogram(fill = "#84216B",
                 color = "white") +
  labs(title = "Fordeling av residualer",
       x = "Residualer",
       y = "Frekvens") +
  theme(plot.title = element_text(hjust = .5)) 

ggplot(reg4, aes(sample = reg4$residuals)) +
  theme_bw() +
  geom_qq(color = "#84216B", pch = 1) +
  geom_qq_line(color = "#42B8B2", linetype = "dashed") +
  labs(title = "Q-Q Plot av residualene",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = .5))

# Reg5 - Utenlandsflygninger per dag
reg5 <- lm(flights_int ~ day_of_week,
           data = df_total_pre)

ggplot(reg5, aes(x = reg5$fitted.values, y = reg5$residuals, col = reg5$model$day_of_week)) + 
  theme_bw() +
  geom_point(
    pch = 1) +
  geom_hline(yintercept = 0,
             color = "#84216B") + 
  labs(title = "Residualer ved predikerte flygninger",
       x = "Predikert antall flygninger",
       y = "Residualer") +
  theme(plot.title = element_text(hjust = .5)) 

plot(reg5$residuals, type = "b",
     ylab = "Residualer", xlab = "Observasjon nummer",
     main = "Residualer mot indeks", col = "#84216B",
     pch = 1, bty = "l")
abline(h = 0, col = "#42B8B2", lty = 2)

acf(reg5$residuals, main = "Autokorrelasjonsplott",
    xlab = "Antall observasjoner tilbake", ylab = "Autokorrelasjonsfaktor",
    col = "#84216B", ci.col = "#42B8B2", bty = "l")

ggplot(reg5, aes(x = reg5$residuals)) +
  theme_bw() +
  geom_histogram(fill = "#84216B",
                 color = "white") +
  labs(title = "Fordeling av residualer",
       x = "Residualer",
       y = "Frekvens") +
  theme(plot.title = element_text(hjust = .5)) 

ggplot(reg5, aes(sample = reg5$residuals)) +
  theme_bw() +
  geom_qq(color = "#84216B", pch = 1) +
  geom_qq_line(color = "#42B8B2", linetype = "dashed") +
  labs(title = "Q-Q Plot av residualene",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = .5))

# Reg6 - Kumulative utenlandsflygninger
reg6 <- lm(cumflights_int ~ day_of_week + trend,
           data = df_total_pre)

ggplot(reg6, aes(x = reg6$fitted.values, y = reg6$residuals, col = reg6$model$day_of_week)) + 
  theme_bw() +
  geom_point(
    pch = 1) +
  geom_hline(yintercept = 0,
             color = "#84216B") + 
  labs(title = "Residualer ved predikerte flygninger",
       x = "Predikert antall flygninger",
       y = "Residualer") +
  theme(plot.title = element_text(hjust = .5)) 

plot(reg6$residuals, type = "b",
     ylab = "Residualer", xlab = "Observasjon nummer",
     main = "Residualer mot indeks", col = "#84216B",
     pch = 1, bty = "l")
abline(h = 0, col = "#42B8B2", lty = 2)

acf(reg6$residuals, main = "Autokorrelasjonsplott",
    xlab = "Antall observasjoner tilbake", ylab = "Autokorrelasjonsfaktor",
    col = "#84216B", ci.col = "#42B8B2", bty = "l")

ggplot(reg6, aes(x = reg6$residuals)) +
  theme_bw() +
  geom_histogram(fill = "#84216B",
                 color = "white") +
  labs(title = "Fordeling av residualer",
       x = "Residualer",
       y = "Frekvens") +
  theme(plot.title = element_text(hjust = .5)) 

ggplot(reg6, aes(sample = reg6$residuals)) +
  theme_bw() +
  geom_qq(color = "#84216B", pch = 1) +
  geom_qq_line(color = "#42B8B2", linetype = "dashed") +
  labs(title = "Q-Q Plot av residualene",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = .5))


# Printer ut regresjonene

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, 
          type = "html", out = "stargazer.html", 
          covariate.labels = c("Mandag", "Tirsdag", "Onsdag", "Torsdag", "Fredag", "Lørdag", "Tidindeks", "Intercept (Søndag)"),
          column.labels = c("Totale flygninger", "Innenlandsflygninger", "Utenlandsflygninger"),
          column.separate = c(2, 2, 2),
          dep.var.caption = "Avhengige variabler",
          dep.var.labels = c("Daglig", "Kumulativt", "Daglig", "Kumulativt", "Daglig", "Kumulativt"), omit.stat = "f")
# Kan legge inn single.row = TRUE om vi har lite plass.


# Oppgave 3 ------

df_total_post$prediksjon1 = as.numeric(predict(reg1, 
                       newdata = df_total_post))
  
df_total_post$prediksjon2 = as.numeric(predict(reg2,
                       newdata = df_total_post))
  
df_total_post$prediksjon3 = as.numeric(predict(reg3, 
                       newdata = df_total_post))

df_total_post$prediksjon4 = as.numeric(predict(reg4, 
                       newdata = df_total_post))
  
df_total_post$prediksjon5 = as.numeric(predict(reg5,
                       newdata = df_total_post))
  
df_total_post$prediksjon6 = as.numeric(predict(reg6,
                       newdata = df_total_post))

ggplot(df_total, aes(x = dep_date, y = flights)) +
  theme_classic() +
  geom_line(col = "red",
            size = 1) +
  geom_line(data = df_total_post, aes(x = dep_date, y = prediksjon1),
            col = "darkblue",
            size = 1) +
  labs(title = "Predikert mot observert antall totale flygninger",
       x = "Dato",
       y = "Antall daglige flygninger",
       subtitle = "Rød er observert. Blå er predikert.") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        plot.subtitle = element_text(size = 10),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_x_date(labels = date_format("%d-%m-%y"))

c1 <- ggplot(df_total, aes(x = dep_date, y = flights_dom)) +
  theme_classic() +
  geom_line(col = "red",
            size = 1) +
  geom_line(data = df_total_post, aes(x = dep_date, y = prediksjon3),
            col = "darkblue",
            size = 1) +
  labs(title = "Predikert mot observert antall innenlandsflygninger",
       x = "Dato",
       y = "Antall daglige flygninger",
       subtitle = "Rød er observert. Blå er predikert.") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        plot.subtitle = element_text(size = 10),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_x_date(labels = date_format("%d-%m-%y"))

c2 <- ggplot(df_total, aes(x = dep_date, y = flights_int)) +
  theme_classic() +
  geom_line(col = "red",
            size = 1) +
  geom_line(data = df_total_post, aes(x = dep_date, y = prediksjon5),
            col = "darkblue",
            size = 1) +
  labs(title = "Predikert mot observert antall utenlandsflygninger",
       x = "Dato",
       y = "Antall daglige flygninger",
       subtitle = "Rød er observert. Blå er predikert.") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        plot.subtitle = element_text(size = 10),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_x_date(labels = date_format("%d-%m-%y"))

grid.arrange(c1, c2, nrow = 1)

ggplot(df_total, aes(x = dep_date, y = cumflights)) +
  theme_classic() +
  geom_line(col = "red",
            size = 1) +
  geom_line(data = df_total_post, aes(x = dep_date, y = prediksjon2),
            col = "darkblue",
            size = 1) +
  labs(title = "Predikert mot observert kumulativt totale flygninger",
       x = "Dato",
       y = "Kumulativt antall flygninger",
       subtitle = "Rød er observert. Blå er predikert.") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 14),
        plot.subtitle = element_text(size = 10),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_x_date(labels = date_format("%d-%m-%y"))

df_total_post %>% 
  select(trend, flights, flights_int, flights_dom) %>% 
  filter(trend <= 63) %>% 
  summarise(`Sum Totalt` = sum(flights),
            `Sum Dom` = sum(flights_dom),
            `Sum Int` = sum(flights_int)) %>%
  stargazer::stargazer(type = "text", summary = FALSE, digits = 2)

df_total_post %>% 
  select(trend, flights, flights_int, flights_dom) %>% 
  filter(trend < 70) %>% 
  filter(trend > 63) %>% 
  summarise(`Sum Totalt` = sum(flights),
            `Sum Dom` = sum(flights_dom),
            `Sum Int` = sum(flights_int)) %>%
  stargazer::stargazer(type = "text", summary = FALSE, digits = 2)


# Oppgave 4 ----

dffly <- df %>% filter(airline == "DY" | airline == "WF" | airline == "SK")
dffly$airline <- as.factor(dffly$airline)
dffly$airline <- as.character(dffly$airline)

df_airlinefly <- df_airline %>%  filter(airline == "DY" | airline == "WF" | airline == "SK")
df_airlinefly$airline <- as.factor(df_airlinefly$airline)
df_airlinefly$airline <- as.character(df_airlinefly$airline)

df_airlineflyf <- df_airlinefly %>% filter(dep_date <= '2020-03-15')
df_airlineflye <- df_airlinefly %>% filter(dep_date > '2020-03-15')
df_nor <- df_airlinefly %>% filter(airline == "DY")
df_sas <- df_airlinefly %>% filter(airline == "SK")
df_wid <- df_airlinefly %>% filter(airline == "WF")

#Norske mot hverandre
plot(x = df_nor$dep_date, y = df_nor$flights, type = "l", col = "red", 
     ylim = c(0, 500), xlab = "Dato", ylab = "Antall flygninger", main = "Flyvninger per dag for Norwegian, SAS og Wideroe")
lines(x = df_sas$dep_date, y = df_sas$flights, type = "l", col = "blue")
lines(x = df_wid$dep_date, y = df_wid$flights, type = "l", col = "forestgreen")

library(scales)


farger <- c("Norwegian" = "red", "SAS" = "blue", "Wideroe" = "forestgreen")

ggplot(df_nor, aes(x = dep_date, y = flights, color = "Norwegian")) +
  theme_classic() +
  geom_line(
    size = 1) +
  geom_line(data = df_sas, aes(x = dep_date, y = flights, color = "SAS"), 
            size = 1) +
  geom_line(data = df_wid, aes(x = dep_date, y = flights, color = "Wideroe"), 
            size = 1) +
  labs(title = "Flygninger per dag for Norwegian, SAS og Widerøe",
       x = "Dato", 
       y = "Antall flygninger",
       color = "Flyselskap") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 18),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 1),
        panel.grid.minor = element_line(size = 1),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_x_date(labels = date_format("%d-%m-%y")) +
  scale_color_manual(values = farger)

#Norske mot hverandre international
plot(x = df_nor$dep_date, y = df_nor$flights_int, type = "l", col = "red", 
     ylim = c(0, 100), xlab = "Dato", ylab = "Antall flygninger", main = "Flyvninger per dag for Norwegian, SAS og Wideroe")
lines(x = df_sas$dep_date, y = df_sas$flights_int, type = "l", col = "blue")
lines(x = df_wid$dep_date, y = df_wid$flights_int, type = "l", col = "forestgreen")

#Norske mot hverandre domestic
plot(x = df_nor$dep_date, y = df_nor$flights_dom, type = "l", col = "red", 
     ylim = c(0, 500))
lines(x = df_sas$dep_date, y = df_sas$flights_dom, type = "l", col = "blue")
lines(x = df_wid$dep_date, y = df_wid$flights_dom, type = "l", col = "forestgreen")

mean(df_airlineflyf$flights)
mean(df_airlineflye$flights)
mean(df_airlineflye$flights)/mean(df_airlineflyf$flights)

mean(df_airlineflyf$flights_int)
mean(df_airlineflye$flights_int)
mean(df_airlineflye$flights_int)/mean(df_airlineflyf$flights_int)

mean(df_airlineflyf$flights_dom)
mean(df_airlineflye$flights_dom)
mean(df_airlineflye$flights_dom)/mean(df_airlineflyf$flights_dom)

# prosentvis nedgang i avganger før og etter 15.mars total -----
#Norwegian:

library(forecast)

norts <- ts(data = df_nor$flights, start = c(1,1), end = c(8, 6), frequency = 7)
norts2 <- ts(data = df_nor$flights, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompnor <- stl(norts, s.window = "periodic")
autoplot(dekompnor)

prediksjonnor <- forecast(dekompnor, h = 22, level = 0.95)
autoplot(prediksjonnor)
plot(prediksjonnor, type = "l", xlim = c(1,12) , ylim = c(0,210))
lines(norts2, type = "b", pch = 20)

Norwegian_tap <- sum(prediksjonnor$mean[2:22])- sum(norts2[57:77])
Norwegian_tap
Norwegian_prosentvis_mistet <- Norwegian_tap/(sum(prediksjonnor$mean[2:22]))
Norwegian_prosentvis_mistet_ttest <- c(1-(norts2[57:77]/prediksjonnor$mean[2:22]))
Norwegian_prosentvis_mistet
Norwegian_prosentvis_mistet_ttest

Sas_prosentvis_mistet_ttest <- c(1-(sasts2[57:77]/prediksjonsas$mean[2:22]))
Sas_prosentvis_mistet_ttest

Wid_prosentvis_mistet_ttest <- c(1-(widts2[57:77]/prediksjonwid$mean[2:22]))
Wid_prosentvis_mistet_ttest

Wid_prosentvis_mistet_ttest - Norwegian_prosentvis_mistet_ttest

wilcox.test(Wid_prosentvis_mistet_ttest, Sas_prosentvis_mistet_ttest, paired = TRUE)
wilcox.test(Wid_prosentvis_mistet_ttest, Norwegian_prosentvis_mistet_ttest, paired = TRUE)
t.test(Sas_prosentvis_mistet_ttest, Wid_prosentvis_mistet_ttest, alternative = "greater",
       paired = TRUE)
t.test(Norwegian_prosentvis_mistet_ttest, Wid_prosentvis_mistet_ttest, alternative = "greater",
       paired = TRUE)
#SAS
sasts <- ts(data = df_sas$flights, start = c(1,1), end = c(8, 7), frequency = 7)
sasts2 <- ts(data = df_sas$flights, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompsas <- stl(sasts, s.window = "periodic")
autoplot(dekompsas)

prediksjonsas <- forecast(dekompsas, h = 22, level = 0.95)
autoplot(prediksjonsas)
plot(prediksjonsas, type = "l", xlim = c(1,12) , ylim = c(0,400))
lines(sasts2, type = "b", pch = 20)

SAS_tap <- sum(prediksjonsas$mean[2:22])- sum(sasts2[57:77])
SAS_tap
SAS_prosentvis_mistet <- SAS_tap/(sum(prediksjonsas$mean[2:22]))
SAS_prosentvis_mistet

#Wideroe
widts <- ts(data = df_wid$flights, start = c(1,1), end = c(8, 6), frequency = 7)
widts2 <- ts(data = df_wid$flights, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompwid <- stl(widts, s.window = "periodic")
autoplot(dekompwid)

prediksjonwid <- forecast(dekompwid, h = 22, level = 0.95)
autoplot(prediksjonwid)
plot(prediksjonwid, type = "l", xlim = c(1,12) , ylim = c(0,400))
lines(widts2, type = "b", pch = 20)

Wideroe_tap <- sum(prediksjonwid$mean[2:22])- sum(widts2[57:77])
Wideroe_tap
Wideroe_prosentvis_mistet <- Wideroe_tap/(sum(prediksjonwid$mean[2:22]))
Wideroe_prosentvis_mistet

#Prosentvis nedgang i flygninger for domestic-----

#Norwegian:

nortsdom <- ts(data = df_nor$flights_dom, start = c(1,1), end = c(8, 6), frequency = 7)
nortsdom2 <- ts(data = df_nor$flights_dom, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompnordom <- stl(nortsdom, s.window = "periodic")
autoplot(dekompnordom)

prediksjonnordom <- forecast(dekompnordom, h = 22, level = 0.95)
autoplot(prediksjonnordom)
plot(prediksjonnordom, type = "l", xlim = c(1,12) , ylim = c(0,210))
lines(nortsdom2, type = "b", pch = 20)

Norwegian_tapdom <- sum(prediksjonnordom$mean[2:22])- sum(nortsdom2[57:77])
Norwegian_tapdom
Norwegian_prosentvis_mistetdom <- Norwegian_tapdom/(sum(prediksjonnordom$mean[2:22]))
Norwegian_prosentvis_mistetdom

#SAS
sastsdom <- ts(data = df_sas$flights_dom, start = c(1,1), end = c(8, 6), frequency = 7)
sastsdom2 <- ts(data = df_sas$flights_dom, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompsasdom <- stl(sastsdom, s.window = "periodic")
autoplot(dekompsasdom)

prediksjonsasdom <- forecast(dekompsasdom, h = 22, level = 0.95)
autoplot(prediksjonsasdom)
plot(prediksjonsasdom, type = "l", xlim = c(1,12) , ylim = c(0,210))
lines(sastsdom2, type = "b", pch = 20)

SAS_tapdom <- sum(prediksjonsasdom$mean[2:22])- sum(sastsdom2[57:77])
SAS_tapdom
SAS_prosentvis_mistetdom <- SAS_tapdom/(sum(prediksjonsasdom$mean[2:22]))
SAS_prosentvis_mistetdom

#Wideroe
widtsdom <- ts(data = df_wid$flights_dom, start = c(1,1), end = c(8, 6), frequency = 7)
widtsdom2 <- ts(data = df_wid$flights_dom, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompwiddom <- stl(widtsdom, s.window = "periodic")
autoplot(dekompwiddom)

prediksjonwiddom <- forecast(dekompwiddom, h = 22, level = 0.95)
autoplot(prediksjonwiddom)
plot(prediksjonwiddom, type = "l", xlim = c(1,12) , ylim = c(0,210))
lines(widtsdom2, type = "b", pch = 20)

Wideroe_tapdom <- sum(prediksjonwiddom$mean[2:22])- sum(widtsdom2[57:77])
Wideroe_tapdom
Wideroe_prosentvis_mistetdom <- Wideroe_tapdom/(sum(prediksjonwiddom$mean[2:22]))
Wideroe_prosentvis_mistetdom

#Prosentvis nedgang i flygninger for international -----
#Norwegian
nortsint <- ts(data = df_nor$flights_int, start = c(1,1), end = c(8, 6), frequency = 7)
nortsint2 <- ts(data = df_nor$flights_int, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompnorint <- stl(nortsint, s.window = "periodic")
autoplot(dekompnorint)

prediksjonnorint <- forecast(dekompnorint, h = 22, level = 0.95)
autoplot(prediksjonnorint)
plot(prediksjonnorint, type = "l", xlim = c(1,12) , ylim = c(0,210))
lines(nortsint2, type = "b", pch = 20)

Norwegian_tapint <- sum(prediksjonnorint$mean[2:22])- sum(nortsint2[57:77])
Norwegian_tapint
Norwegian_prosentvis_mistetint <- Norwegian_tapint/(sum(prediksjonnorint$mean[2:22]))
Norwegian_prosentvis_mistetint

#SAS
sastsint <- ts(data = df_sas$flights_int, start = c(1,1), end = c(8, 6), frequency = 7)
sastsint2 <- ts(data = df_sas$flights_int, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompsasint <- stl(sastsint, s.window = "periodic")
autoplot(dekompsasint)

prediksjonsasint <- forecast(dekompsasint, h = 22, level = 0.95)
autoplot(prediksjonsasint)
plot(prediksjonsasint, type = "l", xlim = c(1,12) , ylim = c(0,210))
lines(sastsint2, type = "b", pch = 20)

SAS_tapint <- sum(prediksjonsasint$mean[2:22])- sum(sastsint2[57:77])
SAS_tapint
SAS_prosentvis_mistetint <- SAS_tapint/(sum(prediksjonsasint$mean[2:22]))
SAS_prosentvis_mistetint

#Wideroe
widtsint <- ts(data = df_wid$flights_int, start = c(1,1), end = c(8, 6), frequency = 7)
widtsint2 <- ts(data = df_wid$flights_int, start = c(1, 1), end = c(11, 7), frequency = 7)

dekompwidint <- stl(widtsint, s.window = "periodic")
autoplot(dekompwidint)

prediksjonwidint <- forecast(dekompwidint, h = 22, level = 0.95)
autoplot(prediksjonwidint)
plot(prediksjonwidint, type = "l", xlim = c(1,12) , ylim = c(0,20))
lines(widtsint2, type = "b", pch = 20)

Wideroe_tapint <- sum(prediksjonwidint$mean[2:22])- sum(widtsint2[57:77])
Wideroe_tapint
Wideroe_prosentvis_mistetint <- Wideroe_tapint/(sum(prediksjonwidint$mean[2:22]))
Wideroe_prosentvis_mistetint

#Oppsummering av oppgave 4 -----
#totalt
Norwegian_tap
Norwegian_prosentvis_mistet
SAS_tap
SAS_prosentvis_mistet
Wideroe_tap
Wideroe_prosentvis_mistet
#domestic
Norwegian_tapdom
Norwegian_prosentvis_mistetdom
SAS_tapdom
SAS_prosentvis_mistetdom
Wideroe_tapdom
Wideroe_prosentvis_mistetdom
#International
Norwegian_tapint
Norwegian_prosentvis_mistetint
SAS_tapint
SAS_prosentvis_mistetint
Wideroe_tapint
Wideroe_prosentvis_mistetint 


