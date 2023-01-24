install.packages("rmarkdown")

#load required packages
library(dslabs)
library(tidyverse)
library(ggplot2)

#examine gapminder data frame
help("gapminder")
str(gapminder)
summary(gapminder)
class(gapminder)

#select only African countries
africadata <-
  filter(gapminder, continent=="Africa")
#select only infant mortality and life expectancy from africadata
obj1<-subset(africadata, select = c(infant_mortality, life_expectancy))
str(obj1)
summary(obj1)
#select only population and life expectancy from africadata
obj2<-subset(africadata, select = c(population, life_expectancy))
str(obj2)
summary(obj2)

#plot life exp as a func of inf mortality
ggplot(data = obj1, aes(infant_mortality, life_expectancy))+
  geom_point(cex=3, alpha=0.3)+
  ylab("Life Expectancy (Yrs)")+
  xlab("Infant Mortality (per 1000 Individuals)")+
  ylim(0,85)+
  theme_classic()


#plot life exp as a func of population size
ggplot(data = obj2, aes(log(population), life_expectancy))+
  geom_point(cex=3, alpha=0.3)+
  ylab("Life Expectancy (Yrs)")+
  xlab(expression("Log"[e]*"(Population)"))+
  theme_classic()

#find missing values in infant mortality
missing<-is.na(africadata$infant_mortality)
missing_years<-africadata$year[missing]
missing_years

africadata2000<-
  filter(africadata, year=="2000")

#plot using only data from 2000

#plot life exp as a func of inf mortality for the year 2000
ggplot(data = africadata2000, aes(infant_mortality, life_expectancy))+
  geom_point(cex=3, alpha=0.3)+
  ylab("Life Expectancy (Yrs)")+
  xlab("Infant Mortality (per 1000 Individuals)")+
  ylim(0,85)+
  theme_classic()

#plot life exp as a func of population size for the year 2000
ggplot(data = africadata2000, aes(log(population), life_expectancy))+
  geom_point(cex=3, alpha=0.3)+
  ylab("Life Expectancy (Yrs)")+
  xlab(expression("Log"[e]*"(Population)"))+
  theme_classic()

#fit linear regression to both plotted functions
fit1<-lm(life_expectancy~infant_mortality, data = africadata2000)
summary(fit1)

fit2<-lm(life_expectancy~population, data = africadata2000)
summary(fit2)
