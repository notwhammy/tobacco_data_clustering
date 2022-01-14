---
title: "R Notebook"
output: html_notebook
---

```{r}
use_ww <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/tobacco_use_ww.csv")
use_us <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/tobacco_use_us.csv")
death <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/death_rates_smoking_age.csv")
sales <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/sales_per_day.csv")
stop <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/stop_smoking.csv")
#resp <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/us_chronic_resp_disease.csv")
prod <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/tobacco_production.csv")
```

```{r}
cdn_sales <- ts(sales[sales$Code=="CAN",4])
gbr_sales <- ts(sales[sales$Code=="GBR",4])

plot(sales[sales$Code=="CAN",3:4], col='red', type = 'l')
lines(sales[sales$Code=="USA",3:4], col = 'blue', lty = 2)
lines(sales[sales$Code=="ITA",3:4], col = 'green', lty = 2)
lines(sales[sales$Code=="FRA",3:4], col = 'pink', lty = 2)
lines(sales[sales$Code=="GBR",3:4], col = 'grey', lty = 2)
lines(sales[sales$Code=="ESP",3:4], col = 'firebrick', lty = 2)
plot(gbr_sales)
```


```{r, warning=FALSE, message=FALSE}
df_cdn <- data.frame(can = sales[sales$Code=="CAN",4], ita = sales[(sales$Code=="ITA" & sales$Year >= 1920),4])
gbr_sales = sales[(sales$Code=="GBR" & sales$Year >= 1920),3:4]
ita_sales = sales[(sales$Code=="ITA" & sales$Year >= 1920),3:4]
esp_sales = sales[(sales$Code=="FRA" & sales$Year >= 1920),3:4]
df_eu = merge(x=gbr_sales, y=ita_sales, by="Year", all.x=TRUE)
names(df_eu)[2:3]<-c("GBR_amt", "ITA_amt")
df_eu = merge(x=df_eu, y=esp_sales, by="Year", all.x=TRUE)
names(df_eu)[4]<-"ESP_amt"
df_eu

library(CausalImpact)
pre.period <- c(1,60)
post.period <- c(61,91)
impact <- CausalImpact(df_cdn, pre.period, post.period)
```

```{r}
plot(impact)
summary(impact)
df_eu_new <- df_eu[13:91,]
pre.period_eu <- c(1,45)
post.period_eu <- c(46,79)
impact_eu <- CausalImpact(df_eu_new[,c(2:4)], pre.period_eu, post.period_eu)
plot(impact_eu)
summary(impact_eu)
```

```{r}
plot(death[death$Code == "FRA", c(3,7)])
```

```{r, warning=FALSE}
wwdata <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/wwdata.csv")
cpi <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/cpi2020.csv")
```


```{r, warning=FALSE}
wwdata
sales
stop
use_ww
death
prod
```

```{r}
library(dplyr)
mpower_tst <- stop[1:208,]
mpower_2012 <- mpower_tst[mpower_tst$Year==2012,]
mpower_2012
names(wwdata)[1]<-"Code"
wwdata_2011 <- wwdata[wwdata$Year==2011,c(1,2,3,4)]
sales_2011 <- sales[sales$Year==2011,c(2,3,4)]
wwdata_2012 <- wwdata[wwdata$Year==2012,c(1,2,3,4)]
sales_2012 <- sales[sales$Year==2012,c(2,3,4)]
wwd_1112 <- left_join(x=wwdata_2011, y=wwdata_2012, by="Code")[,c(1,3,4,6,7)]
names(wwd_1112) <- c("Code", "SmokedPerPerson2011", "DeathRateAll2011", 
                     "SmokedPerPerson2012", "DeathRateAll2012")
sls_1112 <- left_join(x=sales_2011, y=sales_2012, by="Code")[,c(1,3,5)]
names(sls_1112) <- c("Code", "amt2011", "amt2012")
data_1112 <- left_join(x=wwd_1112, y=sls_1112, by="Code")
data_1112["smoked_delta"] <- data_1112["SmokedPerPerson2012"] - data_1112["SmokedPerPerson2011"]
data_1112["death_delta"] <- data_1112["DeathRateAll2012"] - data_1112["DeathRateAll2011"]
data_1112["amt_delta"] <- data_1112["amt2012"] - data_1112["amt2011"] 
data_1112 <- left_join(x=data_1112, y=mpower_2012, by="Code")
data_1112["avgBanHelp"] <- (data_1112["EnforceBansTobaccoAd"]+data_1112["HelpToQuit"])/2
data_1112["smoked/banhelp"] <- data_1112["smoked_delta"] / data_1112["avgBanHelp"]
data_1112["death/banhelp"] <- data_1112["death_delta"] / data_1112["avgBanHelp"]
data_1112["amt/banhelp"] <- data_1112["amt_delta"] / data_1112["avgBanHelp"]
data_1112
```


```{r}
plot(data_1112["smoked_delta"])
plot(data_1112["death_delta"])
plot(data_1112["amt_delta"])
```

```{r}
plot(data_1112[c("smoked_delta", "avgBanHelp")])
plot(data_1112[c("death_delta", "avgBanHelp")])
plot(data_1112[c("amt_delta", "avgBanHelp")])
```

```{r, warning=FALSE}
cpi_2012 <- cpi[,c("Code", "CPI.Score.2012")]
cpi_2012
data_1112 <- left_join(x=data_1112, y=cpi_2012, by="Code")
data_1112
```


```{r}
plot(data_1112[c("CPI.Score.2012","smoked/banhelp")])
plot(data_1112[c("CPI.Score.2012","death/banhelp")])
pct_new <- data_1112[c("CPI.Score.2012","AvgTaxesAsPctCigarettePrice")]
pct_new[1] <- log(pct_new[1])
pct_new[2] <- pct_new[2]
plot(pct_new)
plot(data_1112[c("CPI.Score.2012","AvgCigarettePriceDollars")])
```


```{r}
data_1112[which.min(as.numeric(unlist(data_1112["death/banhelp"]))),]
data_1112[which.min(as.numeric(unlist(data_1112["smoked/banhelp"]))),]
```


```{r}
plot(mpower_2012[,c(4:5)])
```

```{r}
plot(sales[sales$Code=="HUN",3:4])
plot(death[death$Code=="HUN",c(3,7)])
plot(use_ww[use_ww$SpatialDimValueCode=="HUN", c(5,7)])
```

```{r, fig.width=10, fig.height=10}
plot(data_1112[,c(2:7, 13:16, 21)])
```

```{r}
cpi_new <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/cpi_ready.csv")
plot(cpi_new[cpi_new$Code=="UKR",3:4])
plot(prod[(prod$Country.or.Area=="Ukraine" & prod$Unit=="Metric tons"),c(2,4)])
plot(cpi_new[cpi_new$Code=="TUR",3:4])
plot(prod[(prod$Country.or.Area=="Turkey" & prod$Unit=="Metric tons"),c(2,4)])
names(cpi_new)[1] <- "Entity"
cpi_new
cpi_prod <- left_join(x=cpi_new, y=prod, by= c("Entity" = "Country.or.Area", "Year" = "Year" ))
cpi_prod

```


```{r}
cpi_prod_plot <- cpi_prod[(cpi_prod$Unit == "Metric tons" & cpi_prod$Year==2016),c(4,6)]
plot(cpi_prod_plot, ylim=c(0,50000))
```


```{r, warnings=FALSE, message=FALSE}
use_ww[use_ww$SpatialDimValueCode=="CIV",4] <- rep("Cote d'Ivoire",
                                                   length(use_ww[use_ww$SpatialDimValueCode=="CIV",4]))
mpower <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/mpower.csv")
mpower
gini <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/gini_ready.csv")
names(gini)[1] <- "Country"
gini
gdp <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/gdp_ready.csv")
names(gdp)[1] <- "Country"
gdp
literacy <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/literacy_ready.csv")
names(literacy)[1] <- "Country"
literacy
edexp <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/govt_education_expenditure_ready.csv")
names(edexp)[1] <- "Country"
edexp
suicide <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/suicide_ready.csv")
names(suicide)[1] <- "Code"
suicide
```

```{r}
data <- use_ww[use_ww$Year>=2012,c(2:7)]
names(data) <- c("Region", "Code", "Country", "Year", "Sex", "Usage")
data <- data[data$Sex=="Both sexes",]
data <- left_join(x=data, y=cpi_new, by=c("Code" = "Code", "Year" = "Year"))[,c(1,2,3,4,6,8)]
data <- left_join(x=data, y=mpower, by=c("Country" = "Country", "Year" = "Year"))
names(data)[8:13] <- c("Protect_Smoke", "Offer_Help", "Warn", "Enforce_Bans", "Raise_Taxes", "Anti_Mass")
data <- left_join(x=data, y=stop, by=c("Code" = "Code", "Year" = "Year"))[,c(1:13,15,16)]
names(data)[14:15] <- c("Price", "Taxes_per_Cig")
data <- left_join(x=data, y=gini, by=c("Code" = "Code", "Year" = "Year"))[,c(1:15,17)]
data <- left_join(x=data, y=gdp[,2:4], by=c("Code" = "Code", "Year" = "Year"))
data <- left_join(x=data, y=literacy[,2:4], by=c("Code" = "Code", "Year" = "Year"))
data <- left_join(x=data, y=edexp[,2:4], by=c("Code" = "Code", "Year" = "Year"))
data <- left_join(x=data, y=suicide, by=c("Code" = "Code", "Year" = "Year"))
data
```


```{r, fig.width=10, fig.height=10}
train <- data[(data$Year>=2013 & data$Year<2018),]
test <- data[data$Year==2018,]
plot(train[,5:20])
```

```{r}
library(car)
train_clean <- train[,c(5:6, 14:19)]
train_x <- train[,c(6,14:19)]
train_y <- train$Usage
plot(train$Enforce_Bans, train$Offer_Help)
vif_result <- vif(lm(Usage ~ CPI.Score + Price + Taxes_per_Cig + Gini + GDP_per_capita +
                       Literacy + Education_exp, data = train))
vif_result
```

```{r}
lm_usg <- lm(Usage ~ CPI.Score + Gini + GDP_per_capita +
                       Literacy + Education_exp, data = train)
summary(lm_usg)
lm_2 <- lm(Usage ~ CPI.Score + Gini + GDP_per_capita + Education_exp, data = train)
summary(lm_2)
plot(train_clean[,c(1,2,5,6,8)])
plot(log(train_clean$Gini), train_clean$Usage)
plot(log(train_clean$GDP_per_capita), train_clean$Usage)
```

```{r}
plot(fitted(lm_usg), residuals(lm_usg))
plot(fitted(lm_2), residuals(lm_2))
qqnorm(residuals(lm_2))
```


```{r}
combined <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/combined.csv")
comb_bra <- combined[combined$Country == "Brazil",11]
comb_arg <- combined[combined$Country == "Argentina",11]
comb_yr_latam <- combined[combined$Country == "Brazil",10]
df_latam <- data.frame(Brazil = comb_bra, Argentina = comb_arg)
df_latam
impact_latam <- CausalImpact(df_latam, pre.period_new, post.period_new)
plot(impact_latam)
summary(impact_latam)

```

```{r}
comb_ken <- combined[combined$Country == "Kenya",11]
comb_eth <- combined[combined$Country == "Ethiopia",11]
df_afr1 <- data.frame(Kenya = comb_ken, Ethiopia = comb_eth)
df_afr1
pre.period_new <- c(1,16)
post.period_new <- c(17,28)
impact_afr1 <- CausalImpact(df_afr1, pre.period_new, post.period_new)
plot(impact_afr1)
summary(impact_afr1)
```

```{r}
comb_ina <- combined[combined$Country == "Indonesia",11]
comb_tha <- combined[combined$Country == "Thailand",11]
df_as1 <- data.frame(Indonesia = comb_ina, Thailand = comb_tha)
df_as1
pre.period_new <- c(1,17)
post.period_new <- c(18,28)
impact_as1 <- CausalImpact(df_as1, pre.period_new, post.period_new)
plot(impact_as1)
summary(impact_as1)
```

```{r}
comb_col <- combined[combined$Country == "Colombia",11]
comb_par <- combined[combined$Country == "Paraguay",11]
df_lt2 <- data.frame(Colombia = comb_col, Paraguay = comb_par)
df_lt2
pre.period_new <- c(1,17)
post.period_new <- c(18,28)
impact_lt2 <- CausalImpact(df_lt2, pre.period_new, post.period_new)
plot(impact_lt2)
summary(impact_lt2)
```

```{r}
comb_tur <- combined[combined$Country == "Turkey",11]
comb_irq <- combined[combined$Country == "Iraq",11]
df_as2 <- data.frame(Turkey = comb_tur, Iraq = comb_irq)
df_as2
pre.period_new <- c(1,15)
post.period_new <- c(16,28)
impact_as2 <- CausalImpact(df_as2, pre.period_new, post.period_new)
plot(impact_as2)
summary(impact_as2)
```

```{r, fig.width=10, fig.height=10}
library(factoextra)
library(cluster)
latlong_raw <- read.csv("C:/Sam/Files/3B_CoOp_4/Datathon/lat_long.csv")[1:245,2:6]
latlong <- latlong_raw[,1:4]
rownames(latlong) <- latlong[,1]
latlong <- latlong[,-1]
latlong <- na.omit(latlong)
latlong <- scale(latlong)
fviz_nbclust(latlong, kmeans, method="wss")
kmc <- kmeans(latlong, centers=7, iter.max=20, nstart=1)
kmc
fviz_cluster(kmc, data=latlong)
```

```{r}
library(tibble)
kmc_clust = rownames_to_column(data.frame(kmc$cluster))
kmc_clust
```


```{r}
latlong_raw <- na.omit(latlong_raw)
latlong_raw <- left_join(x=latlong_raw, y=kmc_clust, by=c("name" = "rowname"))
latlong_raw
```


```{r}
comb_som <- combined[combined$Country == "Somalia",11]
comb_mau <- combined[combined$Country == "Mauritius",11]
df_afr3 <- data.frame(Mauritius = comb_mau, Somalia= comb_som)
df_afr3
pre.period_new <- c(1,15)
post.period_new <- c(16,28)
impact_afr3 <- CausalImpact(df_afr3, pre.period_new, post.period_new)
plot(impact_afr3)
summary(impact_afr3)
```

















