# - fig1 FKTP - FKRTL daily weekly
# - fig2 Tavg daily, weekly & monthly
# - fig3 corr daily & weekly
# - tab1 poisson regression: model performance
# - fig4 poisson regression: observed & fitted 

# library
library(haven)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
# library(corrplot)
library(zoo)

# dataset
fktpkapitasi <- read_dta("dat/DM2021_fktpkapitasi.dta") # dataset can be obtained through https://data.bpjs-kesehatan.go.id
fktpkapitasi_DIY <- subset(fktpkapitasi, FKP05==34) # "34" is the area code for Yogyakarta Province 
fktpkapitasi_DIY$FKP03 <- as.Date(fktpkapitasi_DIY$FKP03, format="%Y-%m-%d")

fkrtl <- read_dta("dat/DM2021_fkrtl.dta") # dataset can be obtained through https://data.bpjs-kesehatan.go.id
fkrtl_DIY <- subset(fkrtl, FKL05==34) # "34" is the area code for Yogyakarta Province 
fkrtl_DIY$FKL03 <- as.Date(fkrtl_DIY$FKL03, format="%Y-%m-%d")

rm(fktpkapitasi)
rm(fkrtl)
gc()

dat_BMKG <- read.csv("dat/BMKG_GeofisikaSleman_2021.csv", na.strings="#NA", sep=";") # dataset can be obtained through https://dataonline.bmkg.go.id/home?language=english
dat_BMKG$date <- as.Date(dat_BMKG$date, format="%d-%m-%Y")

fktpkapitasi_daily <- fktpkapitasi_DIY %>%
  group_by(FKP03) %>%
  summarise(n_fktp = n())

fktpkapitasi_daily$DoY <- wday(fktpkapitasi_daily$FKP03)

fkrtl_daily <- fkrtl_DIY %>%
  group_by(FKL03) %>%
  summarise(n_fkrtl = n())

fkrtl_daily$DoY <- wday(fkrtl_daily$FKL03)

p1 <- ggplot(fktpkapitasi_daily, aes(x = as.factor(DoY), y = n_fktp)) +
  geom_boxplot() + theme_bw() + ylim(0, 210) +
  labs(title = "primary care", subtitle = "", x="", y="") +
  scale_x_discrete(labels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

p2 <- ggplot(fkrtl_daily, aes(x = as.factor(DoY), y = n_fkrtl)) +
  geom_boxplot() + theme_bw() + ylim(0, 210) +
  labs(title = "referral care", subtitle = "", x="", y="") +
  scale_x_discrete(labels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

# Figure 1
ggarrange(p1, p2,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

fktpkapitasi_daily %>%
  group_by(DoY) %>%
  summarise(mean_fktp = mean(n_fktp),
            sd_fktp = sd(n_fktp))

fkrtl_daily %>%
  group_by(DoY) %>%
  summarise(mean_fkrtl = mean(n_fkrtl),
            sd_fkrtl = sd(n_fkrtl))

names(fktpkapitasi_daily) <- c("date", "n_fktp", "DoY")
names(fkrtl_daily) <- c("date", "n_fkrtl", "DoY")

fkrtl_daily$DoY <- NULL

str(dat_BMKG)

dat <- merge(dat_BMKG, fktpkapitasi_daily, by=c("date"), all.x=T)
dat <- merge(dat, fkrtl_daily, by=c("date"), all.x=T)
dat$T_diu <- dat$Tx - dat$Tn

dat$epiweek <- epiweek(dat$date)
dat$DoY <- wday(dat$date)
dat$YM <- as.yearmon(dat$date)

dat$Tavg_lag00 <- dat$Tavg
dat$Tavg_lag01 <- lag(dat$Tavg, 1)
dat$Tavg_lag02 <- lag(dat$Tavg, 2)
dat$Tavg_lag03 <- lag(dat$Tavg, 3)
dat$Tavg_lag04 <- lag(dat$Tavg, 4)
dat$Tavg_lag05 <- lag(dat$Tavg, 5)
dat$Tavg_lag06 <- lag(dat$Tavg, 6)
dat$Tavg_lag07 <- lag(dat$Tavg, 7)
dat$Tavg_lag08 <- lag(dat$Tavg, 8)
dat$Tavg_lag09 <- lag(dat$Tavg, 9)
dat$Tavg_lag10 <- lag(dat$Tavg, 10)
dat$Tavg_lag11 <- lag(dat$Tavg, 11)
dat$Tavg_lag12 <- lag(dat$Tavg, 12)
dat$Tavg_lag13 <- lag(dat$Tavg, 13)
dat$Tavg_lag14 <- lag(dat$Tavg, 14)

datNholidays <- filter(dat, DoY != 1)

datNholidays <- filter(datNholidays, date != as.Date("2021-01-01", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-01-02", format="%Y-%m-%d")) ## w53 2020
datNholidays <- filter(datNholidays, date != as.Date("2021-02-12", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-03-11", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-03-14", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-04-02", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-01", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-13", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-14", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-26", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-06-01", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-07-20", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-08-10", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-08-17", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-10-19", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-12-25", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-03-12", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-12", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-17", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-18", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-05-19", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-12-24", format="%Y-%m-%d"))
datNholidays <- filter(datNholidays, date != as.Date("2021-12-27", format="%Y-%m-%d"))

datW <- dat[-c(1:2),] %>%
  group_by(epiweek) %>%
  summarise(n_fktp=sum(n_fktp, na.rm=T),
            n_fkrtl=sum(n_fkrtl, na.rm=T),
            Tavg=round(mean(Tavg, na.rm=T),2))

datW$Tavg_lag00 <- datW$Tavg
datW$Tavg_lag01 <- lag(datW$Tavg, 1)
datW$Tavg_lag02 <- lag(datW$Tavg, 2)
datW$Tavg_lag03 <- lag(datW$Tavg, 3)
datW$Tavg_lag04 <- lag(datW$Tavg, 4)
datW$Tavg_lag05 <- lag(datW$Tavg, 5)
datW$Tavg_lag06 <- lag(datW$Tavg, 6)
datW$Tavg_lag07 <- lag(datW$Tavg, 7)
datW$Tavg_lag08 <- lag(datW$Tavg, 8)

datWNholidays <- datNholidays[-c(1:2),] %>%
  group_by(epiweek) %>%
  summarise(n_fktp=sum(n_fktp, na.rm=T),
            n_fkrtl=sum(n_fkrtl, na.rm=T),
            Tavg=round(mean(Tavg, na.rm=T),2))

datWNholidays$Tavg_lag00 <- datWNholidays$Tavg
datWNholidays$Tavg_lag01 <- lag(datWNholidays$Tavg, 1)
datWNholidays$Tavg_lag02 <- lag(datWNholidays$Tavg, 2)
datWNholidays$Tavg_lag03 <- lag(datWNholidays$Tavg, 3)
datWNholidays$Tavg_lag04 <- lag(datWNholidays$Tavg, 4)
datWNholidays$Tavg_lag05 <- lag(datWNholidays$Tavg, 5)
datWNholidays$Tavg_lag06 <- lag(datWNholidays$Tavg, 6)
datWNholidays$Tavg_lag07 <- lag(datWNholidays$Tavg, 7)
datWNholidays$Tavg_lag08 <- lag(datWNholidays$Tavg, 8)

p1 <- ggplot(dat, aes(x = date, y = n_fktp)) +
  geom_line() + theme_bw() + ylim(0, 225) +
  labs(title = "primary care", subtitle = "daily", x="", y="")
p2 <- ggplot(datNholidays, aes(x = date, y = n_fktp)) +
  geom_line() + theme_bw() + ylim(0, 225) +
  labs(title = "primary care", subtitle = "daily (exclude Sunday)", x="", y="")

p3 <- ggplot(dat, aes(x = date, y = n_fkrtl)) +
  geom_line() + theme_bw() + ylim(0, 175) +
  labs(title = "referral care", subtitle = "daily", x="", y="")
p4 <- ggplot(datNholidays, aes(x = date, y = n_fkrtl)) +
  geom_line() + theme_bw() + ylim(0, 175) +
  labs(title = "referral care", subtitle = "daily (exclude Sunday)", x="", y="")

p5 <- ggplot(datW, aes(x = epiweek, y = n_fktp)) +
  geom_line() + theme_bw() + ylim(200, 950) +
  labs(title = "primary care", subtitle = "weekly", x="", y="")
p6 <- ggplot(datWNholidays, aes(x = epiweek, y = n_fktp)) +
  geom_line() + theme_bw() + ylim(200, 950) +
  labs(title = "primary care", subtitle = "weekly (exclude Sunday and holidays)", x="", y="")

p7 <- ggplot(datW, aes(x = epiweek, y = n_fkrtl)) +
  geom_line() + theme_bw() + ylim(200, 700) +
  labs(title = "referral care", subtitle = "weekly", x="", y="")
p8 <- ggplot(datWNholidays, aes(x = epiweek, y = n_fkrtl)) +
  geom_line() + theme_bw() + ylim(200, 700) +
  labs(title = "referral care", subtitle = "weekly (exclude Sunday and holidays)", x="", y="")

# Figure 2
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
          labels = c("A", "B", "C", "D",
                     "E", "F", "G", "H"),
          ncol = 2, nrow = 4)

range(dat$Tavg)

dat$m <- as.Date(dat$YM)
dat$m <- substr(as.character(dat$m), 6,7)
dat$m <- as.integer(as.character(dat$m))

datNholidays$m <- as.Date(datNholidays$YM)
datNholidays$m <- substr(as.character(datNholidays$m), 6,7)
datNholidays$m <- as.integer(as.character(datNholidays$m))

p1 <- ggplot(dat, aes(x = date, y = Tavg)) +
  geom_line() + theme_bw() + ylim(20, 30) +
  labs(title = "Tavg", subtitle = "daily", x="", y="")

p2 <- ggplot(dat, aes(x = as.factor(m), y = Tavg)) +
  geom_boxplot() + theme_bw() + ylim(20, 30) +
  labs(title = "Tavg", subtitle = "monthly", x="", y="")

p3 <- ggplot(dat, aes(x = as.factor(m), y = n_fktp)) +
  geom_boxplot() + theme_bw() + ylim(0, 250) +
  labs(title = "primary care", subtitle = "monthly", x="", y="")

p4 <- ggplot(dat, aes(x = as.factor(m), y = n_fkrtl)) +
  geom_boxplot() + theme_bw() + ylim(0, 250) +
  labs(title = "referral care", subtitle = "monthly", x="", y="")

p5 <- ggplot(datNholidays, aes(x = as.factor(m), y = n_fktp)) +
  geom_boxplot() + theme_bw() + ylim(0, 250) +
  labs(title = "primary care", subtitle = "monthly (exclude Sunday & holidays)", x="", y="")

p6 <- ggplot(datNholidays, aes(x = as.factor(m), y = n_fkrtl)) +
  geom_boxplot() + theme_bw() + ylim(0, 250) +
  labs(title = "referral care", subtitle = "monthly (exclude Sunday & holidays)", x="", y="")

# figure 3
ggarrange(p1, p2, p3, p4, p5, p6,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3)

##
names(dat)
datTavg_fktp <- select(dat, n_fktp,Tavg_lag00:Tavg_lag14)
datTavg_fkrtl <- select(dat, n_fkrtl,Tavg_lag00:Tavg_lag14)

p.mat_fktp <- matrix(NA, 15, 5)
#i <- 1

for(i in c(1:15)){
  datTavg_corfktp <- cor.test(datTavg_fktp[,(i+1)], datTavg_fktp$n_fktp, method="pearson")
  p.mat_fktp[i,1] <- names(datTavg_fktp)[i+1]
  p.mat_fktp[i,2] <- as.numeric(as.character(datTavg_corfktp$estimate[1]))
  p.mat_fktp[i,3] <- as.numeric(as.character(datTavg_corfktp$conf.int[1]))
  p.mat_fktp[i,4] <- as.numeric(as.character(datTavg_corfktp$conf.int[2]))
  p.mat_fktp[i,5] <- as.numeric(as.character(datTavg_corfktp$p.value))
}

p.mat_fkrtl <- matrix(NA, 15, 5)
#i <- 1

for(i in c(1:15)){
  datTavg_corfkrtl <- cor.test(datTavg_fkrtl[,(i+1)], datTavg_fkrtl$n_fkrtl, method="pearson")
  p.mat_fkrtl[i,1] <- names(datTavg_fkrtl)[i+1]
  p.mat_fkrtl[i,2] <- as.numeric(as.character(datTavg_corfkrtl$estimate[1]))
  p.mat_fkrtl[i,3] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[1]))
  p.mat_fkrtl[i,4] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[2]))
  p.mat_fkrtl[i,5] <- as.numeric(as.character(datTavg_corfkrtl$p.value))
}

p.mat.dat <- rbind(p.mat_fktp, p.mat_fkrtl)
p.mat.dat <- as.data.frame(p.mat.dat)
names(p.mat.dat) <- c("lag", "estimate", "lower", "upper", "p.value")

p.mat.dat$lag <- rep(seq(0:14),2)
p.mat.dat$lag <- p.mat.dat$lag-1
p.mat.dat$dataset <- c(rep("fktp",15),rep("fkrtl",15))

p.mat.dat$p.value.cat <- NA

p.mat.dat  <- p.mat.dat %>% 
  mutate(p.value.cat = ifelse(p.value <= 0.05, "sig", "no sig"))

str(p.mat.dat)
p.mat.dat$estimate <- as.numeric(as.character(p.mat.dat$estimate))
p.mat.dat$lower <- as.numeric(as.character(p.mat.dat$lower))
p.mat.dat$upper <- as.numeric(as.character(p.mat.dat$upper))
p.mat.dat$p.value <- as.numeric(as.character(p.mat.dat$p.value))

# write.csv(p.mat.dat, "p.mat.dat.csv")

p1 <- p.mat.dat %>% filter(dataset == "fktp") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 0.5) + theme(legend.position="none") +
  labs(title = "primary care", subtitle = "lag 0-14 day", x="", y="") +
  scale_x_continuous(breaks=seq(0,14,1))

p2 <- p.mat.dat %>% filter(dataset == "fkrtl") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 0.5) + theme(legend.position="none") +
  labs(title = "referral care", subtitle = "lag 0-14 day", x="", y="") +
  scale_x_continuous(breaks=seq(0,14,1))

##
names(datNholidays)
datTavg_fktp <- select(datNholidays, n_fktp,Tavg_lag00:Tavg_lag14)
datTavg_fkrtl <- select(datNholidays, n_fkrtl,Tavg_lag00:Tavg_lag14)

p.mat_fktp <- matrix(NA, 15, 5)
#i <- 1

for(i in c(1:15)){
  datTavg_corfktp <- cor.test(datTavg_fktp[,(i+1)], datTavg_fktp$n_fktp, method="pearson")
  p.mat_fktp[i,1] <- names(datTavg_fktp)[i+1]
  p.mat_fktp[i,2] <- as.numeric(as.character(datTavg_corfktp$estimate[1]))
  p.mat_fktp[i,3] <- as.numeric(as.character(datTavg_corfktp$conf.int[1]))
  p.mat_fktp[i,4] <- as.numeric(as.character(datTavg_corfktp$conf.int[2]))
  p.mat_fktp[i,5] <- as.numeric(as.character(datTavg_corfktp$p.value))
}

p.mat_fkrtl <- matrix(NA, 15, 5)
#i <- 1

for(i in c(1:15)){
  datTavg_corfkrtl <- cor.test(datTavg_fkrtl[,(i+1)], datTavg_fkrtl$n_fkrtl, method="pearson")
  p.mat_fkrtl[i,1] <- names(datTavg_fkrtl)[i+1]
  p.mat_fkrtl[i,2] <- as.numeric(as.character(datTavg_corfkrtl$estimate[1]))
  p.mat_fkrtl[i,3] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[1]))
  p.mat_fkrtl[i,4] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[2]))
  p.mat_fkrtl[i,5] <- as.numeric(as.character(datTavg_corfkrtl$p.value))
}

p.mat.dat <- rbind(p.mat_fktp, p.mat_fkrtl)
p.mat.dat <- as.data.frame(p.mat.dat)
names(p.mat.dat) <- c("lag", "estimate", "lower", "upper", "p.value")

p.mat.dat$lag <- rep(seq(0:14),2)
p.mat.dat$lag <- p.mat.dat$lag-1
p.mat.dat$dataset <- c(rep("fktp",15),rep("fkrtl",15))

p.mat.dat$p.value.cat <- NA

p.mat.dat  <- p.mat.dat %>% 
  mutate(p.value.cat = ifelse(p.value <= 0.05, "sig", "no sig"))

str(p.mat.dat)
p.mat.dat$estimate <- as.numeric(as.character(p.mat.dat$estimate))
p.mat.dat$lower <- as.numeric(as.character(p.mat.dat$lower))
p.mat.dat$upper <- as.numeric(as.character(p.mat.dat$upper))
p.mat.dat$p.value <- as.numeric(as.character(p.mat.dat$p.value))

p.mat.dat$p.value.cat[2] <- "sig"
p.mat.dat$p.value.cat[27] <- "sig"

# write.csv(p.mat.dat, "p.mat.dat_noHoliday.csv")

p3 <- p.mat.dat %>% filter(dataset == "fktp") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 0.5) + theme(legend.position="none") +
  labs(title = "primary care (exclude Sunday & holidays)", subtitle = "lag 0-14 day", x="", y="") +
  scale_x_continuous(breaks=seq(0,14,1))

p4 <- p.mat.dat %>% filter(dataset == "fkrtl") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 0.5) + theme(legend.position="none") +
  labs(title = "referral care (exclude Sunday & holidays)", subtitle = "lag 0-14 day", x="", y="") +
  scale_x_continuous(breaks=seq(0,14,1))

##
names(datW)
datTavg_fktp <- select(datW, n_fktp,Tavg_lag00:Tavg_lag08)
datTavg_fkrtl <- select(datW, n_fkrtl,Tavg_lag00:Tavg_lag08)

datTavg_fktp <- as.data.frame(datTavg_fktp)
datTavg_fkrtl <- as.data.frame(datTavg_fkrtl)

p.mat_fktp <- matrix(NA, 9, 5)
# i <- 1

for(i in c(1:9)){
  datTavg_corfktp <- cor.test(datTavg_fktp[,(i+1)], datTavg_fktp$n_fktp, method="pearson")
  p.mat_fktp[i,1] <- names(datTavg_fktp)[i+1]
  p.mat_fktp[i,2] <- datTavg_corfktp$estimate[1]
  p.mat_fktp[i,3] <- datTavg_corfktp$conf.int[1]
  p.mat_fktp[i,4] <- datTavg_corfktp$conf.int[2]
  p.mat_fktp[i,5] <- datTavg_corfktp$p.value
}

p.mat_fkrtl <- matrix(NA, 9, 5)
#i <- 1

for(i in c(1:9)){
  datTavg_corfkrtl <- cor.test(datTavg_fkrtl[,(i+1)], datTavg_fkrtl$n_fkrtl, method="pearson")
  p.mat_fkrtl[i,1] <- names(datTavg_fkrtl)[i+1]
  p.mat_fkrtl[i,2] <- as.numeric(as.character(datTavg_corfkrtl$estimate[1]))
  p.mat_fkrtl[i,3] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[1]))
  p.mat_fkrtl[i,4] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[2]))
  p.mat_fkrtl[i,5] <- as.numeric(as.character(datTavg_corfkrtl$p.value))
}

p.mat.dat <- rbind(p.mat_fktp, p.mat_fkrtl)
p.mat.dat <- as.data.frame(p.mat.dat)
names(p.mat.dat) <- c("lag", "estimate", "lower", "upper", "p.value")

p.mat.dat$lag <- rep(seq(0:8),2)
p.mat.dat$lag <- p.mat.dat$lag-1
p.mat.dat$dataset <- c(rep("fktp",9),rep("fkrtl",9))

p.mat.dat$p.value.cat <- NA

p.mat.dat  <- p.mat.dat %>% 
  mutate(p.value.cat = ifelse(p.value <= 0.05, "sig", "no sig"))

str(p.mat.dat)
p.mat.dat$estimate <- as.numeric(as.character(p.mat.dat$estimate))
p.mat.dat$lower <- as.numeric(as.character(p.mat.dat$lower))
p.mat.dat$upper <- as.numeric(as.character(p.mat.dat$upper))
p.mat.dat$p.value <- as.numeric(as.character(p.mat.dat$p.value))

p5 <- p.mat.dat %>% filter(dataset == "fktp") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 1.0) + theme(legend.position="none") +
  labs(title = "primary care", subtitle = "lag 0-8 week", x="", y="") +
  scale_x_continuous(breaks=seq(0,8,1))

p6 <- p.mat.dat %>% filter(dataset == "fkrtl") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 1.0) + theme(legend.position="none") +
  labs(title = "referral care", subtitle = "lag 0-8 week", x="", y="") +
  scale_x_continuous(breaks=seq(0,8,1))

# write.csv(p.mat.dat, "p.mat.datW.csv")

##
names(datWNholidays)
datTavg_fktp <- select(datWNholidays, n_fktp,Tavg_lag00:Tavg_lag08)
datTavg_fkrtl <- select(datWNholidays, n_fkrtl,Tavg_lag00:Tavg_lag08)

datTavg_fktp <- as.data.frame(datTavg_fktp)
datTavg_fkrtl <- as.data.frame(datTavg_fkrtl)

p.mat_fktp <- matrix(NA, 9, 5)
# i <- 1

for(i in c(1:9)){
  datTavg_corfktp <- cor.test(datTavg_fktp[,(i+1)], datTavg_fktp$n_fktp, method="pearson")
  p.mat_fktp[i,1] <- names(datTavg_fktp)[i+1]
  p.mat_fktp[i,2] <- datTavg_corfktp$estimate[1]
  p.mat_fktp[i,3] <- datTavg_corfktp$conf.int[1]
  p.mat_fktp[i,4] <- datTavg_corfktp$conf.int[2]
  p.mat_fktp[i,5] <- datTavg_corfktp$p.value
}

p.mat_fkrtl <- matrix(NA, 9, 5)
#i <- 1

for(i in c(1:9)){
  datTavg_corfkrtl <- cor.test(datTavg_fkrtl[,(i+1)], datTavg_fkrtl$n_fkrtl, method="pearson")
  p.mat_fkrtl[i,1] <- names(datTavg_fkrtl)[i+1]
  p.mat_fkrtl[i,2] <- as.numeric(as.character(datTavg_corfkrtl$estimate[1]))
  p.mat_fkrtl[i,3] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[1]))
  p.mat_fkrtl[i,4] <- as.numeric(as.character(datTavg_corfkrtl$conf.int[2]))
  p.mat_fkrtl[i,5] <- as.numeric(as.character(datTavg_corfkrtl$p.value))
}

p.mat.dat <- rbind(p.mat_fktp, p.mat_fkrtl)
p.mat.dat <- as.data.frame(p.mat.dat)
names(p.mat.dat) <- c("lag", "estimate", "lower", "upper", "p.value")

p.mat.dat$lag <- rep(seq(0:8),2)
p.mat.dat$lag <- p.mat.dat$lag-1
p.mat.dat$dataset <- c(rep("fktp",9),rep("fkrtl",9))

p.mat.dat$p.value.cat <- NA

p.mat.dat  <- p.mat.dat %>% 
  mutate(p.value.cat = ifelse(p.value <= 0.05, "sig", "no sig"))

str(p.mat.dat)
p.mat.dat$estimate <- as.numeric(as.character(p.mat.dat$estimate))
p.mat.dat$lower <- as.numeric(as.character(p.mat.dat$lower))
p.mat.dat$upper <- as.numeric(as.character(p.mat.dat$upper))
p.mat.dat$p.value <- as.numeric(as.character(p.mat.dat$p.value))

# write.csv(p.mat.dat, "p.mat.datW_noHolidays.csv")

p7 <- p.mat.dat %>% filter(dataset == "fktp") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 1.0) + theme(legend.position="none") +
  labs(title = "primary care (exclude Sunday & holidays)", subtitle = "lag 0-8 week", x="", y="") +
  scale_x_continuous(breaks=seq(0,8,1))

p8 <- p.mat.dat %>% filter(dataset == "fkrtl") %>% 
  ggplot(aes(x = lag, y = estimate, col= p.value.cat)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() + ylim(-0.5, 1.0) + theme(legend.position="none") +
  labs(title = "referral care (exclude Sunday & holidays)", subtitle = "lag 0-8 week", x="", y="") +
  scale_x_continuous(breaks=seq(0,8,1))

# figure 4
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 2, nrow = 4)

##

# dt <- dat
dt <- datNholidays

dt$group <- factor(dt$m,
                    levels=c(1:12))
is.factor(dt$group)
# boxplot(n_fktp ~ group, data=dt)

# # Test assumptions
# ## homogeneity of variance
# bartlett.test(n_fktp ~ group, data=dt) # bartlett.test (not significant result = variances can be assumed to be equal)
# oneway.test(n_fktp ~ group, data=dt, var.equal=T) 
# 
# bartlett.test(n_fktp ~ group, data=dt) # bartlett.test (significant result = variances cannot be assumed to be equal)
# oneway.test(n_fktp ~ group, data=dt) 

## run an ANOVA using aov( )
aov.out <- aov(n_fktp ~ group, data=dt)
aov.out <- aov(n_fkrtl ~ group, data=dt)

aov.out <- aov(Tavg ~ group, data=dt)

summary(aov.out)
TukeyHSD(aov.out)

lm.out <- lm(Tavg ~ 1 + group, data=dt)
summary(lm.out)

# Test assumptions
## model checking plots
par(mfrow=c(2,2))
plot(aov.out)

# non-parametric alternative to ANOVA
kruskal.test(n_fktp ~ group, data=dt)

###

datM <- dat %>%
  group_by(m) %>%
  summarise(n_fktp=sum(n_fktp, na.rm=T),
            n_fkrtl=sum(n_fkrtl, na.rm=T),
            Tavg=round(mean(Tavg, na.rm=T),2))

modFKTP <- lm(n_fktp ~ Tavg, data=datM)
summary(modFKTP)

modFKRTL <- lm(n_fkrtl ~ Tavg, data=datM)
summary(modFKRTL)

p1 <- ggplot(datM, aes(x=Tavg, y=n_fktp)) + 
  geom_point() +
  geom_smooth(method=lm, col="black") +
  theme_bw() + ylim(1750, 3750) + 
  labs(title="fktp", 
       subtitle=paste("Adj. R-sq. =",signif(summary(modFKTP)$adj.r.squared, 3),
                      "; Inter. =",signif(modFKTP$coef[[1]],3 ),
                      "; Slope =",signif(modFKTP$coef[[2]], 3),
                      "; p-value =",signif(summary(modFKTP)$coef[2,4], 3)), 
       x="temperature", y="monthly visit")

p2 <- ggplot(datM, aes(x=Tavg, y=n_fkrtl)) + 
  geom_point() +
  geom_smooth(method=lm, col="black") +
  theme_bw() + ylim(1750, 3750) + 
  labs(title="fkrtl", 
       subtitle=paste("Adj. R-sq. =",signif(summary(modFKRTL)$adj.r.squared, 3),
                      "; Inter. =",signif(modFKRTL$coef[[1]],3 ),
                      "; Slope =",signif(modFKRTL$coef[[2]], 3),
                      "; p-value =",signif(summary(modFKRTL)$coef[2,4], 3)), 
       x="temperature", y="")

ggarrange(p1, p2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

##
require(sandwich)
summary(m1 <- glm(n_fktp ~ Tavg, family="poisson", data=datW))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

## calculate and store predicted values
datW$phat_fktp <- predict(m1, type="response")
# write.csv(datW, "datW.csv")

## create the plot
datW_plot <- read.csv("dat/datW_long.csv")
p1 <- ggplot(datW_plot, aes(x=Tavg, y=predict, colour=healthcare)) +
  geom_point(aes(y=n), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(title="weekly  observations", x="temperature", y="expected number of visits") + theme_bw() + theme(legend.position = "none")  

#####

summary(m1 <- glm(n_fkrtl ~ Tavg, family="poisson", data=datM))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

## calculate and store predicted values
datM$phat_fkrtl <- predict(m1, type="response")
# write.csv(datM, "datM.csv")

## create the plot
datM_plot <- read.csv("dat/datM_long.csv")

# figure 5
p2 <- ggplot(datM_plot, aes(x=Tavg, y=predict, colour=healthcare)) +
  geom_point(aes(y=n), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(title="monthly observations", x="temperature", y="") + theme_bw()

ggarrange(p1, p2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
