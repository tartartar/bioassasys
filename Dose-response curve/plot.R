# Loading necessary packages
library(drc)
library(ggplot2)
library(scales)

# Loading the data
haemolysis <- read.csv("haemolysis_FEB_normalised_0_to_1.csv", header = TRUE)

# Multiplying by 100 to obtain the percentage values
haemolysis$Haemolysis <- (haemolysis$Haemolysis * 100)

# Splitting by species
haem.dt <- split(haemolysis, f=haemolysis$Species)
horse <- haem.dt$horse
sheep <- haem.dt$sheep

# Fitting the two-parameter log-logistic function to each blood type
horse.LL.2 <- drm(Haemolysis ~ Concentration, data = haem.dt$horse, fct = LL.3(fixed=c(NA,100,NA)))
sheep.LL.2 <- drm(Haemolysis ~ Concentration, data = sheep, fct = LL.3(fixed=c(NA,100,NA)))

# Creating new dataframes grouped by blood type, with mean and standard error values for haemolysis
horse <- data.table(horse)
horse <- setnames(horse[, sapply(.SD, function(x) list(mean=round(mean(x), 2), st.err=round((sd(x)/sqrt(length(x))), 2))), by=Concentration], c("Concentration", sapply(names(horse)[-1], paste0, c(".mean", ".st.err"))))
horse$min <- (horse$Haemolysis.mean - horse$Haemolysis.st.err)
horse$max <- (horse$Haemolysis.mean + horse$Haemolysis.st.err)

sheep <- data.table(sheep)
sheep <- setnames(sheep[, sapply(.SD, function(x) list(mean=round(mean(x), 2), st.err=round((sd(x)/sqrt(length(x))), 2))), by=Concentration], c("Concentration", sapply(names(sheep)[-1], paste0, c(".mean", ".st.err"))))
sheep$min <- (sheep$Haemolysis.mean - sheep$Haemolysis.st.err)
sheep$max <- (sheep$Haemolysis.mean + sheep$Haemolysis.st.err)

# New dose levels as support for the line to be plotted
horse.base <- expand.grid(Concentration=exp(seq(log(0.2), log(100), length=100)))
sheep.base <- expand.grid(Concentration=exp(seq(log(0.2), log(100), length=100)))

# Extracting the curve information from the log-logistic model
extracted.horse <- predict(horse.LL.2, newdata=horse.base, interval="confidence")
extracted.sheep <- predict(sheep.LL.2, newdata=sheep.base, interval="confidence")

horse.base$p <- extracted.horse[,1]
horse.base$pmin <- extracted.horse[,2]
horse.base$pmax <- extracted.horse[,3]

sheep.base$p <- extracted.sheep[,1]
sheep.base$pmin <- extracted.sheep[,2]
sheep.base$pmax <- extracted.sheep[,3]

# Plotting the two curves with error bars and 95% confidence intervals
ggplot(horse.base, aes(x = Concentration)) +
  # Horse data
  geom_line(data=horse.base, aes(x=Concentration, y=p, colour="Horse")) +
  geom_ribbon(data=horse.base, aes(x=Concentration, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_point(data=horse, aes(x=Concentration, y=Haemolysis.mean), colour="black", alpha=1) +
  geom_errorbar(data=horse, aes(x=Concentration, y=Haemolysis.mean, ymin=min, ymax=max), colour="black", width=.07) +
  # Sheep data
  geom_line(data=sheep.base, aes(x=Concentration, y=p, colour="Sheep"), linetype=2) +
  geom_ribbon(data=sheep.base, aes(x=Concentration, y=p, ymin=pmin, ymax=pmax), fill="red", alpha=0.2) +
  geom_point(data=sheep, aes(x=Concentration, y=Haemolysis.mean), colour="red", alpha=1) +
  geom_errorbar(data=sheep, aes(x=Concentration, y=Haemolysis.mean, ymin=min, ymax=max), colour="red", width=.07) +
  # Graph parameters
  theme_bw() + 
  xlab("rNetF final concentration (µg/mL)") + 
  ylab("Haemolysis %") + 
  ggtitle("rNetF haemolytic activity in horse and sheep blood") +
  scale_color_manual("", breaks=c("Horse", "Sheep"), values=c("black","red")) +
  theme(legend.text=element_text(size=13), plot.title=element_text(size=16), axis.text=element_text(size=13), axis.title=element_text(size=16)) +
  scale_y_continuous(breaks=c(0:100*10)) +
  scale_x_continuous(trans=log_trans(), breaks=c(1,2,4,6,8,10,20,40,60,80,100))
