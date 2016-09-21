# Question 3
library(ggplot2)

for(i in seq(4)){
  ggplot(anscombe, aes(anscombe[,i], anscombe[,i+4])) + 
    geom_point(size=3) +
    geom_smooth(method="lm", se=FALSE) +
    ggtitle(paste("Y", i, " ~ X", i, sep="")) +
    xlab(paste("\n X", i, sep="")) + 
    ylab(paste("Y", i, "\n", sep="")) +
    xlim(min(anscombe[,1:4]), max(anscombe[,1:4])) +
    ylim(min(anscombe[,5:8]), max(anscombe[,5:8])) +
    theme(axis.text = element_text(size=24),
          strip.text.y = element_text(size = 24),
          title = element_text(size = 24))
  ggsave(paste0("q3-", i, ".png"), height = 8.5, width = 11)
}

# Question 4
bodyfat <- read.csv("extraColumnsOfRandomData.csv", header=T)

q4 <- function(df){
  r2 <- data.frame("R2"=rep(0, ncol(df)-1), "Adj.R2"=rep(0, ncol(df)-1))
  for(x in 2:ncol(df)){
    df_temp <- df[,1:x]
    lm_temp <- lm(data=df_temp, BODYFAT~.)
    r2$R2[x-1] <- summary(lm_temp)$r.squared
    r2$Adj.R2[x-1] <- summary(lm_temp)$adj.r.squared
  }
  r2 <- reshape2::melt(r2)
  r2$Var <- rep(1:27, 2)
  ggplot(r2, aes(x=Var, y=value, colour=variable)) +
    geom_point(size = 3) +
    geom_line(size = 0.5) +
    ggtitle(expression(paste("Variation of ", R^{2}, " and Adj.", R^{2}))) +
    xlab("\n Number of explanatory variables") + 
    ylab("Value of coefficient of determination \n") +
    theme(axis.text = element_text(size=24),
          strip.text.y = element_text(size = 24),
          title = element_text(size = 24),
          legend.text = element_text(size = 18),
          legend.title=element_blank()) +
    scale_x_continuous(breaks=seq(0, ncol(df), 2))
  ggsave("q4.png", height = 8.5, width = 11)
  
  return(r2)
}

r2 <- q4(bodyfat)

# Question 5
electric <- read.table("electricBillData.txt", header=F, sep="", na.strings = "*")
names(electric) <- c(
  "NUM",
  "YEAR",
  "MONTH",
  "BILL",
  "TEMP",
  "HDD",
  "CDD",
  "SIZE",
  "METER",
  "PUMP1",
  "PUMP2",
  "RIDER TOTAL",
  "CONSUMPTION"
)

# a)
lm1 <- lm(data=electric, BILL~TEMP+HDD+CDD+SIZE+METER+PUMP1+PUMP2)
summary(lm1)

# e)
cor(electric[,5:7])

# f)
lm2 <- lm(data=electric, BILL~HDD+CDD+SIZE+METER+PUMP1+PUMP2)
summary(lm2)

# Question 6
senic <- read.csv("SENIC_data.csv", header=F)
names(senic) <- c(
  "IdNumber", "LengthOfStay", "Age", "InfectionRisk", "RoutineCulturingRatio", "RoutineChestXRayRatio",
  "NumberOfBeds", "MedSchoolAffiliation", "Region", "AverageDailyCensus", "NumberOfNurses", "AvailableFacilitiesAndServices"
)

# 1)
lm_senic <- lm(data=senic, NumberOfNurses~AvailableFacilitiesAndServices + I(AvailableFacilitiesAndServices^2))
summary(lm_senic)
postscript("q6_1", width = 11, height = 8)
car::residualPlot(lm_senic)
dev.off()

# 2)
lm_senic1 <- lm(data=senic, NumberOfNurses~AvailableFacilitiesAndServices)
summary(lm_senic1)

# Question 8
senic <- read.csv("SENIC_data.csv", header=F)
names(senic) <- c(
  "IdNumber", "LengthOfStay", "Age", "InfectionRisk", "RoutineCulturingRatio", "RoutineChestXRayRatio",
  "NumberOfBeds", "MedSchoolAffiliation", "Region", "AverageDailyCensus", "NumberOfNurses", "AvailableFacilitiesAndServices"
)

# 1)
senic$NE <- ifelse(senic$Region == 1, 1, 0)
senic$NC <- ifelse(senic$Region == 2, 1, 0)
senic$S <- ifelse(senic$Region == 3, 1, 0)
lm_senic <- lm(data=senic, LengthOfStay ~ Age +
                 RoutineCulturingRatio +
                 AverageDailyCensus +
                 AvailableFacilitiesAndServices +
                 NE + NC + S)
summary(lm_senic)











