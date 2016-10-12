library(ggplot2)
library(lmtest)

df <- read.csv("goodBellyData.csv", header=T, stringsAsFactors = F)
str(df)
sum(!is.na(df[,13:41])) #Only NAs for cols 13:41
df <- df[,1:12]
df$Date <- as.Date(df$Date, format = "%m/%d/%y")
df$Region[is.na(df$Region)] <- "NA"
df$Region <- as.factor(df$Region)
df$Store <- as.factor(df$Store)
df$Store <- sub("\\(", "\n\\(", df$Store)
df$Endcap <- as.factor(df$Endcap)

lm_complete <- lm(data = df, Units.Sold~.-Date-Region-Store)
summary(lm_complete)

lm_2 <- lm(data = df, Units.Sold~.-Date-Region-Store-Natural-Fitness)
summary(lm_2)

plot(lm_2)
resettest(lm_2, 2, "fitted")

lm_3 <- lm(data = df, Units.Sold~(.-Date-Region-Store-Natural-Fitness)^2)
summary(lm_3)

lm_4 <- lm(data = df, Units.Sold~.-Date-Region-Store-Natural-Fitness+Sales.Rep:Endcap)
summary(lm_4)

subsets <- split(df, df$Region, drop=TRUE)
summaries <- list()
for(i in 1:length(subsets)){
  print(i)
  df_tmp <- subsets[[i]]
  region <- levels(df_tmp$Region)[i]
  include_Sales.Rep <- if(length(unique(df_tmp$Sales.Rep))==1){F}else{T}
  include_Endcap <- if(length(unique(df_tmp$Endcap))==1){F}else{T}
  include_Demo <- if(length(unique(df_tmp$Demo))==1){F}else{T}
  include_Demo1.3 <- if(length(unique(df_tmp$Demo1.3))==1){F}else{T}
  include_Demo4.5 <- if(length(unique(df_tmp$Demo4.5))==1){F}else{T}
  include_Natural <- F # don't need Natural, insignificant for all
  include_Fitness <- F # don't need Fitness, insignificant for all
  df_tmp <- df_tmp[, 
                   c(T,
                     F, # don't need region again
                     T,
                     T,
                     T,
                     include_Sales.Rep,
                     include_Endcap,
                     include_Demo,
                     include_Demo1.3,
                     include_Demo4.5,
                     include_Natural,
                     include_Fitness)]
  lm_tmp <- lm(data=df_tmp, Units.Sold~.-Date-Store)
  summary(lm_tmp)
  summaries[[i]] <- summary(lm_tmp)
  png(paste0(region, ".png", sep=""), width = 800, height = 500)
  if(include_Endcap){
    print(ggplot(df_tmp, aes(Date, Units.Sold)) +
            geom_line(aes(group = Store, color= Store), show.legend = TRUE) +
            geom_point(aes(Date, Units.Sold, shape = Endcap), size=3) +
            xlab("\n Date") +
            ylab("Units Sold\n") +
            ggtitle(paste("Region: ", region, sep = "")) +
            theme(axis.text = element_text(size=13),
                  title = element_text(size=15),
                  legend.text = element_text(size=13)))
  } else{
    print(ggplot(df_tmp, aes(Date, Units.Sold)) +
            geom_line(aes(group = Store, color= Store), show.legend = TRUE) +
            geom_point(aes(Date, Units.Sold), size=3) +
            xlab("\n Date") +
            ylab("Units Sold\n") +
            ggtitle(paste("Region: ", region, sep = "")) +
            theme(axis.text = element_text(size=13),
                  title = element_text(size=15),
                  legend.text = element_text(size=13)))
  }
  dev.off()
  #ggsave(paste0("salesPerStore_", region, ".pdf", sep=""), width=11, height=8.5)
  #readline()
}
summaries

t.test(df$Units.Sold[df$Endcap==1],df$Units.Sold[df$Endcap==0])
t.test(df$Units.Sold[df$Demo==1], df$Units.Sold[df$Demo==0])
