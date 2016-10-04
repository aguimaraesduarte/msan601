library(xlsx)
library(car)
library(lmtest)

df <- read.xlsx("UV6486-XLS-ENG.xlsx", sheetIndex = 2, endRow = 121, header = T, colIndex = c(2,3,4))
df <- df[c(2,1,3)]
pairs(df)
scatterplot(df$Households.with.Account~df$Total.Households.in.Area)
scatterplot(df$Households.with.Account~df$Inside.Outside.Footprint)
lm1 <- lm(data=df, Households.with.Account ~ Total.Households.in.Area*Inside.Outside.Footprint)
summary(lm1)
residualPlot(lm1)
plot(lm1)
resettest(lm1, power=2, type="fitted")
