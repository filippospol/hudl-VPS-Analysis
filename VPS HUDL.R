library(ballr)
library(nbastatR)
library(tidyverse)
library(magrittr)
library(ggrepel)
library(reshape2)
library(ggridges)
library(formattable)


#box_hustle from nbastatR
df <- readxl::read_xlsx(file.choose())
str(df)


charges <- data.frame(box_hustle$dataBoxScore) %>% 
  select(namePlayer,chargesDrawn)
test <- aggregate(chargesDrawn~namePlayer, charges, sum)
summary(test)

df <- df[order(df$namePlayer),]
test <- test[order(test$namePlayer),]

charges <- test$chargesDrawn
df <- cbind(df,charges)

#Calculate VPS:

attach(df)
df$VPS <- round( (PTS + TRB + 2* (AST+STL+BLK+charges)) / 
          ((FTA-FT) + 2* ((FGA-FG)+PF+TOV)) ,2)
detach(df)
df$VPS[which(df$VPS == Inf)] <- 0


# add minute and games played  restriction:
# 1) minimum 33 G, 20 MP/G (one third of a season) or
# 2) average MP and average G  


df1 <- df %>% 
  filter(G >= 20, MP >= mean(df$MP))
formattable(df1[order(df1$VPS, decreasing=T),])


#correlation matrix for adv stats
cormat <- round(cor(df1[,c(18,31,32,33,34,35,44,48,49,51)]),2)
cormat[lower.tri(cormat)] <- NA
melted_cormat <- melt(cormat, na.rm = TRUE)


vpscor <- data.frame(cbind(c("eFG%","PER","TS%","FTRate","OREB%","DREB%","WS","BPM","VORP"),
                           c(0.54,0.64,0.57,0.42,0.58,0.57,0.63,0.64,0.55)))
colnames(vpscor) <- c("Advanced Stat","Linear Correlation with VPS")

formattable(vpscor,
            align = c("l","r"),
            list(
                 `Advanced Stat` =formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                 `Linear Correlation with VPS` = color_tile("pink", "red")            
                 ))

#scatterplots x=VPS y=PER,WS,BPM


 




dfper <- df1[order(df1$PER, decreasing=T),] ; dfper <- dfper[,c(2,6,8,31,44,48,51)] 
names(dfper)[1] <- "Player"
pper <- ggplot(df1, aes(y=VPS, x=PER)) +
  geom_point(shape = 21, fill="gray23", color="black", size=3) +
  labs(title="Hudl's Value Point System", 
       subtitle="At least 19 MPG and 20 GP", x ="PER", y = "VPS") 
pper <- pper + geom_point(data = dfper[1:10,], aes(y=VPS, x=PER),
                          shape = 21, fill="#71CA97", color="black", size=4)
#add theme and banner
formattable(dfper[1:10,c(1,4,7),],
            align = c("l","r","r"),
            list(
              `Player` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `PER` = formatter("span"),
              `VPS` = color_tile("#DeF7E9", "#71CA97")
            ))


dfws <- df1[order(df1$WS, decreasing=T),] ; dfws <- dfws[,c(2,6,8,31,44,48,51)]
names(dfws)[1] <- "Player"
ppws <- ggplot(dfws, aes(y=VPS, x=WS)) +
  geom_point(shape = 21, fill="gray23", color="black", size=3) +
  labs(title="Hudl's Value Point System", 
       subtitle="At least 19 MPG and 20 GP", x ="WS", y = "VPS") 
ppws <- ppws + geom_point(data = dfws[1:10,], aes(y=VPS, x=WS),
                          shape = 21, fill="turquoise4", color="black", size=4)
#add theme and banner
formattable(dfws[1:10,c(1,5,7),],
            align = c("l","r","r"),
            list(
              `Player` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `WS` = formatter("span"),
              `VPS` = color_tile("turquoise1","turquoise4")
            ))

dfbpm <- df1[order(df1$BPM, decreasing=T),] ; dfbpm <- dfbpm[,c(2,6,8,31,44,48,51)]
names(dfbpm)[1] <- "Player"
ppbpm <- ggplot(dfbpm, aes(y=VPS, x=BPM)) +
  geom_point(shape = 21, fill="gray23", color="black", size=3) +
  labs(title="Hudl's Value Point System", 
       subtitle="At least 19 MPG and 20 GP", x ="BPM", y = "VPS") 
ppbpm <- ppbpm + geom_point(data = dfbpm[1:10,], aes(y=VPS, x=BPM),
                          shape = 21, fill="plum", color="black", size=4)
#add theme and banner
formattable(dfbpm[1:10,c(1,6,7),],
            align = c("l","r","r"),
            list(
              `Player` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `BPM` = formatter("span"),
              `VPS` = color_tile("plum1","plum")
            ))










#############################
#Ranking the NBA w/ VPS
#############################
names(df)[2] <- "Player"
# FF: 
c(2,18,33,34,40,51)
# Players at the 25th Percentile:
df[which(df$VPS <= 1.08),c(2,51)]
c(29,59,193,294,306)
per25 <- df[c(29,59,193,294,306),c(2,18,33,34,40,51)]
per25[,2] <- 100*per25[,2] ; per25[,3] <- 100*per25[,3]
formattable(per25,
            align = c("l","r","r","r","r","r"),
            list(
              `Player` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `VPS` = color_tile("white","deepskyblue3")
            ))







# Notable players with average VPS:
df[which(df$VPS == 1.28),c(2,51)]
c(88,98,170,436)
meanvps <- df[c(88,98,170,436),c(2,18,33,34,40,51)]
meanvps[,2] <- 100*meanvps[,2] ; meanvps[,3] <- 100*meanvps[,3]
formattable(meanvps,
            align = c("l","r","r","r","r","r"),
            list(
              `Player` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `VPS` = color_tile("white","deepskyblue3")
            ))


#Players at the 75th Percentile:
df[which(df$VPS >1.28 & df$VPS <= 1.46),c(2,51)]
c(41,44,74,83,212,229,285,319,394,400,426,444,451,475)
per75 <- df[c(41,44,74,83,212,229,285,319,394,400,426,444,451,475),c(2,18,33,34,40,51)]
per75[,2] <- 100*per75[,2] ; per75[,3] <- 100*per75[,3]
formattable(per75,
            align = c("l","r","r","r","r","r"),
            list(
              `Player` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `VPS` = color_tile("white","deepskyblue3")
            ))

#Players over the 75th Percentile:
df[c(23,31,33,71,87,130,174,199,238,242,243,278,279,297,309,316,322,383,424,513),c(2,18,33,34,40,51)]
c(23,31,33,71,87,130,174,199,238,242,243,278,279,297,309,316,322,383,424,513)



# Top 10 players in VPS:
x <- df[order(df$VPS,decreasing=T),]
x <- x[1:10,c(2,18,33,34,40,51)]
x[,2] <- 100*x[,2] ; x[,3] <- 100*x[,3]
formattable(x,
            align = c("l","r","r","r","r","r"),
            list(
              `Player` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `VPS` = color_tile("white","deepskyblue3")
            ))

table(df$pos) 
df$pos[which(df$pos=="C-PF")] <- "C"
df$pos[which(df$pos=="PF-SF")] <- "PF"
df$pos[which(df$pos=="SF-PF")] <- "PF"
df$pos[which(df$pos=="SF-SG")] <- "SF"
df$pos <- factor(df$pos, levels = c("C","PF","SF","SG","PG"))


ggplot(df, aes(x = VPS, y = pos, fill = pos)) +
  geom_density_ridges() +
  scale_fill_brewer(palette="Set1") + 
  theme(legend.position = "none")


