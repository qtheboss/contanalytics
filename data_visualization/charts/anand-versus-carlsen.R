# link: http://www.r-bloggers.com/anand-versus-carlsen-chennai-2013-what-can-we-expect-in-november/
# Download data: https://github.com/Ram-N/Anand-Carlsen/tree/master/Data
# Source: https://github.com/Ram-N/Anand-Carlsen

# Anand versus Carlsen – Chennai 2013 – What can we expect in November?

library(ggplot2)
library(plyr)  
library(reshape2)

df <- read.csv("Data/Anand-Carlsen.csv", stringsAsFactors=F, strip.white=T)
df <- arrange(df, Game) #use plyr's arrange to Sort by Game Number

elo <- read.csv("Data/Anand_Carlsen_ELO.csv", stringsAsFactors=F, strip.white=T)
elo$Period <- as.Date(paste0(elo$Period, "-01"), format="%Y-%b-%d") #convert from string to Date format
#names(elo)
# For ease drop some column We don't need right way
df <- df[, c(1, 2, 3, 5, 7, 8)]

# Who had while? Create new column "Anand.while"
df$Anand.white = ifelse(df$WhiteVsBlack == "Anand vs Carlsen", 1, 0)

df$Anand.won = 0 # We will be overwritting this column in instances where Andand won

for(i in 1: nrow(df)){
  if(df$Result[i] == "Draw") {
    df$Anand.won[i] = "Draw" }
  else if((df$Anand.white[i] == 1 & df$Result[i] == 1) || (df$Anand.white[i] == 0 & df$Result[i] == 0)) {
    df$Anand.won[i] = 1 
  }
  # discover His code forget this
  else {
    df$Anand.won[i] = 0
  }
}

# Lifetime scores

table(df$Anand.won)
nrow(df)

AW = subset(df, Anand.won == 1)
table(AW$Anand.won)
AB = subset(df, Anand.won == 0)
table(AB$Anand.won)

## ELO ratings
m.elo <- melt(elo[, c(1, 2, 4)], id = c("Period"), na.rm = T, value.name = "Rating" )
p <- NULL
p <- ggplot(data = m.elo, aes(Period, y = Rating)) + geom_line(aes(group = variable, color = variable), size = 2)
p = p + geom_text(data = m.elo[68, ], label = "Anand", vjust = 2, size = 8)
p = p + geom_text(data = m.elo[130, ], label = "Carlsen", hjust = 1, size = 8) + theme(legend.position = "none")
p = p + labs(title = "Anand vs carlsen - ELO Ratings Over Time")
p

## Given a data-frame slice, this function compute WLD for that time slice
# This function is useful to call from ddply

WinLoseDraw <- function(df) {
  va.wins <- sum(na.omit(df$Anand.won == "1")) # How many times did Anand wins
  va.loss <- sum(na.omit(df$Anand.won == "0")) # How many times did Anand loss
  va.draw <- sum(na.omit(df$Anand.won == "Draw")) # How many times did Anand draw
  c(va.wins, va.loss, va.draw)
} 

### Win Loss Draw Distributions by Year
WLD <- ddply(df, .(Year), WinLoseDraw)
names(WLD) =  c("Year", "Win", "Loss", "Draw")
m.wld = melt(WLD, id = "Year")


vlines <- seq(2005.5, 2013.5, by=1)
pw <- ggplot(data=m.wld, aes(x=Year, y=value, fill=variable)) + geom_bar(stat="identity", position=position_dodge())
pw <- pw + scale_fill_manual(values=c("darkgreen", "red", "lightblue"))
pw <- pw + geom_vline(xintercept = vlines, color="darkred", size=0.3)
pw <- pw + scale_x_continuous(breaks=seq(2005, 2013))
pw <- pw + labs(title="Anand's Win-Loss-Draw by Year")+ theme(panel.background = element_blank())

pw

WLD

# 4. Consequence of CHOICE OF OPENINGS

#Creating a new column called win just to code the values as numeric
# This column is the same as Anand.won
df$win <- ifelse(df$Anand.won=="Draw", 0.5,
                 ifelse(df$Anand.won=="1", 1, 0)
)

wb_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Anand.white") {
    value[value==1] <- "Anand.White"
    value[value==0] <- "Anand.Black"
  }
  return(value)
}


#Sort the data frame by Openings that helped win (or lose)
df <- transform(df, ECO = reorder(ECO, win))

#Now we are ready to plot...
po <- NULL
po <- ggplot(data=df, aes(x=(ECO))) + geom_bar(aes(fill=Anand.won)) + coord_flip()
po <- po + labs(title="Consequence of Choice of Openings") + ylab("Number of Games") + xlab("Opening Chosen")
po

#po <- po + facet_grid(. ~ Anand.white, labeller = wb_labeller)
po <- po + geom_text(aes(y=3,label=df$Opening), size=4)
po <- po + annotate("text", x=df$ECO, y=2, label=df$Opening, size=3)
po

#####

