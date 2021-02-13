getwd()
#You need to set your own working directory or simply save the log file in your default working directory
#setwd("C:/Users/XXX/Documents/R/Directory")
x <- read.csv("poker_now_log_16.csv") #Update this to whatever you name your poker.now log file.
D <- x$entry # sets the main column of data as a vector "D"
#print(D)
#Reverse vector D so that the action occurs chronologically from top to bottom
D <- rev(D)
#print(D)
#assign letters to suits - âT¥ == h, âT¦ == d, âT  == s, âT£ == c # This is not strictly necessary for the stats produced but if you extract the log file at the end it is useful for manually reading the file.
D <- gsub("âT¥", "h", D)
D <- gsub("âT¦", "d", D)
D <- gsub("âT£", "c", D)
D <- gsub("âT", "s", D)
#Set P as every line in the vector D containing the below string results with vector locations
P <- grep("joined the game with a stack", D)
#print(P)
#Set PL as every line in the vector D containing the below string results with new vector with each line a row in the vector
PL <- D[c(P)]
#removes duplicates
PL <- unique(PL)
#print(PL)
#install.packages("qdapRegex")
library(qdapRegex)
#remove the excess characters
Players <- rm_between(PL, '\"', ' @ ', extract=TRUE)#[[1]]
print(Players)


#******************************************************************************
#1. VPIP
#This stat stands for voluntarily put money in pot. 
#It tells you what percentage of hands somebody plays. 

#Create a vector with the opening stack for each played hand.
GP <- grep("Player stacks", D)
#Create a subset with only lines with Player stacks contained.
GPD <- D[c(GP)]
#Separated by |
GPDS <- unlist(strsplit(GPD, "\\|"))
library(qdapRegex)
#Creates a list of every time a player is named as starting a hand
GPDST <- ex_between(GPDS, '\"', "@")
#install.packages("plyr")
library(plyr)
#Change the list to a vector
v <- unlist(GPDST, use.names=FALSE)
#print(v)
#Count each time a name is listed as starting a hand
#count(v)
#table(v)
cv <- plyr::count(v)
#print(cv)

#install.packages("sjmisc")
library(sjmisc)

#create a data frame that will build throughout the loop
df <- data.frame("Header")
names(df)<-c("Header")
#Set a variable for a later if to search for text string in data
fold <- "folds"
stack <- "Player Stack"
flop <- "flop"
end1 <- "ending"

#Set variable i = 1
i <- 1

#Run loop while the main column in D is not equal to nothing
while (D[i] != "") {
  if (str_contains(D[i],stack, ignore.case = TRUE)) { 
    while ((!str_contains(D[i],end1, ignore.case = TRUE)) & (!str_contains(D[i],flop, ignore.case = TRUE))) {
     #Check if the current index of the loop of D contains the text "folds" if it does proceed else add 1 to i and continue
      if (str_contains(D[i],fold, ignore.case = TRUE)) { 
      #if true, make a new dataframe "de" equal to the current position in the loop on D
      de<-data.frame(D[i])
      #Give its coloumn the same name as df's coloumn
      names(de)<-c("Header")
      #Append de onto the end of df and continue
      df <- rbind(df, de)
      }
      i = i + 1
    }
  }
    # slow down output
    #Sys.sleep(0.1)                    
 i = i + 1 
}

#make df a vector
vdf <- unlist(df, use.names=FALSE)
#remove everything except for names so it can be compared - this makes it a list again
PLFLD <- ex_between(vdf, '"', "@")
#make it a vector again
vplfld <- unlist(PLFLD, use.names=FALSE)
#make a table of the count
cvplfld <- plyr::count(vplfld)

#install.packages("dplyr")
library(dplyr)
vpip <- left_join(cv, cvplfld, by = c("x" = "x"))
vpip[,4] <- vpip[,3] / vpip[,2]
vpip[,5] <- 1 - vpip[,4]

#colnames(vpip)
names(vpip)[names(vpip) == "x"] <- "Player Name"
names(vpip)[names(vpip) == "freq.x"] <- "Hands Played"
names(vpip)[names(vpip) == "freq.y"] <- "Hands Folded Pre-Flop"
names(vpip)[names(vpip) == "V4"] <- "% Folded"
names(vpip)[names(vpip) == "V5"] <- "VPIP"
vpip[, 4][is.na(vpip[, 4])] <- 0
vpip[, 5][is.na(vpip[, 5])] <- 1
vpip[, 3][is.na(vpip[, 3])] <- 0
vpip <- vpip %>% mutate_at(vars("% Folded", "VPIP"), dplyr::funs(round(., 3)))

#******************************************************************************

#2. PFR 
#This is another absolutely crucial poker stat which stands for preflop raise percentage. 
#This is the percentage of hands that somebody raises before the flop.

#Create the df dataframe again
df <- data.frame("Header")
names(df)<-c("Header")
#Set a variable for later if to search for text string in data
fold <- "folds"
stack <- "Player Stack"
flop <- "flop"
end1 <- "ending"
raise <- "raise"

#Set variable i = 1
i <- 1

while (D[i] != "") {
  if (str_contains(D[i],stack, ignore.case = TRUE)) { 
    while ((!str_contains(D[i],end1, ignore.case = TRUE)) & (!str_contains(D[i],flop, ignore.case = TRUE))) {
      #Check if the current index of the loop of D contains the text "raise" if it does proceed else add 1 to i and continue
      if (str_contains(D[i],raise, ignore.case = TRUE)) { 
        #if true, make a new dataframe "de" equal to the current position in the loop on D
        de<-data.frame(D[i])
        #Give its coloumn the same name as df's coloumn
        names(de)<-c("Header")
        #Append de onto the end of df and continue
        df <- rbind(df, de)
      }
      i = i + 1
    }
  }
  # slow down output
  #Sys.sleep(0.1)                    
  i = i + 1 
}

#make df a vector
vdf <- unlist(df, use.names=FALSE)
#remove everything except for names so it can be compared - this makes it a list again
PLRS <- ex_between(vdf, '"', "@")
#make it a vector again
vplrs <- unlist(PLRS, use.names=FALSE)
#count(vplrs)
cvplrs <- plyr::count(vplrs)

library(dplyr)
#join the number of games played to the raises table
pfr <- left_join(cv, cvplrs, by = c("x" = "x"))
pfr[,4] <- pfr[,3] / pfr[,2]

#colnames(pfr)
names(pfr)[names(pfr) == "x"] <- "Player Name"
names(pfr)[names(pfr) == "freq.x"] <- "Hands Played"
names(pfr)[names(pfr) == "freq.y"] <- "Hands Raised Pre-Flop"
names(pfr)[names(pfr) == "V4"] <- "PFR"

pfr[, 4][is.na(pfr[, 4])] <- 0
pfr[, 5][is.na(pfr[, 5])] <- 1
pfr[, 3][is.na(pfr[, 3])] <- 0
pfr <- pfr %>% mutate_at(vars("PFR"), dplyr::funs(round(., 3)))


#******************************************************************************
#3. AF  
#Aggression Factor is another extremely useful poker HUD stat based on the mathematical expression in PokerTracker
#: ( Total Times Bet + Total Times Raised ) / Total Times Called.

dfr <- data.frame("Header")
names(dfr)<-c("Header")
#Set a variable for later if to search for text string in data
fold <- "folds"
stack <- "Player Stack"
flop <- "flop"
end1 <- "ending"
raise <- "raise"
bet <- "bets"
call <- "calls"

#Set variable i = 1
i <- 1

#count the number of times raised
while (D[i] != "") {
  if (str_contains(D[i],stack, ignore.case = TRUE)) { 
    while (!str_contains(D[i],end1, ignore.case = TRUE)) {
      #Check if the current index of the loop of D contains the text "raise" if it does proceed else add 1 to i and continue
      if (str_contains(D[i],raise, ignore.case = TRUE)) { 
        #if true, make a new dataframe "de" equal to the current position in the loop on D
        de<-data.frame(D[i])
        #Give its coloumn the same name as df's coloumn
        names(de)<-c("Header")
        #Append de onto the end of df and continue
        dfr <- rbind(dfr, de)
      }
      i = i + 1
    }
  }
                
  i = i + 1 
}

#count the times bet and add it to the ongoing dfr
i <- 1

while (D[i] != "") {
  if (str_contains(D[i],stack, ignore.case = TRUE)) { 
    while (!str_contains(D[i],end1, ignore.case = TRUE)) {
      #Check if the current index of the loop of D contains the text "bets" if it does proceed else add 1 to i and continue
      if (str_contains(D[i],bet, ignore.case = TRUE)) { 
        #if true, make a new dataframe "de" equal to the current position in the loop on D
        de<-data.frame(D[i])
        #Give its coloumn the same name as df's coloumn
        names(de)<-c("Header")
        #Append de onto the end of df and continue
        dfr <- rbind(dfr, de)
      }
      i = i + 1
    }
  }
                
  i = i + 1 
}

#make df a vector
vdfr <- unlist(dfr, use.names=FALSE)
#remove everything except for names so it can be compared - this makes it a list again
PLRS <- ex_between(vdfr, '"', "@")
#make it a vector again
vplrs <- unlist(PLRS, use.names=FALSE)
#count(vplrs)
cvplrs <- plyr::count(vplrs)

#count the number of times called
i <- 1
dfc <- data.frame("Header")
names(dfc)<-c("Header")

while (D[i] != "") {
  if (str_contains(D[i],stack, ignore.case = TRUE)) { 
    while (!str_contains(D[i],end1, ignore.case = TRUE)) {
      #Check if the current index of the loop of D contains the text "calls" if it does proceed else add 1 to i and continue
      if (str_contains(D[i],call, ignore.case = TRUE)) { 
        #if true, make a new dataframe "de" equal to the current position in the loop on D
        de<-data.frame(D[i])
        #Give its coloumn the same name as df's coloumn
        names(de)<-c("Header")
        #Append de onto the end of df and continue
        dfc <- rbind(dfc, de)
      }
      i = i + 1
    }
  }
                
  i = i + 1 
}

#make df a vector
vdfc <- unlist(dfc, use.names=FALSE)
#remove everything except for names so it can be compared - this makes it a list again
PLC <- ex_between(vdfc, '"', "@")
#make it a vector again
vplc <- unlist(PLC, use.names=FALSE)

#count(vplrs)
cvplc <- plyr::count(vplc)

cvplc <- cvplc[complete.cases(cvplc), ]
cvplrs <- cvplrs[complete.cases(cvplrs), ]

library(dplyr)
AF <- left_join(cvplc, cvplrs, by = c("x" = "x"))
AF[,4] <- AF[,3] / AF[,2]

#colnames(pfr)
names(AF)[names(AF) == "x"] <- "Player Name"
names(AF)[names(AF) == "freq.x"] <- "Calls"
names(AF)[names(AF) == "freq.y"] <- "Raises + Bets"
names(AF)[names(AF) == "V4"] <- "Aggression Factor"
#names(pfr)[names(pfr) == "V5"] <- "% Played"
AF[, 4][is.na(AF[, 4])] <- 0
AF[, 5][is.na(AF[, 5])] <- 1
AF[, 3][is.na(AF[, 3])] <- 0
AF <- AF %>% mutate_at(vars("Aggression Factor"), dplyr::funs(round(., 3)))

FindT <- left_join(vpip, AF, by = c("Player Name" = "Player Name"))
FindT <- left_join(FindT, pfr, by = c("Player Name" = "Player Name"))
Find <- subset(FindT, select = c("Player Name", "VPIP", "PFR", "Aggression Factor"))

print(Find)

library(ggplot2)
barplot(Find$VPIP, main="VPIP for each player", xlab = "Player Names", ylab="VPIP", name=Find[,1], col = rainbow(25),las=2, cex.names=.8, ylim=c(0.0,1.00))
barplot(Find$PFR, main="PFR for each player", xlab = "Player Names", ylab="PFR", name=Find[,1], col = rainbow(25), las=2, cex.names=.8, ylim=c(0.0,0.35))
barplot(Find$`Aggression Factor`, main="AF for each player", xlab = "Player Names", ylab="AF", name=Find[,1], col = rainbow(25), las=2, cex.names=.8, ylim=c(0.0,2.0))


print(vpip)
print(pfr)
print(AF)
print(Find)


