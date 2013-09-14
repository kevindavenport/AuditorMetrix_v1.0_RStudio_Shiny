library(shiny)
library(ggplot2)
library(plyr)
library(grid)
library(googleVis)
library(psych)
library(stringr)
# library(RCurl)
# library(RJSONIO)

# curl_authenicate <- postForm("http://steritech.compliancemetrix.com/api/json/syncreply/Authenticate",
#          .opts = list(postfields = toJSON(list("UserName" = "kevin.davenport", "Password" = "light6785")),
#                       httpheader = c("Content-Type" = "application/json")
#                       )
#          )


# Set directory for images/resources
addResourcePath('images', file.path(getwd(), 'images')) #www

# Mode f(x)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Data load
client1        <- read.csv("data/client1.csv", header = TRUE, fill = FALSE) # is stringsAsFactors = FALSE slower?
client1$Tenure <- rep(factor(c("1-2 yr", "3-4 yr","1-2 yr","5+ yr","5+ yr")), length.out = nrow(client1))
client1 <- client1[c(1:16,22)]


client2        <- read.csv("data/client2.csv", header = TRUE, fill = FALSE) 
client2$Tenure <- rep(factor(c("1-2 yr", "3-4 yr","1-2 yr","5+ yr","5+ yr")), length.out = nrow(client2))
client2 <- client2[c(1:16,22)]


data_sets <- as.list(c("client1","client2")) #call from UI


# 
# split_date_columns <- data.frame(str_split_fixed(Target$Start.Date, " ", 2))
# Target <- cbind(Target,split_date_columns)
# colnames(Target)[18] <- "Day"
# colnames(Target)[19] <- "Time"
# Target$Time <- as.character(Target$Time)  
# Target$Time <- as.numeric(substr(strptime(Target$Time,"%I:%M:%S %p"),12,13)) #start at 12th character end at 19th
# Target$Daypart <- cut(Target$Time, breaks = c(00,11,13,23), labels = c("breakfast",'lunch','dinner'))
# Target$Time <- as.POSIXct(Target$Time,"%H:%M:%S", tz = "") # real POSIX formating not necessary for binning

#Target$Duration <- rnorm(nrow(Target), mean = 151, sd = 51)
#KFC$Duration <- rnorm(nrow(KFC), mean = 134, sd = 41)
#colClasses=c("Completion.Date"="Date"))
#data_sets <- ls() 

#   headerPanel(HTML(' <img id="auditormetrix_logo" align="left" alt="auditormetrix logo" src="images/beta_logo.png" />'), 
#               windowTitle = "AuditorMetrix v.60b"
#   ), 

