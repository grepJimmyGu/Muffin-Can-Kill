library(RODBC)
conn <- odbcConnect("HSS_Medicare", uid = "hssmedicare", pwd = "McKinsey1!")
Practice_state <- sqlQuery(conn,'SELECT Top 54 [Provider Business Practice Location Address State Name], COUNT(DISTINCT NPI) AS Cnt  FROM (SELECT * FROM NPI_Data WHERE [Entity Type Code] = 1) AS A GROUP BY [Provider Business Practice Location Address State Name] ORDER BY Cnt DESC')
State_Network <- matrix(0, nrow = 5400, ncol = 4)


# First column is the name
for(i in 1:nrow(Practice_state)){
  start <- (i-1)*100 + 1
  end <- i*100
  State_Network[start:end,1] <- as.character(Practice_state[i,1])
  State_Network[start:end,2] <- as.numeric(Practice_state[i,2])
}

########################################## Physicians with largest network ##############################################################################################################################################################
# Test query for california
Test_q <- paste("SELECT TOP 100 NPI_from,COUNT(DISTINCT NPI_To) AS OUT_net_work FROM Referral_Data INNER JOIN (SELECT NPI FROM NPI_Data WHERE [Provider Business Practice Location Address State Name] = '",Practice_state[53,1],"'AND [Entity Type Code] = 1) AS X ON (Referral_Data.NPI_from = X.NPI) GROUP BY NPI_from ORDER BY OUT_net_work DESC", sep = "")
Test <- as.data.frame(sqlQuery(conn, Test_q))
State_Network[1:100,3:4] <- as.matrix(Test)

# OUT_net_work
for(i in 1:nrow(Practice_state)){
  start <- (i-1)*100 + 1
  end <- i*100
  Query <-paste("SELECT TOP 100 NPI_from,COUNT(DISTINCT NPI_To) AS OUT_net_work FROM Referral_Data INNER JOIN (SELECT NPI FROM NPI_Data WHERE [Provider Business Practice Location Address State Name] = '",Practice_state[i,1],"'AND [Entity Type Code] = 1) AS X ON (Referral_Data.NPI_from = X.NPI) GROUP BY NPI_from ORDER BY OUT_net_work DESC", sep = "")
  result <- sqlQuery(conn, Query)
  end_2 <- (i-1)*100 + nrow(result) # In case we do not have the top 100
  State_Network[start:min(end,end_2),3:4] <- as.matrix(result)
  print(Practice_state[i,1])
}

State_Network_OUT <- State_Network
sqlSave(conn, as.data.frame(State_Network_OUT))
# IN_net_work
for(i in 1:nrow(Practice_state)){
  start <- (i-1)*100 + 1
  end <- i*100
  Query <-paste("SELECT TOP 100 NPI_To,COUNT(DISTINCT NPI_from) AS IN_net_work FROM Referral_Data INNER JOIN (SELECT NPI FROM NPI_Data WHERE [Provider Business Practice Location Address State Name] = '",Practice_state[i,1],"'AND [Entity Type Code] = 1) AS X ON (Referral_Data.NPI_To = X.NPI) GROUP BY NPI_To ORDER BY IN_net_work DESC", sep = "")
  result <- sqlQuery(conn, Query)
  end_2 <- (i-1)*100 + nrow(result) # In case we do not have the top 100
  State_Network[start:min(end,end_2),3:4] <- as.matrix(result)
  print(Practice_state[i,1])
}
State_Network_IN <- State_Network
sqlSave(conn, as.data.frame(State_Network_IN))
write.csv(State_Network_OUT, "~/Medicare Referral/Data/2014_10_29_Development_ctn/Network_File_FromR.csv")
write.csv(State_Network_IN, "~/Medicare Referral/Data/2014_10_29_Development_ctn/Network_File_FromR.csv")

# Other information we need
State_OUT_extra <- sqlQuery(conn, 'SELECT * FROM State_Network_OUT LEFT JOIN PC_uniq ON (State_Network_OUT.NPI = PC_uniq.NPI)')
State_IN_extra <- sqlQuery(conn, 'SELECT * FROM State_Network_IN LEFT JOIN PC_uniq ON (State_Network_IN.NPI = PC_uniq.NPI)')
write.csv(State_IN_extra, "~/Medicare Referral/Data/2014_10_29_Development_ctn/Network_File_FromR.csv")

############################################## Insight On these Physicians/Map #######################################
names(State_OUT_extra[,13]) <- 'Cnt'
USnetwork <- read.csv("~/Medicare Referral/Data/2014_10_29_Development_ctn/US_network.csv", header = TRUE)

dim(map_data("state"))

# Sample of US. Arrest
if (require("maps")) {
  states <- map_data("state")
  arrests <- USArrests
  names(arrests) <- tolower(names(arrests))
  arrests$region <- tolower(rownames(USArrests))
  
  choro <- merge(states, arrests, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  qplot(long, lat, data = choro, group = group, fill = assault,
        geom = "polygon")
  qplot(long, lat, data = choro, group = group, fill = assault / murder,
        geom = "polygon")
}

# Network Plot
library(ggplot2)
states <- map_data("state")
network <- USnetwork
names(network) <- tolower(names(network))
network$region <- tolower(USnetwork$Name)

choro <- merge(states, network, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, fill = network,geom = "polygon")

qplot(long, lat, data = choro, group = group, fill = network,geom = "polygon")

# Average BeneCnt Plot
states <- map_data("state")
Bene_Pair <- read.csv("~/Medicare Referral/Data/2014_10_29_Development_ctn/2014_11_11_US_Bene_Pair.csv", header = TRUE)
Bene_Pair$StateName <- tolower(Bene_Pair$StateName)
names(Bene_Pair)[1] <- "region"
choro <- merge(states, Bene_Pair, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ] # Very crucial
p<-qplot(long, lat, data = choro[which(choro$region == 'minnesota'),], group = group, fill = Average_Bene, geom = "polygon")
print(p)
p + scale_fill_gradient(low = "", high = "#FF3300")

# If I need to use brewer
# Average Pair Count
choro$Ave_Pair <- cut(choro$Average_Pair, breaks = c(1:15)*5000) #Change to number of bins you want
p <- qplot(long, lat, data = choro, group = group, fill = Ave_Pair, geom = "polygon", colour = "blue")
p + scale_fill_brewer(palette = "YlOrRd")

# Average Bene Count
choro$Ave_Bene <- cut(choro$Average_Bene, breaks = c(1:6)*500)
p <- qplot(long, lat, data = choro, group = group, fill = Ave_Bene, geom = "polygon", colour = "red")
p + scale_fill_brewer(palette = "Blues")

############################################## Insight On these Physicians/Map #######################################
MI_hospital <- read.csv("~/Medicare Referral/Data/2014_10_29_Development_to_2014_11_18/2014_11_7_MI_hospitals.csv", header = TRUE)
sqlSave(conn, MI_hospital)

############################################## 11/20/2014 Map plot for PCP/Critical Care ############################
library(ggplot2)
states <- map_data("state")
Map_infor <- read.csv("~/Medicare Referral/Data/2014_11_19_CST/INPUT.csv", header = TRUE)
Map_infor$Name <- tolower(Map_infor$Name)
names(Map_infor)[2] <- "region"
names(Map_infor)[3:6] <- c("Intensivists_volume", "Intensivists_share", "PCP_volume", "PCP_share")
input <- merge(states, Map_infor, by = "region")
input <- input[order(input$order),]
# Plot
input[,8]<- cut(input[,8], breaks = c(0:5)*2000)
p1 <- qplot(long, lat, data = input, group = group, fill = Intensivists_volume, geom = "polygon", colour = "red")
p1 + scale_fill_brewer(palette = "Blues")
input[,9] <- cut(input[,9], c(0:12)*0.01)
p2 <- qplot(long, lat, data = input, group = group, fill = Intensivists_share, geom = "polygon", colour = "red")
p2 +  scale_fill_brewer(palette = "YlOrRd")
input[,10] <- cut(input[,10], c(0:4)*500000)
p3 <- qplot(long, lat, data = input, group = group, fill = PCP_volume, geom = "polygon", colour = "red")
p3 + scale_fill_brewer(palette = "Blues")
input[,11] <- cut(input[,11], c(0:12)*0.01)
p4 <- qplot(long, lat, data = input, group = group, fill = PCP_share, geom = "polygon", colour = "red")
p4 + scale_fill_brewer(palette = "YlOrRd")

############################################## 1/29/2015 Map plot for PCP/Critical Care ############################
library(ggplot2)
county <- map_data("county", "california")
sample_loyalty <- as.data.frame(matrix(NA, nrow = 58, ncol = 3))
sample_loyalty[,1] <- as.character(levels(factor(county[,6])))
sample_loyalty[,2] <- as.integer(sample(c(20:80), 58))
sample_loyalty[,3] <- as.integer(sample(c(20:80), 58))
names(sample_loyalty)[1] <- "subregion"
names(sample_loyalty)[2:3] <- c("loyalty","market")
input <- merge(county, sample_loyalty, by = "subregion")
input <- input[order(input$order),]

# Plot
input[,7]<- cut(input[,7], breaks = 5)
p1 <- qplot(long, lat, data = input, group = group, fill = loyalty, geom = "polygon", colour = "red")
p1 + scale_fill_brewer(palette = "Blues")

input[,8] <- cut(input[,8], breaks = 5)
p2 <- qplot(long, lat, data = input, group = group, fill = market, geom = "polygon", colour = "red")
p2 +  scale_fill_brewer(palette = "YlOrRd")

############################################## 2015/6/4/ Columbus Map plot for PCP/Critical Care ############################
library(ggplot2)
county <- map_data("county", "ohio")
sample_loyalty <- as.data.frame(matrix(NA, nrow = 58, ncol = 3))
sample_loyalty[,1] <- as.character(levels(factor(county[,6])))
sample_loyalty[,2] <- as.integer(sample(c(20:80), 58))
sample_loyalty[,3] <- as.integer(sample(c(20:80), 58))
names(sample_loyalty)[1] <- "subregion"
names(sample_loyalty)[2:3] <- c("loyalty","market")
input <- merge(county, sample_loyalty, by = "subregion")
input <- input[order(input$order),]

# Plot
input[,7]<- cut(input[,7], breaks = 5)
p1 <- qplot(long, lat, data = 0, group = group, fill = 0, geom = "polygon", colour = "red")
p1 + scale_fill_brewer(palette = "Blues")

input[,8] <- cut(input[,8], breaks = 5)
p2 <- qplot(long, lat, data = input, group = group, fill = market, geom = "polygon", colour = "red")
p2 +  scale_fill_brewer(palette = "YlOrRd")
