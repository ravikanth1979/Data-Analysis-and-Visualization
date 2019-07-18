

get_lpi_dataset <- function(dataset){
  lpi_indicators = c("LP.LPI.CUST.XQ",
                         "LP.LPI.INFR.XQ",
                         "LP.LPI.ITRN.XQ",
                         "LP.LPI.LOGS.XQ",
                         "LP.LPI.OVRL.XQ",
                         "LP.LPI.TIME.XQ",
                         "LP.LPI.TRAC.XQ"
  )
  lpi_dataset <- dataset[dataset$`Indicator Code` %in% lpi_indicators,]

  require(tidyr)
  ### assign column name to the first column
  colnames(lpi_dataset)[1] <- "Country_Name"
  colnames(lpi_dataset)[2] <- "Country_Code"
  colnames(lpi_dataset)[3] <- "Indicator_Name"
  colnames(lpi_dataset)[4] <- "Indicator_Code"
  
  ### combine the year columns into a single column with separate rows for each year; assign to new vector
  lpi_dataset <- gather(lpi_dataset,Year,Indicator_Score,-c(Country_Name,Country_Code,Indicator_Name,Indicator_Code))
  row_nums = list()
  count = 1
  for(row in 1:nrow(lpi_dataset)){
    if(sum(is.na(lpi_dataset[row,]))>0){
      row_nums[count] = row
      count = count + 1
    }
  }
  lpi_dataset = lpi_dataset[-unlist(row_nums),]
  
  return(lpi_dataset)
}

get_tourism_dataset <- function(dataset){
  tourism_indicators = c("ST.INT.ARVL",
                         "ST.INT.DPRT",
                         "ST.INT.RCPT.CD",
                         "ST.INT.RCPT.XP.ZS",
                         "ST.INT.TRNR.CD",
                         "ST.INT.TRNX.CD",
                         "ST.INT.TVLR.CD",
                         "ST.INT.TVLX.CD",
                         "ST.INT.XPND.CD",
                         "ST.INT.XPND.MP.ZS")
  
  tourism_dataset <- dataset[dataset$`Indicator Code` %in% tourism_indicators,]
  
  require(tidyr)
  colnames(tourism_dataset)[1] <- "Country_Name"
  colnames(tourism_dataset)[2] <- "Country_Code"
  colnames(tourism_dataset)[3] <- "Indicator_Name"
  colnames(tourism_dataset)[4] <- "Indicator_Code"
  
  ### combine the year columns into a single column with separate rows for each year; assign to new vector
  tourism_dataset <- gather(tourism_dataset,Year,Indicator_Score,-c(Country_Name,Country_Code,Indicator_Name,Indicator_Code))
  row_nums = list()
  count = 1
  for(row in 1:nrow(tourism_dataset)){
    if(sum(is.na(tourism_dataset[row,]))>0){
      row_nums[count] = row
      count = count + 1
    }
  }
  tourism_dataset = tourism_dataset[-unlist(row_nums),]
  return(tourism_dataset)
}

get_trade_dataset <- function(dataset){
  trade_indicators = c("NE.EXP.GNFS.CD",
                       "NE.EXP.GNFS.KD",
                       "NE.EXP.GNFS.KD.ZG",
                       "NE.EXP.GNFS.ZS",
                       "NE.IMP.GNFS.CD",
                       "NE.IMP.GNFS.KD",
                       "NE.IMP.GNFS.KD.ZG",
                       "NE.IMP.GNFS.ZS",
                       "NE.RSB.GNFS.CD",
                       "NE.RSB.GNFS.ZS",
                       "NE.TRD.GNFS.ZS")
                         
  
  trade_dataset <- dataset[dataset$`Indicator Code` %in% trade_indicators,]
  
  require(tidyr)
  colnames(trade_dataset)[1] <- "Country_Name"
  colnames(trade_dataset)[2] <- "Country_Code"
  colnames(trade_dataset)[3] <- "Indicator_Name"
  colnames(trade_dataset)[4] <- "Indicator_Code"
  
  ### combine the year columns into a single column with separate rows for each year; assign to new vector
  trade_dataset <- gather(trade_dataset,Year,Indicator_Score,-c(Country_Name,Country_Code,Indicator_Name,Indicator_Code))
  row_nums = list()
  count = 1
  for(row in 1:nrow(trade_dataset)){
    if(sum(is.na(trade_dataset[row,]))>0){
      row_nums[count] = row
      count = count + 1
    }
  }
  trade_dataset = trade_dataset[-unlist(row_nums),]
  return(trade_dataset)
}

get_net_trade_dataset <- function(dataset){
  net_trade_indicators = c("TX.VAL.MRCH.CD.WT")
  
  
  net_trade_dataset <- dataset[dataset$`Indicator Code` %in% net_trade_indicators,]
  
  require(tidyr)
  colnames(net_trade_dataset)[1] <- "Country_Name"
  colnames(net_trade_dataset)[2] <- "Country_Code"
  colnames(net_trade_dataset)[3] <- "Indicator_Name"
  colnames(net_trade_dataset)[4] <- "Indicator_Code"
  
  ### combine the year columns into a single column with separate rows for each year; assign to new vector
  net_trade_dataset <- gather(net_trade_dataset,Year,Indicator_Score,-c(Country_Name,Country_Code,Indicator_Name,Indicator_Code))
  row_nums = list()
  count = 1
  for(row in 1:nrow(net_trade_dataset)){
    if(sum(is.na(net_trade_dataset[row,]))>0){
      row_nums[count] = row
      count = count + 1
    }
  }
  net_trade_dataset = net_trade_dataset[-unlist(row_nums),]
  return(net_trade_dataset)
}
