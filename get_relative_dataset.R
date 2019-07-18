
get_relative_dataset <- function(tourism_dataset){
  
  tourism_dataset <- tibble::rowid_to_column(tourism_dataset, "ID")
  
  tourism_dataset$relative_indicator_value<-NA
  
  list_of_countries <- unique(tourism_dataset$Country_Name)
  list_of_tourism_indicators <- unique(tourism_dataset$Indicator_Code)
  for(country in list_of_countries){
    {

      for(indicator_code in list_of_tourism_indicators){
        print(paste("Country Name...",country))
        print(paste("indicator_code...",indicator_code))
        tourism_new_dataset = subset(tourism_dataset,((tourism_dataset$Country_Name %in% country) & 
                                                (tourism_dataset$Indicator_Code %in% indicator_code)))
        #print(tourism_new_dataset)
        if(nrow(tourism_new_dataset)>0){
        
        relative_tourism_value <- tourism_new_dataset$Indicator_Score[1]
        
        minimum_score = min(tourism_new_dataset$Indicator_Score)
        maximum_score = max(tourism_new_dataset$Indicator_Score)
        print(paste("relative_tourism_value....",relative_tourism_value))
        #print(paste("maximum score....",maximum_score))
        
        for(i in 1:nrow(tourism_new_dataset)){
          current_value = tourism_new_dataset$Indicator_Score[i]
          print(paste("relative_tourism_value.... ",current_value))
          if(current_value > relative_tourism_value){
            print(current_value)
            new_min = 0
            new_max = 100
            old_min = relative_tourism_value
            old_max = maximum_score
            new_value = ((current_value-old_min)/(old_max-old_min))*(new_max-new_min) + new_min
            ##tourism_dataset$Indicator_Score[i] = new_value
            tourism_dataset[tourism_dataset$ID==tourism_new_dataset$ID[i],]$relative_indicator_value = new_value
            print(tourism_dataset[tourism_dataset$ID==tourism_new_dataset$ID[i],]$relative_indicator_value)
          }
          
          if(current_value < relative_tourism_value){
            print(current_value)
            new_min = 0
            new_max = 100
            old_min = relative_tourism_value
            old_max = maximum_score
            new_value = ((current_value-old_min)/(old_max-old_min))*(new_max-new_min) + new_min
            tourism_dataset[tourism_dataset$ID==tourism_new_dataset$ID[i],]$relative_indicator_value = new_value
            print(tourism_dataset[tourism_dataset$ID==tourism_new_dataset$ID[i],])
          }
          if(current_value == relative_tourism_value){
            print(current_value)
            tourism_dataset[tourism_dataset$ID==tourism_new_dataset$ID[i],]$relative_indicator_value  = 0
            print(tourism_dataset[tourism_dataset$ID==tourism_new_dataset$ID[i],])
          }
        }
        tourism_dataset[tourism_dataset$ID==tourism_new_dataset$ID[1],]$relative_indicator_value = 0

        }
      }
    }
  }
  return(tourism_dataset)
  
}
