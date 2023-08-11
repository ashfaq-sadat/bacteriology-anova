library(readxl)
bacteriodata_pungus <- read_excel("G:/My Drive/Data Analysis Projects/Rasel Vaai/Bacteriology and Heamatology/Book1.xlsx",range = "c4:g19", col_types = c("text","text", "numeric", "numeric", "numeric"))

bacteriodata_pungus$TR <- as.factor(bacteriodata_pungus$TR)


library(dplyr)
bacteriodata_pungus %>% group_by(TR, Treatments)%>%summarise(across(colnames(bacteriodata_pungus[,3:5]), list(mean = mean, sd =sd)),.groups = 'drop') -> bacterio_mean_pungus

bacterio_mean_table_pungus <-
  cbind( bacterio_mean_pungus$Treatments,
    paste(round(bacterio_mean_pungus$`Day-30_mean`, 3), round(bacterio_mean_pungus$`Day-30_sd`, 3), sep = " ± "),
    paste(round(bacterio_mean_pungus$`Day-60_mean`, 3), round(bacterio_mean_pungus$`Day-60_sd`, 3), sep = " ± "),
    paste(round(bacterio_mean_pungus$`Day-90_mean`, 3), round(bacterio_mean_pungus$`Day-90_sd`, 3), sep = " ± ")) %>% as.data.frame()%>% rename(Treatments = V1, Day30 = V2,Day60 = V3,Day90 = V4)
    
