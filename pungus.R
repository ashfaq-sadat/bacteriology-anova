library(readxl)
bacteriodata_pungus <- read_excel("G:/My Drive/Data Analysis Projects/Rasel Vaai/Bacteriology and Heamatology/Book1.xlsx",range = "c4:g19", col_types = c("text","text", "numeric", "numeric", "numeric"))

bacteriodata_pungus$TR <- as.factor(bacteriodata_pungus$TR)
library(dplyr)
bacteriodata_pungus <- bacteriodata_pungus%>%rename(Day.30 = `Day-30`,Day.60 = `Day-60`,Day.90 = `Day-90`)

library(dplyr)
bacteriodata_pungus %>% group_by(TR, Treatments)%>%summarise(across(colnames(bacteriodata_pungus[,3:5]), list(mean = mean, sd =sd)),.groups = 'drop') -> bacterio_mean_pungus

bacterio_mean_table_pungus <-
  cbind( bacterio_mean_pungus$Treatments,
    paste(round(bacterio_mean_pungus$`Day.30_mean`, 3), round(bacterio_mean_pungus$`Day.30_sd`, 3), sep = " ± "),
    paste(round(bacterio_mean_pungus$`Day.60_mean`, 3), round(bacterio_mean_pungus$`Day.60_sd`, 3), sep = " ± "),
    paste(round(bacterio_mean_pungus$`Day.90_mean`, 3), round(bacterio_mean_pungus$`Day.90_sd`, 3), sep = " ± ")) %>% as.data.frame()%>% rename(Treatments = V1, Day.30 = V2,Day.60 = V3,Day.90 = V4)
    
# Analysis of variance ####
# Shapiro-Wilk test

# shaprio_test <- list()
# shaprio_test_p <- list()
# shaprio_test_summary <- list()
# 
# for (i in colnames(bacteriodata_pungus[, 3:5])) {
#   test <- shapiro.test(bacteriodata_pungus[[i]])
#   shaprio_test[[i]] <- test
#   shaprio_test_p[i] <- test$p.value
#   shaprio_test_p <- as.data.frame(shaprio_test_p)
#   
#   
# }


# Creating variable vector

anova_results <- list()
remove(i)
for (i in colnames(bacteriodata_pungus[, 3:5])) {
  fit <- aov(bacteriodata_pungus[[i]] ~ TR, data = bacteriodata_pungus)
  anova_results[[i]] <- fit
}

# Creating ANOVA Summary

summary_results <- list()
pValues <- list()
remove(i)
for (i in colnames(bacteriodata_pungus[, 3:5])) {
  summaries <- summary(aov(bacteriodata_pungus[[i]] ~ TR, data = bacteriodata_pungus))
  summary_results[i] <- summaries
  pValues[[i]]<- summaries[[1]][1,5]
  
}

pValues <- as.data.frame(pValues)
pValues1 <- cbind(c("pValues"),pValues)
colnames(pValues1) <- c("Treatments","Day.30","Day.60","Day.90")
# (There are a number of other ways of accessing a particular element of a vector that's a particular element of a list, which should also work.)
# 
# If you want to play with the whole anova table, you should assign it to a variable:
# 
# aa <- anova(lm(yield~variety+block))
# and then you can pull out whatever you want:
# 
# aa[1,5]
# You can then rename or replace parts of it, to make it look however you need - but if you change it in substantial ways, you may want to remove the 'anova' class in case it no longer meets the requirements to be an anova table.




# Multiple Comparison

if (!require('multcompView'))
  install.packages('multcompView')
library(multcompView)


tukey.results <- list()
cld.results <- list()
remove(i)
for (i in colnames(bacteriodata_pungus[, 3:5])) {
  tukey <- TukeyHSD(anova_results[[i]])
  tukey.results[[i]] <- tukey
  cld <- multcompLetters4(anova_results[[i]], tukey)
  cld.results[[i]] <- cld
  
}
# Listing the Tukey ANOVA results letters

cld_list <- list()
cld_list1 <- list()
remove(i)
library(dplyr)

library(stringr)
for (i in colnames(bacteriodata_pungus[, 3:5])) {
    cld_list1<- as.data.frame.character(cld.results[[i]][["TR"]][["Letters"]])
    cld_list1<-  cld_list1[order(as.numeric(row.names(cld_list1))), ]
  cld_list[[i]] <- cld_list1
  cld_list <- data.frame(cld_list)
}

#### Exporting datasheets ####
# To sort the cld dataframe according to the row numbers
# cld_list <- cld_list[order(as.numeric(row.names(cld_list))), ]

pungus_final_table <- cbind(bacterio_mean_table_pungus[,1], paste(bacterio_mean_table_pungus$Day.30, cld_list$Day.30),paste(bacterio_mean_table_pungus$Day.60, cld_list$Day.60),paste(bacterio_mean_table_pungus$Day.90, cld_list$Day.90)) 

pungus_final_table <- as.data.frame(pungus_final_table)%>% rename(Treatments = V1, Day.30 = V2,Day.60 = V3,Day.90 = V4) 

readr::write_excel_csv(rbind(pungus_final_table,pValues1), file = "pungus_table.csv")


# Graphs ####
# making graph dataset
install.packages("reshape2")
library(reshape2)
library(dplyr)
library(tidyr)


cld_data_pungus <- cbind(bacterio_mean_pungus[, 1:2], cld_list)


grpah_mean_data_pungus <-  
  bacteriodata_pungus %>% group_by(TR, Treatments)%>%summarise(across(colnames(bacteriodata_pungus[,3:5]), list(mean = mean)),.groups = 'drop') %>%
  pivot_longer(
  cols = Day.30_mean:Day.90_mean,
  names_to = "Days",
  values_to = "means"
)

grpah_mean_data_pungus$Days <- as.factor(grpah_mean_data_pungus$Days)
graph_sd_data_pungus <-
  bacteriodata_pungus %>% group_by(TR, Treatments) %>% 
  summarise(across(colnames(bacteriodata_pungus[, 3:5]),
                   list(sd =sd)),
            .groups = 'drop') %>% 
  pivot_longer(cols = Day.30_sd:Day.90_sd,
               names_to = "Days",
               values_to = "sd")


graph_cld_data_pungus <- pivot_longer(
  cld_data_pungus,
  cols = Day.30:Day.90,
  names_to = "Days",
  values_to = "cld"
)

pungus_graph_data <-
  cbind(grpah_mean_data_pungus,
        graph_sd_data_pungus["sd"],
        graph_cld_data_pungus["cld"])
# Using the pivot_wider() function
# The pivot_wider() function from the dplyr package can also be used accomplish the same task of converting wide to long data. In this case we use the chapter 12 data as the input data frame, select columns Absent0 through Present8 to pivot wider, specify the column names of the selected columns will be placed into a new column called condition.angle, and then the values in to a new column called reaction time. Finally, we will sepatrate the condition and angle variables to be placed in separate columns, condition and angle. The length of the code is roughly the same, but it may be more intuitive to understand the mechanics.
# 
# long_data <- 
#   pivot_longer(chapter_12_table_1,
#                cols = Absent0:Present8,
#                names_to = "condition.angle",
#                values_to = "reaction_time") %>%
#   separate(col = condition.angle,
#            into = c("condition", "angle"),
#            sep = -1)


library(ggplot2)
install.packages("ggthemes")
library(ggthemes)


p <- ggplot(data = pungus_graph_data,
            aes(x = Days,
                y = means,
                fill = Treatments)) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  ylab("TSA (CFU/mL)	10^5") +
  geom_errorbar(
    aes(ymin = means - sd,
        ymax = means + sd),
    position = position_dodge(0.9),
    width = 0.25,
    color = "#000000"
  ) +
  geom_text(
    pungus_graph_data,
    mapping = aes(label = cld,
                  y = means + sd) ,
    stat = "identity",
    position_dodge(width = 0.9),
    
    vjust = -.5
  ) +
  scale_x_discrete(labels = c('Day 30',
                              'Day 60',
                              'Day 90'))+
  theme_pander()+
  theme(legend.position = 'right')+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.ticks = element_line(),
        )
  
  
p
ggsave(filename = "pungus.png", plot = p, device = "png", scale= 1)
