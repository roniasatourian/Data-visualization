library(plotly)
library(dplyr)
library(readr)
library(usmap)
library(ggplot2)

Data_fec <- read.csv("fec_2008-2022.csv")

states <- read.csv("csvData.csv")


state_loan <- Data_fec %>% group_by(Cand_State) %>% summarise(loan = mean(Total_Loan)) 

state_loan_clean <- state_loan %>%
  inner_join(states, by=c("Cand_State"="Code")) %>% 
  mutate(loan_thousand = loan / 1000,
         loan_log = log(loan,10))


names(state_loan_clean)[1] <- 'state'


state_loan_clean_CAGA <- state_loan_clean %>% 
                        filter(state == "CA" | state == "GA")

Data_fec$Coverage_start_year <-format(as.Date(Data_fec$Coverage_Start_Date, format="%d/%m/%Y"),"%Y")

Data_fec <- Data_fec[!is.na(Data_fec$Coverage_start_year),]

state_loan_2 <- Data_fec %>% group_by(Cand_State,Coverage_start_year) %>% summarise(loan = mean(Total_Loan)) 

state_loan_clean_2 <- state_loan_2 %>%
  inner_join(states, by=c("Cand_State"="Code")) %>% 
  mutate(log_loan = log(loan + 1,10))

State_data_2 <- state_loan_clean_2 %>% filter(Cand_State=='CA' | Cand_State=='GA')

state_loan_3 <- Data_fec %>% group_by(Cand_State,Coverage_start_year) %>% summarise(loan = mean(Total_Loan)) 

CA_GA_loan_3 <- state_loan_3 %>% filter(Cand_State=='CA' | Cand_State=='GA')


CA_loan_3 <- CA_GA_loan_3 %>% 
  filter(Cand_State == "CA")

GA_loan_3 <- CA_GA_loan_3 %>% 
  filter(Cand_State == "GA")

CA_laon_year_model <- lm(loan ~ as.numeric(Coverage_start_year), data = CA_loan_3)
GA_laon_year_model <- lm(loan ~ as.numeric(Coverage_start_year), data = GA_loan_3)

state <- c(state_loan_clean_CAGA$state[1],state_loan_clean_CAGA$state[2])
CAGA_trend_slope <- c(CA_laon_year_model$coefficients[2],GA_laon_year_model$coefficients[2])

CAGA_trend_slope_df <- data.frame(state, CAGA_trend_slope)


#---- us map

CA_GA_plot <- plot_usmap(data = CAGA_trend_slope_df, values = "CAGA_trend_slope" ,
                         labels = TRUE, label_color = "white") + 
  scale_fill_continuous(name = "Average Yearly Loan \n       Increase($) \n", limits = c(0,15000),
                        low = "yellow", high = "blue") + 
  theme(legend.position = "right")

CA_GA_plot$layers[[2]]$aes_params$size <- 2

CA_GA_plot



#Recourses
#https://cran.r-project.org/web/packages/usmap/vignettes/advanced-mapping
#https://stackoverflow.com/questions/60806822/how-do-i-change-state-or-counties-label-sizes-in-r-with-the-function-usmap
#https://stackoverflow.com/questions/13888222/ggplot-scale-color-gradient-to-range-outside-of-data-range
#-------------------------------------------------------
# Time series (GOOD  2)


p_1 <- ggplot(State_data_2, aes(x=Coverage_start_year, y=loan/1000, color=Cand_State ,group = Cand_State)) +
  geom_line() + 
  scale_color_manual(labels = c("CA", "GA"), values = c("chartreuse4", "violetred1")) +
  guides(color=guide_legend("State")) +
  xlab("Coverage Start Year") +
  ylab("Average Loan ($1000)") 
p_1
#-----------------------------------------------------------
# Scatter plot (Good 3)



ggplot(CA_GA_loan_3, aes(x=as.numeric(Coverage_start_year), y=loan/1000, color=Cand_State)) +
  geom_point() + 
  xlab("Coverage Start Year") +
  ylab("Average Loan ($1000)") +
  scale_color_manual(labels = c("CA", "GA"), values = c("chartreuse4", "violetred1")) +
  guides(color=guide_legend("State")) +
  scale_x_discrete(limit = seq(2007,2020,1)) +
  geom_smooth(method = lm, se = FALSE)
#-------------------------------------------------------------
# Bar plot (Good 4)

ggplot(data = CAGA_trend_slope_df, aes(x= state , y=CAGA_trend_slope/1000, fill = state)) +
  geom_bar(stat='identity',fill = c("darkolivegreen4", "goldenrod2")) +
  ylab("Average Yearly Loan Increase ($1000)") +
  xlab("State")


#-----------------------------------------------------------
# Times series (BAD 1)
p1_log <- ggplot(State_data_2, aes(x=Coverage_start_year, y=log(loan,10), color=Cand_State ,group = Cand_State)) +
  geom_line() + 
  xlab("Coverage Start year") +
  scale_y_continuous(limits = c(0,6),labels = c(0,100,200,300)) +
  scale_color_manual(labels = c("CA", "GA"), values = c("chartreuse4", "violetred1")) +
  guides(color=guide_legend("State")) +
  ylab("Average Loan ($1000)")

p1_log

#--------------------------
# Scatter plot (BAD 2)
state_loan_3 <- Data_fec %>% group_by(Cand_State,Coverage_start_year) %>% summarise(loan = mean(Total_Loan)) 

CA_GA_loan_3 <- state_loan_3 %>% filter(Cand_State=='CA' | Cand_State=='GA')

ggplot(CA_GA_loan_3, aes(x=as.numeric(Coverage_start_year), y=log(loan,10), color=Cand_State)) +
  geom_point() + 
  xlab("Coverage Start Year") +
  ylab("Average Loan ($1000)") +
  scale_color_manual(labels = c("CA", "GA"), values = c("chartreuse4", "violetred1")) +
  guides(color=guide_legend("State")) +
  scale_x_discrete(limit = seq(2007,2020,1)) +
  scale_y_continuous(limits = c(0,6),labels = c(0,100,200,300)) +
  geom_smooth(method = lm, se = FALSE)
#----------------------------
#bar plot (BAD 3)
ggplot(data = state_loan_clean_CAGA, aes(x= state , y=loan_log, fill = state)) +
  geom_bar(stat='identity',fill = c("skyblue3","skyblue4")) +
  ylab("Average Loan ($1000)") +
  xlab("State")
  scale_y_continuous(limits = c(0,6),labels = c(0,100,200,300))
  
#----------------------------------
#pie plot (BAD 4)
pie <- ggplot(state_loan_clean_CAGA, aes(x="", y=loan_log, fill=state)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() 

pie + scale_fill_manual(values=c("skyblue3", "skyblue4")) +
  ggtitle("                     Average Loan proportion")
   






