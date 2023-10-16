# 1) IMPORTANDO A PLANILHA DO EXCEL PARA O RSTUDIO

# install.packages("rlang")

library(readxl)

TCC2_Database <- read_excel("C:/Users/gusta/Downloads/Lithuania_SCM_Database.xlsx")
View(TCC2_Database)


# 2) ARRUMANDO A BASE DE DADOS PELO R COM O PACOTE "tidyverse"


# Caso não tenha, baixar com o comando abaixo:

# install.packages("ggplot2")
# install.packages("tidyverse")


library("readxl")
library("tibble")
library("tidyverse")
data =  TCC2_Database
tibble = as_tibble(data)


# 2.1) O objetivo é agregar as variáveis iguais que englobam países diferentes


data %>%
  select(c(starts_with("CPI"),date)) %>% 
  pivot_longer(cols = c("CPI_croatia","CPI_czech_republic",
                        "CPI_hungary", "CPI_poland","CPI_sweden","CPI_denmark","CPI_lithuania"), 
               names_to = "Country", 
               values_to = "CPI") ->  CPI

CPI$Country = gsub("CPI_","\\1",CPI$Country)

data %>%
  select(c(starts_with("GDP"),date)) %>% 
  pivot_longer(cols = c("GDP_croatia","GDP_czech_republic",
                        "GDP_hungary", "GDP_poland","GDP_sweden","GDP_denmark","GDP_lithuania"), 
               names_to = "Country", 
               values_to = "GDP") ->  GDP


GDP$Country = gsub("GDP_","\\1",GDP$Country)


# 2.2) Juntando as variáveis, fazendo o arranjo por datas e pelos países, e criando variáveis diferenciais.


new_data = inner_join(x= GDP,y = CPI)
new_data %>% 
  arrange(date) %>% 
  group_by(Country) %>% 
  mutate(Inflacao = log(CPI) - lag(log(CPI)),
         DGDP = log(GDP) - lag(log(GDP)),
         D_Inflacao = Inflacao - lag(Inflacao),
         D_DGDP = DGDP - lag(DGDP),
         log_CPI = log(CPI),
         log_GDP = log(GDP)) -> new_data 


new_data  %>%
filter(date > 0)-> new_data


# --------------------------------------------- // -------------------------------------------------


# 3) AGORA PARTIREMOS PARA O ALGORITMO DO CONTROLE SINTÉTICO

# É necessário instalar os seguintes pacotes, caso não tenha:

#install.packages("devtools")
#devtools::install_github("edunford/tidysynth")
#install.packages("Synth")
#install.packages("tidyverse")


require(tidysynth)
head(new_data)
new_data %>% dplyr::glimpse()

new_data_out <-
  
  new_data %>%
  
  # 3.1) Passando os comandos iniciais, como a variável dependente, a unidade de tratamento e o período de tratamento.
  
  synthetic_control(outcome = CPI, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "lithuania", # unit where the intervention occurred
                    i_time = 21, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  

  
  # 3.2) Gerando os preditores agregados.
  
  generate_predictor(time_window = 1,
                     lagged_inflation_1 = (CPI)) %>%
  generate_predictor(time_window = 2,
                     lagged_inflation_2 = (CPI)) %>%
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (CPI)) %>%
  generate_predictor(time_window = 8,
                     lagged_inflation_8 = (CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (CPI)) %>%
  generate_predictor(time_window = 10,
                     lagged_inflation_10 = (CPI)) %>%
  
  generate_predictor(time_window = 11,
                     lagged_inflation_11 = (CPI)) %>%
  generate_predictor(time_window = 12,
                     lagged_inflation_12 = (CPI)) %>%
  generate_predictor(time_window = 13,
                     lagged_inflation_13 = (CPI)) %>%
  generate_predictor(time_window = 14,
                     lagged_inflation_14 = (CPI)) %>%
  generate_predictor(time_window = 15,
                     lagged_inflation_15 = (CPI)) %>%
  generate_predictor(time_window = 16,
                     lagged_inflation_16 = (CPI)) %>%
  generate_predictor(time_window = 17,
                     lagged_inflation_17 = (CPI)) %>%
  generate_predictor(time_window = 18,
                     lagged_inflation_18 = (CPI)) %>%
  generate_predictor(time_window = 19,
                     lagged_inflation_19 = (CPI)) %>%
  generate_predictor(time_window = 20,
                     lagged_inflation_20 = (CPI)) %>%
  generate_predictor(time_window = 21,
                     lagged_inflation_21 = (CPI)) %>%
 

  
  # 3.3) Gerando os pesos para a otimização:
  
  generate_weights(
    optimization_window = 1:21, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # 3.4) Gerando o controle sintético:
  
generate_control()


# 3.5) Gerando as plotagens e outros resultados relacionados ao método:


new_data_out %>% plot_trends()

new_data_out %>% plot_differences()

new_data_out %>% plot_weights()

new_data_out %>% grab_balance_table()

new_data_out %>% plot_placebos()

new_data_out %>% plot_placebos(prune = FALSE)

new_data_out %>% plot_mspe_ratio()
new_data_out %>% grab_mspe_ratio()


view(new_data_out %>% grab_significance())

new_data_out %>% grab_outcome()
new_data_out %>% grab_outcome()

new_data_out %>% grab_predictors()

new_data_out %>% grab_unit_weights()

new_data_out %>% grab_predictor_weights()

new_data_out %>% grab_loss()

new_data_out %>% grab_synthetic_control()

new_data_out %>% grab_placebos(prune = FALSE)

new_data_out %>% grab_significance()

# 3.6) Passando os resultados para uma planilha vazia do excel para apresentação

install.packages("writexl")

df <- new_data_out %>% grab_significance()

print(df)

library("writexl")

write_xlsx(df,"C:\\Users\\gusta\\Downloads\\dados2_controle_tcc.xlsx")


df2 <- new_data_out %>% grab_unit_weights()

print(df2)

library("writexl")

#write_xlsx(df2,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")


df3 <- new_data_out %>% grab_predictor_weights()

print(df3)

library("writexl")

#write_xlsx(df3,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")


df4 <- new_data_out %>% grab_balance_table()

print(df4)

library("writexl")

#write_xlsx(df4,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")


df5 <- new_data_out %>% grab_significance()

print(df5)

library("writexl")

#write_xlsx(df5,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")


df6 <- new_data_out %>% grab_loss()

print(df6)

library("writexl")

#write_xlsx(df6,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")



# --------------------------- // ----------------------------------------


# 4) ANÁLISE DESCRITIVA

# 4.1) Construindo tabelas com informações da média de inflação


Inflacao_pre_tratamento = mean(new_data$Inflacao[new_data$date < 21])


Inflacao_pos_tratamento = mean(new_data$Inflacao[new_data$date >= 21])


Inflacao_pre_tratamento_slovakia = mean(new_data$Inflacao[new_data$date < 37 & new_data$Country == "slovakia"])


Inflacao_pos_tratamento_slovakia =  mean(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "slovakia"])


Geral_pre_sem_slovakia = mean(new_data$Inflacao[new_data$date < 37 & new_data$Country != "slovakia"])


Geral_pos_sem_slovakia = mean(new_data$Inflacao[new_data$date >= 37 & new_data$Country != "slovakia"])




Inflacao_media = data.frame(c("Geral pré-tratamento"= Inflacao_pre_tratamento,
                              "Geral pós-tratamento"= Inflacao_pos_tratamento,
                              "Eslováquia pré-tratamento"= Inflacao_pre_tratamento_slovakia,
                              "Eslováquia pós-tratamento"= Inflacao_pos_tratamento_slovakia,
                              "Geral sem a Eslováquia pré-tratamento"= Geral_pre_sem_slovakia,
                              "Geral sem a Eslováquia pós-tratamento"= Geral_pos_sem_slovakia))


colnames(Inflacao_media) <- "Média Trimestral da Inflação"




dif_geral = Inflacao_pos_tratamento - Inflacao_pre_tratamento


dif_slovakia = Inflacao_pos_tratamento_slovakia - Inflacao_pre_tratamento_slovakia


dif_geral_sem_slovakia = Geral_pos_sem_slovakia - Geral_pre_sem_slovakia




dif_Inflacao_media = data.frame(c("Geral"= dif_geral,
                                  
                                  "Eslováquia"= dif_slovakia,
                                  
                                  "Geral sem a Eslováquia"= dif_geral_sem_slovakia))

colnames(dif_Inflacao_media) <- "Diferença da Média Trimestral da Inflação - (em %)"



# 4.2) Construindo tabelas com informações do desvio padrão da inflação:



dp_pre_tratamento =  sd(new_data$Inflacao[new_data$date < 37])


dp_pos_tratamento =  sd(new_data$Inflacao[new_data$date >= 37])


dp_pre_tratamento_slovakia =  sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "slovakia"])


dp_pos_tratamento_slovakia =   sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "slovakia"])


Geral_pre_sem_slovakia_dp =  
  (sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "czech_republic"])+
     sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "hungary"])+
     sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "poland"])+
     sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "sweden"])+
     sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "denmark"])+
     sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "lithuania"])+
     sd(new_data$Inflacao[new_data$date < 37 & new_data$Country == "latvia"]))/7


Geral_pos_sem_slovakia_dp =  
  (sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "czech_republic"])+
     sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "hungary"])+
     sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "poland"])+
     sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "sweden"])+
     sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "denmark"])+
     sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "lithuania"])+
     sd(new_data$Inflacao[new_data$date >= 37 & new_data$Country == "latvia"]))/7




Inflacao_dp = data.frame(c("Geral pré-tratamento"= dp_pre_tratamento,
                           "Geral pós-tratamento"= dp_pos_tratamento,
                           "Eslováquia pré-tratamento"= dp_pre_tratamento_slovakia,
                           "Eslováquia pós-tratamento"= dp_pos_tratamento_slovakia,
                           "Geral sem a Eslováquia pré-tratamento"= Geral_pre_sem_slovakia_dp,
                           "Geral sem a Eslováquia pós-tratamento"= Geral_pos_sem_slovakia_dp))


colnames(Inflacao_dp) <- "Volatilidade Trimestral da Inflação"




dif_geral_dp = dp_pos_tratamento - dp_pre_tratamento

dif_slovakia_dp = dp_pos_tratamento_slovakia - dp_pre_tratamento_slovakia

dif_geral_sem_slovakia_dp = Geral_pos_sem_slovakia_dp - Geral_pre_sem_slovakia_dp


dif_Inflacao_dp = data.frame(c("Geral"= dif_geral_dp,
                               
                               "Eslováquia"= dif_slovakia_dp,
                               
                               "Geral sem a Eslováquia"= dif_geral_sem_slovakia_dp))


colnames(dif_Inflacao_dp) <- "Diferença da volatilidade Trimestral da Inflação"



# 4.3) Passando as novas informações para o excel


library("writexl")


#write_xlsx(Inflacao_media,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")


library("writexl")


#write_xlsx(dif_Inflacao_media,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")


library("writexl")


#write_xlsx(Inflacao_dp,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")


library("writexl")


#write_xlsx(dif_Inflacao_dp,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")




t.test(new_data$Inflacao[new_data$date < 37 & new_data$Country == "slovakia"], mu = 1.0353607)



# 4.4) Construindo Histogramas para a inflação:


x <- new_data$Inflacao[new_data$date < 37 & new_data$Country == "slovakia"]
z <- new_data$Inflacao[new_data$date >= 37 & new_data$Country == "slovakia"]

x_2 <- new_data$Inflacao[new_data$date < 37 & new_data$Country != "slovakia"]
z_2 <- new_data$Inflacao[new_data$date >= 37 & new_data$Country != "slovakia"]


par(mfrow=c(2,2))

hist(
  x, 
  main = paste("Eslováquia pré-intervenção" ),
  xlim = c(-0.02,0.04), ylim = c(0,65),
  col = "gray",
  xlab = "Inflação", ylab = "Densidade",
  freq = FALSE)


hist(
  z, 
  main = paste("Eslováquia pós-intervenção" ),
  xlim = c(-0.02,0.04),ylim = c(0,65),
  col = "gray",
  xlab = "Inflação",ylab = "Densidade",
  freq = FALSE)


hist(
  new_data$Inflacao[new_data$date < 37 & new_data$Country != "slovakia"], 
  main = paste("Grupo de controle pré-intervenção" ),
  xlim = c(-0.02,0.04), ylim = c(0,65),
  col = "lightblue",
  xlab = "Inflação", ylab = "Densidade",
  freq = FALSE,
  probability = TRUE)


hist(
  new_data$Inflacao[new_data$date >= 37 & new_data$Country != "slovakia"], 
  main = paste("Grupo de controle pós-intervenção" ),
  xlim = c(-0.02,0.04),ylim = c(0,65),
  col = "lightblue",
  xlab = "Inflação",ylab = "Densidade",
  freq = FALSE)



# 4.5) Construindo tabela com medidas sumário:


# mínimo

min_x = min(x)
min_z = min(z)
min_x_2 = min(x_2)
min_z_2 = min(z_2)

# máximo

max_x = max(x)
max_z = max(z)
max_x_2 = max(x_2)
max_z_2 = max(z_2)

# mediana

median_x = median(x)
median_z = median(z)
median_x_2 = median(x_2)
median_z_2 = median(z_2)

# média

mean_x = mean(x)
mean_z = mean(z)
mean_x_2 = mean(x_2)
mean_z_2 = mean(z_2)

# Primeiro Quartil

p25_x = quantile(x,probs = .25)
p25_z = quantile(z,probs = .25)
p25_x_2 = quantile(x_2,probs = .25)
p25_z_2 = quantile(z_2,probs = .25)

# Terceiro Quartil

p75_x = quantile(x,probs = .75)
p75_z = quantile(z,probs = .75)
p75_x_2 = quantile(x_2,probs = .75)
p75_z_2 = quantile(z_2,probs = .75)




medidas_sumario = data.frame(c("Mínimo"= min_x,min_z,min_x_2,min_z_2),
                             c("1st Qua."= p25_x,p25_z,p25_x_2,p25_z_2),
                             c("Mediana"= median_x,median_z,median_x_2,median_z_2),
                             c("Média"= mean_x,mean_z,mean_x_2,mean_z_2),
                             c("3st Qua."= p75_x,p75_z,p75_x_2,p75_z_2),
                             c("Máximo"= max_x,max_z,max_x_2,max_z_2))


colnames(medidas_sumario) <- c("Mínimo","1st Qua.","Mediana",
                               "Média","3st Qua.","Máximo" )

row.names(medidas_sumario) <- c("Eslováquia pré-tratamento","Eslováquia pós-tratamento","Grupo de controle pré-tratamento",
                                "Grupo de controle pós-tratamento" )



library("writexl")


#write_xlsx(medidas_sumario,"C:\\Users\\gusta\\Downloads\\descritiva.xlsx")



# 4.6) Contruindo boxplots para a inflação



par(mfrow=c(2,2))

boxplot(
  x, 
  main = c("Eslováquia pré-intervenção" ),
  ylim = c(0,0.03),
  col = "gray",
  ylab = "Inflação"
)


boxplot(
  z, 
  main = c("Eslováquia pós-intervenção" ),
  ylim = c(0,0.03),
  col = "gray",
  ylab = "Inflação"
)


boxplot(
  new_data$Inflacao[new_data$date < 37 & new_data$Country != "slovakia"], 
  main = paste("Grupo de controle pré-intervenção" ),
  ylim = c(0,0.03),
  col = "lightblue",
  ylab = "Inflação"
)


boxplot(
  new_data$Inflacao[new_data$date >= 37 & new_data$Country != "slovakia"], 
  main = paste("Grupo de controle pós-intervenção" ),
  ylim = c(0,0.03),
  col = "lightblue",
  ylab = "Inflação"
)




# 4.7) Construindo Barplots para o crescimento da oferta monetária e para o crescimento do PIB



par(mfrow=c(1,2))

barplot(new_data$D_M1[new_data$date >= 37 & new_data$Country == "slovakia"],
        main = c("Eslováquia pós-intervenção" ),
        ylim = c(-0.1,0.15),
        col = "gray",
        ylab = "Crescimento da M1"
)


barplot(new_data$DGDP
        [new_data$date >= 37 & new_data$Country == "slovakia"],
        main = c("Eslováquia pós-intervenção" ),
        ylim = c(-0.1,0.15),
        col = "lightblue",
        ylab = "Crescimento do PIB")


# ------------------------------------------------ // --------------------------------------

# 5) ROBUSTEZ

# 5.1) PLACEBO - SE TIVESSE ADOTADO JÁ EM 2005:


new_data_out <-
  
  new_data %>%
  
  
  
  synthetic_control(outcome = log_CPI, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "czech_republic", # unit where the intervention occurred
                    i_time = 21, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  
  
  generate_predictor(time_window = 5:25,
                     Debt = mean(div, na.rm = TRUE),
                     Gov_expend = mean(D_gov, na.rm = TRUE),
                     Exports = mean(exports, na.rm = TRUE),
                     Openess = mean(open, na.rm = TRUE),
                     GDP_growth = mean(DGDP, na.rm = TRUE),
                     M1_growth = mean(D_M1, na.rm = TRUE)
  ) %>%
  
  
  
  generate_predictor(time_window = 2:25,
                     lagged_inflation = mean(D_Inflacao, na.rm = TRUE)
  ) %>%  
  
  
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (log_CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (log_CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (log_CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (log_CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (log_CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (log_CPI)) %>%
  generate_predictor(time_window = 18,
                     lagged_inflation_18 = (log_CPI)) %>%
  
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(
    optimization_window = 1:26, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # Generate the synthetic control
  generate_control()



new_data_out %>% plot_trends()



# 5.2) VALIDAÇÃO CRUZADA


#new_data_out <-

# new_data %>%

# initial the synthetic control object
# synthetic_control(outcome = log_CPI, # outcome
#    unit = Country, # unit index in the panel data
#    time = date, # time index in the panel data
#   i_unit = "czech_republic", # unit where the intervention occurred
#  i_time = 37, # time period when the intervention occurred
# generate_placebos=TRUE # generate placebo synthetic controls (for inference)
#  ) %>% 

# Generate the aggregate predictors used to fit the weights

# average log income, retail price of cigarettes, and proportion of the
# population between 15 and 24 years of age from 1980 - 1988


# generate_predictor(time_window = 1:19,
#   Debt = mean(div, na.rm = TRUE),
#  Gov_expend = mean(D_gov, na.rm = TRUE),
# Exports = mean(exports, na.rm = TRUE),
#Openess = mean(open, na.rm = TRUE),
#GDP_growth = mean(DGDP, na.rm = TRUE),
#M1_growth = mean(D_M1, na.rm = TRUE)
#) %>%

#generate_predictor(time_window = 2:19,
#                  lagged_inflation = mean(D_Inflacao, na.rm = TRUE)
#) %>%  


#generate_predictor(time_window = 3,
#          lagged_inflation_3 = (log_CPI)) %>%
#generate_predictor(time_window = 6,
#           lagged_inflation_6 = (log_CPI)) %>%
#generate_predictor(time_window = 7,
#                    lagged_inflation_7 = (log_CPI)) %>%
# generate_predictor(time_window = 9,
#                    lagged_inflation_9 = (log_CPI)) %>%
# generate_predictor(time_window = 4,
#                    lagged_inflation_4 = (log_CPI)) %>%
# generate_predictor(time_window = 5,
#                    lagged_inflation_5 = (log_CPI)) %>%
# generate_predictor(time_window = 18,
#                    lagged_inflation_18 = (log_CPI)) %>%



# Generate the fitted weights for the synthetic control
# generate_weights(
#   optimization_window = 1:17, na.rm = TRUE) %>% # time to use in the optimization task


# Generate the synthetic control
# generate_control()



#new_data_out %>% plot_trends()

#new_data_out %>% plot_weights()

#new_data_out %>% plot_placebos(prune = FALSE)


generate_predictor(time_window = 2,
                   lagged_inflation_2 = (CPI)) %>%
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (CPI)) %>%
  generate_predictor(time_window = 8,
                     lagged_inflation_8 = (CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (CPI)) %>%
  generate_predictor(time_window = 10,
                     lagged_inflation_10 = (CPI)) %>%
  generate_predictor(time_window = 11,
                     lagged_inflation_11 = (CPI)) %>%
  generate_predictor(time_window = 12,
                     lagged_inflation_12 = (CPI)) %>%
  generate_predictor(time_window = 13,
                     lagged_inflation_13 = (CPI)) %>%
  generate_predictor(time_window = 14,
                     lagged_inflation_14 = (CPI)) %>%
  generate_predictor(time_window = 15,
                     lagged_inflation_15 = (CPI)) %>%
  generate_predictor(time_window = 16,
                     lagged_inflation_16 = (CPI)) %>%
  
  generate_predictor(time_window = 17,
                     lagged_inflation_17 = (CPI)) %>%
  generate_predictor(time_window = 18,
                     lagged_inflation_18 = (CPI)) %>%
  generate_predictor(time_window = 19,
                     lagged_inflation_19 = (CPI)) %>%
  generate_predictor(time_window = 20,
                     lagged_inflation_20 = (CPI)) %>%
  generate_predictor(time_window = 21,
                     lagged_inflation_21 = (CPI)) %>%
  generate_predictor(time_window = 22,
                     lagged_inflation_22 = (CPI)) %>%
  generate_predictor(time_window = 23,
                     lagged_inflation_23 = (CPI)) %>%
  generate_predictor(time_window = 24,
                     lagged_inflation_24 = (CPI)) %>%
  generate_predictor(time_window = 25,
                     lagged_inflation_25 = (CPI)) %>%
  generate_predictor(time_window = 26,
                     lagged_inflation_26 = (CPI)) %>%
  generate_predictor(time_window = 27,
                     lagged_inflation_27 = (CPI)) %>%
  generate_predictor(time_window = 28,
                     lagged_inflation_28 = (CPI)) %>%
  generate_predictor(time_window = 29,
                     lagged_inflation_29 = (CPI)) %>%
  generate_predictor(time_window = 30,
                     lagged_inflation_30 = (CPI)) %>%
  generate_predictor(time_window = 31,
                     lagged_inflation_31 = (CPI)) %>%
  generate_predictor(time_window = 32,
                     lagged_inflation_32 = (CPI)) %>%
  generate_predictor(time_window = 33,
                     lagged_inflation_33 = (CPI)) %>%
  generate_predictor(time_window = 34,
                     lagged_inflation_34 = (CPI)) %>%
  generate_predictor(time_window = 35,
                     lagged_inflation_35 = (CPI)) %>%
  generate_predictor(time_window = 36,
                     lagged_inflation_36 = (CPI)) %>%
  generate_predictor(time_window = 37,
                     lagged_inflation_37 = (CPI)) %>%
  generate_predictor(time_window = 38,
                     lagged_inflation_38 = (CPI)) %>%
  generate_predictor(time_window = 39,
                     lagged_inflation_39 = (CPI)) %>%
  generate_predictor(time_window = 40,
                     lagged_inflation_40 = (CPI)) %>%
  generate_predictor(time_window = 41,
                     lagged_inflation_41 = (CPI)) %>%
  generate_predictor(time_window = 42,
                     lagged_inflation_42 = (CPI)) %>%
  generate_predictor(time_window = 43,
                     lagged_inflation_43 = (CPI)) %>%
  generate_predictor(time_window = 44,
                     lagged_inflation_44 = (CPI)) %>%
  generate_predictor(time_window = 45,
                     lagged_inflation_45 = (CPI)) %>%
  generate_predictor(time_window = 46,
                     lagged_inflation_46 = (CPI)) %>%
  generate_predictor(time_window = 47,
                     lagged_inflation_47 = (CPI)) %>%
  generate_predictor(time_window = 48,
                     lagged_inflation_48 = (CPI)) %>%
  
  generate_predictor(time_window = 49,
                     lagged_inflation_49 = (CPI)) %>%
  generate_predictor(time_window = 50,
                     lagged_inflation_50 = (CPI)) %>%
  generate_predictor(time_window = 51,
                     lagged_inflation_51 = (CPI)) %>%
  generate_predictor(time_window = 52,
                     lagged_inflation_52 = (CPI)) %>%
  generate_predictor(time_window = 53,
                     lagged_inflation_53 = (CPI)) %>%
  generate_predictor(time_window = 54,
                     lagged_inflation_54 = (CPI)) %>%
  generate_predictor(time_window = 55,
                     lagged_inflation_55 = (CPI)) %>%
  generate_predictor(time_window = 56,
                     lagged_inflation_56 = (CPI)) %>%
  generate_predictor(time_window = 57,
                     lagged_inflation_57 = (CPI)) %>%
  generate_predictor(time_window = 58,
                     lagged_inflation_58 = (CPI)) %>%
  generate_predictor(time_window = 59,
                     lagged_inflation_59 = (CPI)) %>%
  generate_predictor(time_window = 60,
                     lagged_inflation_60 = (CPI)) %>%

  
  generate_predictor(time_window = 40,
                     lagged_inflation_40 = (log_CPI)) %>%
  generate_predictor(time_window = 41,
                     lagged_inflation_41 = (log_CPI)) %>%
  generate_predictor(time_window = 42,
                     lagged_inflation_42 = (log_CPI)) %>%
  generate_predictor(time_window = 43,
                     lagged_inflation_43 = (log_CPI)) %>%
  generate_predictor(time_window = 44,
                     lagged_inflation_44 = (log_CPI)) %>%
  generate_predictor(time_window = 45,
                     lagged_inflation_45 = (log_CPI)) %>%
  generate_predictor(time_window = 46,
                     lagged_inflation_46 = (log_CPI)) %>%
  generate_predictor(time_window = 47,
                     lagged_inflation_47 = (log_CPI)) %>%
  generate_predictor(time_window = 48,
                     lagged_inflation_48 = (log_CPI)) %>%
  
  generate_predictor(time_window = 49,
                     lagged_inflation_49 = (log_CPI)) %>%
  generate_predictor(time_window = 50,
                     lagged_inflation_50 = (log_CPI)) %>%
  generate_predictor(time_window = 51,
                     lagged_inflation_51 = (log_CPI)) %>%
  generate_predictor(time_window = 52,
                     lagged_inflation_52 = (log_CPI)) %>%
  generate_predictor(time_window = 53,
                     lagged_inflation_53 = (log_CPI)) %>%
  generate_predictor(time_window = 54,
                     lagged_inflation_54 = (log_CPI)) %>%
  generate_predictor(time_window = 55,
                     lagged_inflation_55 = (log_CPI)) %>%
  generate_predictor(time_window = 56,
                     lagged_inflation_56 = (log_CPI)) %>%
  generate_predictor(time_window = 57,
                     lagged_inflation_57 = (log_CPI)) %>%
  generate_predictor(time_window = 58,
                     lagged_inflation_58 = (log_CPI)) %>%
  generate_predictor(time_window = 59,
                     lagged_inflation_59 = (log_CPI)) %>%
  generate_predictor(time_window = 60,
                     lagged_inflation_60 = (log_CPI)) %>%
  
  
  
# TESTE PLACEBO - 2012  
  
  new_data_out_placebo <-
  
  new_data %>%
  
  # 3.1) Passando os comandos iniciais, como a variável dependente, a unidade de tratamento e o período de tratamento.
  
  synthetic_control(outcome = CPI, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "lithuania", # unit where the intervention occurred
                    i_time = 9, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  
  
  # 3.2) Gerando os preditores agregados.
  
  generate_predictor(time_window = 1,
                     lagged_inflation_1 = (CPI)) %>%
  generate_predictor(time_window = 2,
                     lagged_inflation_2 = (CPI)) %>%
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (CPI)) %>%
  generate_predictor(time_window = 8,
                     lagged_inflation_8 = (CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (CPI)) %>%

  
  
  
  # 3.3) Gerando os pesos para a otimização:
  
  generate_weights(
    optimization_window = 1:9, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # 3.4) Gerando o controle sintético:
  
  generate_control()


# 3.5) Gerando as plotagens e outros resultados relacionados ao método:


new_data_out_placebo %>% plot_trends()
new_data_out_placebo %>% grab_synthetic_control()
new_data_out_placebo %>% grab_significance()
new_data_out_placebo %>% plot_differences()

# install.packages("writexl")

df_placebo <- new_data_out_placebo %>% grab_synthetic_control()

print(df_placebo)

library("writexl")

write_xlsx(df_placebo,"C:\\Users\\gusta\\Downloads\\placebo_tcc.xlsx")



# VALIDAÇÃO CRUZADA - SUÉCIA


new_data_out_sweden <-
  
  new_data %>%
  
  # 3.1) Passando os comandos iniciais, como a variável dependente, a unidade de tratamento e o período de tratamento.
  
  synthetic_control(outcome = CPI, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "sweden", # unit where the intervention occurred
                    i_time = 21, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  


  # 3.2) Gerando os preditores agregados.
  
  generate_predictor(time_window = 1,
                     lagged_inflation_1 = (CPI)) %>%
  generate_predictor(time_window = 2,
                     lagged_inflation_2 = (CPI)) %>%
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (CPI)) %>%
  generate_predictor(time_window = 8,
                     lagged_inflation_8 = (CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (CPI)) %>%
  generate_predictor(time_window = 10,
                     lagged_inflation_10 = (CPI)) %>%
  
  generate_predictor(time_window = 11,
                     lagged_inflation_11 = (CPI)) %>%
  generate_predictor(time_window = 12,
                     lagged_inflation_12 = (CPI)) %>%
  generate_predictor(time_window = 13,
                     lagged_inflation_13 = (CPI)) %>%
  generate_predictor(time_window = 14,
                     lagged_inflation_14 = (CPI)) %>%
  generate_predictor(time_window = 15,
                     lagged_inflation_15 = (CPI)) %>%
  generate_predictor(time_window = 16,
                     lagged_inflation_16 = (CPI)) %>%
  generate_predictor(time_window = 17,
                     lagged_inflation_17 = (CPI)) %>%
  generate_predictor(time_window = 18,
                     lagged_inflation_18 = (CPI)) %>%
  generate_predictor(time_window = 19,
                     lagged_inflation_19 = (CPI)) %>%
  generate_predictor(time_window = 20,
                     lagged_inflation_20 = (CPI)) %>%
  generate_predictor(time_window = 21,
                     lagged_inflation_21 = (CPI)) %>%
  
  
  
  # 3.3) Gerando os pesos para a otimização:
  
  generate_weights(
    optimization_window = 1:21, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # 3.4) Gerando o controle sintético:
  
  generate_control()


# 3.5) Gerando as plotagens e outros resultados relacionados ao método:


new_data_out_sweden %>% plot_trends()

# install.packages("writexl")

df2 <- new_data_out_sweden %>% grab_synthetic_control()

print(df2)

library("writexl")

write_xlsx(df2,"C:\\Users\\gusta\\Downloads\\val_cruzada_sweden.xlsx")


# VALIDAÇÃO CRUZADA - SUÉCIA


new_data_out_hungary<-
  
  new_data %>%
  
  # 3.1) Passando os comandos iniciais, como a variável dependente, a unidade de tratamento e o período de tratamento.
  
  synthetic_control(outcome = CPI, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "hungary", # unit where the intervention occurred
                    i_time = 21, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  
  
  
  # 3.2) Gerando os preditores agregados.
  
  generate_predictor(time_window = 1,
                     lagged_inflation_1 = (CPI)) %>%
  generate_predictor(time_window = 2,
                     lagged_inflation_2 = (CPI)) %>%
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (CPI)) %>%
  generate_predictor(time_window = 8,
                     lagged_inflation_8 = (CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (CPI)) %>%
  generate_predictor(time_window = 10,
                     lagged_inflation_10 = (CPI)) %>%
  
  generate_predictor(time_window = 11,
                     lagged_inflation_11 = (CPI)) %>%
  generate_predictor(time_window = 12,
                     lagged_inflation_12 = (CPI)) %>%
  generate_predictor(time_window = 13,
                     lagged_inflation_13 = (CPI)) %>%
  generate_predictor(time_window = 14,
                     lagged_inflation_14 = (CPI)) %>%
  generate_predictor(time_window = 15,
                     lagged_inflation_15 = (CPI)) %>%
  generate_predictor(time_window = 16,
                     lagged_inflation_16 = (CPI)) %>%
  generate_predictor(time_window = 17,
                     lagged_inflation_17 = (CPI)) %>%
  generate_predictor(time_window = 18,
                     lagged_inflation_18 = (CPI)) %>%
  generate_predictor(time_window = 19,
                     lagged_inflation_19 = (CPI)) %>%
  generate_predictor(time_window = 20,
                     lagged_inflation_20 = (CPI)) %>%
  generate_predictor(time_window = 21,
                     lagged_inflation_21 = (CPI)) %>%
  
  
  
  # 3.3) Gerando os pesos para a otimização:
  
  generate_weights(
    optimization_window = 1:21, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # 3.4) Gerando o controle sintético:
  
  generate_control()


# 3.5) Gerando as plotagens e outros resultados relacionados ao método:


new_data_out_hungary %>% plot_trends()

# install.packages("writexl")

df3 <- new_data_out_hungary %>% grab_synthetic_control()

print(df3)

library("writexl")

write_xlsx(df3,"C:\\Users\\gusta\\Downloads\\val_cruzada_hungary.xlsx")


# VALIDAÇÃO CRUZADA - 2010/2012 - 2012-2015


new_data_out_validation<-
  
  new_data %>%
  
  # 3.1) Passando os comandos iniciais, como a variável dependente, a unidade de tratamento e o período de tratamento.
  
  synthetic_control(outcome = CPI, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "lithuania", # unit where the intervention occurred
                    i_time = 21, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  
  
  
  # 3.2) Gerando os preditores agregados.
  
  generate_predictor(time_window = 1,
                     lagged_inflation_1 = (CPI)) %>%
  generate_predictor(time_window = 2,
                     lagged_inflation_2 = (CPI)) %>%
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (CPI)) %>%
  generate_predictor(time_window = 8,
                     lagged_inflation_8 = (CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (CPI)) %>%

  
  
  # 3.3) Gerando os pesos para a otimização:
  
  generate_weights(
    optimization_window = 10:21, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # 3.4) Gerando o controle sintético:
  
  generate_control()


# 3.5) Gerando as plotagens e outros resultados relacionados ao método:


new_data_out_validation %>% plot_trends()

new_data_out_validation %>% plot_differences()

# install.packages("writexl")

df5 <- new_data_out_validation %>% grab_synthetic_control()

print(df5)

library("writexl")

write_xlsx(df5,"C:\\Users\\gusta\\Downloads\\val_cruzada.xlsx")



new_data_out_GDP <-
  
  new_data %>%
  
  # 3.1) Passando os comandos iniciais, como a variável dependente, a unidade de tratamento e o período de tratamento.
  
  synthetic_control(outcome = GDP, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "lithuania", # unit where the intervention occurred
                    i_time = 21, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  
  
  # 3.2) Gerando os preditores agregados.
  

  
  generate_predictor(time_window = 11,
                     lagged_inflation_11 = (GDP)) %>%
  generate_predictor(time_window = 12,
                     lagged_inflation_12 = (GDP)) %>%
  generate_predictor(time_window = 13,
                     lagged_inflation_13 = (GDP)) %>%
  generate_predictor(time_window = 14,
                     lagged_inflation_14 = (GDP)) %>%
  generate_predictor(time_window = 15,
                     lagged_inflation_15 = (GDP)) %>%
  generate_predictor(time_window = 16,
                     lagged_inflation_16 = (GDP)) %>%
  generate_predictor(time_window = 17,
                     lagged_inflation_17 = (GDP)) %>%
  generate_predictor(time_window = 18,
                     lagged_inflation_18 = (GDP)) %>%
  generate_predictor(time_window = 19,
                     lagged_inflation_19 = (GDP)) %>%
  generate_predictor(time_window = 20,
                     lagged_inflation_20 = (GDP)) %>%
  generate_predictor(time_window = 21,
                     lagged_inflation_21 = (GDP)) %>%
  
  
  
  # 3.3) Gerando os pesos para a otimização:
  
  generate_weights(
    optimization_window = 1:21, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # 3.4) Gerando o controle sintético:
  
  generate_control()


# 3.5) Gerando as plotagens e outros resultados relacionados ao método:


new_data_out_GDP %>% plot_trends()


# VALIDAÇÃO CRUZADA - POLONIA


new_data_out_poland<-
  
  new_data %>%
  
  # 3.1) Passando os comandos iniciais, como a variável dependente, a unidade de tratamento e o período de tratamento.
  
  synthetic_control(outcome = CPI, # outcome
                    unit = Country, # unit index in the panel data
                    time = date, # time index in the panel data
                    i_unit = "poland", # unit where the intervention occurred
                    i_time = 21, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  
  
  
  # 3.2) Gerando os preditores agregados.
  
  generate_predictor(time_window = 1,
                     lagged_inflation_1 = (CPI)) %>%
  generate_predictor(time_window = 2,
                     lagged_inflation_2 = (CPI)) %>%
  generate_predictor(time_window = 3,
                     lagged_inflation_3 = (CPI)) %>%
  generate_predictor(time_window = 4,
                     lagged_inflation_4 = (CPI)) %>%
  generate_predictor(time_window = 5,
                     lagged_inflation_5 = (CPI)) %>%
  generate_predictor(time_window = 6,
                     lagged_inflation_6 = (CPI)) %>%
  generate_predictor(time_window = 7,
                     lagged_inflation_7 = (CPI)) %>%
  generate_predictor(time_window = 8,
                     lagged_inflation_8 = (CPI)) %>%
  generate_predictor(time_window = 9,
                     lagged_inflation_9 = (CPI)) %>%
  generate_predictor(time_window = 10,
                     lagged_inflation_10 = (CPI)) %>%
  
  generate_predictor(time_window = 11,
                     lagged_inflation_11 = (CPI)) %>%
  generate_predictor(time_window = 12,
                     lagged_inflation_12 = (CPI)) %>%
  generate_predictor(time_window = 13,
                     lagged_inflation_13 = (CPI)) %>%
  generate_predictor(time_window = 14,
                     lagged_inflation_14 = (CPI)) %>%
  generate_predictor(time_window = 15,
                     lagged_inflation_15 = (CPI)) %>%
  generate_predictor(time_window = 16,
                     lagged_inflation_16 = (CPI)) %>%
  generate_predictor(time_window = 17,
                     lagged_inflation_17 = (CPI)) %>%
  generate_predictor(time_window = 18,
                     lagged_inflation_18 = (CPI)) %>%
  generate_predictor(time_window = 19,
                     lagged_inflation_19 = (CPI)) %>%
  generate_predictor(time_window = 20,
                     lagged_inflation_20 = (CPI)) %>%
  generate_predictor(time_window = 21,
                     lagged_inflation_21 = (CPI)) %>%
  
  
  
  # 3.3) Gerando os pesos para a otimização:
  
  generate_weights(
    optimization_window = 1:21, na.rm = TRUE) %>% # time to use in the optimization task
  
  
  # 3.4) Gerando o controle sintético:
  
  generate_control()


# 3.5) Gerando as plotagens e outros resultados relacionados ao método:


new_data_out_poland %>% plot_trends()

# install.packages("writexl")

df10 <- new_data_out_poland %>% grab_synthetic_control()

print(df10)

library("writexl")

write_xlsx(df10,"C:\\Users\\gusta\\Downloads\\val_cruzada_poland.xlsx")
