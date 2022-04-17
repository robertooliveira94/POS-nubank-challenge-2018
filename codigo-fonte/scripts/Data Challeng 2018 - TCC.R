# DATA CHALLENGE 2018 NUBANK TCC  

# LIMPANDO O AMBIENTE ------------------------------------------------------------------
rm(list = ls())
library("janitor")
library(dataPreparation)

# PACOTES ------------------------------------------------------------------
pacotes = c("dplyr", "stringr","tidyverse","tidymodels","ranger","tune","probably",
            "xgboost" ,"tictoc", "ggplot2",  "abjutils", 
            "ggthemes","dataPreparation","tidygeocoder","Cluster","ClusterR","themis","janitor")
novos.pacotes = pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes, repos = 'https://cran.us.r-project.org')
options(warn = -1)
unlist(lapply(pacotes, require, character.only = TRUE))

# CARREGANDO AS BASES INICIAIS ------------------------------------------------------------------
acquisition_train = read.csv("codigo-fonte/data/acquisition_train.csv")

# DICIONARIO DA BASE DE AQUISICAO
dicionario = read.table("codigo-fonte/data/dicionario.txt",sep = "-",header = T,encoding = "UTF-8")

# ENRRIQUECIMENTO DOS DADOS ------------------------------------------------------------------

# CORRIGINDO COLUNAS DE ESTADO E SEPARANDO COLUNAS DE LATITUDE E LONGITUDE
acquisition_train_ = acquisition_train %>%
  dplyr::mutate(shipping_state = stringr::str_sub(string = shipping_state,start = 4,end = 5),
                lat_lon = str_replace(lat_lon, "\\(",""),
                lat_lon = str_replace(lat_lon, "\\)",""))

# NESTA ETAPAZ FOI REALIZADO UMA BUSCA DE ENDERECOS ATRAVES DAS VARIAVEIS DE LATITUDE E LONGITUDE COM A AJUDA DO PACOTE tidygeocoder
# SPLIT COLUNA lat_lon
# base_split = str_split_fixed(acquisition_train_$lat_lon, ",", 2)
# acquisition_train_$Latitude = as.numeric(base_split[,1])
# acquisition_train_$Longitude = as.numeric(base_split[,2])
# reverse <- acquisition_train_ %>%
#   reverse_geocode(lat = Latitude, long = Longitude, method = 'osm',
#                   address = address_found, full_results = TRUE)
# save(reverse,file = "codigo-fonte/data/reverse.Rdata")

# CARREGANDO OBJETO BAIXADO
load(file = "codigo-fonte/data/reverse.Rdata",verbose = T)

# REMOVENDO VARIAVEIS EXTRAS ORIUNDAS DO REVERSE_GEOCODE
base_reverse = reverse %>% 
  dplyr::mutate(municipio = gsub("^.*? de ","",municipality)) %>%
  dplyr::select(c(ids:target_fraud,Latitude,Longitude,municipio,state...60)) %>%
  dplyr::rename(state = state...16,
                state_reverse = state...60)

# ENRRIQUECIMENTO DOS DADOS --------------------------------------------------------------
# BASES DO IBGE IDH
atlas = read.table("codigo-fonte/data/atlas.csv",encoding = 'UTF-8',sep = ',',header = T)
desc = read.csv("codigo-fonte/data/desc.csv",encoding = 'UTF-8',sep = ',')
estados = read.table("codigo-fonte/data/estados.txt",encoding = 'UTF-8',sep = ';',header = T)

# BASE DE 2010 COM COLUNAS RELACIONADAS A ECONOMIA DO MUNICIPIO
atlas_2010 = atlas %>% 
  dplyr::filter(ano=='2010') %>%
  dplyr::select(codmun6,uf,codmun7,município,desc$SIGLA[desc$cat=='econ']) %>%
  dplyr::left_join(estados,by=c("uf"="uf")) %>%
  dplyr::mutate(cidade = paste0(município,"-",state)) %>%
  dplyr::mutate(cidade = str_to_upper(abjutils::rm_accent(cidade)),
                cidade_atlas = str_to_upper(abjutils::rm_accent(cidade)))

# JOIN DAS BASES PELO NOME DO MINICIPIO
base_mesclada = base_reverse %>%
  dplyr::mutate(municipio = stringr::str_to_upper(abjutils::rm_accent(municipio))) %>%
  dplyr::mutate(shipping_state = stringr::str_to_upper(abjutils::rm_accent(shipping_state))) %>%
  dplyr::mutate(state_reverse = stringr::str_to_upper(abjutils::rm_accent(state_reverse))) %>%
  dplyr::rename(state_codificado = state) %>%
  dplyr::left_join(estados %>% 
                     dplyr::mutate(name_state = stringr::str_to_upper(abjutils::rm_accent(name_state))),
                   by = c("state_reverse"="name_state")) %>%
  dplyr::mutate(cidade = ifelse(is.na(municipio),NA,paste0(municipio,"-",state))) %>%
  dplyr::left_join(atlas_2010,by = c("cidade"="cidade"))

# LIMPANDO AS VARIAVEIS  ---------------------------------------------------------------------

# REMOVENDO LINHAS EM QUE A VARIAVEL TARGET_DEFAULT E NULA
base_mesclada_1 = base_mesclada %>% 
  dplyr::mutate(target_default = ifelse(target_default=="",NA,target_default)) %>%
  dplyr::filter(!is.na(target_default))

# QUANTIDADE DE REGISTROS ""
sum(as.data.frame(base_mesclada_1) == "",na.rm = T)

# SUBSTITUINDO "" POR NA
base_mesclada_2 = as.data.frame(base_mesclada_1)
base_mesclada_2[base_mesclada_2 == ""] <- NA

# TRATANDO ALGUMAS VARIAVEIS
# APLICATION_TIME_APLIED: RETIRAR HORA DA APLICACAO
# EXTERNAL_DATA_PROVIDER_EMAIL_SEEN_BEFORE = -999 POR NA
# email.hotmail.com e email.hotmaill.com
# variavel "reported_income" nos valores Inf tranforma-se em NA
# Criando variavel Regiao

base_mesclada_3 = base_mesclada_2 %>%
  dplyr::mutate(hora_aplicacao = as.numeric(substr(application_time_applied,start = 1,stop = 2)),
                external_data_provider_email_seen_before = ifelse(external_data_provider_email_seen_before==-999,NA,external_data_provider_email_seen_before),
                email = ifelse(email=="hotmaill.com","hotmail.com",email),
                email = ifelse(email=="gmaill.com","gmail.com",email),
                reported_income = ifelse(reported_income==Inf,NA,reported_income),
                regiao = dplyr::case_when(state.y %in% c("AM","RR","AP","PA","TO","RO","AC") ~ "Região Norte",
                                          state.y %in% c("MA","PI","CE","RN","PE","PB","SE","AL","BA") ~ "Região Nordeste",
                                          state.y %in% c("MT","MS","GO") ~ "Região Centro-Oeste",
                                          state.y %in% c("SP","RJ","ES","MG") ~ "Região Sudeste",
                                          state.y %in% c("PR","RS","SC") ~ "Região Sul",TRUE ~ "")) %>%
  dplyr::select(-c(application_time_applied))

# TIPO DADOS/QTDE_VALORES_AUSENTES/PERC_VALORES_AUSENTES/QTDE_VALORES_UNICOS
df_colunas = function(base_analise){
  colunas = data.frame(row.names = c("Coluna","Tipo","qtde_Valores_ausentes","Perc_Valores_ausentes"))
  for(i in 1:dim(base_analise)[2]){
    colunas_i = data.frame(coluna=colnames(base_analise)[i],
                           Tipo = class(base_analise[,i]),
                           qtde_Valores_ausentes = sum(is.na(base_analise[,i])),
                           Perc_Valores_ausentes = round(sum(is.na(base_analise[,i]))*100/nrow(base_analise),2),
                           qtde_valores_unicos = length(unique(base_analise[,i])))
    colunas = rbind(colunas,colunas_i) %>% dplyr::arrange(desc(Perc_Valores_ausentes))
  }
  return(colunas)
}
colunas = df_colunas(as.data.frame(base_mesclada_3))

# TRATANDO COLUNAS COM NULOS
colunas_nulos = colunas %>% 
  dplyr::filter(qtde_Valores_ausentes>0) %>% 
  dplyr::arrange(desc(qtde_Valores_ausentes)) %>%
  dplyr::mutate(perc_label = paste0(as.character(format(Perc_Valores_ausentes, decimal.mark = ','))," %"))
colunas_nulos$coluna <- factor(colunas_nulos$coluna,levels = rev(colunas_nulos$coluna),ordered = T)

# REMOVER COLUNAS DA BASE DE ENRRIQUECIMENTO 
library(Hmisc)
colunas_nulos2=colunas_nulos %>% 
  dplyr::filter(qtde_Valores_ausentes!=6644) %>%
  dplyr::filter(coluna %nin% c("municipio", "cidade", "uf.x", "uf.y","state.x"))

# GRAFICO DE COLUNAS COM NULOS
graf_nulos = ggplot(colunas_nulos2, aes(x=coluna))+ 
  labs(title = "Percentual de valores ausentes por coluna",x="Coluna",y="Percentual")+
  geom_bar(aes(weight = Perc_Valores_ausentes,fill = "#66C2A5"))+
  ylim(0,110)+
  geom_text(aes(label = perc_label, y = Perc_Valores_ausentes),hjust = -0.2,size = 4) +
  coord_flip()+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); graf_nulos
# SALVANDO GRAFICO
ggsave(plot = graf_nulos,filename = "codigo-fonte/imagens/graf_colunas_nulas.png",width = 10,height = 7)

# VARIAVEIS COM NULOS QUE RECEBERAO A CLASSE MAIS FREQUENTE
Mode <- function(x){ ux <- sort(unique(x));ux[which.max(tabulate(match(x, ux)))]}
base_mesclada_3.1 = base_mesclada_3 %>%
  dplyr::mutate(facebook_profile = ifelse(is.na(facebook_profile),'Ausente',facebook_profile),
                marketing_channel = ifelse(is.na(marketing_channel),Mode(base_mesclada_3$marketing_channel),marketing_channel),
                n_bankruptcies = ifelse(is.na(n_bankruptcies),Mode(base_mesclada_3$n_bankruptcies),n_bankruptcies),
                n_defaulted_loans = ifelse(is.na(n_defaulted_loans),Mode(base_mesclada_3$n_defaulted_loans),n_defaulted_loans),
                external_data_provider_credit_checks_last_year = ifelse(is.na(external_data_provider_credit_checks_last_year),
                                                                        Mode(base_mesclada_3$external_data_provider_credit_checks_last_year),
                                                                        external_data_provider_credit_checks_last_year))

# CRIANDO VARIAVEIS
# BINS State
# Turno através da variavel hora_aplicação
df_state_bins = data_frame(state = unique(acquisition_train$state), bins_state = paste0("bins_",rep(seq(1,length(unique(acquisition_train$state)),5),5)[1:51]))
base_mesclada_4 = base_mesclada_3.1 %>% dplyr::left_join(df_state_bins,by=c("state_codificado"="state"))%>%
  dplyr::mutate(turno = dplyr::case_when(hora_aplicacao>=0 & hora_aplicacao< 6 ~ "Madrugada",
                                         hora_aplicacao>=6 & hora_aplicacao< 12 ~ "Manha",
                                         hora_aplicacao>=12 & hora_aplicacao< 18 ~ "Tarde",
                                         hora_aplicacao>=18 ~ "Noite")) %>%
  dplyr::mutate(Latitude = ifelse(is.na(Latitude),0,Latitude)) %>%
  dplyr::mutate(Longitude = ifelse(is.na(Longitude),0,Longitude)) %>%
  dplyr::mutate(last_amount_borrowed = ifelse(is.na(last_amount_borrowed),0,last_amount_borrowed),
                last_borrowed_in_months = ifelse(is.na(last_borrowed_in_months),0,last_borrowed_in_months),
                ok_since = ifelse(is.na(ok_since),0,ok_since))

# Criando Cluster para a variável longitude e latitude
df = base_mesclada_4 %>% dplyr::select(Latitude,Longitude)
set.seed(240) # Setting seed
kmeans.re <- kmeans(df, centers = 10, nstart = 20)
base_mesclada_4$Group_lat_lon = paste0("cluster_",kmeans.re$cluster)

save(x=base_mesclada_4,file="codigo-fonte/data/base_mesclada_4.Rdata")

# PRE PROCESSAMENTO  ------------------------------------------------------------------

# SELECAO VARIAVEIS

# Removendo variáveis duplicadas que surgiram nos joins
base_mesclada_5 = base_mesclada_4 %>%
  dplyr::rename(estado_loc = state.x) %>%
  dplyr::select(-c(name_state,state.y,cidade_atlas,uf.y,uf.x,cidade,codmun6,codmun7,municipio,município))

# Removendo variáveis que tiveram mais de 50% de valores Nulos
base_mesclada_6 = base_mesclada_5 %>%
  dplyr::select(-c(channel,external_data_provider_credit_checks_last_2_year))

# Variaveis que serão removidas por conta da codificacao ou ter classe demais
base_mesclada_7 = base_mesclada_6 %>%
  dplyr::select(-c(ids,score_1,score_2,reason,state_codificado,zip,job_name, profile_tags, user_agent,shipping_state,state_reverse))

# Outras removidas
base_mesclada_8 = base_mesclada_7 %>%
  dplyr::select(-c(external_data_provider_first_name,lat_lon, shipping_zip_code, target_fraud,
                   Latitude,Longitude,profile_phone_number))

# TRANSFORMANDO COLUNA TARGET EM 0 OU 1, E depois em fator
base_mesclada_9 = base_mesclada_8 %>%
  dplyr::mutate(target_default = ifelse(target_default=="True",1,0)) %>%
  dplyr::mutate(target_default = factor(target_default,levels = c(1,0),ordered = T))

# GARANTINDO QUE OS NOMES ESTEJAM EM MINUSCULO
base_preparada <- base_mesclada_9 %>% janitor::clean_names()
dim(base_preparada)

# DEFININDO SEMENTE
set.seed(123)
base_preparada$facebook_profile = factor(base_preparada$facebook_profile,ordered = T)
base_preparada$facebook_profile = ifelse(base_preparada$facebook_profile=="True",1,0)
b_oneHotEncoder = dataPreparation::one_hot_encoder(base_preparada,drop=T)
b_oneHotEncoder$target_default = factor(b_oneHotEncoder$target_default,levels = c(1,0),ordered = T)
dim(b_oneHotEncoder)

# DIVIDINDO A POPULACAO EM TREINO E TESTE
dt_split = rsample::initial_split(data = b_oneHotEncoder , prop = 0.75, strata = target_default)
# save(dt_split, file = "codigo-fonte/data/dt_split.Rdata")

# DATA PREPARATION
data_recipe = rsample::training(x = dt_split) %>%
  recipes::recipe(target_default ~ .) %>% # INFORMANDO QUAL O DEFAULT
  themis::step_downsample(target_default, under_ratio = 1) %>% 
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_novel(recipes::all_nominal_predictors(), -recipes::all_outcomes()) %>% #TRANSFORMANDO AS VARIAVEIS NOMINAIS EM FATOR
  recipes::step_center(recipes::all_numeric_predictors()) %>% # NORMALIZANDO OS DADOS NUMERICOS PARA TER UMA MEDIA DE ZERO
  recipes::step_scale(recipes::all_numeric_predictors()) %>% # NORMALIZANDO OS DADOS NUMERICOS PARA TER UM DESVIO PADRAO DE UM
  recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.85, method = "pearson") %>% #REMOVENDO VARIAVEIS NUMERICAS ALTAMENTE CORRELACIONADAS  
  recipes::step_zv(recipes::all_numeric_predictors(), -recipes::all_outcomes()) #REMOVENDO VARIAVEIS COM VARIABILIDADE PROXIMA DE ZERO

# AJUSTE DOS MODELOS --------------------------------------------------------------

# CROSS VALIDATION
cv = rsample::vfold_cv(rsample::training(x = dt_split), v = 6, repeats = 2, strata = target_default)

# RANDOM FOREST ========================
# ESPECIFICANDO O MODELO
rf_spec = parsnip::rand_forest() %>% 
  set_args(mtry = tune()) %>%
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("classification")
# CRIANDO WORKFLOW 
rf_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(rf_spec)
# GRID DE PARAMETROS
rf_grid = expand.grid(mtry = c(45,50,55))
# AJUSTE DO MODELO
rf_fit_tune = rf_workflow %>% 
  tune::tune_grid(resamples = cv, # CV object
                  grid = rf_grid, # grid of values to try
                  metrics = yardstick::metric_set(recall,f_meas,accuracy,kap,roc_auc,sens), # metrics we care about
                  control = tune::control_resamples(save_pred = TRUE,verbose=T)
  )
# SALVANDO O MODELO EM UM ARQUIVO .RDATA
# save(rf_fit_tune, file = "codigo-fonte/data/rf_fit_tune_predicao_1.Rdata")
# save(rf_workflow, file = "codigo-fonte/data/rf_workflow.Rdata")
# CARREGANDO O MODELO
# load(file = "codigo-fonte/data/rf_fit_tune_predicao_1.Rdata", verbose = T)
# load(file = "codigo-fonte/data/rf_workflow.Rdata", verbose = T)

# METRICAS NO TREINO
rf_fit_tune %>% collect_metrics()

# MOSTRANDO O MELHOR MODELO NO TREINO
rf_fit_tune %>% tune::show_best(metric = "recall")
rf_fit_tune %>% tune::show_best(metric = "accuracy")

# AVALIE O MODELO NO CONJUNTO DE TESTE --------------------------------------

# FINALIZANDO O FLUXO DE TRABALHO
param_final <- rf_fit_tune %>% tune::select_best(metric = "accuracy")
rf_workflow_best <- rf_workflow %>% finalize_workflow(param_final)

# Ajustar no conjunto de treinamento e avaliar no conjunto de teste
rf_fit <- rf_workflow_best %>% tune::last_fit(dt_split)

# COLENTANDO METRICAS MELHOR MODELO AVALIADO no TESTE
rf_fit %>% collect_metrics()

# GERANDO PREDICOES DO DATASET DE TREINO
train_predictions <- rf_fit %>% collect_predictions(); train_predictions

# CONFUSION MATRIX
graf_MC_rf = rf_fit %>% 
  collect_predictions() %>%
  conf_mat(truth = target_default, estimate = .pred_class, dnn = c("Predição", "Real")) %>%
  autoplot(type = "heatmap") +
  labs(title = "Matriz de confusão - Randon Forest");graf_MC_rf
ggsave(plot = graf_MC_rf,filename = "codigo-fonte/imagens/graf_MC_rf.png",width = 4,height =3)

# METRICAS
rf_metrics <- metric_set(accuracy, sens, spec)
train_predictions %>% rf_metrics(truth = target_default, estimate = .pred_class)

# CURVA ROC
rf_curva_ROC = rf_fit_tune %>%
  collect_predictions() %>%
  group_by(id) %>% # id contains our folds
  roc_curve(target_default, .pred_1) %>% 
  autoplot() +
  labs(title="Curva ROC - Randon Forest",
       x = "1 - Especificidade",
       y = "Sensibilidade",
       fill="Repetição")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw(); rf_curva_ROC
ggsave(rf_curva_ROC,filename = "codigo-fonte/imagens/rf_curva_ROC.png",width = 6,height = 4.2)

# OTIMIZANDO O CUTOFF RF --------------------------------------

# OTIMIZANDO O PONTO DE CORTE
rf_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target_default, .pred_1) %>% 
  dplyr::filter(specificity >= 0.5) %>% 
  dplyr::arrange(desc(sensitivity)) %>%
  dplyr::filter(dplyr::row_number() == 1)

# OTIMIZANDO O PONTO DE CORTE
set.seed(123)
rf_fit_preds = rf_fit %>% collect_predictions()
rf_fit_preds_new <- 
  rf_fit_preds %>%
  mutate(.pred_class = make_two_class_pred(.pred_1, levels(target_default), threshold = 0.416))

# OTIMIZANDO O PONTO DE CORTE
rf_metrics <- metric_set(accuracy, sens, spec)
rf_fit_preds_new %>% rf_metrics(truth = target_default, estimate = .pred_class)

# CONFUSION MATRIX nova
graf_MC_rf_otim = rf_fit_preds_new %>%
  conf_mat(truth = target_default, estimate = .pred_class, dnn = c("Predição", "Real")) %>%
  autoplot(type = "heatmap") +
  labs(title = "Matriz de confusão - Randon Forest \nOtimizado para sensibilidade"); graf_MC_rf_otim
ggsave(plot = graf_MC_rf_otim,filename = "codigo-fonte/imagens/graf_MC_rf_otim.png",width = 4,height =3)

# GRAFICO ANALISANDO A PREDICAO DO TREINO AJUSTADO
graf_dens_rf_train = rf_fit_preds_new %>%
  ggplot() +
  geom_density(aes(x = .pred_1, fill = target_default), alpha = 0.5) +
  labs(x = "Predição",y="Densidade",
       title = " Distribuição das predições para as classes 1 e 0 - Modelo Randon Forest",fill="Default") +
  scale_fill_viridis_d() +
  theme_bw(); graf_dens_rf_train

# SVM ========================
# ESPECIFICANDO O MODELO
SVM_spec = parsnip::svm_rbf(
    cost = tune(),
    rbf_sigma = tune()) %>% 
  parsnip::set_engine("kernlab", importance = "impurity") %>%
  parsnip::set_mode("classification")
# CRIANDO WORKFLOW 
SVM_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(SVM_spec)
# GRID DE PARAMETROS
SVM_grid = expand.grid(cost = c(0.1),rbf_sigma=c(0.01))
# AJUSTE DO MODELO
SVM_fit_tune = SVM_workflow %>% 
  tune::tune_grid(resamples = cv, # CV object
                  grid = SVM_grid, # grid of values to try
                  metrics = yardstick::metric_set(recall,f_meas,accuracy,kap,roc_auc,sens), # metrics we care about
                  control = tune::control_resamples(save_pred = TRUE,verbose=T)
  )

# SALVANDO O MODELO EM UM ARQUIVO .RDATA
# save(SVM_fit_tune, file = "codigo-fonte/data/SVM_fit_tune_predicao_1.Rdata")
# save(SVM_workflow, file = "codigo-fonte/data/SVM_workflow.Rdata")

# CARREGANDO O MODELO
# load(file = "codigo-fonte/data/SVM_fit_tune_predicao_1.Rdata", verbose = T)
# load(file = "codigo-fonte/data/SVM_workflow.Rdata", verbose = T)

# METRICAS NO TREINO
SVM_fit_tune %>% collect_metrics()

# MOSTRANDO O MELHOR MODELO NO TREINO
SVM_fit_tune %>% tune::show_best(metric = "recall")
SVM_fit_tune %>% tune::show_best(metric = "accuracy")

# AVALIE O MODELO NO CONJUNTO DE TESTE --------------------------------------

# FINALIZANDO O FLUXO DE TRABALHO
param_final <- SVM_fit_tune %>% tune::select_best(metric = "accuracy")
SVM_workflow_best <- SVM_workflow %>% finalize_workflow(param_final)

# Ajustar no conjunto de treinamento e avaliar no conjunto de teste
SVM_fit <- SVM_workflow_best %>% tune::last_fit(dt_split)

# COLENTANDO METRICAS MELHOR MODELO AVALIADO no TESTE
SVM_fit %>% collect_metrics()

# GERANDO PREDICOES DO DATASET DE TREINO
train_predictions <- SVM_fit %>% collect_predictions(); train_predictions

# CONFUSION MATRIX
graf_MC_svm = SVM_fit %>% 
  collect_predictions() %>%
  conf_mat(truth = target_default, estimate = .pred_class, dnn = c("Predição", "Real")) %>%
  autoplot(type = "heatmap") +
  labs(title = "Matriz de confusão - SVM "); graf_MC_svm
ggsave(plot = graf_MC_svm, filename = "codigo-fonte/imagens/graf_MC_svm.png", width = 4, height =3)

# METRICAS
SVM_metrics <- metric_set(accuracy, sens, spec)
train_predictions %>% SVM_metrics(truth = target_default, estimate = .pred_class)

# CURVA ROC
SVM_curva_ROC = SVM_fit_tune %>%
  collect_predictions() %>%
  group_by(id) %>% # id contains our folds
  roc_curve(target_default, .pred_1) %>% 
  autoplot() +
  labs(title="Curva ROC - SVM",
       x = "1 - Especificidade",
       y = "Sensibilidade")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw();SVM_curva_ROC
ggsave(SVM_curva_ROC,filename = "codigo-fonte/imagens/SVM_curva_ROC.png",width = 6,height = 4.2)


# OTIMIZANDO O CUTOFF SVM --------------------------------------

# OTIMIZANDO O PONTO DE CORTE PARA SENSIBILIDADE
SVM_fit %>%
  tune::collect_predictions() %>%
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target_default, .pred_1) %>% 
  dplyr::filter(specificity >= 0.5) %>% 
  dplyr::arrange(desc(sensitivity)) %>%
  dplyr::filter(dplyr::row_number() == 1)
# 0.413

# OTIMIZANDO O PONTO DE CORTE
set.seed(123)
SVM_fit_preds = SVM_fit %>% collect_predictions()
SVM_fit_preds_new <- 
  SVM_fit_preds %>%
  mutate(.pred_class = make_two_class_pred(.pred_1, levels(target_default), threshold = 0.413))

# OTIMIZANDO O PONTO DE CORTE
SVM_metrics <- metric_set(accuracy, sens, spec)
SVM_fit_preds_new %>% SVM_metrics(truth = target_default, estimate = .pred_class)

# CONFUSION MATRIX nova
graf_MC_svm_otim = SVM_fit_preds_new %>%
  conf_mat(truth = target_default, estimate = .pred_class, dnn = c("Predição", "Real")) %>%
  autoplot(type = "heatmap") +
  labs(title = "Matriz de confusão - SVM \nOtimizado para sensibilidade"); graf_MC_svm_otim
ggsave(plot = graf_MC_svm_otim,filename = "codigo-fonte/imagens/graf_MC_svm_otim.png",width = 4,height =3)

# GRAFICO ANALISANDO A PREDICAO DO TREINO AJUSTADO
graf_dens_SVM_train = SVM_fit_preds_new %>%
  ggplot() +
  geom_density(aes(x = .pred_1, fill = target_default), alpha = 0.5) +
  labs(x = "Predição",y="Densidade",
       title = " Distribuição das predições para as classes 1 e 0 - Modelo SVM",fill="Default") +
  scale_fill_viridis_d() +
  theme_bw(); graf_dens_SVM_train

# Xgboost ========================
# ESPECIFICANDO O MODELO
xgb_spec <- parsnip::boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                     
  sample_size = tune(), 
  mtry = tune(),        
  learn_rate = tune()
) %>%
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("classification")

# GRADE DE PARAMETROS
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(),  training(dt_split)),
  learn_rate(),
  size = 5
)

# CRIANDO WORKFLOW 
xgb_workflow = workflows::workflow() %>% 
  workflows::add_recipe(data_recipe) %>%
  workflows::add_model(XGB_spec)
# AJUSTE DO MODELO
xgb_fit_tune = xgb_workflow %>% 
  tune::tune_grid(resamples = cv, # CV object
                  grid = XGB_grid, # grid of values to try
                  metrics = yardstick::metric_set(recall,f_meas,accuracy,kap,roc_auc,sens),
                  control = tune::control_resamples(save_pred = TRUE,verbose=T)
  )

# SALVANDO O MODELO EM UM ARQUIVO .RDATA
# save(xgb_fit_tune, file = "codigo-fonte/data/xgb_fit_tune_predicao_1.Rdata")
# save(xgb_workflow, file = "codigo-fonte/data/xgb_workflow.Rdata")
# CARREGANDO O MODELO
# load(file = "codigo-fonte/data/xgb_fit_tune_predicao_1.Rdata", verbose = T)
# load(file = "codigo-fonte/data/xgb_workflow.Rdata", verbose = T)
# METRICAS NO TREINO
xgb_fit_tune  %>% collect_metrics()

# MOSTRANDO O MELHOR MODELO NO TREINO
xgb_fit_tune %>% tune::show_best(metric = "accuracy")

# CURVA ROC
xgb_curva_ROC = xgb_fit_tune %>%
  collect_predictions() %>%
  group_by(id) %>% # id contains our folds
  roc_curve(target_default, .pred_1) %>% 
  autoplot() +
  labs(title="Curva ROC - XGBoost",
       x = "1 - Especificidade",
       y = "Sensibilidade")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw(); xgb_curva_ROC
ggsave(plot = xgb_curva_ROC,filename = "codigo-fonte/imagens/xgb_curva_ROC.png",width = 6,height = 4.2)

# AVALIE O MODELO NO CONJUNTO DE TESTE --------------------------------------

# FINALIZANDO O FLUXO DE TRABALHO
param_final <- xgb_fit_tune %>% tune::select_best(metric = "accuracy")
xgb_workflow_best <- xgb_workflow %>% finalize_workflow(param_final)

# Ajustar no conjunto de treinamento e avaliar no conjunto de teste
xgb_fit <- xgb_workflow_best %>% tune::last_fit(dt_split)

# COLENTANDO METRICAS MELHOR MODELO AVALIADO no TESTE
xgb_fit %>% collect_metrics()

# GERANDO PREDICOES DO DATASET DE TREINO
train_predictions <- xgb_fit %>% collect_predictions(); train_predictions

# CONFUSION MATRIX
graf_MC_xgb = xgb_fit %>% 
  collect_predictions() %>%
  conf_mat(truth = target_default, estimate = .pred_class, dnn = c("Predição", "Real")) %>%
  autoplot(type = "heatmap") +
  labs(title = "Matriz de confusão - XGBoost"); graf_MC_xgb
ggsave(plot = graf_MC_xgb,filename = "codigo-fonte/imagens/graf_MC_xgb.png",width = 4,height =3)

# METRICAS
xgb_metrics <- metric_set(accuracy, sens, spec)
train_predictions %>% xgb_metrics(truth = target_default, estimate = .pred_class)
ggsave(plot = xgb_curva_ROC,filename = "codigo-fonte/imagens/xgb_curva_ROC.png",width = 6,height = 4.2)

# OTIMIZANDO O CUTOFF XGB --------------------------------------

# OTIMIZANDO O PONTO DE CORTE
a = xgb_fit %>%
  tune::collect_predictions() %>% 
  dplyr::group_by(id) %>%
  yardstick::roc_curve(target_default, .pred_1) %>% 
  dplyr::filter(specificity >= 0.5) %>% 
  dplyr::arrange(desc(sensitivity)) %>%
  dplyr::filter(dplyr::row_number() == 1)

# OTIMIZANDO O PONTO DE CORTE
set.seed(123)
xgb_fit_preds = xgb_fit %>% collect_predictions() 
xgb_fit_preds_new <- 
  xgb_fit_preds %>%
  mutate(.pred_class = make_two_class_pred(.pred_1, levels(target_default), threshold = 0.4999863))

# OTIMIZANDO O PONTO DE CORTE
xgb_metrics <- metric_set(accuracy, sens, spec)
xgb_fit_preds_new %>% xgb_metrics(truth = target_default, estimate = .pred_class)

# CONFUSION MATRIX nova
graf_MC_xgb_otim = xgb_fit_preds_new %>%
  conf_mat(truth = target_default, estimate = .pred_class, dnn = c("Predição", "Real")) %>%
  autoplot(type = "heatmap")+
  labs(title = "Matriz de confusão - XGBoost\n Otimizado para sensibilidade");graf_MC_xgb_otim
ggsave(plot = graf_MC_xgb_otim,filename = "codigo-fonte/imagens/graf_MC_xgb_otim.png",width = 4,height =3)

confusion_matrix_new = xgb_fit_preds_new %>% 
  yardstick::conf_mat(truth = target_default, estimate = .pred_class);confusion_matrix_new

True_Positive = confusion_matrix_new$table[1] # Eram inadimplentes e eu disse que era inadimplente ()
False_Positive = confusion_matrix_new$table[3] # Eram adimplentes e eu disse que era inadimplentes ()
False_Negativo = confusion_matrix_new$table[2] # Eram inadimplentes e eu disse que era adimplentes (Pior)
True_Negative = confusion_matrix_new$table[4] # Eram adimplentes e eu disse que era adimplentes ()


# GRAFICO ANALISANDO A PREDICAO DO TREINO AJUSTADO
graf_dens_xgb_train = xgb_fit_preds_new %>%
  ggplot() +
  geom_density(aes(x = .pred_1, fill = target_default), alpha = 0.5) +
  labs(x = "Predição",y="Densidade",
       title = " Distribuição das predições para as classes 1 e 0 - Modelo Xgboost",fill="Default") +
  scale_fill_viridis_d() +
  theme_bw(); graf_dens_xgb_train

# COMPARANDO MODELOS COM METRICAS DE TESTE ------------------------------------------------------
table_metrics <- metric_set(accuracy, sens, spec)

# MODEL METRICS TREINO
rf_metrics = rf_fit %>% 
  collect_predictions() %>% 
  table_metrics(truth = target_default, estimate = .pred_class) %>%
  dplyr::mutate(model = "Random Forest") 

svm_metrics = SVM_fit %>% 
  collect_predictions() %>% 
  table_metrics(truth = target_default, estimate = .pred_class) %>%
  dplyr::mutate(model = "SVM") 

xgb_metrics = xgb_fit %>% 
  collect_predictions() %>% 
  table_metrics(truth = target_default, estimate = .pred_class) %>%
  dplyr::mutate(model = "XGBoost")

model_compare = dplyr::bind_rows(
  rf_metrics,
  svm_metrics,
  xgb_metrics
) %>% dplyr::mutate(.metric = dplyr::case_when(.metric=="accuracy" ~ "Acurácia",
                                               .metric=="sens" ~ "Sensibilidade",
                                               .metric=="spec" ~ "Especificidade")) %>%
  dplyr::rename(Metricas =.metric,
                Modelos = model) %>%
  dplyr::mutate(label_name = paste0(format(round(.estimate*100,2),decimal.mark=","),"%"))

graf_comp_modelos = ggplot(data=model_compare, aes(x=Metricas, y=.estimate, group=Metricas)) +
  geom_bar(stat="identity",fill="#66C2A5")+
  geom_text(aes(label=label_name), vjust=-0.5, size=4)+
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Métricas dos modelos ajustados na base de treino",
       y= "Valor",
       x = "Métricas")+
  theme_bw()+
  facet_grid(. ~ Modelos); graf_comp_modelos
ggsave(plot = graf_comp_modelos,filename = "codigo-fonte/imagens/graf_comp_modelos.png",width = 12,height =5)


# MODEL METRICS TREINO OTIMIZADO
rf_metrics_otm = rf_fit %>% 
  collect_predictions() %>% 
  mutate(.pred_class = make_two_class_pred(.pred_1, levels(target_default), threshold = 0.416)) %>%
  table_metrics(truth = target_default, estimate = .pred_class) %>%
  dplyr::mutate(model = "Random Forest") 

svm_metrics_otm = SVM_fit %>% 
  collect_predictions() %>% 
  mutate(.pred_class = make_two_class_pred(.pred_1, levels(target_default), threshold = 0.413)) %>%
  table_metrics(truth = target_default, estimate = .pred_class) %>%
  dplyr::mutate(model = "SVM") 

xgb_metrics_otm = xgb_fit %>% 
  collect_predictions() %>% 
  mutate(.pred_class = make_two_class_pred(.pred_1, levels(target_default), threshold = 0.4999863)) %>%
  table_metrics(truth = target_default, estimate = .pred_class) %>%
  dplyr::mutate(model = "XGBoost")

model_compare_otm = dplyr::bind_rows(
  rf_metrics_otm,
  svm_metrics_otm,
  xgb_metrics_otm
) %>% dplyr::mutate(.metric = dplyr::case_when(.metric=="accuracy" ~ "Acurácia",
                                               .metric=="sens" ~ "Sensibilidade",
                                               .metric=="spec" ~ "Especificidade")) %>%
  dplyr::rename(Metricas =.metric,
                Modelos = model) %>%
  dplyr::mutate(label_name = paste0(format(round(.estimate*100,2),decimal.mark=","),"%"))

graf_comp_modelos_otim = ggplot(data=model_compare_otm, aes(x=Metricas, y=.estimate, group=Metricas)) +
  geom_bar(stat="identity",fill="#66C2A5")+
  geom_text(aes(label=label_name), vjust=-0.5, size=4)+
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Métricas dos modelos ajustados na base de treino - OTIMIZADOS PARA SENSIBILIDADE",
       y= "Valor",
       x = "Métricas")+
  theme_bw()+
  facet_grid(. ~ Modelos);graf_comp_modelos_otim
ggsave(plot = graf_comp_modelos_otim,filename = "codigo-fonte/imagens/graf_comp_modelos_otim.png",width = 12,height =5)

# ======= Variáveis mais importantes
library(vip)
# Finalizando o modelo
wf_rf_final <- rf_workflow %>% finalize_workflow(select_best(rf_fit_tune, "accuracy"))
# treinando o modelo com a base de treino
modelo_final <- fit(wf_rf_final, rsample::training(x = dt_split))
graf_var_importantes = vip(modelo_final$fit$fit) + 
  aes(fill = "#66C2A5") +
  labs(title = "Variáveis mais importantes",
       x= "Variáveis",
       y = "Importância")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = "none");graf_var_importantes
ggsave(plot = graf_var_importantes,filename = "codigo-fonte/imagens/graf_var_importantes.png",width = 7,height =5)


