#devtools::install_github("rpradosiqueira/brazilmaps")
# https://anderlerv.netlify.app/mapas-com-brazilmaps/01/2019/
library(brazilmaps)
library(ggplot2)
library(sf)
library(tidyverse)
library(sp)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(tidyr)

load(file="codigo-fonte/data/base_mesclada_4.Rdata",verbose=T)
# ANALISE DESCRITIVA

# DIMENSAO DA BASE ANALISADA  ===================================================================
dim(base_mesclada_4)

# Variáveis Descritiva  ===================================================================
library(psych)
psych::describe(base_mesclada_4)

# ESTUDANDO A VARIAVEL DEFAULT   ===================================================================
base_default = base_mesclada_4 %>%
  dplyr::add_tally() %>%
  dplyr::group_by(target_default) %>%
  dplyr::summarise(Quantidade = dplyr::n(),
                   percentual = dplyr::n()/nrow(base_mesclada_4)) %>%
  dplyr::mutate(label_name = paste0(format(Quantidade,big.mark=".")," (",format(round(percentual*100,2), decimal.mark = ','), "%)"))

graf_gg_default <- 
  ggplot(data = base_default,
         mapping = aes(x = target_default, y = Quantidade, fill = target_default)) +
  geom_bar(stat = "identity") +
  labs(title = "Quantidade de registros na variável Default",
       x= "Default",y="Quantidade",
       fill="Default") +
  ylim(0,38000)+
  geom_text(
    mapping = aes(label = label_name, y = Quantidade),
    position = position_dodge(0.9),
    vjust = -1,size = 4) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(); graf_gg_default

ggsave(plot = graf_gg_default,filename = "codigo-fonte/imagens/graf_gg_default.png",width = 7,height = 5)

# Facebook   ===================================================================
base_face = base_mesclada_4 %>%
  dplyr::group_by(target_default) %>%
  dplyr::add_tally() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(target_default,facebook_profile) %>%
  dplyr::summarise(Quantidade = dplyr::n(),
                   percentual = dplyr::n()/n) %>%
  dplyr::mutate(label_name = paste0(format(round(percentual*100,2), decimal.mark = ','),"%\n(",format(Quantidade,big.mark="."),")"))

graf_gg_face <- 
  ggplot(data = base_face,
         mapping = aes(x = target_default, y = percentual, group=facebook_profile)) +
  geom_bar(aes(fill = facebook_profile), stat = "identity", position=position_dodge()) +
  labs(title = "Percentual de registros do default na variável tem conta no Facebook",
       x= "Inadimplência",y="Percentual",
       fill="Tem conta no Facebook") +
  ylim(0,0.8)+
  geom_text(
    mapping = aes(label = label_name, y = percentual),
    position = position_dodge(0.9),
    vjust = -0.5,size = 4) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(); graf_gg_face

ggsave(plot = graf_gg_face,filename = "codigo-fonte/imagens/graf_gg_face.png",width = 7,height = 5)

# email   ===================================================================
base_email = base_mesclada_4 %>%
  dplyr::group_by(target_default) %>%
  dplyr::add_tally() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(target_default,email) %>%
  dplyr::summarise(Quantidade = dplyr::n(),
                   percentual = dplyr::n()/n) %>%
  dplyr::mutate(label_name = paste0(format(round(percentual*100,2), decimal.mark = ','),"%\n(",format(Quantidade,big.mark="."),")"))

graf_gg_email <- 
  ggplot(data = base_email,
         mapping = aes(x = target_default, y = percentual, group=email)) +
  geom_bar(aes(fill = email), stat = "identity", position=position_dodge()) +
  labs(title = "Percentual de registros do default na variável Email",
       x= "Inadimplência",y="Percentual",
       fill="Email") +
  ylim(0,0.6)+
  geom_text(
    mapping = aes(label = label_name, y = percentual),
    position = position_dodge(0.9),
    vjust = -0.5,size = 3) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(); graf_gg_email

ggsave(plot = graf_gg_email,filename = "codigo-fonte/imagens/graf_gg_email.png",width = 7,height = 5)

# BoxPlot score 3, 4, 5 e 6 (Numéricos)   ===================================================================
base_3 = base_mesclada_4 %>%
  dplyr::select(target_default,score_3,score_4,score_5,score_6) %>% 
  pivot_longer(
    cols = score_3:score_6, # as colunas desse intervalo
    names_to = "score", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "score_", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "valor") # e os seus valores armazenados nessa nova coluna

bp3 <- ggplot(base_3 %>% filter(score == "3"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="Score 3",x="Default","Quantidade")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp3

bp4 <- ggplot(base_3 %>% filter(score == "4"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default)) +
  labs(title="Score 4",x="Default","Quantidade")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp4

bp5 <- ggplot(base_3 %>% filter(score == "5"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="Score 5",x="Default","Quantidade")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp5

bp6 <- ggplot(base_3 %>% filter(score == "6"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="Score 6",x="Default","Quantidade")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp6

bp_scores = grid.arrange(bp3,bp4,bp5,bp6,ncol=4)
ggsave(plot = bp_scores,filename = "codigo-fonte/imagens/bp_scores.png",width = 12,height = 4)

# Região   ===================================================================

base_regiao = base_mesclada_4 %>%
  dplyr::mutate(regiao = ifelse(regiao=="","Ausente",regiao)) %>%
  dplyr::group_by(target_default) %>%
  dplyr::add_tally() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(target_default,regiao) %>%
  dplyr::summarise(Quantidade = dplyr::n(),
                   percentual = dplyr::n()/n) %>%
  dplyr::mutate(label_name = paste0(format(round(percentual*100,2), decimal.mark = ','),"%\n(",format(Quantidade,big.mark="."),")"))

graf_gg_regiao <- 
  ggplot(data = base_regiao,
         mapping = aes(x = target_default, y = percentual, group=regiao)) +
  geom_bar(aes(fill = regiao), stat = "identity", position=position_dodge()) +
  labs(title = "Percentual de registros do default na variável Região",
       x= "Inadimplência",y="Percentual",
       fill="Região") +
  ylim(0,0.4)+
  geom_text(
    mapping = aes(label = label_name, y = percentual),
    position = position_dodge(0.9),
    vjust = -0.5,size = 3) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(); graf_gg_regiao

# Mapa estados  ===================================================================
BR <- get_brmap("State")

base_estados = base_mesclada_4 %>%
  dplyr::filter(!is.na(name_state))%>%
  dplyr::ungroup() %>%
  dplyr::group_by(uf.x,name_state) %>%
  dplyr::add_tally() %>%
  dplyr::summarise(Quantidade = dplyr::n(),
                   quantidade_inadimplente = sum(target_default=="True")) %>%
  dplyr::mutate(Percentual_Inadimplentes = quantidade_inadimplente/Quantidade)
base_estadosDF = base_estados %>% dplyr::filter(uf.x==52) %>%
  dplyr::mutate(uf.x = 53)
base_estados_ = rbind(base_estados,base_estadosDF)

graf_map_estados <- plot_brmap(BR, data_to_join = base_estados_, 
                    join_by = c("State" = "uf.x"), 
                    var = "Percentual_Inadimplentes");graf_map_estados
ggsave(plot = graf_map_estados,filename = "codigo-fonte/imagens/graf_map_estados.png",width = 7,height = 5)

# salvar estados que se destacaram
base_destacados = base_estados_ %>%
  dplyr::filter(name_state %in% c("Rio Grande do Norte","Rondônia"));base_destacados 
write.table(base_destacados,file = "codigo-fonte/data/estados_destacados.csv",sep=";",dec=",",row.names = F)

# Analise inadimplencia dos municipios (se sobrar tempo faz mapa do municipio)
base_municipios = base_mesclada_4 %>%
  dplyr::ungroup() %>%
  dplyr::group_by(municipio,name_state) %>%
  dplyr::add_tally() %>%
  dplyr::summarise(Quantidade = dplyr::n(),
                   quantidade_inadimplente = sum(target_default=="True")) %>%
  dplyr::mutate(Percentual_Inadimplentes = quantidade_inadimplente/Quantidade)

# salvar estados que se destacaram
write.table(base_municipios,file = "codigo-fonte/data/base_municipios.csv",sep=";",dec=",",row.names = F)

# Variavel reported_income e reported_income =======================================================
# Renda Informada pelo Usuario
base_mesclada_4$reported_income
# Renda calculada pelos provedores
base_mesclada_4$income

corrigir_outlier = function(variavel,prob_quantil){
  valor_quantil = quantile(variavel,probs = .99 ,na.rm=T)
  var_new = ifelse(variavel>valor_quantil,valor_quantil,variavel)
}
base_mesclada_4$reported_income = corrigir_outlier(base_mesclada_4$reported_income,.99)
base_mesclada_4$income = corrigir_outlier(base_mesclada_4$income,.99)

# base para BoxPlot 
base_renda_informada = base_mesclada_4 %>%
  dplyr::select(target_default,reported_income,income) %>% 
  pivot_longer(
    cols = reported_income:income, # as colunas desse intervalo
    names_to = "variavel", # terão seus nomes armazenados nessa nova coluna
    values_to = "valor") # e os seus valores armazenados nessa nova coluna

bp_reported_income <- ggplot(base_renda_informada %>% filter(variavel == "reported_income"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="Renda Anual Informada pelo Solicitante",x="Default","Valor em Reais")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp_reported_income

bp_income <- ggplot(base_renda_informada %>% filter(variavel == "income"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="Renda Anual calculada pelos provedores",x="Default","Valor em Reais")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp_income

bp_renda_informada = grid.arrange(bp_reported_income,bp_income,ncol=2)
ggsave(plot = bp_renda_informada,filename = "codigo-fonte/imagens/bp_renda_informada.png",width = 8,height = 4)

# Analisar com boxPlot amostra de variaveis de enrriquecimento =======================================================
# Renda percapita
base_mesclada_4$rdpc
# Rendimento médio dos ocupados - 18 anos ou mais
base_mesclada_4$renocup
# % de extremamente pobres
base_mesclada_4$pind

# base para BoxPlot 
base_amostra_enr = base_mesclada_4 %>%
  dplyr::select(target_default,rdpc,renocup,pind) %>% 
  pivot_longer(
    cols = rdpc:pind, # as colunas desse intervalo
    names_to = "variavel", # terão seus nomes armazenados nessa nova coluna
    #names_prefix = "score_", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "valor") # e os seus valores armazenados nessa nova coluna

bp_rdpc <- ggplot(base_amostra_enr %>% filter(variavel == "rdpc"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="Renda percapita",x="Default","Valor em Reais")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp_rdpc

bp_renocup <- ggplot(base_amostra_enr %>% filter(variavel == "renocup"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="Rendimento médio dos ocupados - 18+",x="Default","Valor em Reais")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp_renocup

bp_pind <- ggplot(base_amostra_enr %>% filter(variavel == "pind"), aes(x=target_default, y=valor, group=target_default)) + 
  geom_boxplot(aes(fill=target_default))+
  labs(title="% de extremamente pobres",x="Default","Percentual")+
  scale_fill_brewer(palette = "Set2") +
  theme_bw()+theme(legend.position = "none"); bp_pind

bp_amostra_enr = grid.arrange(bp_rdpc,bp_renocup,bp_pind,ncol=3)
ggsave(plot = bp_amostra_enr,filename = "codigo-fonte/imagens/bp_amostra_enr.png",width = 12,height = 4)

# Correlacao entre as variaveis numéricas e o default =======================================================

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

colunas = df_colunas(as.data.frame(base_mesclada_4))
colunas_numeric = colunas %>% filter(Tipo=="numeric" |Tipo=="integer")
v_colunas_int = colunas_numeric$coluna
vetor_cor = NULL
default_int = ifelse(base_mesclada_4$target_default=="True",1,0)
for(i in 1:length(v_colunas_int)){
  vetor_cor[i] = cor(default_int,base_mesclada_4[v_colunas_int[i]])
}
sort(vetor_cor)
