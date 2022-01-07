##################################################################################
#                              INSTALL AND LOAD LIBRARY                          #
##################################################################################
#Used library
pacotes <- c("plotly","tidyverse","readxl","stringr","tidytext","tm","tidyr","textmineR","Rcpp","factoextra","ggdendro")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

##############################################################################
#                          LOADING THE DATABASE INTO R                       #
##############################################################################
#Using the Library -readxl- to load the database file exported from SAP GUI.

# Loading the xlsx file database
bd_bruto <- read_excel("Encerradas.xlsx")
#summary(bd_bruto)
#str(bd_bruto)

#Starting Data Wrangling
bd_CDs <- bd_bruto %>% 
          #renaming columns
          rename(
                    nota_cd = 1,
                    ecode = 2,
                    desc_material= 3,
                    cod_cta=4,
                    cta=5,
                    cod_cto=6,
                    cto=7,
                    prog=8,
                    acft=9,
                    modo_falha=10,
                    titulo_cd=11,
                    pn=12,
                    emitente=13,
                    tipo_doc_ref=14,
                    num_doc_ref=15,
                    aplic=16,
                    op=17,
                    ata=18,
                    sub_ata=19,
                    secao_ata=20,
                    prog2=21,
                    data_emissao=22,
                    data_encerramento=23,
                    qtd_defeito=24,
                    cnq=25,
                    hh=26
                 ) %>% 
                #leaving only the columns that matters for our analysis.
                 select(nota_cd,titulo_cd,everything(),-prog2,-sub_ata,-secao_ata,-emitente) %>% 
                #correcting some names from CTA columns.
                  mutate( 
                         cta=replace(cta,cta=="SJK010-Pre Equipagem Asa 170/190 E2","SJK010 - Equip. de Asa"),
                         cta=replace(cta,cta=="SJK010-Pre Equipagem Asa 170/190 E1","SJK010 - Equip. de Asa"),
                         cta=replace(cta,cta=="SJK031-Junção Asa-Fuselagem 170/190 E2","SJK031 - Junção de Asa"),
                         cta=replace(cta,cta=="SJK031-Junção Asa-Fuselagem 170/190 E1","SJK031 - Junção de Asa"),
                         cta=replace(cta,cta=="SJK072-Equipagem 170/190 E2","SJK072 - Equip. Fuselagem"),
                         cta=replace(cta,cta=="SJK072-Equipagem 170/190 E1","SJK072 - Equip. Fuselagem"),
                         cta=replace(cta,cta=="SJK271-Preparação Mont. Fuselagens E2","SJK271 - Cel. Preparacao"),
                         cta=replace(cta,cta=="SJK271-Preparação Mont. Fuselagens E1","SJK271 - Cel. Preparacao")
                        ) %>% 
                #creating a new column with PN using the package -stringr-
                mutate(pn_aux=str_sub(pn,1,-4)) %>%
                #filling the NA num_doc_ref with 0's
                mutate(num_doc_ref = replace_na(num_doc_ref, 0)) %>% 
                #Adding a new merge column to get some text process in the next step.
                unite(cdfulltext,desc_material,modo_falha,titulo_cd,num_doc_ref,pn_aux,sep = " ",remove=FALSE)


#correcting some data convertion (Dates)
bd_CDs$data_emissao<-as.Date(bd_CDs$data_emissao)
bd_CDs$data_encerramento<-as.Date(bd_CDs$data_encerramento)

