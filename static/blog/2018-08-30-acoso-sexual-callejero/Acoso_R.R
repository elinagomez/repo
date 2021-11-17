
##cargo librerías


##seteo espacio de trabajo

setwd("C:/Users/Elina/Desktop/RLadies")

##cargo bases de los grupos que tengo guardadas en CSV

# ma=read.csv("CSV/MA.csv",sep=";",header = F)
# mj_1=read.csv("CSV/MJ_1.csv",sep=";",header = T)
# mj_2=read.csv("CSV/MJ_2.csv",sep=";",header = T)
# mj_3=read.csv("CSV/MJ_3.csv",sep=";",header = F)
# vj=read.csv("CSV/VJ.csv",sep=";",header = F)
# va=read.csv("CSV/VA.csv",sep=";",header = F)
# 
# ##saco las menciones que refieren a lxs moderadorxs
# ma$per=substr(ma$V1,1,3)
# ma=subset(ma,ma$per!="M: ")
# ma=toString(ma[,1])
# 
# mj_1$per=substr(mj_1$Citas,1,3)
# mj_1=subset(mj_1,mj_1$per!="E: ")
# mj_1=toString(mj_1[,1])
# 
# mj_2$per=substr(mj_2$Citas,1,3)
# mj_2=subset(mj_2,mj_2$per!="E: ")
# mj_2=toString(mj_2[,1])
# 
# mj_3$per=substr(mj_3$V1,1,3)
# mj_3=subset(mj_3,mj_3$per!="M: ")
# mj_3=toString(mj_3[,1])
# 
# va$per=substr(va$V1,1,3)
# va=subset(va,va$per!="Mod")
# va=subset(va,va$per!="M: ")
# va=toString(va[,1])
# 
# vj$per=substr(vj$V1,1,3)
# vj=subset(vj,vj$per!="A: ")
# vj=toString(vj[,1])
# 
# 
# ##los paso a txt
# writeLines(ma, "grupos_acoso/Mujeres_Adultos_1.txt")
# writeLines(mj_1, "grupos_acoso/Mujeres_Jovenes_1.txt")
# writeLines(mj_2, "grupos_acoso/Mujeres_Jovenes_2.txt")
# writeLines(mj_3, "grupos_acoso/Mujeres_Jovenes_3.txt")
# writeLines(va, "grupos_acoso/Varones_Adultos_1.txt")
# writeLines(vj, "grupos_acoso/Varones_Jovenes_1.txt")

## desde acá
library(devtools)
require(quanteda)
library(readtext)
library(stringr)
library(ggplot2)
library(syuzhet)
library(gridExtra)
library(dplyr)
library(tm)

mytf<-readtext("grupos_acoso/*", docvarsfrom ="filenames")

myCorpus<-corpus(mytf,text_field="text")


##abro un archivo con stopwords
stop=read.csv("stopes.csv",sep=";")
vector=as.character(stop$X0)

mydfm <- dfm(myCorpus,
             stem = FALSE,
             tolower = TRUE,
             remove = c(stopwords("spanish"),vector), 
             remove_punct = TRUE, 
             remove_numbers = TRUE, 
             verbose = TRUE)

#topfeatures(mydfm) #most frequent 'tokens'

words<-featnames(mydfm)
#Saco palabras con 1 y 2 caracteres
words_Length_1<-words[sapply(words,str_length)==1]
words_Length_2<-words[sapply(words,str_length)==2]
otherWords<-c("risas",words_Length_1,words_Length_2) #you can expand this list
mydfm<-dfm(mydfm,remove=otherWords)

#Ver palabras mas frecuentes
#topfeatures(mydfm)

#ver si hacerlo
#mydfm<-dfm_trim(mydfm, min_termfreq  = 2, min_docfreq = 2, 
#                max_termfreq = Inf, max_docfreq = 44, verbose = TRUE)

#pdf("wcgeneral.pdf")
pdf("wcgeneral.pdf",width=15,height=15)
# Nube de palabras: general
textplot_wordcloud(mydfm, min.count = 3,max_words = 200,random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

#cambio los nombres de los grupos
rownames(mydfm)=c("Mujeres adultas","Mujeres jóvenes","Mujeres jóvenes","Mujeres jóvenes","Varones adultos","Varones jovenes")
# Nube de palabras: comparado
textplot_wordcloud(mydfm, min.count = 3,max_words = 500,random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),comparison = T)


# Nube de palabras: mujeres/varones
dfm_sexo <- dfm(mydfm, groups = "docvar1")
textplot_wordcloud(dfm_sexo, min.count = 2,max_words = 200,random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),comparison = T)

# Nube de palabras: adultos/jovenes
dfm_edad <- dfm(mydfm, groups = "docvar2")
textplot_wordcloud(dfm_edad, min.count = 2,max_words = 200,random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),comparison = T)


##Palabras más frecuentes! en general y en cada grupo

  topacoso=data.frame(topfeatures(mydfm,20))
  topacoso$palabra=rownames(topacoso)
  acosopplot=topacoso[1:20, ] %>%
    ggplot( aes(x = reorder(palabra,topfeatures.mydfm..20.), y = topfeatures.mydfm..20., fill = palabra)) + 
    geom_col(show.legend = FALSE) +
    coord_flip() +
    geom_text(aes(hjust = -0.1, label = topfeatures.mydfm..20.)) +
    theme_minimal() +
    theme(axis.title.y =element_blank() , axis.title.x =element_blank()) +
    ggtitle("Palabras más frecuentes (n=15)")
  

mydfm_mujer <- corpus_subset(myCorpus, docvar1 == "Mujeres")  
mydfm_mujer <- dfm(mydfm_mujer,
             stem = FALSE,
             tolower = TRUE,
             remove = c(stopwords("spanish"),vector), 
             remove_punct = TRUE, 
             remove_numbers = TRUE, 
             verbose = TRUE)
words<-featnames(mydfm_mujer)
#Saco palabras con 1 y 2 caracteres
words_Length_1<-words[sapply(words,str_length)==1]
words_Length_2<-words[sapply(words,str_length)==2]
otherWords<-c("risas",words_Length_1,words_Length_2) #you can expand this list
mydfm_mujer<-dfm(mydfm_mujer,remove=otherWords)

topacoso_mujer=data.frame(topfeatures(mydfm_mujer,20))
topacoso_mujer$palabra=rownames(topacoso_mujer)
acosopplot_mujer=topacoso_mujer[1:20, ] %>%
  ggplot( aes(x = reorder(palabra,topfeatures.mydfm_mujer..20.), y = topfeatures.mydfm_mujer..20., fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = topfeatures.mydfm_mujer..20.)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank()) +
  ggtitle("Palabras más frecuentes: Mujeres (n=20)")

mydfm_varon <- corpus_subset(myCorpus, docvar1 == "Varones")  
mydfm_varon <- dfm(mydfm_varon,
                   stem = FALSE,
                   tolower = TRUE,
                   remove = c(stopwords("spanish"),vector), 
                   remove_punct = TRUE, 
                   remove_numbers = TRUE, 
                   verbose = TRUE)
words<-featnames(mydfm_varon)
#Saco palabras con 1 y 2 caracteres
words_Length_1<-words[sapply(words,str_length)==1]
words_Length_2<-words[sapply(words,str_length)==2]
otherWords<-c("risas",words_Length_1,words_Length_2) #you can expand this list

mydfm_varon<-dfm(mydfm_varon,remove=otherWords)

topacoso_varon=data.frame(topfeatures(mydfm_varon,20))
topacoso_varon$palabra=rownames(topacoso_varon)

acosopplot_varon=topacoso_varon[1:20, ] %>%
  ggplot( aes(x = reorder(palabra,topfeatures.mydfm_varon..20.), y = topfeatures.mydfm_varon..20., fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = topfeatures.mydfm_varon..20.)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank()) +
  ggtitle("Palabras más frecuentes: Varones (n=20)")

grid.arrange(acosopplot_mujer, acosopplot_varon, ncol = 2, heights = c(5, 0.30))


#Text similarities
#doc_simil <- textstat_simil(mydfm, 
#                            margin = "documents", method = "cosine")

#mydfm_nuevo<-dfm_trim(mydfm, min_termfreq  = 8, min_docfreq = 2, 
#                max_termfreq = Inf, max_docfreq = Inf, verbose = TRUE)

sim <- textstat_simil(mydfm_nuevo, c("calle"), method = "cosine", margin = "features")
lapply(as.list(sim), head, 20)
#sim <- textstat_simil(mydfm_mujer, c("calle"), method = "cosine", margin = "features")
#lapply(as.list(sim), head, 20)
#sim <- textstat_simil(mydfm_varon, c("calle"), method = "cosine", margin = "features")
#lapply(as.list(sim), head, 20)

##Clusters por menciones

#mujeres
mujeres=read.csv("CSV/men_mujeres.csv",sep=";",header = T)
corpus_men_mujeres <- Corpus(VectorSource(mujeres$Texto),readerControl = list(encoding = "UTF-8",language="es"))
corpus_men_mujeres <- tm_map(corpus_men_mujeres, content_transformer(tolower))
corpus_men_mujeres <- tm_map(corpus_men_mujeres, removeNumbers)
corpus_men_mujeres <- tm_map(corpus_men_mujeres, removePunctuation)
corpus_men_mujeres <- tm_map(corpus_men_mujeres, removeWords, vector)
corpus_men_mujeres_tdm=TermDocumentMatrix(corpus_men_mujeres, control = list(encoding = "UTF-8",reader=readPlain,language="es"))
corpus_men_mujeres_mat <- as.matrix(corpus_men_mujeres_tdm)
dim(corpus_men_mujeres_tdm)
corpus_men_mujeres_mat <- corpus_men_mujeres_mat %>% rowSums() %>% sort(decreasing = TRUE)
corpus_men_mujeres_mat <- data.frame(palabra = names(corpus_men_mujeres_mat), frec = corpus_men_mujeres_mat)
corpus_men_mujeres_tdm_nuevo <- removeSparseTerms(corpus_men_mujeres_tdm, sparse = .985)
nrow(corpus_men_mujeres_tdm)
nrow(corpus_men_mujeres_tdm_nuevo)
corpus_men_mujeres_tdm_nuevo <- corpus_men_mujeres_tdm_nuevo %>% as.matrix()
corpus_men_mujeres_tdm_nuevo <- corpus_men_mujeres_tdm_nuevo / rowSums(corpus_men_mujeres_tdm_nuevo)
corpus_men_mujeres_tdm_nuevo_dist <- dist(corpus_men_mujeres_tdm_nuevo, method = "euclidian")
corpus_men_mujeres_tdm_nuevo_hclust <-  hclust(corpus_men_mujeres_tdm_nuevo_dist, method = "ward.D")
plot(corpus_men_mujeres_tdm_nuevo_hclust, main = "Dendrograma - Mujeres", sub = "", xlab = "")


#varones
varones=read.csv("CSV/men_varones.csv",sep=";",header = T)
corpus_men_varones <- Corpus(VectorSource(varones$Texto),readerControl = list(encoding = "UTF-8",language="es"))
corpus_men_varones <- tm_map(corpus_men_varones, content_transformer(tolower))
corpus_men_varones <- tm_map(corpus_men_varones, removeNumbers)
corpus_men_varones <- tm_map(corpus_men_varones, removePunctuation)
corpus_men_varones <- tm_map(corpus_men_varones, removeWords, vector)
corpus_men_varones_tdm=TermDocumentMatrix(corpus_men_varones, control = list(encoding = "UTF-8",reader=readPlain,language="es"))
corpus_men_varones_mat <- as.matrix(corpus_men_varones_tdm)
dim(corpus_men_varones_tdm)
corpus_men_varones_mat <- corpus_men_varones_mat %>% rowSums() %>% sort(decreasing = TRUE)
corpus_men_varones_mat <- data.frame(palabra = names(corpus_men_varones_mat), frec = corpus_men_varones_mat)
corpus_men_varones_tdm_nuevo <- removeSparseTerms(corpus_men_varones_tdm, sparse = .97)
nrow(corpus_men_varones_tdm)
nrow(corpus_men_varones_tdm_nuevo)
corpus_men_varones_tdm_nuevo <- corpus_men_varones_tdm_nuevo %>% as.matrix()
corpus_men_varones_tdm_nuevo <- corpus_men_varones_tdm_nuevo / rowSums(corpus_men_varones_tdm_nuevo)
corpus_men_varones_tdm_nuevo_dist <- dist(corpus_men_varones_tdm_nuevo, method = "euclidian")
corpus_men_varones_tdm_nuevo_hclust <-  hclust(corpus_men_varones_tdm_nuevo_dist, method = "ward.D")
plot(corpus_men_varones_tdm_nuevo_hclust, main = "Dendrograma - Varones", sub = "", xlab = "")


#seleccionar algunas: hacer gráficos!
colores=c(rep("#D7B5D8",5))
corpus_men_mujeres_tdm_2 <- removeSparseTerms(corpus_men_mujeres_tdm, sparse = .995)
corpus_men_varones_tdm_2 <- removeSparseTerms(corpus_men_varones_tdm, sparse = .995)

#$calle
calle_mujer=as.data.frame(findAssocs(corpus_men_mujeres_tdm_2, terms = c("calle"), corlimit = .05))
calle_mujer$palabra=rownames(calle_mujer)
plotcalle_mujer=calle_mujer[c(3,6,8,9,10), ] %>%
  ggplot( aes(x = reorder(palabra, calle), y = calle, fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = calle)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank()) +
  ggtitle("Asociación con $calle en mujeres (n=5,liminf=0.05)")

calle_varon=as.data.frame(findAssocs(corpus_men_varones_tdm_2, terms = c("calle"), corlimit = .05))
calle_varon$palabra=rownames(calle_varon)
plotcalle_varon=calle_varon[c(1,7,9,11,13), ] %>%
  ggplot( aes(x = reorder(palabra, calle), y = calle, fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = calle)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank()) +
  ggtitle("Asociación con $calle en varones (n=5,liminf=0.05)")

grid.arrange(plotcalle_mujer, plotcalle_varon, ncol = 2, heights = c(1, 0))

#$acoso
acoso_mujer=as.data.frame(findAssocs(corpus_men_mujeres_tdm_2, terms = c("acoso"), corlimit = .05))
acoso_mujer$palabra=rownames(acoso_mujer)
plotacoso_mujer=acoso_mujer[c(1,5,8,13,14), ] %>%
  ggplot( aes(x = reorder(palabra, acoso), y = acoso, fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = acoso)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank(),axis.text = element_text(size = 15)) +
  ggtitle("Asociación con $acoso en mujeres (n=5,liminf=0.05)")+
  scale_fill_manual(values=colores)

acoso_varon=as.data.frame(findAssocs(corpus_men_varones_tdm_2, terms = c("acoso"), corlimit = .05))
acoso_varon$palabra=rownames(acoso_varon)
plotacoso_varon=acoso_varon[c(1,7,9,10,15), ] %>%
  ggplot( aes(x = reorder(palabra, acoso), y = acoso, fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = acoso)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank(),axis.text = element_text(size = 15)) +
  ggtitle("Asociación con $acoso en varones (n=5,liminf=0.05)")+
  scale_fill_manual(values=colores)

pdf("acoso.pdf",width=10,height=5)
grid.arrange(plotacoso_mujer, plotacoso_varon, ncol = 2, heights = c(1, 0))
dev.off()
#$mujer
mujer_mujer=as.data.frame(findAssocs(corpus_men_mujeres_tdm_2, terms = c("mujer"), corlimit = .05))
mujer_mujer$palabra=rownames(mujer_mujer)
plotmujer_mujer=mujer_mujer[c(2,3,8,14,15), ] %>%
  ggplot( aes(x = reorder(palabra, mujer), y = mujer, fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = mujer)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank()) +
  ggtitle("Asociación con $mujer en mujeres (n=5,liminf=0.05)")

mujer_varon=as.data.frame(findAssocs(corpus_men_varones_tdm_2, terms = c("mujer"), corlimit = .05))
mujer_varon$palabra=rownames(mujer_varon)
plotmujer_varon=mujer_varon[c(3,5,7,9,1), ] %>%
  ggplot( aes(x = reorder(palabra, mujer), y = mujer, fill = palabra)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(hjust = -0.1, label = mujer)) +
  theme_minimal() +
  theme(axis.title.y =element_blank() , axis.title.x =element_blank()) +
  ggtitle("Asociación con $mujer en varones (n=5,liminf=0.05)")

grid.arrange(plotmujer_mujer, plotmujer_varon, ncol = 2, heights = c(1, 0))



##análisis emociones

lexicon <- readRDS("EmoPosNeg_SPA.rds")
# apply loaded dictionary to dfm object corpus
sent_dfm <- dfm_lookup(mydfm, dictionary = lexicon,nomatch="_unmatched")
sent = as.data.frame(sent_dfm)
sent$score <- sent$EmoPos-sent$EmoNeg
sent$score_norm<-(sent$score-mean(sent$score))/sd(sent$score)
sent$rowsums<-rowSums(as.matrix(mydfm))
sent$score_norm2<-sent$score/sent$rowsums
sent$sexo=substr(sent$document,1,7)
sent$edad=substr(sent$document,9,15)
sent$nro=rownames(sent)
# Color and shape depend on factor (categorical variable)
a=ggplot(sent, aes(x=nro, y=score_norm2, color=sexo, shape=sexo)) +
  geom_point(size=4, alpha=0.6) + 
  ggtitle("Método diccionario LWIC-Spanish")+
  theme(axis.title.y =element_blank() , axis.title.x =element_blank(),legend.title=element_blank()) 


#Otro método
syuzhet_vector<-get_sentiment(texts(mytf),method="syuzhet",language="spanish")
mytf$syuzhet<-syuzhet_vector
data<-merge(docvars(mydfm),sent,by.x="row.names",by.y="document") 
data<-merge(data,mytf,by.x="Row.names",by.y="doc_id")
data$score<-data$EmoPos-data$EmoNeg
cor(data$score,data$syuzhet)
data$nro=c(1:6)
data$nro=as.character(data$nro)
b=ggplot(data, aes(x=nro, y=syuzhet, color=docvar1.x, shape=docvar1.x)) +
  geom_point(size=4, alpha=0.6) +
  ggtitle("Método Syuzhet")+
  theme(axis.title.y =element_blank() , axis.title.x =element_blank(),legend.title=element_blank()) 

pdf("metodos.pdf",width=10,height=5)
grid.arrange(a, b, ncol = 2, heights = c(1, 0))
dev.off()

##con diccionario afinn casero

library(dplyr)
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()
afinn$sentimiento=ifelse(afinn$Puntuacion > 0, "Positiva", "Negativa")
corpus_men_mujeres_mat$palabra=as.character(corpus_men_mujeres_mat$palabra)
corpus_men_mujeres_mat %>%  
  inner_join(afinn, by = c("palabra" = "Palabra")) %>%
  count(palabra, sentimiento, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentimiento) %>%
  top_n(12) %>%
  ungroup() %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(palabra, n, fill = sentimiento)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = NULL, x = NULL) +
  coord_flip() +
  ggtitle("Sentimientos más extremos - Mujeres") +
  theme_minimal()+
  scale_fill_manual(values=c('#F8766D' ,"#00BA38")) 


corpus_men_varones_mat$palabra=as.character(corpus_men_varones_mat$palabra)
corpus_men_varones_mat %>%  
  inner_join(afinn, by = c("palabra" = "Palabra")) %>%
  count(palabra, sentimiento, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentimiento) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(palabra, n, fill = sentimiento)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = NULL, x = NULL) +
  coord_flip() +
  ggtitle("Sentimientos más extremos - Varones") +
  theme_minimal()+
  scale_fill_manual(values=c('#F8766D' ,"#00BA38")) 


##II. Paper review

##GRÁFICOS DESCRIPTIVOS

pr=read.csv("PR/base_pr.csv",sep=";",header = T)


##sexo y año

sexo=subset(pr,pr$Sexo!="")
sexo=data.frame(table(sexo$Año,sexo$Sexo))
sexo=subset(sexo,sexo$Freq!=0)
tot=data.frame(table(pr$Año))
tot$Var1=as.numeric(as.character(tot$Var1))
tot$Var2="Total"
tot=tot[,c("Var1","Var2","Freq")]
tot=rbind(sexo,tot)
tot$Var1=as.numeric(as.character(tot$Var1))

gg <- ggplot(tot, aes(x=Var1)) 
gg + geom_line(aes(y=Freq, color=Var2), size=1.2) + scale_color_discrete(name="")+
  ggtitle("Cantidad de publicaciones por sexo (1971-2018)")

##continente

reciente=subset(pr,pr$Año>2000&pr$Año<2018)
cont=data.frame(table(reciente$Año,reciente$Región))
#sexo=subset(sexo,sexo$Freq!=0)
cont$Var1=as.numeric(as.character(cont$Var1))

gg <- ggplot(cont, aes(x=Var1)) 
gg + geom_line(aes(y=Freq, color=Var2), size=1.2) + scale_color_discrete(name="")+
  ggtitle("Cantidad de publicaciones por región (2000-2017)")
  
  
##wordcloud por décadas

# dec_80=read.csv("PR/80´.csv",sep=";",header = T)
# dec_90=read.csv("PR/90´.csv",sep=";",header = T)
# dec_00=read.csv("PR/00´.csv",sep=";",header = T)
# dec_10=read.csv("PR/10´.csv",sep=";",header = T)
# dec_80=toString(dec_80[,1])
# dec_90=toString(dec_90[,1])
# dec_00=toString(dec_00[,1])
# dec_10=toString(dec_10[,1])
# writeLines(dec_80, "PR/TXT/80´.txt")
# writeLines(dec_90, "PR/TXT/90´.txt")
# writeLines(dec_00, "PR/TXT/00´.txt")
# writeLines(dec_10, "PR/TXT/10´.txt")

mytf_pr<-readtext("PR/TXT/*", docvarsfrom ="filenames")
myCorpus_pr<-corpus(mytf_pr,text_field="text")
mydfm_PR <- dfm(myCorpus_pr,
             stem = FALSE,
             tolower = TRUE,
             remove = c(stopwords("spanish"),vector), 
             remove_punct = TRUE, 
             remove_numbers = TRUE, 
             verbose = TRUE)
words<-featnames(mydfm_PR)
#Saco palabras con 1 y 2 caracteres
words_Length_1<-words[sapply(words,str_length)==1]
words_Length_2<-words[sapply(words,str_length)==2]
otherWords<-c("hey",words_Length_1,words_Length_2) #you can expand this list
mydfm_PR<-dfm(mydfm_PR,remove=otherWords)

rownames(mydfm_PR)=c("00´","10´","80´","90´")
# Nube de palabras: comparado
textplot_wordcloud(mydfm_PR, min.count = 2,max_words = 500,random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),comparison = T,min_size = 2,max_size = 3)

dtm <- convert(mydfm_PR_t, to = "topicmodels")
lda <- LDA(dtm, k = 10)
terms(lda, 8)

set.seed(100)
if (require(topicmodels)) {
  
  my_lda_fit20 <- LDA(convert(mydfm_PR, to = "topicmodels"), k = 10)
  get_terms(my_lda_fit20, 8)
}


mytf_pr_t<-readtext("PR/Titulos/*")
myCorpus_pr_t<-corpus(mytf_pr_t,text_field="text")
mydfm_PR_t <- dfm(myCorpus_pr_t,
                stem = FALSE,
                tolower = TRUE,
                remove = c(stopwords("spanish"),vector), 
                remove_punct = TRUE, 
                remove_numbers = TRUE, 
                verbose = TRUE)
words<-featnames(mydfm_PR_t)
#Saco palabras con 1 y 2 caracteres
words_Length_1<-words[sapply(words,str_length)==1]
words_Length_2<-words[sapply(words,str_length)==2]
otherWords<-c("hey",words_Length_1,words_Length_2) #you can expand this list
mydfm_PR_t<-dfm(mydfm_PR_t,remove=otherWords)


tit_pr=read.csv("PR/base_pr.csv",sep=";",header = T)
corpus_tit_pr <- Corpus(VectorSource(tit_pr$TITESP),readerControl = list(encoding = "UTF-8",language="es"))
corpus_tit_pr <- tm_map(corpus_tit_pr, content_transformer(tolower))
corpus_tit_pr <- tm_map(corpus_tit_pr, removeNumbers)
corpus_tit_pr <- tm_map(corpus_tit_pr, removePunctuation)
corpus_tit_pr <- tm_map(corpus_tit_pr, removeWords, vector)
corpus_tit_pr <- tm_map(corpus_tit_pr, stemDocument,"spanish")

corpus_tit_pr_tdm=TermDocumentMatrix(corpus_tit_pr, control = list(encoding = "UTF-8",reader=readPlain,language="es"))
corpus_tit_pr_tdm <- removeSparseTerms(corpus_tit_pr_tdm, sparse = .97)
#corpus_tit_pr_mat <- as.matrix(corpus_tit_pr_tdm)
corpus_tit_pr_tdm <- corpus_tit_pr_tdm %>% as.matrix()
corpus_tit_pr_tdm <- corpus_tit_pr_tdm / rowSums(corpus_tit_pr_tdm)
corpus_tit_pr_tdm_dist <- dist(corpus_tit_pr_tdm, method = "euclidian")
corpus_tit_pr_tdm_hclust <-  hclust(corpus_tit_pr_tdm_dist, method = "ward.D")
plot(corpus_tit_pr_tdm_hclust, main = "Dendrograma - Paper Review", sub = "", xlab = "")
rect.hclust(corpus_tit_pr_tdm_hclust, k = 6, border="blue")
