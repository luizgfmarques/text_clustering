##############################################################################
#                           DOCUMENT CLUSTERING                              #
##############################################################################
#Let’s read in some data and make a document term matrix (DTM) and get started.

#library(textmineR)
#library(Rcpp)
#library(factoextra)

#Using the Library -tidytext- to do some text processing.
#using the function tolower
bd_CDs$cdfulltext<-tolower(bd_CDs$cdfulltext)

#Defining some personal stopwords
my_stopwords=c("a","b","c","d","e","f","g", "h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
               "c2","c1","ro","ts","cl","gt",
               "0","1","2","3","4","5","6","7","8","9")

#combining english and portuguese stopwords with my own.
all_stopwords<-unique(c(my_stopwords,stopwords(kind="en"),stopwords(kind="portuguese")))

# create a document term matrix using the package -textmineR-  (Sparse Matrix)
dtm <- CreateDtm(doc_vec = bd_CDs_031$cdfulltext, # character vector of documents
                 doc_names = bd_CDs_031$nota_cd, # document names (ID)
                 ngram_window = c(1, 1), # minimum and maximum n-gram length
                 stopword_vec = all_stopwords, # stopwords removal
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = FALSE, # numbers - true is the default
                 verbose = TRUE) # Turn on status bar for this default

# construct the matrix of term counts to get the IDF vector (term_freq, doc_Freq and idf)
tf_mat <- TermDocFreq(dtm)

# Calculating the TF-IDF and using the cosine similarity
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf     #tf*idf
tfidf <- t(tfidf)  #transposing the matrix   (Document X Words)

#The next step is to calculate cosine similarity and change it to a distance.
#We’re going to use some linear algebra to do this. 
#The dot product of two positive-valued, unit-length vectors is the cosine similarity between the two vectors. 
#For a deeper explanation of the math and logic, read this article: https://anythingbutrbitrary.blogspot.com/2013/03/build-search-engine-in-20-minutes-or.html

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))      #cosine similarity
csim <- csim %*% t(csim) #cosine similarity calculated.

#R’s various clustering functions work with distances, not similarities. 
#We convert cosine similarity to cosine distance by subtracting it from 1.
#This works because cosine similarity is bound between 0 and 1.
#While we are at it, we’ll convert the matrix to a dist object.

cdist <- as.dist(1 - csim)     #transforming the cosine in distance

#CLUSTERING

#TESTs ABOVE. Need to use the silhouette method to choose the correct number.
hc <- hclust(cdist, "ward.D")

#Creating a dendogram with -ggplotly- and -ggdendro-.
dhc <- as.dendrogram(hc)
data <- dendro_data(dhc, type = "rectangle")
ggplotly(
  ggdendrogram(hc, rotate = TRUE, size = 0.2)
)

# ELBOW and SILHOUETTE METHOD TO OPTIMIZE K NUMBER
tfidf_df <- as.data.frame(as.matrix(tfidf))
#tfidf_df<-scale(tfidf_df)       #During my researchs, don't seems to be necessary. Source: https://datascience.stackexchange.com/questions/33730/should-i-rescale-tfidf-features
plot_silh<-fviz_nbclust(tfidf_df, FUN = hcut, method = "silhouette",nrow(tfidf_df)*0.8,diss=cdist,print.summary=TRUE) #silhouette graph using calculated distances of cosine similarity
plot_elbow<-fviz_nbclust(tfidf_df, FUN = hcut, method = "wss",k.max = nrow(tfidf_df)*0.8,diss=cdist,print.summary=TRUE) #elbow graph using calculated distances of cosine similarity

max_silh<-max(plot_silh$data$y)
k_op_silh<-as.numeric(plot_silh$data[which.max(plot_silh$data$y), ]$clusters)
y_elbow<-as.numeric(plot_elbow$data[k_op_silh, ]$y)

#Creating a dendogram with -ggplotly- and -ggdendro-, with the choosen K.
dhc <- as.dendrogram(hc)
data <- dendro_data(dhc, type = "rectangle")
ggplotly(
  ggdendrogram(hc, rotate = TRUE, size = 0.2)+
  geom_hline(yintercept = hc$height[21], linetype="solid",
             color = "red", size=0.5)
)

#Defining a better plot for the silhouette graph
#plot_silh
ggplotly(
  ggplot(data=plot_silh$data, aes(x=as.numeric(clusters), y=as.numeric(y),group=1)) +
  geom_line(color="darkblue",size=0.2)+
  geom_point(color="darkblue",size=0.5)+
  theme(axis.text = element_text(size = 0.2))+
  labs(y="Average silhouette width", x = "K - Number of Clusters")+
  ggtitle("Optimal number of clusters - Silhouette Graph")+
  geom_vline(xintercept = k_op_silh, linetype="dashed", 
               color = "red", size=0.5)
)

  #Defining a better plot for the elbow graph
#plot_elbow
ggplotly(
  ggplot(data=plot_elbow$data, aes(x=as.numeric(clusters), y=as.numeric(y),group=1)) +
    geom_line(color="darkblue",size=0.2)+
    geom_point(color="darkblue",size=0.5)+
    theme(axis.text = element_text(size = 0.2))+
    labs(y="Total Within Sum of Square (WSS)", x = "K - Number of Clusters")+
    ggtitle("Optimal number of clusters - Elbow Graph")+
    geom_vline(xintercept = k_op_silh, linetype="dashed", 
               color = "red", size=0.5)+
    geom_hline(yintercept = y_elbow, linetype="dashed", 
               color = "red", size=0.5)
)

   #optimized k for silhouette method
clustering <- cutree(hc, k_op_silh)

plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, k_op_silh, border = "red")

#Verificando os grupos
bd_agrupado<-bd_CDs_031 %>% 
  mutate(grupo=clustering) %>% 
  select(nota_cd,cta,cto,grupo,titulo_cd,desc_material,modo_falha,num_doc_ref)

#Propose the group name using the last document created.

  group_name<-bd_agrupado %>% 
  group_by(grupo,titulo_cd) %>%
  summarize(ult_cd=max(nota_cd),n=n()) %>% 
  arrange(grupo,desc(n)) %>% 
  mutate(titulo_grupo=titulo_cd[which.max(n)]) %>% 
  ungroup() %>%
  select(grupo,titulo_grupo) %>% 
  unique()
  
  bd_agrupado<-left_join(bd_agrupado,group_name,by="grupo")
  

#Viewing the groups
  bd_agrupado %>% 
  group_by(grupo,titulo_grupo) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  ungroup()

#Saving the Defined Group in the Original dataframe

bd_CDs_031<-left_join(bd_CDs_031,group_name,by="grupo")

#Seeing the final result by group
    df_priorizacao<- bd_CDs_031 %>% 
    group_by(grupo,titulo_grupo) %>% 
    summarise(repet=n(),cnq=sum(cnq),hh=sum(hh)) %>%
    ungroup() %>% 
    column_to_rownames(var = 'grupo')
    

view(df_priorizacao)
#teste1





