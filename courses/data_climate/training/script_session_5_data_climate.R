library(dplyr)
library(magrittr)

#setwd('C:/Users/jbgui/OneDrive - Université Paris 1 Panthéon-Sorbonne/COURS_DISPENSES/IEDES_2022_2023/Data_Climat')


"L'influence humaine a réchauffé l'atmosphère, l’océan et les terres."

ma_phrase <- "L'influence humaine a réchauffé l'atmosphère, l’océan et les terres."

is.character(ma_phrase)
nchar(ma_phrase) #nombre de caractères dans le string

#install.packages("stringr")
library(stringr)
str_detect(ma_phrase,"influence")
str_detect(ma_phrase,'calamar')

ma_phrase <- str_replace(ma_phrase,'humaine', "de l'homme")


ma_phrase <- gsub("'|,|’", " ", ma_phrase)
print(ma_phrase)


# install.packages('tidytext')
library(tidytext)


#install.packages("tidyft") #Pour le recodage des variables textuelles 
library(tidyft)
bdd_speech <- read.csv2('Data_Climat/DATA/bdd_discours_2007_2022.csv') %>%
  as.data.table() %>%
  utf8_encoding(titles) %>%
  utf8_encoding(dates) %>%
  as.data.frame()
head(bdd_speech$titles, n=6)


bdd_speech$titles <- gsub("[[:punct:]]", " ", bdd_speech$titles)
bdd_speech$titles <- gsub("[[:digit:]]+", "", bdd_speech$titles)
bdd_speech$titles <- gsub("[\r\n]", "", bdd_speech$titles)
bdd_speech$titles <- gsub("  ", " ", bdd_speech$titles)
bdd_speech$titles <- gsub("  ", " ", bdd_speech$titles)
head(bdd_speech$titles, n=4)


tidy_text <- dplyr::tibble(bdd_speech) %>%
  unnest_tokens("word", titles)

head(tidy_text, n=4)


#install.packages("lsa") # packages pour le chargement de stopwords (pour différentes langues)
library(lsa)
data(stopwords_fr)
df_stopwords_fr <- data.frame(word=stopwords_fr,
                              lexicon = "?")
tidy_text <- tidy_text %>% dplyr::anti_join(df_stopwords_fr) 
head(tidy_text, n=4)


head(tidy_text %>% dplyr::count(word, sort = TRUE))


new_stop_words_fr <- data.frame("word" = c("mme", "janvier", "février","mars","avril","mai","juin","juillet", "août", "septembre","octobre","novembre","décembre"), stringsAsFactors = FALSE)
tidy_text <- tidy_text %>% dplyr::anti_join(new_stop_words_fr)
head(tidy_text %>% dplyr::count(word, sort = FALSE))


head(tidy_text %>% dplyr::count(word, sort = TRUE))

tidy_text_stems <- tidy_text %>%
  dplyr::mutate_at("word", dplyr::funs(wordStem((.), language="fr"))) 

head(tidy_text_stems)

#install.packages("wordcloud")
library(wordcloud)

tidy_text %>% 
  dplyr::count(word) %>% 
  with(wordcloud(word, n, max.words=15, colors = brewer.pal(8, "Dark2")))


new_stop_words_fr2 <- data.frame("word" = c("affaires","ministre", "ministres","communiqué","déclaration","conseil","secrétaire","interview","jean", "françois"), stringsAsFactors = FALSE)

tidy_text <- tidy_text %>% dplyr::anti_join(new_stop_words_fr2)

tidy_text_stems <- tidy_text %>%
  mutate_at("word", funs(wordStem((.), language="fr"))) 

tidy_text_stems %>% 
  dplyr::count(word) %>% 
  with(wordcloud(word, n, min.freq = 900, colors = brewer.pal(8, "Dark2")))

tidy_text_tfidf <- tidy_text %>%
  dplyr::count(word, dates) %>%
  bind_tf_idf(word, dates, n) %>%
  dplyr::arrange(desc(tf_idf))

head(tidy_text_tfidf)


tidy_text_tfidf$dates <- as.Date(tidy_text_tfidf$dates, format = c("%d %b %Y"))
tidy_text_tfidf$month <- paste("01/",format(tidy_text_tfidf$dates, "%m"),"/",format(tidy_text_tfidf$dates, "%Y"), sep="")
tidy_text_tfidf$month_date <- as.Date(tidy_text_tfidf$month, format=c('%d/%m/%Y'))


library(ggplot2)
tidy_text_tfidf %>% 
  dplyr::filter(month_date >= as.Date("2021-01-01")) %>% 
  dplyr::group_by(month_date) %>% 
  dplyr::top_n(10, tf_idf) %>% 
  ungroup() %>% 
  dplyr::mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = month_date)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~month_date, scales = "free") + 
  coord_flip() 


df_envi <- tidy_text_tfidf %>%
  dplyr::filter(word=="environnement") %>%
  dplyr::group_by(month_date) %>%
  dplyr::summarise(sum_n = sum(n))

ggplot(data=df_envi, aes(x=month_date, y=sum_n)) +
  geom_line(linetype = "dashed")+
  geom_point()

df_envi <- tidy_text_tfidf %>%
  dplyr::filter(word %in% c("environnement","climat","transition","écologie","durable","réchauffement")) %>%
  dplyr::group_by(month_date) %>%
  dplyr::summarise(sum_n = sum(n))

ggplot(data=df_envi, aes(x=month_date, y=sum_n)) +
  geom_line(linetype = "dashed")+
  geom_point()


df_ecologie <- tidy_text_tfidf %>%
  dplyr::filter(word=="écologie") %>%
  dplyr::group_by(month_date) %>%
  dplyr::summarise(sum_n = sum(n))

ggplot(data=df_ecologie, aes(x=month_date, y=sum_n)) +
  geom_line(linetype = "dashed")+
  geom_point()

df_transi <- tidy_text_tfidf %>%
  dplyr::filter(word=="transition") %>%
  dplyr::group_by(month_date) %>%
  dplyr::summarise(sum_n = sum(n))

ggplot(data=df_transi, aes(x=month_date, y=sum_n)) +
  geom_line(linetype = "dashed")+
  geom_point()

df_biodi <- tidy_text_tfidf %>%
  dplyr::filter(word=="biodiversité") %>%
  dplyr::group_by(month_date) %>%
  dplyr::summarise(sum_n = sum(n))

ggplot(data=df_biodi, aes(x=month_date, y=sum_n)) +
  geom_line(linetype = "dashed")+
  geom_point()


#install.packages('tm')
#install.packages('qdapTools')
library(tm)
library(qdapTools)
corpus <- bdd_speech %>%
  dplyr::mutate(text = stringr::str_replace_all(.$titles, "’", " ")) %>% 
  .$text %>% 
  VectorSource()%>%
  VCorpus()%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, stopwords("french"))%>%
  TermDocumentMatrix()

associations <- findAssocs(corpus, "environnement", corlimit = 0.05) %>% 
  list_vect2df() %>%
  data.frame()
names(associations) <- c("source", "terme", "corr")
associations <- dplyr::arrange(associations, desc(corr))


ggplot(associations[1:10,], aes(x = terme, y = corr)) + 
  geom_bar(stat="identity") + 
  coord_flip()+
  xlab("Terme") + 
  ylab("Corrélation") + 
  theme_minimal()

library(reshape2)

prenoms_fr <-read.csv2('Data_Climat/DATA/Prenoms.csv') 

#%>% tidyft::utf8_encoding(X01_prenom) %>%
 # as.data.frame()

bdd_speech_pair <- bdd_speech %>%
  unnest_tokens(output = word, input = titles, token = "ngrams", n = 2)

bdd_speech_pair <- cbind(bdd_speech_pair, colsplit(bdd_speech_pair$word, pattern=" ",names=c("word1", "word2"))) %>%
  dplyr::filter(!word1 %in% stopwords_fr,
                !word2 %in% stopwords_fr,
                !word1 %in% prenoms_fr$X01_prenom,
                !word2 %in% prenoms_fr$X01_prenom,
                !word1 %in% new_stop_words_fr$word,
                !word2 %in% new_stop_words_fr$word,
                !word1 %in% new_stop_words_fr2$word,
                !word2 %in% new_stop_words_fr2$word) %>%
  tidyr::drop_na(word1, word2) %>%
  dplyr::count(word1, word2, sort = TRUE)

# filter for only relatively common combinations
bigram_graph <- bdd_speech_pair %>%
  dplyr::filter(n > 70) %>%
  igraph::graph_from_data_frame()

# Network graph

library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE, alpha = .5) +
  geom_node_point(color = "#0052A5", size = 3, alpha = .5) +
  geom_node_text(aes(label = name), vjust = 1.5) +
  ggtitle("Réseau des mots - (2007-2023)") +
  theme_void() 


#install.packages("pdftools")
library("pdftools")


pdf.text_report_2001 <- pdftools::pdf_text("Data_Climat/DATA/2001_policy_report.pdf")
pdf.text_report_2007 <- pdftools::pdf_text("Data_Climat/DATA/2007_policy_report.pdf")
pdf.text_report_2014 <- pdftools::pdf_text("Data_Climat/DATA/2014_policy_report.pdf")

# selection d'une page en particulier
# cat(pdf.text_report_2001[[8]]) 

pdf.text_report_2001<-unlist(pdf.text_report_2001)
pdf.text_report_2001<-tolower(pdf.text_report_2001)
pdf.text_report_2007<-unlist(pdf.text_report_2007)
pdf.text_report_2007<-tolower(pdf.text_report_2007)
pdf.text_report_2014<-unlist(pdf.text_report_2014)
pdf.text_report_2014<-tolower(pdf.text_report_2014)


text_2001 <- paste(pdf.text_report_2001, collapse = '')
text_2007 <- paste(pdf.text_report_2007, collapse = '')
text_2014 <- paste(pdf.text_report_2014, collapse = '')

df_giec <- data.frame(date = c(2001,2007,2014),
                      text = c(text_2001,text_2007,text_2014))
df_giec$text <- gsub("[[:punct:]]", " ", df_giec$text)
df_giec$text <- gsub("[[:digit:]]+", "", df_giec$text)
df_giec$text <- gsub("[\r\n]", "", df_giec$text)



tidy_text_giec <- dplyr::tibble(df_giec) %>%
  unnest_tokens("word", text)

data(stopwords_en)
df_stopwords_en <- data.frame(word=stopwords_en,
                              lexicon = "?")
tidy_text_giec <- tidy_text_giec %>% dplyr::anti_join(df_stopwords_en) 
head(tidy_text, n=4)

tidy_text_giec %>% 
  dplyr::count(word) %>% 
  with(wordcloud(word, n, max.words=50, colors = brewer.pal(8, "Dark2")))




