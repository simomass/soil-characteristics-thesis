

                                        # Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

##text <- readLines("/home/simone/Simo_GIT/tesi/Tesi_GIT.txt")
txt <- DirSource(
    #directory = "/home/simone/Simo_GIT/wordcloud",
    directory = "/home/ottorino/Documenti/BitBucket/Simo_GIT/wordcloud",
    encoding = "UTF-8",
    pattern = "*.txt")

docs <-
    VCorpus(txt, readerControl = list(language = "it"))



##inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

## Remove punctuations
docs <- tm_map(docs, removePunctuation)
## Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

                                        # Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
                                        # Remove numbers
docs <- tm_map(docs, removeNumbers)
                                        # Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("italian"))
                                        # Remove your own stop word

docs <- tm_map(docs, removeWords, c("della", "è", "del", "dei", "una", "che", "sono", "con", "delle", "per", "ha", "cit", "degli", "dati", "non",
                                    "misura", "alla", "–", "gli", "dal", "tra", "cui", "due", "tabella", "come", "dello", "più", "nel", "essere",
                                    "url", "alle", "viene", "rispetto", "dalla", "questo", "totale", "tre", "quindi", "risultati", "figura", "sul",
                                    "ogni", "sia", "https", "nei", "altri", "sulla", "fig", "dove", "basso", "tab", "autori", "lungo", "questi",
                                    "stati", "http", "hanno", "quello", "nella", "stati", "sua", "dalle", "quelli", "state", "ottenuti", "numero",
                                    "fine", "stato", "ovvero", "punto", "anche", "secondo", "quali", "vengono", "dopo", "circa", "possono",
                                    "presenti", "provenienti", "misurato", "stata", "inizio", "questa", "scopo", "progetto", "diverse", "maggiore",
                                    "analisi", "seguente", "penetrazione", "uno", "quanto", "tipo", "modo", "equazione", "mediante", "the",
                                    "agricoltura", "anno", "Massenzio", "Magistrale", "due", "fase", "tre", "fig.", "tabella", "metodo",
                                    "campione", "campioni", "rottura", "studio", "condotti", "successivamente", "strumento", "anni", "tecniche",
                                    "trattamenti", "materiale", "materiali", "indica", "esempio", "relativa", "sistema", "che"
                                    ))


## Text stemming
## docs <- tm_map(docs, stemDocument)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
##head(d, 40)
pettina <- d
pettina <- pettina[!pettina$freq==30,]
pettina <- pettina[1:77,]
pettina$freq[pettina$word=="biologico"] <-
    sum(pettina$freq[pettina$word=="biologica"|pettina$word=="biologici"])
pettina <- pettina[!(pettina$word=="biologica"|pettina$word=="biologici"),]

set.seed(1234)
#png("~/Simo_GIT/foto/wordcloud.png")
png("~/Documenti/BitBucket/Simo_GIT/foto/wordcloud.png")
wordcloud(words = pettina$word, freq = pettina$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0,
          colors=brewer.pal(8, "Dark2"), fixed.asp=TRUE)
dev.off()
