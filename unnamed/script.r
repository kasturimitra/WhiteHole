library(tidyverse)
library(tidytext)
library(textreadr)
library(qdap)
library(RColorBrewer)
library(wordcloud) 	
library(ggraph)
library(igraph)
library(widyr)

data(stop_words)
unnamed<-read_docx("F:/Kasturi/Unnamed.docx")
unnameddf<-data_frame(unnamed)
unnamed_df<-sapply(unnameddf, str_replace_all, "[“”]", "\"")
unnamed_df<-data_frame(unnamed_df) #auto converts to matrix with sapply
unnamed_df<-sapply(unnamed_df, str_replace_all, "[’‘]", "\'")
unnamed_df<-data_frame(unnamed_df[,1]) #apparent extra columns
colnames(unnamed_df)<-"text"
unnamed_words<-unnest_tokens(unnamed_df, words, text)
colnames(unnamed_words)<-"word"
unnamed_words_ns<-anti_join(unnamed_words, stop_words)
c<-count(unnamed_words_ns, word, sort=TRUE)
#windows() #for better visibility/saving ideal graph
c[1:50,]%>%ggplot(aes(x=reorder(word, n), y=n, fill=n))+geom_bar(stat='identity')+coord_flip() #bar chart top 50 words
c[1:50,]%>%ggplot(aes(x=reorder(word, n), y=n, fill=n))+geom_bar(stat='identity')+coord_flip()+scale_fill_gradient(high="#1f1020", low="#dc9070")+xlab(NULL)+ylab("Frequency") #coloured, better labelled
customYlOrRd<-brewer.pal(9, "YlOrRd")
customYlOrRd<-customYlOrRd[-(1:2)] #redefining palette; excluding lighter colours
c[1:50,]%>%ggplot(aes(x=reorder(word, n), y=n, fill=n))+geom_bar(stat='identity')+coord_flip()+scale_fill_gradientn(colours=customYlOrRd)+xlab(NULL)+ylab("Frequency")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
customPuRd<-brewer.pal(9, "PuRd") #redefining palette; excluding lighter colours
customPuRd<-customPuRd[-(1:2)]
wordcloud(c$word, c$n, max.words = 100, random.order = FALSE, colors=colorRampPalette(customPuRd)(length(unique(c$n))))
wordcloud(c$word, c$n, max.words = 100, random.order = FALSE, colors=colorRampPalette(customPuRd)(length(unique(c$n[1:100])))) #only takes unique values for top 100
unnamed_ngrams<-unnest_tokens(unnamed_df, bigram, text, token='ngrams', n=2)
c_ngram<-count(unnamed_ngrams, bigram, sort=TRUE)
split_c_ngram<-separate(c_ngram, bigram, c("word1", "word2"), sep = " ")
split_c_ngram_ns<- split_c_ngram%>%
	filter(!word1 %in% stop_words$word) %>%
	filter(!word2 %in% stop_words$word)
c_ngram_ns<- unite(split_c_ngram_ns, bigram, word1, word2, sep = " ")
bigram_graph_ns<-split_c_ngram_ns%>%
	filter(n>=5) %>%
	graph_from_data_frame()
windows()
ggraph(bigram_graph_ns, layout = "fr") +geom_edge_link() +geom_node_point() +geom_node_text(aes(label = name), vjust = 1, hjust = 1) #simple, no colours or arrows
ggraph(bigram_graph_ns, layout = "fr") +
	geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
	arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) +
	geom_node_point(color = "lightblue", size = 5) +
	geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
	theme_void()
bigram_graph<-split_c_ngram[1:50,]%>%
	graph_from_data_frame()
ggraph(bigram_graph, layout = "fr") +
	geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches'))+
	geom_node_point(color = "lightblue", size = 5) +
	geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
	theme_void()
unnamed_sentences<-unnest_tokens(unnamed_df, s, text, token = "sentences")
unnamed_sentences$sentence<-c(1:length(unnamed_sentences$s))
unnamed_sentences<-unnest_tokens(unnamed_sentences, word, s, token="words")
unnamed_sentences_ns<-unnamed_sentences%>%filter(!word %in% stop_words$word)
unnamed_section_ns<-unnamed_sentences_ns
unnamed_section_ns$sentence<-unnamed_section_ns$sentence%/%4 #taking four sentences as one section
pairwise_count(unnamed_section_ns,word, sentence, sort=TRUE)
unnamed_section_ns%>%group_by(word)%>%filter(n()>20)%>%pairwise_cor(word, sentence, sort=TRUE)
section_cor<-unnamed_section_ns%>%group_by(word)%>%filter(n()>20)%>%pairwise_cor(word, sentence, sort=TRUE)
section_cor%>%
	filter(correlation > .1) %>%
	graph_from_data_frame() %>%
	ggraph(layout = "fr") +
	geom_edge_link(aes(edge_alpha = correlation), colour="#ef5b7f", show.legend = FALSE) +
	geom_node_point(color = "#5befcb", size = 2) +
	geom_node_text(aes(label = name), colour="#dddddd",repel = TRUE, size=3.2, vjust=1) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="#222222"), panel.border = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),) #correlation graph without external borders or axes
ggsave("Correlation of Words v2 (better quality).png") #saving internally for higher res
