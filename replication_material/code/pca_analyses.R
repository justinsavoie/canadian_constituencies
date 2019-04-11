library(tidyverse)

word_embeddings <- read_csv("saved_models/word_embeddings_with_oral_questions.csv")
doc_embeddings <- read_csv("saved_models/doc_embeddings_with_oral_questions_PROVMEMBERS.csv")

pca <- prcomp(doc_embeddings%>%select(-document))

doc_embeddings$pca1 <- c(pca$x[,1])
doc_embeddings$pca2 <- c(pca$x[,2])
doc_embeddings$pca3 <- c(pca$x[,3])

lipad_under_trudeau <- df %>% filter(speechdate > "2015-10-19")
lipad_under_trudeau <- unique(lipad_under_trudeau %>% select(speakerparty,speakerriding,speakername))
lipad_under_trudeau

to_plot$riding2 <- gsub("[^[:alnum:]]","",to_plot$riding)

lipad_under_trudeau <- left_join(lipad_under_trudeau,to_plot,c("speakerriding"="riding2"))

doc_embeddings_under_trudeau <- doc_embeddings %>% filter(document %in% lipad_under_trudeau$speakername)

all_joined_under_trudeau <- left_join(lipad_under_trudeau,doc_embeddings_under_trudeau, c("speakername" = "document"))

summary(lm(pca1~`Mean score 1D`,all_joined_under_trudeau %>% filter(speakerparty=="Liberal",province!='Quebec')))

ggplot(all_joined_under_trudeau%>%filter(province!='Quebec'),aes(x=`Mean score 1D`,y=pca1, color = speakerparty)) +
  geom_point(size = 2.5) +
  theme_minimal() +
  geom_text(aes(label=speakername)) +
  #geom_smooth(se=FALSE, color = "grey50") +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        legend.background = element_rect(colour = "black",fill="transparent"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Party", values = c("blue","darkgreen","red","orange"))

ggsave("figures/doc2vecMinistersNoMinisters.pdf",width = 8, height = 6, bg = "transparent")

ggplot(all_joined_under_trudeau %>% mutate(Quebec = ifelse(Province == "Quebec","Quebec","ROC")),
       aes(x=`Mean score 1D`,y=pca1, color = speakerparty,shape=Quebec)) +
  geom_point(size = 3.5) +
  theme_minimal() +
  #geom_smooth(method = "lm", se=FALSE) +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        legend.background = element_rect(colour = "black",fill="transparent"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Party", values = c("lightblue","blue","darkgreen","red","orange"))

ggsave("figures/doc2vecQuebec.pdf",width = 12, height = 8, bg = "transparent")

ggplot(all_joined_under_trudeau,aes(x=pca1,y=pca2, color = speakerparty,shape=cut(`Mean score 1D`,c(0,0.25,0.5,0.75,1)))) +
  geom_point(size=4) +
  geom_text(aes(label = speakername)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        legend.background = element_rect(colour = "black",fill="transparent"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Party", values = c("lightblue","blue","darkgreen","red","orange"))

# WORD2VEC

word_embeddings <- read_csv("saved_models/word_embeddings_basic_oral.csv")

prcomp_model <- prcomp(word_embeddings[,1:50])

word_embeddings$score1 <- prcomp_model$x[,1]

word_embeddings_prc <- word_embeddings %>% arrange((score1),word) %>% select(word,score1)

unique(df$speakername)

to_plot_joinedw2v <- to_plot_joined

to_plot_joinedw2v$MEAN <- NA
to_plot_joinedw2v$MEDIAN <- NA
to_plot_joinedw2v$N <- NA

to_plot_joinedw2v <- to_plot_joinedw2v %>% filter(speakername %in% unique(df$speakername))

for (k in 1:339){
  
  print(k)
  which_speaker <- to_plot_joinedw2v$speakername[k]
  
  full_speechtext <- df[df$speakername == which_speaker,] %>% pull(speechtext)
  full_speechtext <- unlist(str_split(full_speechtext," "))
  full_speechtext <- tibble(full_speechtext)
  full_speechtext <- full_speechtext[full_speechtext!="",]
  full_speechtext <- full_speechtext[nchar(full_speechtext$full_speechtext) > 3,]
  
  to_plot_joinedw2v$MEAN[k] <- full_speechtext %>% left_join(word_embeddings_prc,c("full_speechtext"="word")) %>%
    summarise(x = mean(score1,na.rm=TRUE)) %>% pull(x)
  
  to_plot_joinedw2v$MEDIAN[k] <- full_speechtext %>% left_join(word_embeddings_prc,c("full_speechtext"="word")) %>%
    summarise(x = median(score1,na.rm=TRUE)) %>% pull(x)
  
  to_plot_joinedw2v$N[k] <- full_speechtext %>% left_join(word_embeddings_prc,c("full_speechtext"="word")) %>%
    summarise(x = n()) %>% pull(x)
  
}

to_plot_joinedw2v <- to_plot_joinedw2v %>% rename(`Primary component mean` = MEAN)

to_plot_joinedw2v$`Primary component mean` <- scales::rescale(to_plot_joinedw2v$`Primary component mean`,c(1,0))

ggplot(to_plot_joinedw2v,aes(x=`Mean score 1D`, y = `Primary component mean`, color = speakerparty)) +
  geom_point(size = 3.5) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        legend.background = element_rect(colour = "black",fill="transparent"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Party", values = c("lightblue","blue","darkgreen","red","orange"))

ggsave("figures/figWord2VecOral.pdf",width = 9, height = 6, bg = "transparent")

