library(tidyverse)
library(quanteda)
library(rstan)

all_files_path <- list.files("../lipad", recursive = TRUE, full.names = TRUE)
all_files_path <- all_files_path[str_sub(all_files_path,17,-5) > "2016-01-01"]

empty_list <- list()

for (k in seq_along(all_files_path)){
  print(k)
  empty_list[[k]] <- read_csv(all_files_path[k])
}

df <- bind_rows(empty_list)

df <- df[nchar(df$speechtext) > 100,]
df$speechtext <- gsub("[^[:alpha:][:space:]]","",df$speechtext)
df$speechtext <- str_replace_all(gsub("[^[:alnum:][:space:]]","",df$speechtext),"\n"," ")
df$speechtext <- gsub("Mr. Speaker","",df$speechtext)
df$speechtext <- gsub("Madam Speaker","",df$speechtext)

df$speakerriding <- gsub("[^[:alnum:]]","",df$speakerriding)

df <- df %>% filter(maintopic %in% c("Oral Questions"))
df <- df %>% filter(maintopic %in% c("Oral Questions"))

top_subtopics <- rev(names(tail(sort(table(df$subtopic,useNA = "ifany")),40)))
top_subtopics <- top_subtopics[!is.na(top_subtopics)]

top_subtopics <- top_subtopics[top_subtopics!='Questions on the Order Paper']
top_subtopics <- top_subtopics[top_subtopics!="Questions Passed as Orders for Returns"]
top_subtopics <- top_subtopics[top_subtopics!="Presence in Gallery"]
top_subtopics <- top_subtopics[top_subtopics!="Government Response to Petitions"]
top_subtopics <- top_subtopics[top_subtopics!="Questions Passed as Orders for Return"]
top_subtopics <- top_subtopics[top_subtopics!="Marijuana"]
top_subtopics <- top_subtopics[top_subtopics!="Pensions"]
top_subtopics <- top_subtopics[top_subtopics!="Canada Revenue Agency"]
top_subtopics
df <- df %>% filter(speakername!="Leona Alleslev")

df$speechtext <- str_replace_all(df$speechtext,"Mr Speaker","")
df$speechtext <- str_to_lower(df$speechtext)
write_csv(df %>% select(speechtext),"~/Dropbox (Personal)/UofT/canadian/essay2/large_data_file_for_python.csv")

getTheta <- function(df,subtopic_){
  
  temp <- df %>% 
    filter(subtopic == subtopic_,
           !is.na(opid)) %>% 
    select(speechtext,speakername,subtopic,speakerriding,speakerparty) %>%
    mutate(speakerparty = ifelse(speakerparty=="NDP","New Democratic Party",speakerparty)) %>%
    mutate(speakerparty = ifelse(speakerparty %in% c("Bloc Québécois","Bloc"),"Bloc",speakerparty)) %>%
    mutate(speakerparty = ifelse(speakerparty %in% c("Green","Green Part"),"Green",speakerparty)) %>%
    filter(speakerparty %in% c("Liberal","New Democratic Party","Conservative","Bloc","Green"))
  
  unique_members <- unique(temp[,-1])
  
  unique_members_list <- list()
  
  for (k in 1:nrow(unique_members)){
    speakername <-  unique_members$speakername[k]
    subtopic <-  unique_members$subtopic[k]
    speakerriding <-  unique_members$speakerriding[k]
    speakerparty <-  unique_members$speakerparty[k]
    
    speech_text <- temp[temp$speakername==speakername & 
                          temp$subtopic==subtopic &
                          temp$speakerriding==speakerriding &
                          temp$speakerparty==speakerparty,] %>% pull(speechtext) %>%
      paste0(collapse = " ")
    
    unique_members_list[[k]] <-
      tibble(text = speech_text, speakername, subtopic, speakerriding, speakerparty)
    
  }
  
  temp <- bind_rows(unique_members_list)
  temp <- temp %>% filter(nchar(text) > 100)

  
  topic_corpus <- corpus(temp$text, docvars = (temp %>% select(-text)))
  topic_corpus$documents
  dfm_partis <- dfm(topic_corpus,
                    remove = c(quanteda::stopwords(language = "en"),
                               c("Mr. Speaker","the hon. member, the member",
                                 "mr. speaker")),
                    remove_punct = TRUE)
  
  
  dfm_partis <- dfm_wordstem(dfm_partis, language = "en")
  dfm_partis <- dfm_trim(dfm_partis, min_termfreq = 5)
  
  # Estimate Wordfish model
  wf <- textmodel_wordfish(dfm_partis)#, dir = c(6,5))
  
  wf$features[order(wf$beta)][1:20]
  wf$beta[order(wf$beta)][1:20]
  wf$features[order(wf$beta)][(length(wf$beta)):(length(wf$beta)-20)]
  wf$beta[order(wf$beta)][(length(wf$beta)):(length(wf$beta)-20)]
  
  temp$theta <- wf$theta
  
  temp
  
}

df$speakerriding <- gsub("[^[:alnum:]]","",df$speakerriding)
sort(unique(df$speakerriding))
getThetaList <- list()

for (j in 1:length(top_subtopics)){
  print(j)
  getThetaList[[j]] <- getTheta(df=df,subtopic_=top_subtopics[j])
}

bindedThetas <- bind_rows(getThetaList)

bindedThetas <- bindedThetas %>%
  select(speakername,speakerparty,speakerriding,theta,subtopic) %>%
  spread(subtopic,theta)

bindedThetas <- bindedThetas %>%
  gather(key,value,-speakername,-speakerparty,-speakerriding) %>%
  filter(!is.na(value)) %>%
  mutate(key_numeric = as.numeric(as.factor(key))) %>%
  mutate(speakername_numeric = as.numeric(as.factor(speakername)))

bindedThetasStan <- bindedThetas

model_code_linear <- "
data {

int n_items;
int n_respondents;
int n_obs;

int k[n_obs];
int j[n_obs];
real y[n_obs];

} 

parameters {

vector[n_respondents] theta_unconstrained;         // ideology
vector[n_items] beta_unconstrained;                // discrimination
vector[n_items] alpha;                // discrimination
real<lower=0> sigma[n_items];

}

transformed parameters {
}

model {

// Priors
beta_unconstrained ~ cauchy(0, 5);
theta_unconstrained ~ normal(0, 1);
for(m in 1:n_items) {

}

// Likelihood
for(m in 1:n_obs) {
y[m] ~ normal(alpha[j[m]] + beta_unconstrained[j[m]] * theta_unconstrained[k[m]], sigma[j[m]]);
}

}

generated quantities {

// Fix reflection invariance
vector[n_respondents] theta = beta_unconstrained[1] > 0 ? theta_unconstrained : theta_unconstrained * -1;
vector[n_items] beta = beta_unconstrained[1] > 0 ? beta_unconstrained : beta_unconstrained * -1;

}


"

full_data <- list(n_respondents = length(unique(bindedThetasStan$speakername)),
                  n_items = length(unique(bindedThetasStan$key_numeric)),
                  n_obs = nrow(bindedThetasStan),
                  k = bindedThetasStan$speakername_numeric,
                  j = bindedThetasStan$key_numeric,
                  y = bindedThetasStan$value)

model_irt <- stan_model(model_code = model_code_linear)
#posterior_irt1 <- rstan::vb(object = model_irt, data = full_data, iter = 30000, output_samples = 5000)
posterior <- rstan::sampling(object = model_irt, data = full_data, iter = 1200, chains = 2 , cores = 2)

theta <- sapply(rstan::extract(posterior, paste0("theta[", 1:full_data$n_respondents, "]")), median)

to_plot_wordfish <- left_join(
  
  bindedThetasStan %>%
    select(speakername,speakername_numeric,speakerparty,speakerriding) %>% unique(),
  
  tibble(speakername_numeric = 1:length(theta),theta))

to_plot$riding2 <- gsub("[^[:alnum:]]","",to_plot$riding)

mapping_clean_dirty_riding_names <- to_plot %>% select(riding,riding2) %>% unique()
  
to_plot_joined <- left_join(to_plot_wordfish,
          to_plot,
          c("speakerriding"="riding2")) 

to_plot_joined$theta <- scales::rescale(to_plot_joined$theta)

ggplot(to_plot_joined,aes(x=`Mean score 1D`, y = theta, color = speakerparty)) +
  geom_point(size = 2.5) + #geom_smooth(se=FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        legend.background = element_rect(colour = "black",fill="transparent"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Party", values = c("lightblue","blue","darkgreen","red","orange"))

ggsave("figures/figWordschoal.pdf",width = 9, height = 6, bg = "transparent")

to_plot_by_party <- left_join(to_plot_joined %>% select(speakerparty,riding),
                              to_plot,c("riding"="riding"))

ggplot(to_plot_by_party, aes(x = `Mean self placement`, y = `Mean score 1D`, color = speakerparty)) +
  geom_point(size = 2.5) +
  theme_minimal() +
  geom_smooth(se=FALSE, color = "grey50") +
  theme(legend.position = c(0.25,0.75),
        text = element_text(size = 18),
        legend.background = element_rect(colour = "black",fill="transparent"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Party", values = c("lightblue","blue","darkgreen","red","orange"))

ggsave("figures/figSelfPlacementScore1D.pdf",width = 8, height = 6, bg = "transparent")

