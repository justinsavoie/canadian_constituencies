library(tidyverse)

all_files_path <- list.files("../lipad", recursive = TRUE, full.names = TRUE)
all_files_path <- all_files_path[str_sub(all_files_path,17,-5) > "2016-01-01"]

empty_list <- list()

for (k in seq_along(all_files_path)){
  print(k)
  empty_list[[k]] <- read_csv(all_files_path[k])
}

df <- bind_rows(empty_list)

df$speakerparty[df$speakerparty=="NDP"] <- "New Democratic Party"
df$speakerparty[df$speakerparty=="Green Party"] <- "Green"
df$speakerparty[df$speakerparty=="Bloc Québécois"] <- "Bloc"

df <- df %>% filter(speakerparty %in% c("Liberal","New Democratic Party","Conservative","Bloc","Green","Progressive Conservative","Canadian Alliance","Reform"))

df$speakerriding <- gsub("[^[:alnum:]]","",df$speakerriding)

df$maintopic[df$maintopic=="Statements by Members"] <- "Statements By Members"

df$maintopic[df$maintopic=="Oral Question Period"] <- "Oral Questions"

df <- df %>% filter(maintopic %in% c("Statements By Members"))

df$speechtext <- str_replace_all(df$speechtext,"\n"," ")

df$speechtext <- str_replace_all(df$speechtext, "Mr. Speaker, ", "")
df$speechtext <- str_replace_all(df$speechtext, "Mr Speaker, ", "")
df$speechtext <- str_replace_all(df$speechtext, "Madam Speaker, ", "")
df$speechtext <- str_replace_all(df$speechtext, "Mr. Speaker", "")
df$speechtext <- str_replace_all(df$speechtext, "Madam Speaker", "")

df <- df[(nchar(df$speechtext) > 100),]

df$speakerparty2 <- ifelse(df$speechdate < "2015-10-19", paste0(df$speakerparty,"HARPER"), paste0(df$speakerparty,"TRUDEAU"))
df <- left_join(df,to_plot %>% select(Province,riding2),c("speakerriding"="riding2"))
df <- df %>% filter(!is.na(speechtext))
df <- df %>% filter(!is.na(Province))

df$Province <- ifelse(df$Province=="Quebec","Quebec", "ROC")

write_csv(df, "data/file_for_doc2vec_v2MEMBERS.csv")
