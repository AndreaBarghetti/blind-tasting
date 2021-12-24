library(tidyverse)
library("RColorBrewer")

setwd("/Users/andreabarghetti/Desktop/Fun/wine_birthday/")

rating_raw <- readxl::read_xlsx("wine_rating.xlsx", "rating")
sommeliers <- readxl::read_xlsx("wine_rating.xlsx", "sommeliers")
labels <- readxl::read_xlsx("wine_rating.xlsx", "labels")

rating <- rating_raw %>%
  mutate(other = str_split(other, ",")) %>%
  unnest(other) %>%
  mutate(other= case_when(is.na(other) ~ "noother",
                          T~other),
           is_other = case_when(!is.na(other)~"1")) %>%
  spread(other, is_other) %>%
  select(-noother) %>%
  left_join(labels) %>%
  left_join(sommeliers)


# people pay more for what they like more
rating %>%
  ggplot(aes(x=pay, y=grade)) +
  geom_jitter(shape=21, size=3, fill="#FEE0D2") +
  theme_classic() +
  ggtitle("rating VS accepted price") +
  xlab("accepted price") + ylab("rating")

cor(rating$pay, rating$grade)

# people have no clue about the real price
rating %>%
  ggplot(aes(x=price, y=pay)) +
  geom_boxplot(aes(group=price)) +
  geom_jitter(shape=21, size=3, fill="#FEE0D2") +
  scale_x_continuous(breaks = unique(rating$price)) +
  theme_classic() +
  geom_abline(slope = 1,intercept = 0, col="red", linetype="dashed") +
  ylab("accepted price") + 
  xlab("real price") +
  ggtitle("we should only buy the cheap wine")


# how much is wine worth?
rating %>%
  select(name, grade, pay) %>%
  mutate(index = pay/grade) %>%
  ggplot(aes(x=name, y=index)) +
  geom_boxplot() +
  geom_point(col="red", stat="summary")



# how people value countries rather than taste
rating %>%
  filter(!is.na(from)) %>%
  ggplot(aes(x=from, y=grade)) +
  geom_violin() + 
  geom_point(shape=21, size=3, position=position_jitter(width=.3, height = .1), aes(fill=name)) +
  theme_classic() +
  xlab("") + ylab("score") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  ggtitle("We think Chile makes shit wine")


# people ratings over time
rating %>%
  ggplot(aes(x=order, y=grade)) +
  geom_jitter(shape=21, size=3) +
  theme_classic() +
  scale_x_continuous(breaks = unique(rating$order %>% na.omit())) +
  ggtitle("it doesn´t get better with drinking",
          subtitle = paste("pearson:",cor1%>% round(digits = 2)))
cor1 <- cor(rating$order[!is.na(rating$order)], rating$grade[!is.na(rating$order)])


# grade by Wine+label
rating %>%
  ggplot(aes(y=grade, x= paste(wine_name, label))) +
  geom_violin(fill="#FEE0D2") +
  geom_point(position=position_dodge2(width=.2), shape=21, alpha=.5, col="black", fill="black", size=2) +
  geom_point(size=2,shape=23, stat="summary", fill="red") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  xlab("") + ylab("rating") +
  ggtitle("Rating of the wine by bottle", subtitle = "* red dot =mean")

# grade by Wine
rating %>%
  ggplot(aes(y=grade, x= paste(wine_name))) +
  geom_violin(fill="#FEE0D2") +
  geom_point(position=position_dodge2(width=.2), shape=21, alpha=.5, col="black", fill="black", size=2) +
  geom_point(size=2,shape=23, stat="summary", fill="red") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  xlab("") +
  ggtitle("Rating of the wine")

# grades by Sommelier
rating %>%
  ggplot(aes(y=grade, x= name)) +
  geom_violin(trim = T, scale = "count") +
  geom_point(position=position_dodge2(width=.5), size=3, shape=21, aes(fill = wine_name), stat="identity") +
  geom_point(size=2, stat="summary", fill="red", shape=23, stroke=1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  xlab("") +
  ggtitle("Ratings by sommeleir")

# grades by Sommelier on identical wine
rating %>%
  filter(label %in% c("C","G","D","F")) %>%
  ggplot(aes(y=grade, x= name)) +
  geom_point(position=position_dodge2(width=.2), size=3, shape=21, aes(fill = wine_name), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  xlab("") +
  ggtitle("Ratings by sommeleir")

# ranking
rating %>%
  filter(label %in% c("C","G","D","F")) %>%
  group_by(name, wine_name) %>%
  summarise(diff=max(grade)-min(grade)) %>%
  group_by(name) %>%
  summarise(score= sqrt(sum(diff^2))) %>%
  arrange(score) %>%
  ggplot(aes(x=reorder(name,score), y=score)) +
  geom_bar(stat="identity", fill="#A50F15") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=8)) +
  xlab("") + ylab("Inconcistensy") +
  ggtitle("Guessing the identical wines", subtitle = 'Inconcistensy: square root ( sum("difference in score between identical wines" ^ 2) )')
  
  
# heap map and clustering
rating %>%
  select(name,label,grade, wine_name) %>%
  unite(wine_name, wine_name, label, sep= " ") %>%
  spread(wine_name, grade) %>%
  column_to_rownames("name") %>%
  as.matrix() %>% t() %>%
  scale(center = F, scale = F) %>%
  pheatmap::pheatmap()

# heap map and clustering without label var
rating %>%
  group_by(wine_name, name) %>%
  summarise(grade=mean(grade)) %>%
  select(name,grade, wine_name) %>%
  spread(wine_name, grade) %>%
  column_to_rownames("name") %>%
  as.matrix() %>% t() %>%
  scale(center = T, scale = T) %>%
  pheatmap::pheatmap()



# work clouds for wine descriptions
description_words <- c("fruity", "dry", "fullbodied", "tannic", "smoky", "austere", 
                       "earthy", "delicate", "sweet", "buttery", "elegant", "spicy", 
                       "acid", "bitter", "round", "savory", "silky", "sharp", "bloody", 
                       "rich", "alcohol", "bad", "berries", "boring", "fresh", "grassy", 
                       "rough", "smooth", "tasteless", "tropical")

description_df <- rating %>%
  select(name, wine_name,label, all_of(description_words)) %>%
  gather(adjective, value, all_of(description_words)) %>%
  filter(!is.na(value)) %>%
  select(-value)

for (labelx in unique(description_df$label)) {
  description_by_wine <- description_df %>%
    filter(label==labelx) %>%
    group_by(adjective) %>%
    summarise(n=n()) %>% ungroup() %>%
    mutate(freq=n/sum(n))

  png(paste0("plots/wc_",labelx,".png"), width = 20, height = 20, units = "cm", res=300)
  
  wordcloud::wordcloud(words = description_by_wine$adjective, 
                       freq = description_by_wine$n, 
                       min.freq = 0, 
                       random.order=F,
                       random.color =F,
                       max.words =50,
                       scale = c(5,1),
                       rot.per = 0.35,
                       colors=brewer.pal(9, "Reds"),
                       ordered.colors =F)
  
  dev.off()
}



