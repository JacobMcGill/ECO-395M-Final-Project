# Converting energy data to PCA
baseline = read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/final_data.csv")
pre_pca_total <- baseline %>% 
  select(-1:-5, -9:-10)
pre_pca_gen <- baseline %>% 
  select(16:26)
state_data = baseline %>%
  select (2:5,9) 
#Note to self: Be sure not to include date column in regressions. 
#Lets get started by taking the pca of everyting numeric
PCA_total = prcomp(pre_pca, scale=TRUE, rank=5)
plot(PCA_total)
summary(PCA_total)
#Look at loadings of PCA
gen_summary = PCA_total$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Var')
gen_summary %>%
  select(Var, PC1) %>%
  arrange(desc(PC1))
gen_summary %>%
  select(Var, PC2) %>%
  arrange(desc(PC2))
gen_summary %>%
  select(Var, PC3) %>%
  arrange(desc(PC3))
gen_summary %>%
  select(Var, PC4) %>%
  arrange(desc(PC4))
gen_summary %>%
  select(Var, PC5) %>%
  arrange(desc(PC5))
#Now we're going to merge PCA data
state_pca_total = merge(state_data, PCA_total$x[,1:5], by="row.names") %>%
  select(-1) %>%
  arrange(desc(Month), desc(Year), State)
#Take summary data for regression
state_pca_summary = state_pca_total %>%
  group_by(State) %>%
  summarize(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3),
            PC4= mean(PC4),
            PC5 = mean(PC5))
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC1), y=PC1)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC2), y=PC2)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC3), y=PC3)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC4), y=PC4)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC5), y=PC5)) + 
  coord_flip()
#Now lets focus on just energy generation
PCA_gen = prcomp(pre_pca_gen, scale=TRUE, rank=5)
plot(PCA_gen)
summary(PCA_gen)
#Look at loadings of PCA
gen_summary = PCA_gen$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Var')
gen_summary %>%
  select(Var, PC1) %>%
  arrange(desc(PC1))
gen_summary %>%
  select(Var, PC2) %>%
  arrange(desc(PC2))
gen_summary %>%
  select(Var, PC3) %>%
  arrange(desc(PC3))
gen_summary %>%
  select(Var, PC4) %>%
  arrange(desc(PC4))
gen_summary %>%
  select(Var, PC5) %>%
  arrange(desc(PC5))
#Now we're going to merge PCA data
state_pca_gen = merge(state_data, PCA_gen$x[,1:5], by="row.names") %>%
  select(-1) %>%
  arrange(desc(Month), desc(Year), State)
#Take summary data for regression
state_pca_gen_summary = state_pca_gen %>%
  group_by(State) %>%
  summarize(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3),
            PC4= mean(PC4),
            PC5 = mean(PC5))
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC1), y=PC1)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC2), y=PC2)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC3), y=PC3)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC4), y=PC4)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC5), y=PC5)) + 
  coord_flip()


