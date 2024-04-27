# Converting energy data to PCA
baseline = read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/final_data.csv")
pre_pca <- baseline %>% 
  select(-1:-5, -9:-10)
state_data = baseline %>%
  select (2:5,9) 
#Note to self: Be sure not to include date column in regressions
PCA_gen = prcomp(pre_pca, scale=TRUE, rank=5)
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
state_pca = merge(state_data, PCA_gen$x[,1:5], by="row.names") %>%
  select(-1) %>%
  arrange(desc(Year)) %>%
  arrange(desc(Month))
