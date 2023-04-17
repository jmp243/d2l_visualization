# Jung Mee Park
# visualizations
# 2023-04-14

# libraries to load
bc_1 <- `Background Context` %>% 
  ggplot(aes(x = COLLEGE_NAME, y = `No D2L Site (Perc.)`, color = Term)) +
  geom_point() + 
  geom_line(aes(group = Term))

bc_1+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Background content course level data
# group by bucket and then do a facet wrap
bc_cl  <- `Background Context Course Level` %>%  
  ggplot(aes(x = COLLEGE_NAME, y = `No D2L Site (Perc.)`, color = BUCKET_LEVEL)) +
  geom_point() 

bc_cl + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
