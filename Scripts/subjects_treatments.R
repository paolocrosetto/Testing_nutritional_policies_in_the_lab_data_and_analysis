### count number of subjects per treatment

df %>% 
  select(subject, session_number, treatment) %>% 
  distinct() %>% 
  group_by(treatment) %>% 
  tally() %>% 
  mutate(N = sum(n))
  
