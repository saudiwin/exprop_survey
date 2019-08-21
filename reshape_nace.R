require(readr)
require(dplyr)
require(tidyr)

russian <- read_csv("NACE_REV2_20190802_151433.csv") %>% 
  select(Order,Level,Code,Parent,Description) %>% 
  mutate(level1=if_else(Level==1,Description,NA_character_),
         level2=if_else(Level==2,Description,NA_character_),
         level3=if_else(Level==3,Description,NA_character_),
         level4=if_else(Level==4,Description,NA_character_)) %>% 
  fill(level1,level2,level3,level4,.direction="down") %>% 
  fill(level2,level3,level4,.direction="up") %>% 
  select(matches("level",ignore.case=F)) %>% 
  distinct
spanish <- read_csv("NACE_REV2_spanish.csv") %>% 
  select(Order,Level,Code,Parent,Description) %>% 
  mutate(level1=if_else(Level==1,Description,NA_character_),
         level2=if_else(Level==2,Description,NA_character_),
         level3=if_else(Level==3,Description,NA_character_),
         level4=if_else(Level==4,Description,NA_character_)) %>% 
  fill(level1,level2,level3,level4,.direction="down") %>% 
  fill(level2,level3,level4,.direction="up") %>% 
  select(matches("level",ignore.case=F)) %>% 
  distinct

english <- read_csv("NACE_REV2_english.csv") %>% 
  select(Order,Level,Code,Parent,Description) %>% 
  mutate(level1=if_else(Level==1,Description,NA_character_),
         level2=if_else(Level==2,Description,NA_character_),
         level3=if_else(Level==3,Description,NA_character_),
         level4=if_else(Level==4,Description,NA_character_)) %>% 
  fill(level1,level2,level3,level4,.direction="down") %>% 
  fill(level2,level3,level4,.direction="up") %>% 
  select(matches("level",ignore.case=F)) %>% 
  distinct

write_csv(x = russian,"russian_sector.csv",col_names = F)
write_csv(x=spanish, "spanish_sector.csv",col_names = F)
write_csv(x=english, "english_sector.csv",col_names = F)
