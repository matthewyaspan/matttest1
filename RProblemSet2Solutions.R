setwd("Downloads/") ## SET THIS TO YOUR WORKING DIRECTORY. THAT DIRECTORY OUGHT TO HAVE `all2001.csv` IN IT!!!
library(tidyverse) 
all.2001 <- read_csv(file.choose())

### Find descriptions of the fields of events files here: https://www.retrosheet.org/datause.txt

all.2001 %>% head()
all.2001 %>% View()

############# EXAMPLES #############

## What Percentage of Events ended in a 3 ball count?

all.2001 %>% summarize(mean(BALLS_CT == 3))

## Percentage of Events with NA Pitch Sequences:

all.2001 %>% summarize(mean(is.na(PITCH_SEQ_TX)))

## What Percentage of Events occurred during plate appearances in which the first pitch was put into play?

all.2001 %>% filter(!is.na(PITCH_SEQ_TX)) %>% summarize(mean(str_sub(PITCH_SEQ_TX, 1, 1) == "X"))

## What percentage of the time does a runner score from third on a given event?

all.2001 %>% filter(!is.na(BASE3_RUN_ID)) %>% summarize(mean(RUN3_DEST_ID >= 4))

############# QUESTION 1 #############

## How many Pinch Hit Home Runs Occurred in 2001?

all.2001 %>% filter(PH_FL) %>% filter(EVENT_CD == 23) %>% nrow()

############# QUESTION 2 #############

## How many events had a pitch sequence that *started* with two consecutive balls and a called strike?

all.2001 %>% filter(str_sub(PITCH_SEQ_TX, 1, 3) == "BBC") %>% nrow()

############# QUESTION 3 #############

## Find the average number of runs scored on plays in which there was an error.

all.2001 %>% filter(ERR_CT > 0) %>% summarize(mean(EVENT_RUNS_CT))

############# QUESTION 4 #############

## Using the 'grepl' function (amongst others), find how many events had a pitch sequence with two consecutive balls and then a swinging strike.

all.2001 %>% filter(grepl("BBS", PITCH_SEQ_TX)) %>% nrow()

############# QUESTION 5 #############

## What player led the league in defensive assists in 2001? (their ID in retrosheet is sufficient.)

# My solution....

all.2001.assists <- all.2001 %>%
  mutate(all.assists=paste0(ASS1_FLD_CD, ASS2_FLD_CD,ASS3_FLD_CD, ASS4_FLD_CD, ASS5_FLD_CD, ASS6_FLD_CD, ASS7_FLD_CD, ASS8_FLD_CD, ASS9_FLD_CD, ASS10_FLD_CD)) %>%
  filter(ASS1_FLD_CD > 0)

assists.df  <- all.2001.assists %>% filter(grepl("1", all.assists)) %>% group_by_at(vars("RESP_PIT_ID")) %>% summarize(!!"assists.1" := n()) %>% rename(fielder.id = 1)

for (i in 2:9) {
  pos.assists.df <- all.2001.assists %>% filter(grepl(i, all.assists)) %>% group_by_at(vars(paste0("POS", i, "_FLD_ID"))) %>% summarize(!!paste0("assists.", i) := n()) %>% rename(fielder.id = 1)
  assists.df <- assists.df %>% full_join(pos.assists.df)
}

assists.df %>% mutate_all(funs(replace(., is.na(.), 0))) %>%  mutate(total.assists = assists.1 + assists.2 + assists.3 + assists.4 + assists.5 + assists.6 + assists.7 + assists.8 + assists.9) %>% arrange(desc(total.assists)) %>%
  slice(1)

# Excellent submission by Brian Bauer

all.2001 %>%
  select(RESP_PIT_ID, POS2_FLD_ID,
         POS3_FLD_ID, POS4_FLD_ID, POS5_FLD_ID, POS6_FLD_ID, POS7_FLD_ID, POS8_FLD_ID,
         POS9_FLD_ID, ASS1_FLD_CD, ASS2_FLD_CD, ASS3_FLD_CD, ASS4_FLD_CD, ASS5_FLD_CD,
         ASS6_FLD_CD, ASS7_FLD_CD, ASS8_FLD_CD, ASS9_FLD_CD, ASS10_FLD_CD) %>%
  filter(ASS1_FLD_CD > 0) %>%
  gather(position,value,ASS1_FLD_CD:ASS10_FLD_CD) %>%     # this turns a row with many columns into many rows with a "single" column
  filter(value > 0) %>%
  mutate(playerID = case_when(
    value == 1 ~ RESP_PIT_ID, value == 2 ~ POS2_FLD_ID, value == 3 ~ POS3_FLD_ID, value == 4 ~ POS4_FLD_ID,
    value == 5 ~ POS5_FLD_ID, value == 6 ~ POS6_FLD_ID, value == 7 ~ POS7_FLD_ID, value == 8 ~ POS8_FLD_ID,
    value == 9 ~ POS9_FLD_ID
  )) %>%
  group_by(playerID) %>%
  summarize(assists = n()) %>%
  filter(assists > 400) %>%
  inner_join(Master, by=c("playerID" = "retroID")) %>%
  select(nameFirst, nameLast, assists) %>%
  arrange(desc(assists))

############# QUESTION 6 #############

## Using the Master table from the Lahman database, the inner_join function, and our all.2001 dataframe,
## find the number of events that had a player whose first name was "Troy" at third base

library(Lahman)
data("Master")
all.2001 %>% inner_join(Master, by=c("POS5_FLD_ID" = "retroID")) %>% filter(nameFirst == "Tony") %>% nrow()

