library(tidyverse)
library(viridis)

R1 <- read_lines("../big-fastq-play/data/Battle_Creek_01_chinook_R1.fq.gz") %>%
  matrix(ncol = 4, byrow = TRUE) %>%
  .[,-3]

colnames(R1) <- c("ID", "seq", "qual")

R1 <- as_tibble(R1)

R1

R1_sep <- R1 %>%
  separate(ID, into = c("id", "part2"), sep = " ") %>%
  separate(
    id, 
    into = c("machine", "run", "flow_cell", "lane", "tile", "x", "y"), 
    sep = ":",
    remove = FALSE
  ) %>%
  separate(
    part2,
    into = c("read", "filter", "cnum", "barcode"),
    sep = ":",
    remove = FALSE
  )
R1_sep

R1_sep <- R1 %>%
  separate(ID, into = c("id", "part2"), sep = " ") %>%
  separate(
    id, 
    into = c("machine", "run", "flow_cell", "lane", "tile", "x", "y"), 
    sep = ":",
    remove = FALSE,
    convert = TRUE
  ) %>%
  separate(part2, into = c("read", "filter", "cnum", "barcode"), sep = ":", convert = TRUE)


R1_sep %>% count("read")


utf8ToInt("!*JGH")


utf8ToInt("!+5?IJ") - 33


R1_sepq <- R1_sep %>%
  mutate(mean_qual = map_dbl(.x = qual, .f = function(x) mean(utf8ToInt(x) - 33)))


ggplot(R1_sepq, aes(x = mean_qual)) +
  geom_histogram(binwidth = 1)         


ggplot(R1_sepq, aes(x = x)) +
  geom_histogram(bins = 500)


ggplot(R1_sepq, aes(x = y)) +
  geom_histogram(bins = 500) +
  coord_flip()



ggplot(R1_sepq, aes(x = x, y = y)) +
  geom_hex(bins = 100) +
  scale_fill_viridis_c()



ggplot(R1_sepq, aes(x = x, y = y, z = mean_qual)) +
  stat_summary_hex(bins = 100) +
  scale_fill_viridis_c()
