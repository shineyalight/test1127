rm(list=ls())
library(tidyverse)
library(ggplot2)
require(openintro)
require(lattice)

mat_comm <- matrix(
  c(9, 10, 13, 4,
    18, 7, 8, 11),
  nrow=4, ncol=2,
  dimnames = list(c("A Community", "B Community", "C Community", "D Community"), c("male", "female"))
)
knitr::kable(mat_comm)

d_comm <- as.data.frame(as.table(mat_comm))
names(d_comm) <- c("community", "sex", "freq")
knitr::kable(d_comm)

# Q1: Plot Barchart by sex.
ggplot(d_comm, aes(x = sex, y = freq, fill = community)) +
  geom_col()

# Q2: Plot Barchart by community.
ggplot(d_comm, aes(x = community, y = freq, fill = sex)) +
  geom_col()

# Q3: Plot Barchart by using dodge. (Hint: position = ‘dodge’).
ggplot(d_comm,aes(y=freq,x=sex,fill=community)) +
  geom_bar(position="dodge",stat="identity")

# Q4: Plot Barchart with text.
# (Hint: Compute percentage, use geom_text() to create text, and sprintf() to set up format for percentage.)

ggplot(d_comm, aes(x = community, y = freq, fill = sex)) +
  geom_col() +






