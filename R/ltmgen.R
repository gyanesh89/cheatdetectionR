#' @export
ltmgen<-function(){
# ?ltm::rmvlogis
#
# diffs <- as.matrix(seq(-2, 2, by = 1))
# discrs <- as.matrix(rep(1, 5))
# params <- cbind(diffs, discrs)
#
# dat <- rmvlogis(1000, params)
#
# mod <- ltm(dat ~ z1)
# mod
#
# factor.scores(mod)

## test design
## 20 question test
# 10 questions hard
# 10 easy

# hard questions diff range 1 -- 3
# easy question diff range -3 -- -1

set.seed(1700)
# hard questions
diffs1 <- as.matrix(runif(10, 1, 3))
diffs2 <- as.matrix(runif(10, -3, -1))
discrs <- as.matrix(rep(1, 20))
params <- cbind(rbind(diffs1, diffs2), discrs)

n1 <- 4000 # control dataset size
n2 <- 200 # experimental dataset size

# nc <- 100 # number of centers
# cheating_pct <- 15

# for now, we have 100 centers
# centers 1 to 85 do not show cheating
# centers 86 to 100 show cheating

scale_factor <- 23

cheating_probvec1 <- c(rep(1, 85), rep(scale_factor, 15)) / sum(c(rep(1, 85), rep(scale_factor, 15)))
cheating_probvec2 <- c(rep(1, 85), rep(scale_factor * 2, 15)) / sum(c(rep(1, 85), rep(scale_factor * 2, 15)))
normal_probvec <- c(rep(scale_factor, 85), rep(1, 15)) / sum(c(rep(scale_factor, 85), rep(1, 15)))

# sampling a single center for cheating
# sample(1:100, 1, prob = cheating_probvec1)

# sampling a single center for non-cheating
# sample(1:100, 1000, replace = TRUE, prob = normal_probvec)

### irt model based simulated data

irt_dat <- rmvlogis(n1, params)

irt_df <- as.data.frame(irt_dat) %>%
  as_tibble() %>%
  mutate(center = sample(1:100, n1, replace = TRUE, prob = normal_probvec))

## all items cheating
sapply(1:n2, function(x) {
  diff_corrects <- rbinom(10, 1, .9)
  easy_incorrects <- rbinom(10, 1, .1)
  respvec <- c(diff_corrects, easy_incorrects)
}) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(center = sample(1:100, n2, replace = TRUE, prob = cheating_probvec2)) -> hard_easy_df

## hard items have cheating info, easy items random answers
sapply(1:n2, function(x) {
  diff_corrects <- rbinom(10, 1, .9) # difficult corrects stays
  easy_incorrects <- rbinom(10, 1, .5) # random answering
  respvec <- c(diff_corrects, easy_incorrects)
}) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(center = sample(1:100, n2, replace = TRUE, prob = cheating_probvec1)) -> hard_random_df


## easy items have cheating info, hard items random answers
sapply(1:n2, function(x) {
  diff_corrects <- rbinom(10, 1, .5) # random answering
  easy_incorrects <- rbinom(10, 1, .1) # easy incorrect stays
  respvec <- c(diff_corrects, easy_incorrects)
}) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(center = sample(1:100, n2, replace = TRUE, prob = cheating_probvec1)) -> random_easy_df


## all random answers
sapply(1:n2, function(x) {
  diff_corrects <- rbinom(10, 1, .5) # random answering
  easy_incorrects <- rbinom(10, 1, .5) # random answering
  respvec <- c(diff_corrects, easy_incorrects)
}) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble()  %>%
  mutate(center = sample(1:100, n2, replace = TRUE, prob = normal_probvec)) -> random_random_df

## combining
bind_rows(
  irt_df,
  hard_easy_df,
  hard_random_df,
  random_easy_df,
  random_random_df
) -> export_rdf

table(random_random_df$center)

irt_df %>%
  mutate(x = center > 85) %>%
  count(x)

hard_easy_df %>%
  mutate(x = center > 85) %>%
  count(x)

hard_random_df %>%
  mutate(x = center > 85) %>%
  count(x)

random_easy_df %>%
  mutate(x = center > 85) %>%
  count(x)
export_rdf[17,13]=NA

return(export_rdf)
}
