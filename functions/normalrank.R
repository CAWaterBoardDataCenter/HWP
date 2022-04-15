#### Ranks values & normalizes them between 0 and 1

normalrank <- function(x) {
  ranks <- rank(x, na.last = "keep")
  (ranks - 1)/ (max(ranks, na.rm=TRUE) - 1)
}
