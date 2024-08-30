library(pacman)
p_load(tidyverse, conflicted, janitor, sf, sp, labelled, units)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Loading Upazila data and the outcome data in census

