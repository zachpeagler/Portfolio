# Portfolio Data

This document contains metadata for the various datasets used throughout my portfolio

## LH_demo

This csv contains demographic data for family farms from localharvest.org.
Demographics include farming practices and memberships, but not personal information such as name or contact info.


## LH_prod

This csv contains processed production data from localharvest.org, as of 09/2024.
Each farm has the production of each crop for each season in binomial form (0,1).
Note that this does _not_ represent the amount of each crop that each farm produces, only whether the farm produces the crop or not for any given season.

## LH_dirty

This csv contains unprocessed data from localharvest.org, as of 09/2024.
If you want a challenge or are looking for practice, this is for you.

## LH_crops_cities

This csv contains farm crop production count data by city for the continental 48 states.
Geographical data comes from tigris, and is from 2022.
Note that this does _not_ represent the amount of a crop that each city produces, but the number of farms in a city that produce said crop.

## LH_crops_states

This csv contains farm crop production count data grouped by state for the continental 48 states.
Geographical data comes from tigris, and is from 2022.
Note that this does _not_ represent the amount of a crop that each state produces, but the number of farms in a state that produce said crop.