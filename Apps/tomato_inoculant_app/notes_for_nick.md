## Notes for Nick

### 1. Heteroscedasticity
Many of my variables are coming up heteroscedastic, tested with Levene and Bartlett tests. Should I make a glmer with the appropriate distribution then use that to perform a weighted least squares glmer? Or is a Welch's ANOVA appropriate?
I don't know if these analyses account for the *dependence* of our target variables. Because we measured the same plants every time, I think our gsw, PhiPS2, etc are dependent within groups. Although, that is assuming that the state of the plant's photosynthetic system is dependent on the prior state of the plant's photosynthetic system (which is a reasonable assumption, to me)

### 2. Ways to check for continuity
I want to pass variables to plots as a color, where sometimes the variable is discrete and sometimes it's continuous. I know how to implement both discrete and continuous color scales, but not how to *detect* when to do so.