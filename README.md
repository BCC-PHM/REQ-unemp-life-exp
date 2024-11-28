# Unemployment and Life Expectancy

Modelling the relationship between unemployment and life expectancy at MSOA level.

## Data

-   Life expectancy – All England 2011 MSOAs for years 2016 to 2020 broken down by male and female. Source: ONS
-   Claimant count – Averaged from 2016 to 2020 for all England 2011 MSOAs, broken down by male and female. Source: NOMIS
-   Mid-year population estimate - Averaged from 2016 to 2020 for all England 2011 MSOAs, broken down by male and female.
-   2019 IMD domains: Income, Education, Crime, Housing, Living Environment. Averaged for each MSOA.

## Model

We will fit a linear regression model with the following formula:

LE = c<sub>0</sub> + c<sub>1</sub>Claimant<sub>%</sub> + c<sub>2</sub>(Is Male) + c<sub>3</sub>IMD<sub>Income</sub> + c<sub>4</sub>IMD<sub>Education</sub> + c<sub>5</sub>IMD<sub>Crime</sub> + c<sub>6</sub>IMD<sub>Housing</sub> + c<sub>7</sub>IMD<sub>LE</sub>

Where:

-   LE – Life expectancy (as above)
-   Claimant<sub>%</sub> - Percentage of working age adult population claiming JSA or UC equivalent. This is equal to the average claimant count divided by the working age adult mid-year estimate for each 2011 MSOA.
-   (Is Male) – Binary variable equal to 1 for males and 0 for females. (i.e. female is the reference category)
-   IMD<sub>domain</sub> – IMD score for each of the 2019 IMD domains given above (from 0 to 1)
-   c<sub>i</sub> – Coefficients for each variable.

The modelling will estimate the coefficients c<sub>i</sub> from the data. 
These can be used to interpret the association between each variable and life expectancy. 
For example, once we have calculated c<sub>1</sub> we can say that “For each 1-point increase in claimant count percentage 
there is an associated -c<sub>1</sub> year decrease in life expectancy”.

This modelling will be performed using MSOA 2011 data above for all of England with a sub-analysis restricted to Birmingham MSOAs only.

### License

This repository is dual licensed under the [Open Government v3](%5Bhttps://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) & MIT. All code can outputs are subject to Crown Copyright.
