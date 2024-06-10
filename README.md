
# Veteran Discharge Outcomes Analysis

This report explores factors affecting veterans' employment prospects after service using data from the U.S. Department of Veterans Affairs (VA), Bureau of Labor Statistics (BLS), and Integrated Public Use Microdata Series (IPUMS). The report finds that veterans who served between 2000 and 2015 have higher earnings than those who served in other years. Additionally, the report finds that certain occupational categories, such as communications and intelligence specialists, are associated with lower employment rates

## Acknowledgements

 - [U.S. Census Bureau. (n.d.). Veteran Employment Outcomes (VEO). Lehmann Employment Dynamics](https://lehd.ces.census.gov/data/veo_experimental.html)
 - [Bureau of Labor Statistics (BLS). (2023, January 20). Veterans employment in January 2023](https://www.bls.gov/news.release/vet.toc.htm)



## Research Focus

- Which industry grants veterans more lucrative opportunities?
- Does education have a long-term effect on veterans' earnings?
- Which pay grades contribute more to veterans' total earnings?

## Data Source and Schema 

The data is organized into tabulation levels, with each level representing a specific identifier or predictor variable. The key variables include:
- Cohort (year of enlistment)
- Occupation code
- Paygrade level
- Industry
- Education
- Earnings at different time points (y1, y5, y10) after discharge
## Mathematical Modeling

- Linear Regression Model.
- Random Forest Regressor with cross-validation.
- Linear Regression Model with interaction variable.
## Model Inferences


- Veterans in the NAICS industries have a higher median earnings compared to other industries.
- Electrical/Mechanical Equipment Repairers have the highest median earnings among different occupations.
- Veterans with a college degree or higher qualification tend to have higher earnings compared to those with a GED or high school diploma.
- Pay grade level has a positive correlation with earnings. Veterans with higher pay grades tend to have higher median earnings.
