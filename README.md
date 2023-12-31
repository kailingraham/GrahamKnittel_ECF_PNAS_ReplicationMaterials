# U.S. Employment Carbon Footprints
This online repository contains the data and code necessary to replicate the results from the paper "Assessing the distribution of employment vulnerability to the energy transition using employment carbon footprints" by Graham & Knittel (2024).

## Repository overview & structure
This repository contains the output datafiles from the U.S. Employment Carbon Footprints project conducted by MIT's Center for Energy & Environmental Policy Research (CEEPR). These files represent an open dataset describing the employment vulnerability of U.S. counties as measured by their employment carbon footprint (ECF), a measure of the reliance of jobs in a county on fossil fuels. The ECFs account for emissions across all “scopes” of the energy value chain, including direct fossil fuel combustion as well as the indirect emissions that occur due to electricity consumption and the production of fossil fuels that are burnt elsewhere. The dataset covers the following sectors: agriculture ("ag"), construction ("cn"), manufacturing ("mf"), mining ("mn"), commercial sectors ("comm"), and fossil-fuel power generation ("pwr"). These sectors account for 86% of total U.S. employment and 94% of U.S. carbon emissions outside of the transportation sector.

The repository is structured as follows:
- _Data_: For each sector, contains the raw input data files ("Input"), data-cleaning python scripts ("Scripts"), temporary files ("Temp"), and final clean data files to be used in the rest of the analysis ("Output").
- _Analysis_: This folder contains three sub-folders:
  - _overallFootprintCalc_: this folder contains i) python scripts used to calculate the ECF for each sector-scope-county combination (in "Scripts"), ii) temporary files (in "Temp"), iii) the final ECF data files (in "Output" and described in the "File naming" section), and iv) each of the figures used in the paper (in "Figures").
  - _iraComparison_: contains i) python scripts used to generate comparisons between ECF results and IRA energy communities (in "Scripts"), ii) the raw data files used in this analysis (in "Input"), iii) temporary files (in "Temp"), and iv) each of the figures on the comparison (in "Figures").
  - _explainedVar_: contains i) python scripts used to conduct the explained variance analysis (in "Scripts"), ii) the raw data files used in this analysis (in "Input"), and iii) temporary files (in "Temp").
- _PNAS_figures_tables_: The figures and tables submitted as part of the article submission.

## File formats
Each datafile contains data on total "effective" emissions (including direct and indirect emissions and scaled to avoid double counting across emission "scopes"), employment, population, the "passthrough rate" of emissions along the energy value chain ("rho", used to scale emissions across scopes), the effective emissions per capita, and the effective emissions per employee (i.e. the ECF). Analagous figures for "burden" are reported, which describe the social cost of these emissions using the U.S. EPA's proposed $190/metric tonne social cost of carbon. Datafiles are indexed by county---for those files that consider different sectors and/or scopes, these become secondary and tertiary indices. 

For most fields, a minimum, maximum, and average value are reported based on the range of potential passthrough rates used in the analysis. These are indicated by the suffix of the field i.e. "_min", "_max", "_avg". The average values ("_avg") are used in all CEEPR analysis from this dataset.

## File naming
- ECF_final.csv - describes the overall ECF of each country across all covered sectors and scopes.
- ECF_sector.csv - describes the ECF across all scopes for each covered sector, for each county.
- ECF_scope1.csv - describes the ECF of each county, across all sectors, when only "Scope 1" emissions (those from direct combustion of fossil fuels) are considered.
- ECF_scope2.csv - describes the ECF of each county, across all sectors, when only "Scope 2" emissions (those associated with the consumption of electricity) are considered.
- ECF_scope3.csv - describes the ECF of each county, across all sectors, when only "Scope 3" emissions (those embedded in fossil fuels produced by the county) are considered. Note that the true definition of Scope 3 emissions includes all indirect emissions in a firm's value chain, however for the purposes of this analysis we consider only the emissions embedded in coal, oil and natural gas produced by a firm when determining its Scope 3 emissions.
- ECF_sector_scope.csv - a breakdown of ECFs for each scope within each covered sector.
- ECF_sector_ffextract.csv - county-level ECFs for fossil-fuel extraction sectors only.
- ECF_sector_noextract.csv - county-level ECFs for all other sectors.
