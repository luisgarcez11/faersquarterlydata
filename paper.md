---
title: 'faersquarterlydata : an R package to analyze adverse drug reactions in Food and Drug Administration (FDA) database'
tags:
  - R
  - Food and Drug Administration
  - adverse Events
  - signal studies
authors:
  - name: Luis Garcez
    orcid: 0000-0002-8637-7946
    equal-contrib: true
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
affiliations:
 - name: CEAUL – Centro de Estatística e Aplicações, Faculdade de Ciências, Universidade de Lisboa, Portugal
   index: 1
 - name: Instituto de Medicina Molecular João Lobo Antunes, Lisbon, Portugal
   index: 2
date: 6 November 2023
editor_options: 
  markdown: 
    wrap: 72
bibliography: bibliography.bib
---

# Summary

The package *faersquarterlydata* for R programming language provides
easy access and analysis to FDA Adverse Event Report System (FAERS)
database. This database contains information on the reported Adverse
Drug Events (ADRs) in the United States since 2004. The available data
format in FDA website is in XML or ASCII format, and therefore, the
users need to be familiar with creation of relational databases. This
package allows the reading of these files and transform them into
tabular format, computing summary counts and estimating some useful
statistics like the Reporting Odds Ratio (ROR) and Proportional
Reporting Ratio (PRR), and therefore, enabling reproducible research on
this topic. To explain this R package potential, a step-by-step analysis
of reported ADRs from ALS patients is presented. Analysis by sex, age
and year of report were conducted. This analysis provides information to
better manage drug safety concerns of ALS patients. This work describes
the *faersexplorer* package practical use.

# Statement of need

An adverse drug reaction (ADR) is any unwanted or harmful reaction that
occurs in response to a medication or drug. ADRs can range in severity
from mild to severe and can affect any organ system in the body. Some
common examples of ADRs include nausea, vomiting, dizziness, headache,
skin rash, and allergic reactions.

ADRs can occur for a variety of reasons, including individual variations
in drug metabolism, drug interactions, inappropriate dosing, and
underlying medical conditions. ADRs can also be caused by medication
errors, such as prescribing, dispensing, or administration errors.

It is important to note that not all ADRs are necessarily caused by the
drug in question. Some adverse events may be coincidental or related to
other factors, such as underlying disease or environmental factors.

Some reasons why reporting ADRs is so important is related with patient
safety since, by reporting ADRs, healthcare professionals can identify
previously unknown or under-recognized side effects of drugs, which can
help them make more informed decisions about treatment options for their
patients. Also, access to this kind of information have the potential to
improve drug labelling and prescribing information. This information can
help healthcare professionals make better informed decisions about which
drugs to prescribe and how to monitor patients for potential side
effects. Other impact of reporting ADR is that regulators use
information collected through ADR reporting to evaluate the safety and
efficacy of drugs and to make decisions about whether to approve new
drugs or withdraw drugs from the market.

The United States Food and Drug Administration (FDA) Adverse Event
Reporting System (FAERS) is a database that contains reports of adverse
events and medication errors submitted to the FDA by healthcare
professionals, patients, and manufacturers (
@U.S.FoodandDrugAsministrationFDA2023 ). The FAERS database is a
valuable resource since it contains public information on reported ADR
since 2004, accounting for more than 26 million Safety Reports. For each
Safety Report, there is information on key features like ADR
description, patient age, patient sex, date of report, associated
consumed drugs, among others. Given its extension this database is able
to give unique insights on drug safety.

FAERS publishes quarterly on its website new raw data in XML and ASCII
format, which requires the users to be familiar with creation of
relational databases using applications such as ORACLE, Microsoft Office
Access, MySQL, IBM DB2 or SAS analytic tools. This R package is meant to
fill this gap, making R users capable of reading these ASCII files,
computing summary counts and estimating some useful statistics like the
Reporting Odds Ratio. FAERS database has some limitations like
underreporting, incomplete data, duplicated reports, data quality issues
and lack of causality information (i.e. FAERS data does not provide
information on causality or the likelihood that a drug caused an adverse
event).

## Methods

### FAERS database organization

The FAERS database is constituted by XML and ASCII files, which are
released every quarter. On this paper, we focus on reading and analyzing
ASCII files which contain roughly the same information as XML files. The
ASCII files are constituted by 7 text files, each one detailing
information on the Demographics of patients (e.g. age, sex, weight), on
consumed drugs associated with the ADR (e.g. drug name, administration
route, dose), drug indications, the outcome of the adverse reactions,
the ADR description, the report source and the therapy duration.

### Reporting Odds Ratio

The reporting odds ratio (ROR) is a statistical measure that is used to
identify potential signals of adverse drug reactions (ADRs) in
spontaneous reporting databases such as the FDA Adverse Event Reporting
System (FAERS).

The ROR is calculated as the ratio of the odds of reporting a particular
adverse event in a group of interest compared to the odds of reporting
the same event in the comparing group. It is expressed as a number, with
values greater than 1 indicating a higher reporting rate for the adverse
event of interest within the group of interest, and values less than 1
indicating a lower reporting rate.

$$
ROR = \frac{n_{11}/n_{01}}{n_{10}/n_{00}}
$$

Where is the number of specified ADRs in the group of interest, and is
the the number of specified ADRs in the comparing group. Also, is the
number of all other ADRs in the group of interest, and is the the number
of ADRs in the comparing group. The 95% Confidence interval is estimated
as follows:

$$
IC = e^{ln(ROR)±1.96\sqrt{\frac{1}{n_{11}}+ \frac{1}{n_{10}}+\frac{1}{n_{01}} + \frac{1}{n_{00}}}}
$$

In order to calculate this estimate, all the cells (*n~11~, n~01~,
n~10~, n~00~*) have to be higher than 0, and results are not always
reliable if the numbers are low ( @VanPuijenbroek2002 ). ROR calculation
is easy applicable, but its interpretation might be difficult.

### Proportional Reporting Ratio

Other disproportionality method that can be used to access statistical
associations between groups and specific ADRs is Proportional Reporting
Ratio (PRR) (@Evans2001) . This easily interpretable measure compares
the proportion of a specific ADR within a group of interest with the
proportion of the same event out of the group of interest. If the PRR is
2, it means the proportion of a specific ADR is the double within the
group of interest compared to the control group.

### Other measures

Other measures of association between events and group of interest like
the Information Component (@Khaleel2022) and the Chi-squared statistic (
@Yates1934) can also be used.

# Step-by-Step analysis using code examples

### First Step: Unzipping the .zip folders

FAERS database files are typically distributed in .zip files which
contain text files within. In order to facilitate the opening of these
files, we provided here this function:

```{r}
unzip_faerszip(zip_folders_dir= "directory_with_zip_files", ex_dir = "directory_with_text_files")
```

This function takes as an argument (`zip_folders_dir`), a string
specifying the directory which contains the zip files. This directory
should only contain the FAERS zip files of interest. After the function
is called, the ASCII files within the zipped folders will be available
at the specified directory `ex_dir`.For each zip file, seven readable
text files will be exported, and then, can be read as tables. In our
example, we have unzipped all zip files available since 2012. Also, this
function will export a folder named "deleted" containing all the safety
reports that were deleted, in past releases, by FDA or manufacturers for
various reasons including combining cases. This "deleted" folder is made
available since the first quarter of 2019.

### Second Step: Reading and Filtering files

Each quarterly ASCII file will result in seven tables containing diverse
information. In order for the Demographic information and others to be
binded into one single table, and the same for the other types of text
files, the following function is available:

```{r}
als_faers_data <- retrieve_faersascii(ascii_dir = "directory_with_text_files/ascii", drug_indication_pattern =  "Amyothrophic lateral sclerosis|Motor neuron disease", primary_suspect = TRUE, duplicates_dir = "directory_with_text_files/deleted" )
```

This function will take as an argument a string representing the
directory where the text files are (`ascii_dir`). It will return a list
with length equals seven, each list element representing a table with
information on demographics, drug, drug indication, ADR outcome, ADR
description, report source and therapy duration. Optionally, the
`duplicates_dir` argument can be filled with the directory containing
the deleted safety reports identification. This will remove these safety
reports from the returned object.

Also, the package provides the functionality of filtering the list by
patient drug indication or by drug name. This means the returned list
only contains safety reports from patients who have taken a specified
drug name or the drugs taken were indicated for a specified reason. The
sintax used to filter by these elements comes from `stringr` package (
@Wickham2022 ).

In this example, we filtered the safety reports from patients which have
consumed drugs indicated for Amyothrophic Lateral Sclerosis or Motor
Neuron Disease, by filling the argument `drug_indication_pattern`. Also,
by filling the argument `primary_suspect = TRUE`, all the filtered
safety reports are from patients which have consumed drugs indicated for
ALS or MND and these drugs are primary suspects for causing the ADRs.

### Third Step: Unify text files into a single table

In order to merge all these seven tables into one, and therefore, allow
more meaningful analysis, the package makes available the following
function:

```{r}
als_faers_data_unified <- unify_tabular_ascii(ascii_list = faers_data)
```

The merging of all seven tables use mainly the "primaryid" variable as
key (Figure \autoref{fig:faersdatabase}). In the end, each row
corresponds to one adverse reaction and the drug, indication and therapy
information are collapsed into a single cell.

![Figure 1: Detailed example of the keys used to merge the different
tables.](article/faers_database.png){#faersdatabase width="393"}

This function also addresses the possible existence of duplicates, and
therefore, it removes the rows with the same event start date, reporter
country, sex, age, reaction, drug names, indication and primary suspect
drug start date.

### Forth Step: Database description

The filtered database can be described based on demographic information,
drug-related characteristics, ADR description, report source, outcome or
counts based on the date of the event. This description is computed,
partly, by `tableone` package (@anon). The following code was used to
describe the filtered database:

```{r}
summary_faers <- summary_faersdata(faers_data_table)
```

The list `summary_faers` contain some descriptive tables like the number
of ADR by reported date by running `summary_faers$rpt_dt` (Figure 1). It
is clear that most of the ADRs were reported in 2017 until 2023. If one
want to access the most frequent ADR descriptions and its age and weight
mean, has to run `summary_faers$rpt_dt` (Table 2). The most frequent ADR
present in this filtered database are "Death" with 321 events. Also, it
is possible to access the mean age and weight for each one of these
ADRs.

| **event date** | **number of ADRs** | **number of safety reports** |
|----------------|--------------------|------------------------------|
| **1993**       | 9                  | 1                            |
| **1994**       | 2                  | 1                            |
| **1995**       | 1                  | 1                            |
| **1996**       | 7                  | 2                            |
| **1999**       | 2                  | 1                            |
| **2001**       | 1                  | 1                            |
| **2002**       | 1                  | 1                            |
| **2004**       | 2                  | 1                            |
| **2005**       | 1                  | 1                            |
| **2006**       | 35                 | 7                            |
| **2010**       | 6                  | 2                            |
| **2011**       | 3                  | 2                            |
| **2012**       | 15                 | 11                           |
| **2013**       | 86                 | 12                           |
| **2014**       | 30                 | 6                            |
| **2015**       | 60                 | 16                           |
| **2016**       | 41                 | 21                           |
| **2017**       | 339                | 146                          |
| **2018**       | 522                | 266                          |
| **2019**       | 303                | 160                          |
| **2020**       | 216                | 99                           |
| **2021**       | 204                | 117                          |
| **2022**       | 249                | 116                          |
| **2023**       | 42                 | 21                           |
| **(NO DATE)**  | 2798               | 1557                         |

: Table 1: Report date, number of ADR and number of unique safety
reports.

| **pt**                          | number of ADRs | age mean | weight mean |
|---------------------------------|----------------|----------|-------------|
| Death                           | 461            | 66       | 65          |
| **Drug ineffective**            | 186            | 61       | 80          |
| Disease progression             | 173            | 60       | 74          |
| Amyotrophic lateral sclerosis   | 143            | 67       | 75          |
| A sthenia                       | 119            | 63       | 74          |
| Fatigue                         | 118            | 64       | 72          |
| Condition aggravated            | 92             | 65       | 80          |
| Dyspnoea                        | 92             | 67       | 76          |
| Therapeutic response unexpected | 84             | 62       | 80          |
| Nausea                          | 75             | 64       | 78          |
| Muscular weakness               | 74             | 62       | 79          |

: Table 2: ADR reaction, number of reported ADR, Age and Weight mean.

### Fifth Step: Reporting ROR and PRR

The Reporting Odds Ratio and Proportional Reporting Ratio can be
estimated using the data subset, using any binary characteristic present
within the safety report. Here, we calculated the ROR based on Age, Sex
and Year of report. The function `estimate_ror_bygroup` takes as
arguments the table from Step 3, a string representing the binary
variable corresponding to the group of
interest(`group_of_interest_col`), its reference value
(`group_of_interest_ref`) and a string representing the variable
corresponding to the event of interest (`event_of_interest_col`).

#### Age

We estimated the ROR for each ADR based on participants with less or
more or equal than 65 years old, by running the following code:

```{r}
estimate_ror_bygroup(als_faers_data_unified %>% filter(!is.na(age)) %>% mutate(age_r = case_when(age >= 65 ~ ">=65",age < 65 ~ "<65")) %>% (age_r %in% c(">65", "<=65")), group_of_interest_col = "age_r", group_of_interest_ref = ">=65", event_of_interest_col = "pt")
```

In Table 3 , the output shows that "Respiratory failure" is more
reported in patients with more than 65 years old (ROR: 2.94 (1.38-6.28)
and PRR: 2.90 (1.36-6.20). This means the reporting odds of the older
group are 2.94 times higher than the reporting odds of the younger
group, regarding this specific ADR. Also, it means the reported
proportion of this ADR within the group of patients with more than 65
years old is 2.90 times higher than this ADR proportion within the group
of patients with less than 65 years old.

| **pt**                            | **\>=65** | **\<65** | **Total events** | **Other Events\>=65** | **Other events\<65** | **ROR**  | **ROR lower IC** | **ROR upper IC** | **PRR**  | **PRR lower IC** | **PRR upper IC** |
|:----------------------------------|:----------|:---------|:-----------------|:----------------------|:---------------------|:---------|:-----------------|:-----------------|:---------|:-----------------|:-----------------|
| Death                             | 99        | 95       | 194              | 1152                  | 1115                 | 1,01     | 0,75             | 1,35             | 1,01     | 0,75             | 1,35             |
| **Amyotrophic lateral sclerosis** | **57**    | **24**   | **81**           | **1194**              | **1186**             | **2,36** | **1,46**         | **3,83**         | **2,30** | **1,42**         | **3,73**         |
| Nausea                            | 34        | 26       | 60               | 1217                  | 1184                 | 1,27     | 0,76             | 2,13             | 1,26     | 0,75             | 2,12             |
| Drug ineffective                  | 20        | 31       | 51               | 1231                  | 1179                 | 0,62     | 0,35             | 1,09             | 0,62     | 0,35             | 1,1              |
| Dyspnoea                          | 28        | 19       | 47               | 1223                  | 1191                 | 1,44     | 0,8              | 2,59             | 1,43     | 0,79             | 2,57             |
| Asthenia                          | 22        | 24       | 46               | 1229                  | 1186                 | 0,88     | 0,49             | 1,58             | 0,89     | 0,49             | 1,59             |
| Diarrhoea                         | 25        | 18       | 43               | 1226                  | 1192                 | 1,35     | 0,73             | 2,49             | 1,34     | 0,73             | 2,47             |
| Fatigue                           | 19        | 21       | 40               | 1232                  | 1189                 | 0,87     | 0,47             | 1,63             | 0,88     | 0,47             | 1,64             |
| Pneumonia                         | 17        | 21       | 38               | 1234                  | 1189                 | 0,78     | 0,41             | 1,49             | 0,78     | 0,41             | 1,49             |
| **Respiratory failure**           | **27**    | **9**    | **36**           | **1224**              | **1201**             | **2,94** | **1,38**         | **6,28**         | **2,90** | **1,36**         | **6,2**          |
| Disease progression               | 12        | 22       | 34               | 1239                  | 1188                 | 0,52     | 0,26             | 1,06             | 0,53     | 0,26             | 1,07             |

: Table 3: ADR Description, total number of ADRs for patients over 65
regarding that reaction, total number of ADRs for patients equal or
below 65 regarding that reaction, all other ADRs for patients over 65,
all other events for patients equal or below 65, ROR estimate and it 95
confidence interval, PRR estimate and it 95 confidence interval. Only
the most frequent reactions with age available shown.

#### Sex

We estimated the ROR and PRR for each ADR based on patient's sex. The
code used to do it was the following:

```{r}
estimate_ror_bygroup(als_faers_data_unified  %>% filter(sex %in% c("F", "M")), group_of_interest_col = "sex", group_of_interest_ref = "M", event_of_interest_col = "pt")
```

In Table 4, the output shows that "Drug ineffective" (ROR: 3.05
(1.51-6.15); PRR: 3.01 (1.49-6.07))  and "Condition aggravated" (ROR:
1.75 (1.04-2.95); PRR: 1.73 (1.03-2.92)) are more reported in male than
female patients. While, "Nausea" is more reported among female patients
(ROR: 0.35 (0.21-0.59); PRR: 0.35 (0.21-0.59), as well as "Therapeutic
response unexpected" (ROR: 0.57 (0.34-0.96); PRR: 0.57 (0.34-0.97).

|                                     |        |                  |                    |                    |          |                  |                  |          |                  |                  |
|-------------------------------------|--------|------------------|--------------------|--------------------|----------|------------------|------------------|----------|------------------|------------------|
| **pt**                              | **F**  | **Total events** | **Other Events M** | **Other events F** | **ROR**  | **ROR lower IC** | **ROR upper IC** | **PRR**  | **PRR lower IC** | **PRR upper IC** |
| Death                               | 149    | 358              | 1916               | 1569               | 1,15     | 0,92             | 1,43             | 1,13     | 0,91             | 1,41             |
| **Drug ineffecive**                 | **55** | **156**          | **2024**           | **1663**           | **1,51** | **1,08**         | **2,11**         | **1,48** | **1,06**         | **2,07**         |
| Diseaseprogression                  | 49     | 119              | 2055               | 1669               | 1,16     | 0,8              | 1,68             | 1,15     | 0,8              | 1,67             |
| Asthenia                            | 42     | 98               | 2069               | 1676               | 1,08     | 0,72             | 1,62             | 1,08     | 0,72             | 1,62             |
| Amyotrophic lateral sclerosis       | 39     | 96               | 2068               | 1679               | 1,19     | 0,79             | 1,8              | 1,18     | 0,78             | 1,78             |
| Fatigue                             | 48     | 92               | 2081               | 1670               | 0,74     | 0,49             | 1,12             | 0,74     | 0,49             | 1,12             |
| Dyspnoea                            | 26     | 73               | 2078               | 1692               | 1,47     | 0,91             | 2,38             | 1,46     | 0,9              | 2,37             |
| **Nausea**                          | **48** | **69**           | **2104**           | **1670**           | **0,35** | **0,21**         | **0,59**         | **0,35** | **0,21**         | **0,59**         |
| **Condition aggravated**            | **21** | **66**           | **2080**           | **1697**           | **1,75** | **1,04**         | **2,95**         | **1,73** | **1,03**         | **2,92**         |
| **Therapeutic response unexpected** | **34** | **58**           | **2101**           | **1684**           | **0,57** | **0,34**         | **0,96**         | **0,57** | **0,34**         | **0,97**         |
| Aphasia                             | 25     | 54               | 2096               | 1693               | 0,94     | 0,55             | 1,61             | 0,94     | 0,55             | 1,61             |

: Table 4: ADR Description, total number of ADRs for male patients
regarding that reaction, total number ADRs for female patients regarding
that reaction, all other ADRs for male patients, all other events for
female patients, ROR estimate and it 95% confidence interval, PRR
estimate and it 95% confidence interval. Only the most frequent
reactions with sex available shown.

#### Year of report

We estimated the ROR for each ADR based on if the ADR was reported after
2020 or before, in order to access the COVID-19 pandemic effect on
reporting, with the following code:

```{r}
estimate_ror_bygroup(als_faers_data_unified %>% 
filter(!is.na(event_dt)) %>% mutate(event_dt_r = case_when(event_dt >= as.Date("2020-01-01") ~ ">=2020",event_dt < as.Date("2020-01-01") ~ "<2020")), group_of_interest_col = "event_dt_r", group_of_interest_ref = ">=2020", event_of_interest_col = "pt")
```

Before 2020, the ADR "Drug ineffective" (ROR: 0.39 (0.17-0.88); PRR:
0.40 (0.18-0.91) and "Disease progression" (ROR: 0.41 (0.17-0.99); PRR:
0.41 (0.17-1.00)  was more reported than in 2020 or later (Table
4).Also, the ADR "Nausea" was more reported in 2020 and after (ROR: 2.59
(1.36-4.94); PRR: 2.55 (1.34-4.86).

|                               |             |            |                  |                          |                         |          |                  |                  |            |                  |                  |
|-------------------------------|-------------|------------|------------------|--------------------------|-------------------------|----------|------------------|------------------|------------|------------------|------------------|
| **pt**                        | **\>=2020** | **\<2020** | **Total events** | **Other Events \>=2020** | **Other events \<2020** | **ROR**  | **ROR lower IC** | **ROR upper IC** | **PRR IC** | **PRR lower IC** | **PRR upper IC** |
| Death                         | 79          | 167        | 246              | 632                      | 1299                    | 0,97     | 0,73             | 1,29             | 0,98       | 0,74             | 1,29             |
| Amyotrophic lateral sclerosis | 25          | 46         | 71               | 686                      | 1420                    | 1,12     | 0,68             | 1,84             | 1,12       | 0,68             | 1,84             |
| Dyspnoea                      | 12          | 37         | 49               | 699                      | 1429                    | 0,66     | 0,34             | 1,27             | 0,67       | 0,35             | 1,29             |
| Asthenia                      | 10          | 38         | 48               | 701                      | 1428                    | 0,54     | 0,27             | 1,09             | 0,54       | 0,27             | 1,1              |
| **Drug ineffective**          | **7**       | **36**     | **43**           | **704**                  | **1430**                | **0,39** | **0,17**         | **0,88**         | **0,40**   | **0,18**         | **0,91**         |
| Fatigue                       | 13          | 29         | 42               | 698                      | 1437                    | 0,92     | 0,48             | 1,78             | 0,92       | 0,48             | 1,79             |
| Fall                          | 10          | 31         | 41               | 701                      | 1435                    | 0,66     | 0,32             | 1,35             | 0,67       | 0,32             | 1,36             |
| **Nausea**                    | **21**      | **17**     | **38**           | **690**                  | **1449**                | **2,59** | **1,36**         | **4,94**         | **2,55**   | **1,34**         | **4,86**         |
| **Disease progression**       | **6**       | **30**     | **36**           | **705**                  | **1436**                | **0,41** | **0,17**         | **0,99**         | **0,41**   | **0,17**         | **1,00**         |
| Pneumonia                     | 16          | 18         | 34               | 695                      | 1448                    | 1,85     | 0,94             | 3,65             | 1,83       | 0,93             | 3,62             |
| Respiratory failure           | 7           | 26         | 33               | 704                      | 1440                    | 0,55     | 0,24             | 1,27             | 0,56       | 0,24             | 1,29             |

: Table 5: ADR Description, total ADRs reported in 2020 and later
regarding that reaction, total ADRs reported before 2020 regarding that
reaction, all other ADRs reported later than 2020, all other ADRs
reported before 2020, ROR estimate and it 95% confidence interval, PRR
estimate and it 95% confidence interval. Only the ten most frequent
reactions with year reported available shown.

## Limitations

R software might take a long time reading big datafiles such as FAERS
quarterly extracts.  This can be mitigated by only choosing to analyze a
subset of all available datafiles, filtering safety reports associated
with a specific drug name, active ingredient or indication.

One of the major challenges analyzing these datafiles, is that drug
names and other text variables often need to be normalized, due to
﻿unstructured data entry. And therefore, some additional work on this
might be needed. This package tries to mitigate this by filtering
datafiles based on drug name and drug indication text similarity
recurring to R package `stringr` (@Wickham2022) sintax. The estimated
confidence intervals might need to be adjusted for multiple comparisons,
in order to prove more meaningful signal detections.

## Conclusion

This package allows easy access to FAERS quarterly files by R users,
providing them an useful description and estimating the Reporting Odds
Ratio, Proportional Reporting Ratio and other estimates for any event of
interest for any group of interest since FAERS database contains that
information. This package proved to be helpful estimating statistical
measures which may unveil potential or under-recognized side effects of
drugs on patients with ALS, which can be used to improve healthcare
decision-making and monitoring.

## Bibliography
