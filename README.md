Tidy Tuesday: PhDs Awarded by Field
================
February 19, 2019

-   [Question: How long does it take to earn a doctorate degree?](#question-how-long-does-it-take-to-earn-a-doctorate-degree)
    -   [Data Cleaning](#data-cleaning)
    -   [Data Visualization](#data-visualization)

This data cleaning exercise was inspired by Tidy Tuesday (<https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-02-19>). The dataset can be downloaded from <https://ncses.nsf.gov/pubs/nsf19301/data>.

Question: How long does it take to earn a doctorate degree?
===========================================================

This week's Tidy Tuesday exercise linked to many datasets. The one used here is: "Median years to doctorate, by broad field of study: Selected years, 1992–2017". It reports the median number of years it takes to receive a doctorate, broken down by broad field of study, for selected years between 1992 and 2017. Years to doctorate is measured in three ways: 1) years since receiving a bachelor's degree, 2) years since starting the graduate program, and 3) years since starting the doctoral program. Median years since starting the doctoral program is only reported at the last timepoint (2017), so we'll ignore that data.

``` r
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(ggplot2)
theme_set(theme_light())
```

Data Cleaning
-------------

``` r
yr_to_phd <- read_xlsx("sed17-sr-tab031.xlsx", skip = 3)

head(yr_to_phd)
```

    ## # A tibble: 6 x 7
    ##   `Field of study a~ `1992.0` `1997.0` `2002.0` `2007.0` `2012.0` `2017.0`
    ##   <chr>              <chr>    <chr>    <chr>    <chr>    <chr>       <dbl>
    ## 1 All fields         <NA>     <NA>     <NA>     <NA>     <NA>        NA   
    ## 2 Since bachelor's   10.6     10.6     10.3     9.5      9.0          8.80
    ## 3 Since starting gr~ 8.7      8.7      8.6      7.9      7.7          7.50
    ## 4 Since starting do~ na       na       na       na       na           5.80
    ## 5 Life sciencesb     <NA>     <NA>     <NA>     <NA>     <NA>        NA   
    ## 6 Since bachelor's   9.5      9.4      9.0      8.6      8.5          8.30

The following problems are immediately apparent:

1.  The "field of study and time to degree" column contains two types of information:

-   Field of study (e.g, "All fields", "Life sciences"). Rows with this information contain no other data across the other columns.
-   Time to degree ("since bachelor's", "since starting graduate school", etc.). These rows contain the data, but no information about field of study.

The Field of study and Time to degree information needs to be moved to the same row. It could also use some general tidying (misspellings, etc.)

``` r
yr_to_phd_clean <- yr_to_phd %>% 
  rename(field_time = `Field of study and time to degree`) %>% 
  # Use field_time to create two columns: One for field of study and another for measurement period.
  # If field_time begins with "Since", we know it contains information about the period
  # If field_time does not begin with "Since", we know it contains information about the field of study
  mutate(
    field = ifelse(str_detect(field_time, "^Since"), NA, field_time),
    period = ifelse(str_detect(field_time, "^Since"), field_time, NA)) %>% 
  # Copy the field of study information into all empty cells below it, 
  # stopping when you reach the next non-empty cell
  fill(field, .direction = "down") %>% 
  # The rows that do not have information about the period contain no data - we can remove them
  # (You can see this in the earlier peek at the dataset - these are the rows that are all NAs)
  filter(!is.na(period)) %>% 
  # Convert fields with strings to factors, and correct spelling as needed
  mutate(
    field = as_factor(field),
    field = fct_recode(field,
                       "Life sciences" = "Life sciencesb",
                       "Other" = "Otherc"),
    period = as_factor(period),
    period = fct_recode(period,
                        "Since completing bachelor's" = "Since bachelor's",
                        "Since starting doctoral program" = "Since starting doctoral programa")
  ) %>% 
  # Remove field_time (it's now redundant) and arrange the variables in a nice order
  select(field, period, everything(), -field_time)

yr_to_phd_clean %>% select(1:4) %>% head()
```

    ## # A tibble: 6 x 4
    ##   field         period                          `1992.0` `1997.0`
    ##   <fct>         <fct>                           <chr>    <chr>   
    ## 1 All fields    Since completing bachelor's     10.6     10.6    
    ## 2 All fields    Since starting graduate school  8.7      8.7     
    ## 3 All fields    Since starting doctoral program na       na      
    ## 4 Life sciences Since completing bachelor's     9.5      9.4     
    ## 5 Life sciences Since starting graduate school  7.9      7.7     
    ## 6 Life sciences Since starting doctoral program na       na

1.  The dataset should be reshaped to move informative data out of the column names (1992, 1997, etc.) and into the dataset (e.g., a "year" column containing 1992, 1997, etc.)

``` r
yr_to_phd_long <- yr_to_phd_clean %>% 
  gather(year, duration, -field, -period) %>% 
  mutate(
    year = as.integer(year),
    duration = as.numeric(duration)
  )

yr_to_phd_long %>% head()
```

    ## # A tibble: 6 x 4
    ##   field         period                           year duration
    ##   <fct>         <fct>                           <int>    <dbl>
    ## 1 All fields    Since completing bachelor's      1992    10.6 
    ## 2 All fields    Since starting graduate school   1992     8.70
    ## 3 All fields    Since starting doctoral program  1992    NA   
    ## 4 Life sciences Since completing bachelor's      1992     9.50
    ## 5 Life sciences Since starting graduate school   1992     7.90
    ## 6 Life sciences Since starting doctoral program  1992    NA

Data Visualization
------------------

Let's take a look at doctoral program length by field of study. Which fields have shorter doctoral programs? Longer doctoral programs?

``` r
yr_to_phd_long %>%   
  filter(period == "Since starting graduate school" & field != "All fields") %>% 
  group_by(field) %>% 
  summarize(avg_duration = mean(duration)) %>% 
  mutate(field = fct_reorder(field, avg_duration)) %>% 
  ggplot(aes(field, avg_duration)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  coord_flip() +
  labs(
    title = "Time to Doctorate Since Starting Graduate School",
    subtitle = "Selected years: 1992-2017",
    x = "Field of Study",
    y = "Time to to Doctorate (Years)")
```

![](time-to-phd_files/figure-markdown_github/Graph%20Duration%20by%20field-1.png)

Education, humanities, and social sciences appear to have longer doctoral programs, while math, life sciences, engineering, and social sciences have shorter programs. A footnote in the original data file indicates that the "Other" catgory is composed of non-science and engineering fields, so it's not suprising that the duration is similar to other humanities. We do have over 10 years of data here, however, so it would be interesting to see how the program durations many have changed over time.

``` r
yr_to_phd_long %>% 
  filter(period == "Since starting graduate school" & field!="All fields") %>% 
  ggplot(aes(year, duration, colour = fct_reorder2(field, year, duration))) +
  geom_line() + 
  scale_x_continuous(expand = c(0,0), breaks = seq(1992,2017,5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 16)) +
  guides(color = guide_legend(title = "Field of Study")) +
  labs(
    title = "Time to Doctorate Since Starting Graduate School",
    x = "Year",
    y = "Time to to Doctorate (Years)")
```

![](time-to-phd_files/figure-markdown_github/Graph%20Duration%20by%20field%20over%20time-1.png)

All fields appear to have at least slight decreases in length. The field with the largest change is education. At its peak in 1997, the reported median program length duration was almost 16 years. Since then, the duration has decreased to 12 years, whch is still two years more than any other field.

(Including this many factor levels on a line graph can be a little messy. The fct\_reorder2 function helps ensure that the factors in the key are listed in the same order as they appear in the graph, making it much easier to read.)

So far we've been looking at program duration, or number of years to receive doctorate since starting a graduate program. The dataset also provides the number of years to receive a doctrate since receiving a bachelor's degree. While there is no data on what the students were doing in the years between receiving a bachelor's degree and entering a graduate program, it could be interesting to look at whether students in some areas take more time between programs than others.

``` r
yr_to_phd_long %>%   
  filter(period %in% c("Since completing bachelor's", "Since starting graduate school") & 
           field != "All fields") %>% 
  group_by(field, period) %>% 
  summarize(duration = mean(duration)) %>% 
  ungroup() %>% 
  mutate(field = fct_reorder(field, duration),
         period = fct_reorder(period, duration)) %>% 
  ggplot(aes(x=field, y=duration, fill = period)) +
  geom_col(position="dodge")+
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,18,2), limits = c(0, 18)) +
  coord_flip() +
  guides(fill = guide_legend(reverse=T, title = "Measurement period")) +
  labs(
    title = "Time to Doctorate",
    subtitle = "Selected years: 1992-2017",
    x = "Field of Study",
    y = "Time to to Doctorate (Years)")
```

![](time-to-phd_files/figure-markdown_github/Graph%20Gap%20years%20v1-1.png)

Both measurements have similar trends. Fields in which people take longer to complete a graduate program are also fields in which people tend to take more time between completing a bachelor's degree and starting a graduate program. Students in the life sciences, math, engineering, and physical sciences especially tend to start graduate programs soon after completing undergrad. Doctorate students in education tended to take longer breaks - perhaps they were more likely to gain work experience before starting graduate school (as teachers or administrators). Maybe these students also take longer to complete their graduate programs because they's more likely to be studying on a part-time basis.

Here's another way to present the same information. This graph stacks bars instead displaying them side-by-side. The dataset doesn't directly contain a measure of the number of years betwen completing a bachelor's degree and starting graduate school, so we'll need to create that.

``` r
yr_to_phd_long %>%   
  spread(period, duration) %>% 
  mutate(
    `Years between bachelor's and starting graduate school` = 
      `Since completing bachelor's`-`Since starting graduate school`) %>% 
  gather(period, duration, -field, -year) %>% 
  filter(period %in% c("Since starting graduate school", 
                       "Years between bachelor's and starting graduate school") & 
           field != "All fields") %>% 
  mutate(
    period = ifelse(str_detect(period, "^Years"), 
                    "Pre-graduate program", 
                    "In graduate program")) %>% 
  group_by(field, period) %>% 
  summarize(duration = mean(duration)) %>% 
  ungroup() %>% 
  mutate(field = fct_reorder(field, duration),
         period = fct_reorder(period, -duration)) %>% 
  ggplot(aes(x=field, y=duration, fill = period)) +
  geom_col()+
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,18,2), limits = c(0,18)) +
  coord_flip() +
  guides(fill = guide_legend(reverse=T, title = "Period")) +
  labs(
    title = "Time to Doctorate After Receiving Bachelor's Degree",
    subtitle = "Selected years: 1992-2017",
    x = "Field of Study",
    y = "Time to to Doctorate (Years)")
```

![](time-to-phd_files/figure-markdown_github/Graph%20gap%20years%20v2-1.png)

I think the extra data wrangling was worth it - this graph seems the more easily interpretable of the two.
