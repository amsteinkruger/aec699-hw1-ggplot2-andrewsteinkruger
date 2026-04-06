# Assignment 1


Andrew Steinkruger  
AEC 699  
April 6, 2026

# 1. Introduction and Data

Let’s check out Forest Inventory Analysis data on tree growth in Oregon.

In this exercise, I use four basic geoms:

1.  geom_point
2.  geom_line
3.  geom_rug
4.  geom_function

I did learn something new about ggplot2: geom_function is only a nice
shortcut for univariate functions. With a multivariate function, the
syntax for geom_function becomes unwieldy. In that case, computing a
large set of function values explicitly (rather than implicitly with
geom_function) becomes easier than figuring out the syntax, and then
geom_line or geom_smooth would do just as well.

Anyway, let’s load some packages.

``` r
library(tidyverse) # General
library(gt) # Tables
library(stargazer) # Regression Tables
library(magrittr) # Pipes

color_beav = "#D73F09" # In the absence of a beav theme. 
```

Now, data. We can process some large tables into our estimates of
interest.

``` r
dat_condition =
  "data/OR_COND.csv" %>%
  read_csv %>%
  select(INVYR,
         PLT_CN,
         CONDID,
         OWNGRPCD,
         FORTYPCD,
         SITECLCD)

dat_tree =
  "data/OR_TREE.csv" %>%
  read_csv %>%
  select(INVYR,
         PLT_CN,
         TRE_CN = CN,
         CONDID,
         SPGRPCD)

dat_growth =
  "data/OR_TREE_GRM_ESTN.csv" %>%
  read_csv %>%
  select(INVYR,
         PLT_CN,
         TRE_CN,
         LAND_BASIS,
         ESTIMATE,
         COMPONENT,
         SUBPTYP_GRM,
         REMPER,
         TPAGROW_UNADJ,
         ANN_NET_GROWTH,
         EST_BEGIN,
         EST_END) %>%
  filter(LAND_BASIS == "TIMBERLAND") %>% # Subset to timberland
  filter(SUBPTYP_GRM == 1) %>% # Subset to subplots
  filter(ESTIMATE == "VOLBFNET") %>% # Subset to net board feet
  filter(COMPONENT == "SURVIVOR") %>% # Subset to surviving trees
  select(-ESTIMATE, -COMPONENT, -LAND_BASIS) %>%
  left_join(dat_tree) %>%
  filter(SPGRPCD == 10) %>% # Subset to Douglas fir species
  left_join(dat_condition) %>%
  filter(OWNGRPCD == 40) %>% # Subset to private owners
  filter(FORTYPCD %in% 200:203) %>% # Subset to Douglas fir conditions
  # Board feet/acre
  mutate(EST_BEGIN_ACRE = EST_BEGIN * TPAGROW_UNADJ,
         EST_END_ACRE = EST_END * TPAGROW_UNADJ,
         ANN_NET_GROWTH_ACRE = ANN_NET_GROWTH * TPAGROW_UNADJ) %>% 
  # Board feet/acre by plot
  group_by(INVYR, PLT_CN, REMPER, SITECLCD) %>%
  summarize(EST_BEGIN_ACRE_PLOT = sum(EST_BEGIN_ACRE),
            EST_END_ACRE_PLOT = sum(EST_END_ACRE),
            ANN_NET_GROWTH_ACRE_PLOT = sum(ANN_NET_GROWTH_ACRE)) %>%
  ungroup %>% 
  # Drop outliers
  filter(ntile(ANN_NET_GROWTH_ACRE_PLOT, 100) %in% 2:99) %>% 
  filter(ntile(EST_BEGIN_ACRE_PLOT, 100) %in% 2:99) %>% 
  filter(ntile(EST_END_ACRE_PLOT, 100) %in% 2:99) %>% 
  # Thousands of board feet/acre by plot. Site classes in two bins. 
  mutate(MBF_0 = EST_BEGIN_ACRE_PLOT / 1000,
         MBF_1 = EST_END_ACRE_PLOT / 1000,
         MBF_Annual = ANN_NET_GROWTH_ACRE_PLOT / 1000,
         SITECLCD_Bin = ifelse(SITECLCD < 4, 0, 1)) %>% 
  select(-ends_with("_PLOT")) %T>% 
  # Export
  write_csv("data/dat_assignment1.csv")
```

We can read and tabulate the processed data.

``` r
dat = "data/dat_assignment1.csv" %>% read_csv

dat %>% head %>% gt
```

<div id="avbpijrmab" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#avbpijrmab table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#avbpijrmab thead, #avbpijrmab tbody, #avbpijrmab tfoot, #avbpijrmab tr, #avbpijrmab td, #avbpijrmab th {
  border-style: none;
}

#avbpijrmab p {
  margin: 0;
  padding: 0;
}

#avbpijrmab .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#avbpijrmab .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#avbpijrmab .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#avbpijrmab .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#avbpijrmab .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#avbpijrmab .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#avbpijrmab .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#avbpijrmab .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#avbpijrmab .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#avbpijrmab .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#avbpijrmab .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#avbpijrmab .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#avbpijrmab .gt_spanner_row {
  border-bottom-style: hidden;
}

#avbpijrmab .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#avbpijrmab .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#avbpijrmab .gt_from_md > :first-child {
  margin-top: 0;
}

#avbpijrmab .gt_from_md > :last-child {
  margin-bottom: 0;
}

#avbpijrmab .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#avbpijrmab .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#avbpijrmab .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#avbpijrmab .gt_row_group_first td {
  border-top-width: 2px;
}

#avbpijrmab .gt_row_group_first th {
  border-top-width: 2px;
}

#avbpijrmab .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#avbpijrmab .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#avbpijrmab .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#avbpijrmab .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#avbpijrmab .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#avbpijrmab .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#avbpijrmab .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#avbpijrmab .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#avbpijrmab .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#avbpijrmab .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#avbpijrmab .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#avbpijrmab .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#avbpijrmab .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#avbpijrmab .gt_left {
  text-align: left;
}

#avbpijrmab .gt_center {
  text-align: center;
}

#avbpijrmab .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#avbpijrmab .gt_font_normal {
  font-weight: normal;
}

#avbpijrmab .gt_font_bold {
  font-weight: bold;
}

#avbpijrmab .gt_font_italic {
  font-style: italic;
}

#avbpijrmab .gt_super {
  font-size: 65%;
}

#avbpijrmab .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#avbpijrmab .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#avbpijrmab .gt_indent_1 {
  text-indent: 5px;
}

#avbpijrmab .gt_indent_2 {
  text-indent: 10px;
}

#avbpijrmab .gt_indent_3 {
  text-indent: 15px;
}

#avbpijrmab .gt_indent_4 {
  text-indent: 20px;
}

#avbpijrmab .gt_indent_5 {
  text-indent: 25px;
}

#avbpijrmab .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#avbpijrmab div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th id="INVYR" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">INVYR</th>
<th id="PLT_CN" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">PLT_CN</th>
<th id="REMPER" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">REMPER</th>
<th id="SITECLCD"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">SITECLCD</th>
<th id="MBF_0" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">MBF_0</th>
<th id="MBF_1" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">MBF_1</th>
<th id="MBF_Annual"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">MBF_Annual</th>
<th id="SITECLCD_Bin"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">SITECLCD_Bin</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_right" headers="INVYR">2011</td>
<td class="gt_row gt_right" headers="PLT_CN">4.820267e+13</td>
<td class="gt_row gt_right" headers="REMPER">9.6</td>
<td class="gt_row gt_right" headers="SITECLCD">3</td>
<td class="gt_row gt_right" headers="MBF_0">0.6751257</td>
<td class="gt_row gt_right" headers="MBF_1">2.2952884</td>
<td class="gt_row gt_right" headers="MBF_Annual">0.16876695</td>
<td class="gt_row gt_right" headers="SITECLCD_Bin">0</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="INVYR">2011</td>
<td class="gt_row gt_right" headers="PLT_CN">4.820273e+13</td>
<td class="gt_row gt_right" headers="REMPER">9.9</td>
<td class="gt_row gt_right" headers="SITECLCD">3</td>
<td class="gt_row gt_right" headers="MBF_0">4.5962706</td>
<td class="gt_row gt_right" headers="MBF_1">10.5816417</td>
<td class="gt_row gt_right" headers="MBF_Annual">0.60458293</td>
<td class="gt_row gt_right" headers="SITECLCD_Bin">0</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="INVYR">2011</td>
<td class="gt_row gt_right" headers="PLT_CN">4.820274e+13</td>
<td class="gt_row gt_right" headers="REMPER">9.9</td>
<td class="gt_row gt_right" headers="SITECLCD">2</td>
<td class="gt_row gt_right" headers="MBF_0">12.0469038</td>
<td class="gt_row gt_right" headers="MBF_1">24.9146419</td>
<td class="gt_row gt_right" headers="MBF_Annual">1.29977153</td>
<td class="gt_row gt_right" headers="SITECLCD_Bin">0</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="INVYR">2011</td>
<td class="gt_row gt_right" headers="PLT_CN">4.820275e+13</td>
<td class="gt_row gt_right" headers="REMPER">10.0</td>
<td class="gt_row gt_right" headers="SITECLCD">3</td>
<td class="gt_row gt_right" headers="MBF_0">4.5586696</td>
<td class="gt_row gt_right" headers="MBF_1">6.0840920</td>
<td class="gt_row gt_right" headers="MBF_Annual">0.15254225</td>
<td class="gt_row gt_right" headers="SITECLCD_Bin">0</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="INVYR">2011</td>
<td class="gt_row gt_right" headers="PLT_CN">4.820278e+13</td>
<td class="gt_row gt_right" headers="REMPER">10.3</td>
<td class="gt_row gt_right" headers="SITECLCD">2</td>
<td class="gt_row gt_right" headers="MBF_0">0.3383332</td>
<td class="gt_row gt_right" headers="MBF_1">0.7098767</td>
<td class="gt_row gt_right" headers="MBF_Annual">0.03607219</td>
<td class="gt_row gt_right" headers="SITECLCD_Bin">0</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="INVYR">2011</td>
<td class="gt_row gt_right" headers="PLT_CN">4.820278e+13</td>
<td class="gt_row gt_right" headers="REMPER">9.9</td>
<td class="gt_row gt_right" headers="SITECLCD">3</td>
<td class="gt_row gt_right" headers="MBF_0">16.6759229</td>
<td class="gt_row gt_right" headers="MBF_1">25.8011576</td>
<td class="gt_row gt_right" headers="MBF_Annual">0.92174088</td>
<td class="gt_row gt_right" headers="SITECLCD_Bin">0</td>
</tr>
</tbody>
</table>

</div>

Next, we can visualize the data.

# 2. Visualization and Modeling

## 2.1. Yield

What do the data really show? Yields over intervals.

``` r
vis_1 = 
  dat %>% 
  mutate(INVYR_0 = INVYR - REMPER) %>% 
  rename(INVYR_1 = INVYR) %>% 
  mutate(INVYR_1 = INVYR_1 - INVYR_0,
         INVYR_0 = INVYR_0 - INVYR_0) %>% 
  pivot_longer(cols = c(INVYR_0, INVYR_1, MBF_0, MBF_1),
               names_to = c("VARIABLE", "YEAR"),
               names_sep = "_") %>% 
  pivot_wider(names_from = VARIABLE, values_from = value) %>% 
  ggplot() + 
  geom_point(aes(x = INVYR,
                 y = MBF)) +
  geom_line(aes(x = INVYR,
                y = MBF,
                group = PLT_CN),
            alpha = 0.25) +
  scale_x_continuous(breaks = 0:12) +
  labs(x = "Measurement Year",
       y = "Thousands of Board Feet per Acre") +
  theme_minimal()

ggsave("out/vis_1_assignment1.png",
       vis_1,
       dpi = 300,
       width = 4.5,
       height = 4.5)
```

<img src="out/vis_1_assignment1.png" style="width:66.0%"
data-fig-alt="A plot of yield over measurement years."
data-fig-align="center" />

**Figure 1.** Thousands of board feet per acre for each plot by
measurement year.

## 2.2. Growth

But what do we really care about? Growth, or the change in yield with
time.

``` r
vis_2 = 
  dat %>% 
  ggplot(aes(x = MBF_0,
                 y = MBF_Annual,
                 color = SITECLCD_Bin %>% factor(labels = c("1-3", "4-6")))) + 
  geom_point(shape = 21,
             fill = NA) +
  geom_rug() +
  scale_color_manual(values = c("black", color_beav)) +
  labs(x = "Initial MBF/Acre",
       y = "Annualized Growth in MBF/Acre",
       color = "Site Class") +
  theme_minimal() 

ggsave("out/vis_2_assignment1.png",
       vis_2,
       dpi = 300,
       width = 6,
       height = 4.5)
```

<img src="out/vis_2_assignment1.png" style="width:80.0%"
data-fig-alt="A plot of annualized yield growth over initial yield."
data-fig-align="center" />

**Figure 2.** Annualized growth in yield with respect to initial yield.

## 2.3. Time Series

What if the calendar year matters?

``` r
vis_3 = 
  dat %>% 
  ggplot() + 
  geom_boxplot(aes(x = INVYR %>% factor,
                   y = MBF_Annual,
                   color = SITECLCD_Bin %>% factor(labels = c("1-3", "4-6"))),
               fill = NA) +
  scale_color_manual(values = c("black", color_beav)) +
  labs(x = "Second Measurement Year",
       y = "Annualized Growth in MBF/Acre",
       color = "Site Class") +
  theme_minimal()

ggsave("out/vis_3_assignment1.png",
       vis_3,
       dpi = 300,
       width = 6,
       height = 4.5)
```

<img src="out/vis_3_assignment1.png" style="width:80.0%"
data-fig-alt="A plot of annualized yield growth by calendar year."
data-fig-align="center" />

**Figure 3.** Annualized growth in yield by site class with respect to
calendar years.

It doesn’t really look like the calendar year matters with the
information at hand.

## 2.4. Growth Modeling

To wrap up, let’s fit a simple growth model to the data.

We can first derive a convenient form for the [Ricker
model](https://doi.org/10.1139/f54-039).

Let *N*<sub>*t*</sub> denote yield at time *t*, *r* a growth rate, and
*K* a limiting parameter (not a carrying capacity).

Let *d**N*<sub>*t*</sub> = *N*<sub>*t* + 1</sub> − *N*<sub>*t*</sub>.

$$
\begin{align}
N\_{t + 1} & = N_t e ^ {r(1 - \frac{N_t}{k})} \\
ln(N\_{t + 1}) & = ln(N_t e ^ {r(1 - \frac{N_t}{K})}) \\
ln(N\_{t + 1}) - ln(N_t) & = r - \frac{r}{K}N_t \\
ln(\frac{dN_t}{N_t}) & = r - \frac{r}{K}N_t
\end{align}
$$

We can estimate this expression by ordinary least squares.

Let $Y = ln(\frac{dN_t}{N_t})$, *β*<sub>0</sub> = *r*,
$\beta_1 = \frac{r}{K}$, and *X*<sub>1</sub> = *N*<sub>*t*</sub>.

Then we are estimating
*Y* = *β*<sub>0</sub> + *β*<sub>1</sub>*X*<sub>1</sub> + *ϵ*.

With binned site class *X*<sub>2</sub> ∈ {0, 1} we can instead estimate
*Y* = *β*<sub>0</sub> + *β*<sub>1</sub>*X*<sub>1</sub> + *β*<sub>2</sub>*X*<sub>2</sub> + *ϵ*.

``` r
mod = 
  dat %>% 
  mutate(Y = log(MBF_Annual / MBF_0),
         X_1 = MBF_0,
         X_2 = SITECLCD_Bin, 
         .keep = "none") %>% 
  lm(Y ~ X_1 + X_2,
     data = .)

b_0 = mod$coefficients[[1]]
b_1 = mod$coefficients[[2]]
b_2 = mod$coefficients[[3]]

stargazer(mod, type = "html")
```

<table style="text-align:center">
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Y
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
X_1
</td>
<td>
-0.058<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.003)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
X_2
</td>
<td>
-0.839<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.060)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
-1.703<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.045)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
489
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.465
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.463
</td>
</tr>
<tr>
<td style="text-align:left">
Residual Std. Error
</td>
<td>
0.613 (df = 486)
</td>
</tr>
<tr>
<td style="text-align:left">
F Statistic
</td>
<td>
211.613<sup>\*\*\*</sup> (df = 2; 486)
</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td style="text-align:right">
<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01
</td>
</tr>
</table>

The convenient Ricker transformation unfortunately returns inconvenient
coefficient values.

That aside, things look good! This is a fair model for a first pass.

Let’s visualize the model results and call it a day.

``` r
vis_4 = 
  dat %>% 
  ggplot() + 
  geom_point(aes(x = MBF_0,
                 y = MBF_Annual,
                 color = SITECLCD_Bin %>% factor(labels = c("1-3", "4-6"))),
             shape = 21,
             alpha = 0.50, 
             fill = NA) +
  geom_function(aes(x = MBF_0,
                    color = "1-3"),
                fun = ~ .x * exp(b_0 + b_1 * .x)) +
  geom_function(aes(x = MBF_0,
                    color = "4-6"),
                fun = ~ .x * exp(b_0 + b_1 * .x + b_2)) +
  scale_color_manual(values = c("black", color_beav)) +
  labs(x = "Initial MBF/Acre",
       y = "Annualized Change in MBF/Acre",
       color = "Site Class") +
  theme_minimal()

ggsave("out/vis_4_assignment1.png",
       vis_4,
       dpi = 300,
       width = 6,
       height = 4.5)
```

<img src="out/vis_4_assignment1.png" style="width:80.0%"
data-fig-alt="A plot of annualized yield growth by initial yield with model estimates."
data-fig-align="center" />

**Figure 4.** Annualized growth in yield by site class with model
estimates.

The fit is great for lower initial yields, and not so great for higher
ones.

A next step is to compare these model results to some values in the
literature.

# 3. Conclusion

We learned a little about Douglas fir growth.
