# Guided Data Cleaning Project in R: NYC Schools Perceptions
## Introduction
Each year the New York City Department of Education organizes one of the largest national surveys on the quality of education of its municipal region. Teachers, parents and students in grades 6 to 12 take the survey to rate their local schools on academic expectations, communication, engagement, and safety and respect. The results of the NYC School Survey account for 10%-15% of a school's Progress Report grade. The objective of the survey is to enhance school environment for all members of the community. In this project, I aim to determine numeric factors which demonstrate a moderate/strong correlation with academic performance. To measure academic performance, I use SAT and AP scores. Since SAT and AP are high school level tests, the focus of my data analysis is solely on the NYC secondary school data. Once I identify characteristics that may explain the variation in the academic performance of the NYC high school students, I examine the distribution of teachers', parents' and students' evaluations of their school environment.
## Key Variables
| Variable | Description |
| :---: | :---: |
| DBN | School identification code (district borough number) | 
| avg_sat_score | Average SAT score at each high school  |
| high_score_percent | Percentage of high-scoring AP exams  |
| gened | General education schools |
| d75 | Schools which serve children with special needs |
| saf_p_11 | Safety and Respect score based on parent responses |
| com_p_11 | Communication score based on parent responses |
| eng_p_11 | Engagement score based on parent responses |
| aca_p_11 | Academic expectations score based on parent responses |
| saf_t_11 | Safety and Respect score based on teacher responses |
| com_t_11 | Communication score based on teacher responses |
| eng_t_11 | Engagement score based on teacher responses |
| aca_t_11 | Academic expectations score based on teacher responses |
| saf_s_11 | Safety and Respect score based on student responses |
| com_s_11 | Communication score based on student responses |
| eng_s_11 | Engagement score based on student responses |
| aca_s_11 | Academic expectations score based on student responses |
| saf_tot_11 | Safety and Respect score based on total responses |
| com_tot_11 | Communication score based on total responses |
| eng_tot_11 | Engagement score based on total responses |
| aca_tot_11 | Academic expectations score based on total responses |
| frl_percent| The percentage of students who receive lunch at a price discount |
| ell_percent| The percentage of students who do not speak English as their first language|
| sped_percent| The percentage of students participating in special education programs |

## Data Cleaning & Analysis
### Data 
I collected the data from the New York City Department of Education and Dataquest. The New York City Department of Education presents the results of its 2011 survey. Dataquest offers a clean version of the demographic and academic performance data -- called *combined* -- which is also extracted from the New York City Department of Education database.

### Data Cleaning
Before I can analyze the NYC school information at hand, I need to improve the quality of the New York City Department of Education data. The New York City Department of Education data comes in the form of two files: the first file contains the survey results for "general education" schools and the second file contains the survey results for District 75 schools. The first step in my data cleaning process is to exclude any non-secondary school information from the two survey data frames. While the process is feasible for the "general education" data frame, it is not workable in the case of the District 75 data frame: the data frame does not indicate if a particular institution is a high school or elementary school. The second type of information I choose to retain from the survey data frames is the set of aggregate scores for each school based on the survey responses. Now that I have extracted relevant survey results to conduct my investigation, I am able to merge clean survey data using  *bind_rows()* function. I call the joint survey data frame *GenedD75*. 

The last step is to incorporate *combined* and *GenedD75* into a single data frame. Aggregating the data into a seperate table facilitates my task of locating variables which display a moderate/strong relationship with academic performance. Since *combined* consists of the demographic and academic performance information, merging it with *GenedD75* by employing the *bind_rows()* function is not an option; however, I can unite the two data frames using **outer joins**. There are three types of outer joins: left join, right join and full join. Executing **left join** preserves all observations in the data frame on the left and abandons observations from the data frame on the right that have no **key** match; executing **right join** preserves all observations in the data frame on the right and abandons observations from the data frame on the left that have no **key** match; executing **full join** preserves all observations from both data frames and substitutes missing variables with "NA". A **key** is a variable that the two data frames share in common. *DBN* is a **key** variable in the case of *combined* and *GenedD75*. Since *combined* comprises of only the high school data, I perform the **right join** operation to merge the two data frames together. 
```
#join relevant NYC school survey data into a single data frame
GenedD75 <- bind_rows(gened,d75)
#capitalize all the letters in "dbn" in NYC school survey data so that we can combine the data frame with "combined"
agg_NYC_school_data <- GenedD75%>%rename(DBN = dbn) %>% right_join(combined, by = "DBN")

```
### Data Analysis

With the clean  data, I produce a correlation matrix to examine whether the NYC student demographics and the community's opinons on their school environment are tied to the academic performance of high school students. 

```
cor_mat <- agg_NYC_school_data%>%select_if(is.numeric)%>%cor(use = "pairwise.complete.obs")
```
Unfortunately, simply generating a correlation matrix is not an efficient way of identifying interesting relationships between academic performance and other variables due to a large number of observations. However, all is not lost as I can convert the correlation matrix into a tibble for *avg_sat_score* and *high_score_percent*. A tibble allows me to pick out only those variables that have a moderate/strong relationship with *avg_sat_score* and *high_score_percent*. 

```
cor_tib <- cor_mat %>% as_tibble(rownames = "variable")

#select variables which are at least moderately correlated with "avg_sat_score"
sat_cors <- cor_tib%>%select(variable,avg_sat_score)%>%filter(avg_sat_score < -0.25 | avg_sat_score > 0.25)%>%filter(variable != "avg_sat_score")
#select variables which are at least moderately correlated with "high_score_percent"
ap_cors <- cor_tib%>%select(variable,high_score_percent)%>%filter(high_score_percent < -0.25 | high_score_percent > 0.25)%>%filter(variable != "high_score_percent")

```

A good way to visualize and present a relationship between two variables is by creating a scatter plot. 
### Moderate/Strong Relationship Between *avg_sat_score* and Other Variables

![](https://i.ibb.co/v12m9zd/sat-scatter-plots-page-001-1.jpg)

![](https://i.ibb.co/FBFDNVS/Sat-Scatter-Plot-2.jpg)

![](https://i.ibb.co/bLNCbzj/Sat-Scatter-Plot-4.png)





An average SAT score in each NYC high school demonstrates either a strong or moderate correlation with 25 variables. Taking a quick glance, it appears that an average SAT score is likely to be higher when more students at a school write the SAT and AP exams. A relatively high number of SAT and AP takers could serve as a sign that a particular school provides an exceptional quality of education. Another interesting piece of observation to point is that the students', teachers' and parents' opinions on the **safety and respect** of their local school illustrate positive relationship with an average SAT score. In other words, if students feel safe and respected, then they are more likely to perform better on the SAT test. 

Surprisingly, there is a negative correlation between the mean SAT score and the percentage of students who receive lunch at a price discount. If the government subsidizes lunch to a large number of students at a specific school, then that school might be located in a neighborhood susceptible to poverty. If that is not the case, then an alternative explanation could be 
that schools do not provide nutritious lunches to students which may hinder their academic performance. On another note, the ratio of Hispanic and black students at a school exhibits a negative relationship with the *avg_sat_score* while the opposite appears to be true at schools which receive higher rates of white and Asian students. The discrepancy in the average SAT score emphasizes the existence of racial inequality that persists in the US to this day. Schools with a higher percentage of black and Hispanic  student minorities tend to obtain less funding while families of those students often find themselves at an economic disadvantage to purchase additional educational resources to assist their children in exceling in academics.  

### Moderate/Strong Relationship Between *high_score_percent* and Other Variables

![](https://i.ibb.co/gZLrRjD/ap-scatter-plots-page-001.jpg)

![](https://i.ibb.co/Pr4bbYL/0002.jpg)

The percentage of high scoring AP exams in each NYC high school displays either a strong or moderate correlation with 24 variables. The results correspond to the correlation findings shown in the *avg_sat_score* scatter plots; however, there is a noteworthy dissimilarity in the NYC school perceptions. The NYC school perceptions display a more significant positive relationship with AP scores at the aggregate level. In particular, when students positively appraise their schools based on **safety and respect**, **engagement**, **communication** and **academic expectations**, then they are more likely to achieve a higher score on the AP examination. 

In my last step of the data analysis, I would like to investigate the distribution of teacher, parent and student evaluations of their school environment. The distribution of the survey scores will offer me a valuable insight into how similar or dissimilar the opinions of the members of the community are to one another. To figure out whether parents, students and teachers share similar views on the four school characteristics, I will construct a boxplot; however, before I can create a proper boxplot, I will need to reshape my data first. My aggregate data frame does not currently contain two variables of interest: metric (safety and respect, engagement, communication and academic expectations) and response type (student, teacher, parent and total). To create the two new variables, I pick out the necessary information from the *survey_question* variable (*saf_p_11, saf_s,11*,etc.) using the *str_sub()* function. I employ the *mutate()* function to join *metric* and *response_type* to my aggregate data frame.

### Distribution of Teacher,Parent and Student Scores for Each Survey Question

![](https://i.ibb.co/b3ZWQdR/Response-type-Boxplot.png)

The boxplot demonstrates substantial differences in parent, teacher and student opinions on the quality of the NYC high schools. Parents are typically content with the quality of the secondary school education while students do not express the same level of satisfaction. The discrepancy in the student and parent scores might come from the fact that students dislike attending school in general; however, at the same time, students rate schools far lower on safety and respect than their parents do. Poor safety and respect scores, which come from students, may suggest that a lot of NYC high school students experience bullying that their parents are unaware of. The last interesting piece of observation to remark about the boxplot is the large distribution of teacher evaluations of school metrics. The wide range of teacher scores may indicate that some teachers might be more engaged in school activities outside the classroom than others.
### Distribution of Total Scores for Each Survey Question
 
![](https://i.ibb.co/PgvcLHp/Total-Scores-Boxplot.png)

The distribution of total scores roughly follows a normal distribution: scores near the mean are more frequent in occurrence than scores far from the mean. At the aggregate level, academic expectations have the highest median score; yet, the median scores across other school metrics are about the same. The distributions of safety and respect and communication are fairly wide relative to those of academic expectations and engagement. There are outliers present in the data but we can conclude that NYC high schools normally receive a score between 6 and 8 on the quality of their enviornment.  

## Conclusion

The NYC schools perceptions and demographic variables demonstrate moderate/strong relationships with the academic performance of the high school students. High schools in black and Hispanic neighborhoods are more likely to see lower average SAT scores relative to schools in wealtier districts since they are generally poorly funded. Furthermore, students are more likely to  succeed academically when the community has positive perceptions about the quality of education and environment at a particular high school. On a scale from 1 to 10, the NYC community rates its high schools from 6 to 8 based on academic expectations, safety and respect, engagement and communication; however, the school evaluations substantially differ between each individual group. 
## References
[New York City Department of Education](https://data.cityofnewyork.us/Education/2011-NYC-School-Survey/mnz3-dyi8)

[Dataquest](https://data.world/dataquest/nyc-schools-data/workspace/file?filename=combined.csv)

## Code

You can access the entire code [here](https://github.com/SkyKnight9/Guided-Data-Analytics-Project-in-R-NYC-Schools-Perceptions/blob/master/R%20Script).
  
