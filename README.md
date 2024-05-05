# Best-Sales-Method-Analysis-Project-In-R-Programming-
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/b5fb6d8f-5763-4cd2-908a-41ebb99f9602)

# Product Sales Analysis Project For Data Analyst Professional

![](images/clipboard-2855555299.png)

### Using the validation criteria, the following validation was made:

-   week: 6 unique values, without any missing data.

-   sales_method: had 5 unique values before validation: Email, Call, Email + Call, em + call, and email, which after validation were Email, Call, and Email + Call.

-   customer_id: 15,000 unique values. Needed no cleaning.

-   nb_sold: 10 unique values, no cleaning required and no missing values.

-   revenue: had 1074 missing values, of which the rows were removed from the data set.

-   years_as_customer: had two major values not corresponding: 47 and 63 which were way more than the number of years Pens and Printers has been in existence, 39 years. It made no sense having a customer when the business was not in existence. These rows were dropped.

-   nb_site_visits: Needed no cleaning.

-   state: Needed no cleaning too. At the end of the validation and cleaning process, the data that remained is 13,924 rows and 8 columns

## Data Import, Validation, Cleaning, and Exploration

```{r warning=FALSE, message=FALSE}
#import Libraries
library(tidyverse)
library(janitor)
library(DataExplorer)

```

```{r warning=FALSE, message=FALSE}
sales <- read_csv("product_sales.csv")
head(sales)
dim(sales)
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/08d70df7-f81e-4896-a3ce-455596f776cd)

The data set contains \*15,000 rows/observations and 8 columns/features before the cleaning and validation process.

```{r warning=FALSE, message=FALSE}
#check duplicates
sum(duplicated(sales$customer_id))#no duplicates
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/13d97f12-fdfe-461a-a756-1b0253f822a3)

```{r warning=FALSE, message=FALSE}
#Check mising values
colSums(is.na(sales)) %>% as.data.frame()#1074 missing values in #revenue column
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/d05b90c3-df90-4898-94bd-731e4bf63d92)

```{r warning=FALSE, message=FALSE}
#Remove Missing values
sales <- na.omit(sales)
```

```{r warning=FALSE, message=FALSE}
colSums(is.na(sales))#No Missing values

```

```{r warning=FALSE, message=FALSE}
names(sales)
#Remove Column customer_id
sales$customer_id <- NULL
```

```{r warning=FALSE, message=FALSE}
# week: 6 unique values, without any missing data.
unique(sales$week)
```

```{r warning=FALSE, message=FALSE}
# sales_method: had 5 unique values before validation: Email, Call, Email + Call, em + call, and email, which after validation were Email, Call, and Email + Call.

unique(sales$sales_method)
```

```{r warning=FALSE, message=FALSE}
#Sales method now has 3 uniques values as per description
sales <- sales %>% 
     mutate(
       sales_method=ifelse(sales_method=="em + call","Email + Call",
                     ifelse(sales_method=="email","Email", sales_method)      )
     )

#Then we Check
unique(sales$sales_method)
```

```{r  warning=FALSE, message=FALSE}
# years_as_customer: had two major values not corresponding: 47 and 63 which were way more than the number of years Pens and Printers has been in existence, 39 years. It made no sense having a customer when the business was not in existence. These rows were dropped.
summary(sales$years_as_customer)
boxplot(sales$years_as_customer)
sales1 <- sales %>%  filter(!years_as_customer>39)#remove outliers(47,63)

```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/2e88eeb2-f653-49e5-a1dd-ab2fe844c2d6)

**Our Data is Clean Now**

```{r warning=FALSE, message=FALSE}
sales_clean <- sales1
sales_clean$sales_method <- as.factor(sales_clean$sales_method)
glimpse(sales_clean)
#Reorder labels
levels(sales_clean$sales_method) #<- c("Email","Call" ,"Email + Call")
```

# Back to the Business Objectives

## **The Business goals**

1.  How many customers were there for each approach?

2.  What does the spread of the revenue look like overall? And for each method?

3.  Was there any difference in revenue over time for each of the methods?

4.  Based on the data, which method would you recommend we continue to use?

## **The Business Metrics**

## **The Recommendations**

### 1. How many customers were there for each sales method/ approach?

```{r warning=FALSE, message=FALSE}
theme_set(theme_test())
sales_clean %>% 
  ggplot(aes(x=fct_infreq(sales_method)))+
  geom_bar(fill=rainbow(3))+
   geom_text(aes(label=after_stat(count)),                      
     stat='count',
    position=position_dodge(1.0),
     vjust= -0.4, 
     size=3)+
  theme(legend.position = 'bottom')+
   labs(
     x='Sales Approach',
     y='Sum of Each Approach',
     title = 'Customer per Sales Method'
   ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/8e8d0603-4c73-4fa6-a9e2-2fa8c6a018ff)

The **Email** sales method has the vast majority of **6921** customers, followed by **Call** & Combination of **Email and Call, with the respective count of 4780 and 2223.**

### 2.What does the spread of the revenue look like overall? And for each method?

```{r}
 sales_clean %>% 
  ggplot(aes(revenue, fill=sales_method))+
  geom_density()+
   ggtitle('Revenue Distibution')

```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/da18383e-303f-4cb2-adcd-654af997c09c)

**Calls** is associated with lower Revenues in comparison to other methods. Both **Email** and (**Email+Call**) generates more revenues

```{r warning=FALSE, message=FALSE}
library(patchwork)
 sales_clean %>% 
  ggplot(aes(revenue, fill=sales_method)) +
  geom_boxplot() +
  facet_wrap(~sales_method, scales = "free")+
   theme(legend.position = 'bottom')+
   ggtitle('Revenue And Sales Method')

```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/2ead604f-09e0-43eb-a6b8-e295a9bdb4d4)

Low revenues were majorly generated by the calls method. This can be clearly seen on the Call graph above, with revenue ranging between 0-70

Email sales_approach generated revenues in the  range of 80-120, with larger values start from 130-150.

Both (Email + Call) generate higher revenues ranging from 120-240 as seen from the histogram and boxplot for Email + call.

### 3.Was there any difference in revenue over time for each of the methods?

```{r warning=FALSE, message=FALSE}
d <- sales_clean %>% 
    group_by(sales_method) %>% 
  summarise(Total_Revenue=sum(revenue)) %>% 
  arrange(desc(Total_Revenue))

DT::datatable(d)
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/f1c4b643-12fc-404c-94b7-68f9ab9cdf26)

```{r warning=F, message=FALSE}
library(scales)
d %>% 
  ggplot(aes(x=reorder(sales_method, desc(Total_Revenue)),
             y=Total_Revenue, fill=rainbow(3)))+
  geom_col()+
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(
    aes(label=Total_Revenue),
    position = position_stack(0.5),
    hjust=1,
    vjust=-.3
  )+
  xlab(' ')+
   ggtitle('Total Revenue Per Sales Method')
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/e0628efe-770d-47b7-983c-f2dcac6ee208)

```{r warning=FALSE, message=FALSE}
library(scales)
d1 <- sales_clean %>% 
     group_by(week) %>% 
       summarize(Total_revenue=sum(revenue)) %>% 
       arrange(desc(Total_revenue))

DT::datatable(d1)
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/97edf129-1ad1-4de0-a6d6-a889bb0cd331)

```{r}
d1 %>% 
  ggplot(aes(x=fct_infreq(reorder(week, Total_revenue)),
             y=Total_revenue,  fill='orange')) +
  geom_col() +
  scale_y_continuous(labels=scales::comma)+
  geom_text(aes(label=Total_revenue),
            position = position_stack(1),
           # hjust=1,
            vjust=-.2)+
  theme(legend.position = 'none')+
  xlab('Weekly Sales Total')+
  ggtitle("Total Sales Per Week")
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/6b303298-90d8-4574-92e8-38f5f9f3dd8f)

### 4.Based on the data, which method would you recommend we continue to use?
```{r warning=FALSE, message=FALSE}
sales_clean %>% 
  ggplot(aes(x=week, y=revenue, color=sales_method))+
  geom_line()
  # facet_wrap(~sales_method, scales = "free")
```
![image](https://github.com/LangatErick/Best-Sales-Method-Analysis-Project-In-R-Programming-i/assets/124883947/411c6962-9d01-4c9c-b815-000b6269003e)

## **The Business Metrics**

Based on the analysis, I recommend discontinuing the Calls method and focusing only on the Email and Email + Call sales method. This is due to the higher sales and revenue generated by these approaches, as well as the shorter average time required per sale compared to calls approach which is (30 minutes). However, the Calls approach can still be used on condition the customer doesn't have an email address.

## **The Recommendations**

**Based on the analysis conducted using the provided data, the following recommendations are proposed:**

-   Monitor key metrics to track any changes in the sales approach.

-   It is recommended to utilize the Email method frequently to inform customers about new products. Additionally, follow-up calls in the second and third week can be made to discuss their needs and how the new product will assist them.

-   It is advisable to minimize the usage of the Call method or eliminate it altogether. This approach consumes more time for sales and ultimately generates the lowest revenue, despite having a higher number of sales.

-   The sales team should prioritize the Email and Email + Call approaches. As demonstrated in analysis, the Email sales approach yields the highest revenue during the initial three weeks, although it declines as the week progresses. To enhance sales and generate more revenue, a follow-up call should be made in the second or third week.

-   To broaden the customer segment, focus on enhancing marketing strategies and improving the conversion rate based on website visits. As indicated in the correlation graph, the longer customer tenure corresponds to lower revenue. To address this, onboard new customers and establish customer retention initiatives to boost sales and revenue from both new and existing customers.

-   Ensure accurate data collection to facilitate comprehensive analysis, particularly for revenue, which contains numerous missing values.
