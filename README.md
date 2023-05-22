# Viz for Social Good project: World Health Organisation 

I made this visualisation for [Viz for Social Good's collaboration with the World Health Organisation](https://www.vizforsocialgood.com/join-a-project/who-2023). The WHO provided a dataset on global disabilities; their report on the data is available [here](https://www.who.int/publications/i/item/9789240063600).

# Submission

<img src = "vis/vfsg_who_gms.png" width="750">

# Anyone can have a disability

1 in 6 people have a significant disability.

Worldwide, there are 1.3 billion people living with disabilities.

People with disabilities are diverse:
* They can be any sex: 44% are male and 56% are female
* They can be any age: 7% are 0 to 14 years old, 63% are 15 to 59 years old, and 30% are at least 60 years old 
* And they live in countries with different income levels: 7% in low income, 40% in lower-middle income, 33% in upper-middle income, and 20% in high income countries

These factors all impact the health inequities experienced by people with disabilities.

Visualisation sources:
* Visualisation by Gabrielle M. Schroeder
* Data source: [World Health Organisation (2021 global disability data)](https://www.who.int/publications/i/item/9789240063600)
* [Viz for Social Good](https://www.vizforsocialgood.com/) volunteer project

# Design discussion 

## Story

I chose to present the key message that I took away from the WHO's VFSG presentation: anyone can have a disability. In particular, disability is (1) common, and (2) impacts people with many different demographics. In other words, people with disabilities are a large and diverse group. I decided to keep the data analysis and statistics fairly simple to focus on this main message. 

As such, I used the demographic information to answer the question, "What are the characteristics of people with disabilities?" (for example, what percentage of people with disabilities are female?) This approach differs from the main visualisations in the [WHO global disability report](https://www.who.int/publications/i/item/9789240063600) (see pages 24-25), which instead answer, "What percentage of each group (e.g., females) has a disability?" Importantly, my approach does not reveal the prevalence within each demographic, which could potentially lead to misinterpretations. For example, someone might assume that disability is less prevalent in high-income countries, even though high-income countries actually have the highest prevalence (see pages 23-24 of the WHO report for a discussion of the driving factors). However, my visualisation can also correct erroneous assumptions. For example, one viewer was surprised that only 30% of people with disabilities are 60+ years old. The prevalence of disability in people 60+ years old is indeed higher than in other age groups, but, because of the global population structure, most people with disabilities are actually under 60 years old. Thus, these statistics can help challenge stereotypes about people with disabilities. Ultimately both of these approaches for summarising and visualising the disability data are insightful, and both are needed to answer different questions. 
 
I also wanted to present this data in a memorable and impactful way. One of my first experiences seeing data visualisation used as a story-telling tool was the [New York Time's article on how race impacts economic mobility](https://www.nytimes.com/interactive/2018/03/19/upshot/race-class-white-and-black-men.html). Rather than just presenting bar charts or a Sankey diagram, the article uses animated points to represent the economic outcomes of individual people. Years later, I still remember that message because the data was connected to individuals. Inspired by that approach, I decided to try capturing the global scale and impact of disabilities by representing every one million people with one small point. Without animations, I don't expect my visualisation to have the same impact as the NY charts, but my hope is that this visualisation will encourage people to sit with the data and mentally grasp the number of people living with disabilities. However, representing the data this way could also lead to accessibility issues if people struggle to perceive the small dots. I aimed to address these issues with other design choices (discussed in the accessibility section below). 

## Accessibility 
