# Analyze Sentiment, Key Phrases and Entities of China-US Relations Articles with AWS Translate and Comprehend
## Introduction
This is a project for my studies at [Central European University](https://www.ceu.edu/). The aim is to demonstrate how we can use some of the machine learning tools offered by [Amazon Web Services (AWS)](https://aws.amazon.com) to do some cool things such as text translation (with [AWS Translate](https://aws.amazon.com/translate/)) and text analytics (with [AWS Comprehend](https://aws.amazon.com/comprehend/)) in R.

As China-US relations have been one of the top real-world topics for quite a long time, and I am not an expert on this topic myself, so I would like to explore a bit about this topic and try to understand the perspective of Chinese media regarding China-US relations.

## Scraping Website
The People’s Daily is the largest and one of the most authoritative newspaper groups in China. So I scraped all of the articles available in China-US relations’ channel on People’s Daily Online and stored the data in tabular form in R.

## AWS Translate and AWS Comprehend
Firsly I used AWS Translate service to translate the text content from Chinese to English. Then I used AWS Comprehend to answer the following questions with visuals:
+ How do the percentages of positive, negative and neutral sentiments look like and how do they change over time?
+ What are the top 20 key phrases in the articles?
+ What are the most frequently detected entities in the articles?

You can read the detailed explanation in this [story](https://medium.com/@xibei_chen/analyze-articles-in-china-us-relations-channel-on-people-cn-b21a08fed220) published on Medium.

## Data Visualizations
<img src="/visuals/sentiment_analysis_over_time_barchart.png" width="600" height="500">
<img src="/visuals/top_20_phrases_barchart.png" width="600" height="500">
<img src="/visuals/entities_detection_wordcloud.png" width="600" height="500">



