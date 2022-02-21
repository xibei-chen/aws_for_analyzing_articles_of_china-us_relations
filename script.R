####################################################################################
##                                                                                ##
##        Analyze Articles in China-US Relations Channel on People.cn             ##
##      Regarding Entities, Key Phrases Detection and Sentiment Analysis          ##
##                     Using AWS Translate and AWS Comprehend                     ##
##                                                                                ##
####################################################################################



########################################################
##                                                    ##
##       Step 1. Set Up AWS and Load R Packages       ##
##                                                    ##
########################################################


#Clear environment
rm(list=ls())

# Install cloudyr
if (Sys.info()["sysname"] == 'Darwin'){
        Sys.setenv(LDFLAGS="-L/usr/local/opt/openssl@1.1/lib",
                   CPPFLAGS="-I/usr/local/opt/openssl@1.1/include",
                   PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig",
                   LIBRARY_PATH=paste(Sys.getenv("LIBRARY_PATH"),
                                      "/usr/local/opt/openssl@1.1/lib",
                                      sep=""))
        dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
        install.packages("xml2", configure.vars='INCLUDE_DIR=/usr/local/opt/libxml2/include/libxml2 LIB_DIR=/usr/local/opt/libxml2/lib/')
        install.packages('curl', lib = Sys.getenv("R_LIBS_USER"))
        install.packages('httr')
        install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
} else { # On Windows
        install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"), INSTALL_opts = "--no-multiarch")
        # if not working use:
        # install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
}

# Set up R w/ AWS
keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
        stop("ERROR: AWS key file not found")
} 

keyTable <- read.csv(keyfile, header = T) # *accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

# Activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 


# Install and load AWS Translate and AWS Comprehend
# install.packages("aws.translate", repos = c(getOption("repos"), "http://cloudyr.github.io/drat"))
# install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))
library("aws.translate")
library("aws.comprehend")

# Load packages necessary for web scraping and data wrangling 
library(rvest)
library(data.table)
library(gsubfn)
library(tidyverse)
# Load packages necessary for data visualization
# install.packages("RColorBrewer")
# install.packages("wordcloud")
# install.packages("wordcloud2")
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)


#########################################################
##                                                     ##
##     Step 2. Scrape all Articles on all Pages of     ##
##           the China-US Relations Column             ##
##             (Title, Date and Content)               ##
##                                                     ##
#########################################################


# Create a function to get content from all the articles in one page
get_one_page <- function(one_page_link) {
        
        webpage <- read_html(one_page_link, encoding = "GBK")
        rel_links <- webpage %>% html_nodes('.mt10 a') %>% html_attr('href')
        links <- paste0('http://usa.people.com.cn', rel_links)
        
        # Create a function to get content from one article page
        get_one_article <- function(one_article_link) {
                
                article_page <- read_html(one_article_link, encoding = "GBK")
                
                title <- article_page %>% html_nodes('h1') %>% html_text()
                content <- article_page %>% html_nodes('#rwb_zw') %>% html_text()
                content <- gsubfn(' |\n|\t', list(' ' = '','\n' = '','\t' = '' ),content)
                date <- article_page %>% html_nodes('.box01 .fl') %>% html_text()
                date <- date %>% substr(1, 11) 
                date <- as.Date(strptime(gsub("\\D","-",date),"%Y-%m-%d"))
                
                return(data.frame('title'= title,'date'= date,'content'= content))
                
        }
        
        # Apply the function to all the article links
        list_of_dfs<- lapply(links, get_one_article)
        return(rbindlist(list_of_dfs))
        
}

# Create links for all the pages of this channel
page_links<- paste0('http://usa.people.com.cn/GB/406587/index',1:7,'.html')

# Get data frame containing all the article content on all pages
list_of_page_dfs<- lapply(page_links, get_one_page)
all_page_df <- rbindlist(list_of_page_dfs)
rm(list_of_page_dfs)



#########################################################
##                                                     ##
##       Step 3. Translate Content of Articles         ##
##    from Chinese to English for Further Analysis     ##
##                with AWS Translate                   ##
##                                                     ##
#########################################################


# Create a function to translate long text
get_translation <- function(long_text){
        long_text = paste(long_text, collapse = '')
        
        # Breaking the input text into character vectors of length.segm characters each
        char.segments <- function(x, segm.length){
                byte.counter <- nchar(x, type = 'bytes')
                f <- c(1, rep(0, segm.length - 1))
                f <- cumsum(rep(f, length.out = byte.counter))
                s <- split(unlist(strsplit(x,'')), f)
                unname(sapply(s, paste, collapse = ''))
        }
        
        five.thousand.byte.chunk <- char.segments(long_text, 5000)
        
        # Iterating through the chunks 
        for (i in 1:length(five.thousand.byte.chunk)) { 
                current_chunk = five.thousand.byte.chunk[i]
                if (current_chunk > "") {  
                        # Some cats so that you can see the chunks and their byte sum
                        
                        translation <- translate(current_chunk, from = "zh", to = "en")
                        translation <- translation[1]
                        
                }
        }
        return(translation)
}

# Add another column for English translations to the data frame
list_of_translations <- lapply(all_page_df$content, get_translation)
all_page_df <- all_page_df %>% mutate(en_translation=list_of_translations)
rm(list_of_translations)



#########################################################
##                                                     ##
## Step 4. Extract Entities, Key Phrases and Sentiment ##
##          of the Text with AWS Comprehend            ##
##                                                     ##
#########################################################


# Add another column for detected Entities to the data frame
list_of_entities <- lapply(all_page_df$en_translation, detect_entities)
all_page_df <- all_page_df %>% mutate(entities=list_of_entities)
rm(list_of_entities)

# Add another column for detected Key Phrases to the data frame
list_of_phrases <- lapply(all_page_df$en_translation, detect_phrases)
all_page_df <- all_page_df %>% mutate(key_phrases=list_of_phrases)
rm(list_of_phrases)

# Create an empty table for the detected entities
df_entities <- data.frame( 'date'= double(),'entities' = character() )

# Create a for loop to extract all the text from detected entities
for(i in 1:length(all_page_df$entities)){
        if (typeof(all_page_df$entities[[i]]) == 'list'){
                newrow <- data.frame('date' = all_page_df$date[i], 'entities' = all_page_df$entities[[i]]$Text)
                df_entities<- rbind( df_entities, newrow )
        }
}

# Create an empty table for the detected key phrases
df_key_phrases <- data.frame( 'date'= double(),'key_phrases' = character() )

# Create a for loop to extract all the text from detected key phrases
for(i in 1:length(all_page_df$key_phrases)){
        if (typeof(all_page_df$key_phrases[[i]]) == 'list'){
                newrow <- data.frame('date' = all_page_df$date[i], 'key_phrases' = all_page_df$key_phrases[[i]]$Text)
                df_key_phrases<- rbind( df_key_phrases, newrow )
        }
}

# Create an empty table for the detected sentiment
df_sentiment <- data.frame( 'date'= double(),
                            'sentiment' = character(),
                            'mixed'=double(),
                            'negative'=double(),
                            'neutral'=double(),
                            'positive'=double() )

# Create a for loop to detect sentiment
for(i in 1:length(all_page_df$entities)){
        if (typeof(all_page_df$entities[[i]]) == 'list'){
                newrow <- data.frame('date' = all_page_df$date[i], 
                                     'sentiment' = detect_sentiment(all_page_df$en_translation[[i]])$Sentiment,
                                     'mixed'= detect_sentiment(all_page_df$en_translation[[i]])$Mixed,
                                     'negative'= detect_sentiment(all_page_df$en_translation[[i]])$Negative,
                                     'neutral'= detect_sentiment(all_page_df$en_translation[[i]])$Neutral,
                                     'positive'= detect_sentiment(all_page_df$en_translation[[i]])$Positive)
                df_sentiment<- rbind( df_sentiment, newrow )
        }
}



#########################################################
##                                                     ##
##          Step 5. Visualize the Analysis             ##
##                                                     ##
#########################################################


# Create a sentiment data frame containing only data from 2020 April to 2021 Jan, calculate average sentiment value per day
df_sentiment_recent<- df_sentiment %>% 
                        filter(date>='2020-04-01') %>% 
                        group_by(date) %>% 
                        summarize(date=date, positive=mean(positive), negative=mean(negative),neutral=mean(neutral)) %>% 
                        unique() %>% 
                        mutate(positive=100*round(positive, digits=2),
                               negative=100*round(negative, digits=2),
                               neutral=100*round(neutral, digits=2)) 

# Create a bar chart analyzing the share of negative and positive sentiment and how sentiment change with time 
df_sentiment_recent %>% ggplot(aes(x=date)) + 
                        geom_col(aes(y=negative,fill="Negative"),  width= 5, alpha=0.6, stat = "identity", position = position_nudge(x = 2)) +
                        geom_col(aes(y=positive, fill='Positive'), width= 5, alpha=0.6, stat = "identity", position = position_nudge(x = 4)) +
                        scale_x_date(date_labels = "%Y %b", limits=c(as.Date('2020-04-01'), as.Date('2021-01-08')))+
                        labs(x=element_blank(),y="Sentiment (%)",title = "Sentiment Analysis (2020 Apr - 2021 Jan)" )+
                        scale_colour_manual(name="Sentiment", values=c(Negative="#f8776c", Positive="#00bec4"))+
                        geom_segment( aes(y = mean( df_sentiment_recent$negative, na.rm = T ), 
                                          yend = mean( df_sentiment_recent$negative, na.rm = T ),
                                          x = as.Date('2020-04-01'),
                                          xend = as.Date('2021-01-08')) , color = '#f8776c',linetype="dashed")+
                        annotate( "text" , y = mean( df_sentiment_recent$negative, na.rm = T )+1, x = as.Date('2021-01-08') , label =  paste0(round(mean( df_sentiment_recent$negative, na.rm = T )),'%') , color = '#f8776c')+
                        geom_segment( aes(y = mean( df_sentiment_recent$positive, na.rm = T ), 
                                          yend = mean( df_sentiment_recent$positive, na.rm = T ),
                                          x = as.Date('2020-04-01'),
                                          xend = as.Date('2021-01-08')) , color = '#00bec4',linetype="dashed")+
                        annotate( "text" , y = mean( df_sentiment_recent$positive, na.rm = T )+1, x = as.Date('2021-01-08') , label =  paste0(round(mean( df_sentiment_recent$positive, na.rm = T )),'%') , color = '#00bec4')+
                        theme_minimal() %+replace% 
                        theme(legend.title=element_blank())


# Create a bar chart analyzing top 10 detected key phrases
df_key_phrases %>% filter(date>='2020-04-01') %>% 
                   group_by(key_phrases) %>% 
                   summarize(freq=n()) %>% 
                   arrange(-freq) %>% 
                   head(20) %>% 
                   ggplot( mapping = aes( x = reorder( key_phrases, freq ), y = freq ) ) +
                   geom_col( fill = '#83c5be' ) +
                   coord_flip() +
                   labs(y="Frequency of Appearance", x=element_blank(), title="Top 20 Key Phrases (2020 Apr - 2021 Jan)")+
                   theme_minimal()


# Create a word cloud analyzing detected entities
df_entities_freq <- df_entities %>% filter(date>='2020-04-01') %>% 
                               group_by(entities) %>% 
                               summarize(freq=n())


wordcloud2(data=df_entities_freq, size=10, shape = 'circle', color='random-dark')

