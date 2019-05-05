########################
# Mensabot @ Binzmühle #
########################

#a piece of code by Tobias Füchslin (@tofu89), based on initial code by Ueli Reber (@el_ueli)
#inspired by this great tutorial: http://www.r-datacollection.com/blog/Programming-a-Twitter-bot/
library(rvest)
library(stringr)
library(twitteR)
library(lubridate)

#harvest today's menu from the mensa's website
date <- Sys.Date()#gather today's date
weekday <-  wday(date)#define weekday (1 = Sunday,....7=Saturday)
wochentag <- factor(weekday, levels = 1:7, labels = c("sonntag","montag","dienstag", "mittwoch", "donnerstag", "freitag", "samstag"))#name weekday in German to access proper website

    #create stop variable for weekend
    if (wochentag == "samstag" | wochentag == "sonntag") {
      stop <- 1
      wochentag <- "dienstag"
    } else {
      stop <- 0
    }

URL <- paste0("https://www.mensa.uzh.ch/de/menueplaene/mensa-uzh-binzmuehle/", wochentag, ".html")
menu <- readLines(URL,encoding = "UTF-8")

#get today's carnivore menu location
einfachgut <- grep("einfach gut",menu)+1

#copy the "einfach gut" menu
menu.einfachgut <- menu[einfachgut]
#trim
menu.einfachgut <- str_replace_all(menu.einfachgut, "<br> </p>","")
menu.einfachgut <- str_replace_all(menu.einfachgut, "<br>", " ,")
menu.einfachgut <- str_replace_all(menu.einfachgut, "  ", " ")
menu.einfachgut <- str_replace_all(menu.einfachgut, " ,", ",")
menu.einfachgut <- str_replace_all(menu.einfachgut, "<p>", "")
menu.einfachgut <- str_replace_all(menu.einfachgut, ", Fleisch:",  " |Fleisch:")
menu.einfachgut <- str_replace_all(menu.einfachgut, ", Fisch:",  " |Fisch:")
menu.einfachgut <- str_replace_all(menu.einfachgut, "\\|Fleisch:(.*)", "")#as long as the 140 character limit exists
menu.einfachgut <- str_replace_all(menu.einfachgut, "\\|Fisch:(.*)", "")#as long as the 140 character limit exists
menu.einfachgut <- str_replace_all(menu.einfachgut, ", mit", " &")
menu.einfachgut <- str_replace_all(menu.einfachgut, "mit", "&")
menu.einfachgut <- str_trim(menu.einfachgut, "both")

#get today's vegetarian menu location
vegi <- grep("natürlich vegi",menu)+1
menu[vegi]

#copy the "natürlich vegi" menu
menu.vegi <- menu[vegi]
#trim
menu.vegi <- str_replace_all(menu.vegi, "<br> </p>","")
menu.vegi <- str_replace_all(menu.vegi, "<br>", " ,")
menu.vegi <- str_replace_all(menu.vegi, "  ", " ")
menu.vegi <- str_replace_all(menu.vegi, " ,", ",")
menu.vegi <- str_replace_all(menu.vegi, "<p>", "")
menu.vegi <- str_replace_all(menu.vegi, ", mit", " &")
menu.vegi <- str_replace_all(menu.vegi, "mit", "&")
menu.vegi <- str_trim(menu.vegi, "both")
#menu.vegi <- str_replace_all(menu.vegi, ", Menüsalat", "")

#assemble the tweet
tweet.text <- paste0("🥩 ", menu.einfachgut,"\n","🥗 ",menu.vegi)

#set up Twitter access
source("Credentials.R", encoding = "UTF-8")
setup_twitter_oauth(consumer_key =  twitter_api_key,
                    consumer_secret =  twitter_api_secret,
                    access_token =  twitter_access_token,
                    access_secret =  twitter_access_token_secret)

#send Tweet
if (stop==0){
tweet(tweet.text)
  }

#save today's menu to the log file
log.df <- data.frame(date = Sys.Date(), stop = stop, pacman = NA, menu = menu.einfachgut, tweet = tweet.text)
write.table(log.df, file = "menu_log.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ";")
