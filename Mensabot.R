########################
# Mensabot @ Binzmühle #
########################

#code by Tobias Fuechslin (@tofu89), adapted from initial code by Ueli Reber (@el_ueli)
#inspired by this great tutorial: http://www.r-datacollection.com/blog/Programming-a-Twitter-bot/
library(stringr)
library(twitteR)
library(lubridate)

#get today's menu from the mensa's website
date <- Sys.Date()#gather today's date
weekday <-  wday(date)#define weekday (1 = Sunday,....7=Saturday)
wochentag <- factor(weekday, levels = 1:7, labels = c("sonntag","montag","dienstag", "mittwoch", "donnerstag", "freitag", "samstag"))#name weekday in German to access proper website

#create stop variable for weekend
if (wochentag == "samstag" | wochentag == "sonntag") {
  stop.day <- 1
} else {
  stop.day <- 0
}

#run bot if it is not the weekend
if(stop.day == 0) {
  URL <- paste0("https://www.mensa.uzh.ch/de/menueplaene/mensa-uzh-binzmuehle/", wochentag, ".html")
  menu <- try(readLines(URL,encoding = "UTF-8"))
  #keep running the bot unless the website is not available
  if(class(menu)!="try-error"){
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
    vegi <- grep("rlich vegi",menu)+1 #"natürlich vegi"
    
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
    
    #get today's special menu location
    spez <- grep("voll anders",menu)+1 
    
    #copy the "voll anders" menu
    menu.spez <- menu[spez]
    #trim
    menu.spez <- str_replace_all(menu.spez, "<br> </p>","")
    menu.spez <- str_replace_all(menu.spez, "<br>", " ,")
    menu.spez <- str_replace_all(menu.spez, "  ", " ")
    menu.spez <- str_replace_all(menu.spez, " ,", ",")
    menu.spez <- str_replace_all(menu.spez, "<p>", "")
    menu.spez <- str_replace_all(menu.spez, ", Fleisch:",  " |Fleisch:")
    menu.spez <- str_replace_all(menu.spez, ", Fisch:",  " |Fisch:")
    menu.spez <- str_replace_all(menu.spez, "\\|Fleisch:(.*)", "")#as long as the 140 character limit exists
    menu.spez <- str_replace_all(menu.spez, "\\|Fisch:(.*)", "")#as long as the 140 character limit exists
    menu.spez <- str_replace_all(menu.spez, ", mit", " &")
    menu.spez <- str_replace_all(menu.spez, "mit", "&")
    menu.spez <- str_trim(menu.spez, "both")
    
    #assemble the tweets
    tweet.einfachgut <- paste0("Fleisch: ", menu.einfachgut)
    tweet.vegi <- paste0("Vegi: ", menu.vegi)
    tweet.spez <- paste0("Spezial: ", menu.spez)
    }else{
      tweet.empty <- "Leider (noch) kein Menü auf Webseite"
    }
  #set up Twitter access for user mensabot_BIN
  source("Credentials.R", encoding = "UTF-8")
  setup_twitter_oauth(consumer_key =  twitter_api_key,
                      consumer_secret =  twitter_api_secret,
                      access_token =  twitter_access_token,
                      access_secret =  twitter_access_token_secret)
  
  #send Tweets if they contain any text at all
  try( if (length(menu.einfachgut)!=0){
    tweet(tweet.einfachgut)
    })
  try( if (length(menu.vegi)!=0){
    tweet(tweet.vegi)
    })
  try( if (length(menu.spez)!=0){
    tweet(tweet.spez)
    })
  try( if (class(menu)=="try-error"){
    tweet(tweet.empty)
    })
}

#log activity for all three scearios (weekday w/ tweets; weekday w/o tweets; weekend idle)
if(stop.day == 0) {
  if(class(menu)!="try-error"){
    log.df <- data.frame(date = Sys.Date(), stop = stop.day,
                         tweet.einfachgut = tweet.einfachgut,
                         tweet.vegi = tweet.vegi,
                         tweet.spez = tweet.spez)
    write.table(log.df, file = "menu_log_2019.csv", row.names = F, col.names = F, append = T, sep = ";")
  }else{
    log.df <- data.frame(date = Sys.Date(), stop = stop.day,
                         tweet.einfachgut = tweet.empty,
                         tweet.vegi = tweet.empty,
                         tweet.spez = tweet.empty)
    write.table(log.df, file = "menu_log_2019.csv", row.names = F, col.names = F, append = T, sep = ";")
  }
  }else {
    log.df <- data.frame(date = Sys.Date(), stop = stop.day,
                       tweet.einfachgut = wochentag,
                       tweet.vegi = wochentag,
                       tweet.spez = wochentag)
    write.table(log.df, file = "menu_log_2019.csv", row.names = F, col.names = F, append = T, sep = ";")
}

