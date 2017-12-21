#
# # NIVEL FLU TREND 2018
# # by Paul Schneider
# 

# SETTING & FUNCTIONS


# SETTING
  
    # Install and load all required packages
    required_packages<-c("RCurl","ISOweek","jsonlite","ggplot2","prophet","dplyr","gtrendsR","wikipediatrend","pageviews","caret","imputeTS","gridExtra","cowplot","doParallel","glmnet", "Cubist","pls","devtools","sandwich","elasticnet","plyr") # 
    
    pft_packages <- function(package){
      for(i in 1:length(package)){
        if(eval(parse(text=paste("require(",package[i],")")))==0) {
          install.packages(package)}}
      return (eval(parse(text=paste("require(",package,")"))))}
    
    pft_packages(required_packages)
    
    
# FUNCTIONS
  
    # GET WIKI DATA
    fGetWikipediaData = function(pages = wiki.pages[1:3],
                                 language_of_interest =  "nl", 
                                 from = as.Date("2010-08-01"),
                                 to = as.Date("2016-07-31"),
                                 status=1){
      # ask.wiki.new
      # This function does this
      # This function does this
      # This function does this
      
      from.wikishark = format(from,format="%m/%d/%Y")
      to.wikishark = format(as.Date("2015-09-30"),format="%m/%d/%Y")
      from.wikiapi = as.Date("2015-08-01") 
      to.wikiapi = as.Date(to)
      if(to == Sys.Date()){to = as.Date(to)-1}
      
      wiki = data.frame(date=seq(from=as.Date(from),to=as.Date(to),by=1))
      for(p in 1:length(pages)){
        tryCatch({
          
          if(status==1){
            cat("Downloading data for page",p,"of",length(pages)," - ",(p/length(pages))*100 ,"% \n")
          }
          
          if(to >= as.Date("2015-08-01")){
            temp.dat<-article_pageviews(article = pages[p],
                                        project = paste(language_of_interest,".wikipedia",sep=""),
                                        start = "2015080100",end = pageview_timestamps(to))
            temp.wiki.pageview = data.frame(as.Date(temp.dat$date),(temp.dat$views)) 
            names(temp.wiki.pageview) = c("date",pages[p])
          }
          
          if(from < as.Date("2015-08-01")){
            url.lookup = paste("www.wikishark.com/title/",language_of_interest,"/",pages[p],sep="")
            raw.url.lookup = getURLContent(url.lookup)
            start.at =regexpr("translate/id/",raw.url.lookup)[1]
            stop.at = regexpr("values=",raw.url.lookup)[1]
            page.id =  substr(raw.url.lookup,start.at+nchar("translate/id/"),stop.at-2)
            # function below not working: wiki shark doesnt use Wikipedia pageid- they make up their own 'values'  
            # page.id=fromJSON(getURL(paste("https://",language_of_interest,".wikipedia.org/w/api.php?action=query&prop=info&titles=",pages[p],"&format=json",sep="")))$query$pages[[1]]$pageid
            ws.url = paste("http://www.wikishark.com/json_print.php?values=",page.id,"&datefrom=",from.wikishark,"&dateto=",to.wikishark,"&view=2&normalized=0&scale=0&peak=0&log=0&zerofix=0&sumall=0&format=csv", sep="")
            ws.csv = read.csv(ws.url)[,1:2]
            temp.ws.pageview = data.frame(as.Date( ws.csv[,1] ,format="%m/%d/%Y"),(as.numeric(ws.csv[,2])))
            names(temp.ws.pageview) = c("date",pages[p])
            if(to >= as.Date("2015-08-01")){
              temp.wiki.pageview = rbind(temp.wiki.pageview,temp.ws.pageview)
            } else {
              temp.wiki.pageview = temp.ws.pageview
            }
            
          }
          
          wiki = merge(wiki,temp.wiki.pageview,by="date",all=T)
        },
        error=function(e) {if(status==1){cat("Uups...Something went wrong with",pages[p],"\n")}})
      }
      names(wiki)[-1] = paste("wiki.",names(wiki)[-1],sep="")
      
      aggregate.data = aggregate(wiki[,-1], list(ISOweek(wiki$date)),FUN=function(x){sum(x,na.rm=T)},simplify=T)[,2]
      wiki= data.frame(date=unique(ISOweek(wiki$date)),
                       aggregate.data)
      return(wiki)
    }
    
    # GET WIKI PAGES
    pft_wiki_lp = function(term = "Influenza",
                           language_of_interest = "de", 
                           backlinked = 1 ,
                           manual.pages=c("Halsschmerzen","Hausmittel")){
      wiki.query<-paste("https://",language_of_interest,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",term,  sep="")
      linked_pages<-fromJSON(getURL(wiki.query)) # this query asks for links on the respective page
      links = NULL
      for(i in 1:length(linked_pages$query$pages[[1]]$links$title)){
        # links[i]<-linked_pages$query$pages[[1]]$links[[i]]$title
        links[i]<-linked_pages$query$pages[[1]]$links$title[i]
        
      }
      # If the number of linked pages is larger than 500 (on the English Influenza page there are 660 links), we need to send multiple queries using the code below
      if( !is.null(linked_pages$continue[1])){
        temp.links<-NULL
        while(!is.null(linked_pages$continue[1])){ # Only 500 linked pages can be retrieved per query, if >500, more than 1 query is neccessary
          linked_pages<-paste("https://",language_of_interest,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",term,"&plcontinue=",linked_pages$continue[1],  sep="")
          linked_pages<-fromJSON(getURL(linked_pages))
          for(l in 1:length(linked_pages$query$pages[[1]]$links)){
            #  temp.links[l]<-linked_pages$query$pages[[1]]$links[[l]]$title
            temp.links[l]<-linked_pages$query$pages[[1]]$links$title[l]
            
          }
          links = c(links,temp.links)
        }
        str(links)}
      
      # 3. Backlined pages (Pages which link to influenza)
      if(backlinked ==1 ){
        backlinked.pages<-paste("https://",language_of_interest,".wikipedia.org/w/api.php?action=query&list=backlinks&bllimit=500&format=json&bltitle=",term,  sep="")
        backlinked_pages<-fromJSON(getURL(backlinked.pages)) # this query asks for links on the respective page
        backlinks = NULL
        for(i in 1:length(backlinked_pages$query$backlinks$title)){
          backlinks[i]<-backlinked_pages$query$backlinks$title[i]
        }} else(backlinks = NULL)
      # Limited to 500 results (???)
      
      # 4. Specify your own pages manually if you like to, but make sure they are actual Wikipedia pages 
      manual.pages = manual.pages 
      
      # 5. combine all terms
      wiki.pages = c(term,links,backlinks,manual.pages)
      wiki.pages = gsub(" ","_",wiki.pages)
      wiki.pages = unique(wiki.pages) # To remove duplicated terms
      return(wiki.pages)
    }
    
    # GET GOOGLE DATA
    fGetGoogleData = function(keyword = "influenza",
                              country_of_interest="DE",
                              from="2015-08-01",
                              to="2017-07-31",
                              status= 1,
                              prefix="g.trends.",
                              gprop = "web") {
      
      # Splitting the time span into two spans, each < 5 years
      if(length(seq(as.Date(from),as.Date(to),by=1) ) > (365*5 -1)){
        time.span1 =  paste(as.Date(from), as.Date(from)+(5*365-1))
        time.span1.dates = ISOweek(seq(from=as.Date(from),to=as.Date(from)+(5*365-1), by=7))
        time.span2 = paste(as.Date(to)-(5*365-1), as.Date(to))
        time.span2.dates = ISOweek(seq(from=as.Date(as.Date(to)-(5*365-1)),to=as.Date(to), by=7))
        time.span = unique(c(time.span1.dates,time.span2.dates))
      } else {
        time.span1 =  paste(as.Date(from), as.Date(to))
        time.span1.dates = ISOweek(seq(from=as.Date(from),to=as.Date(to), by=7))
        time.span = time.span1.dates
      }
      if(ISOweek(Sys.Date()) %in% time.span) {
        time.span = time.span[-which(time.span ==ISOweek(Sys.Date()))]
      }
      
      google.input.data = as.data.frame(
        matrix(nrow = length(time.span),
               ncol = 1+length(keyword)))
      google.input.data[,1] =  time.span
      
      
      for(p in 1:length(keyword)){
        if(status==1){cat("asking Google for statistics for",keyword[p]," - ",round(p/length(keyword),3)*100,"%" ,"\n")}
        tryCatch({ 
          google.temp.t1 = gtrends(keyword = keyword[p],
                                   geo = country_of_interest,
                                   time= time.span1 ,gprop = gprop) 
          # Sys.sleep(0.1) 
          if(length(seq(as.Date(from),as.Date(to),by=1) ) > 365*5-1){
            google.temp.t2 = gtrends(keyword = keyword[p],
                                     geo = country_of_interest,
                                     time= time.span2 ,gprop = gprop) 
            
            # Rescaling the older data set to match (more or less) with the more recent data set
            hits = Rescale.gtrends(df.t1=google.temp.t1,df.t2=google.temp.t2)
            google.input.data[,p+1] = as.numeric(hits)
          } else {
            if(sum(class(google.temp.t1$interest_over_time$date[1])=="POSIXct" )>0){
              hits = aggregate(google.temp.t1$interest_over_time$hits, list(ISOweek(google.temp.t1$interest_over_time$date)),sum)
              google.input.data[,p+1] = as.numeric(hits$x)
              
            } else {
              google.input.data[,p+1] = as.numeric(google.temp.t1$interest_over_time$hits)
              
            }
            
          }
          error.rate <<- 0
        },  error=function(e) { 
          if(status==1){cat("\n Uups...Something went wrong with",keyword[p],"\n")  }
          Sys.sleep(0.5)
        })
        
      }
      
      names(google.input.data) = c("date",paste(prefix,gsub(" ","\\.",keyword),sep=""))
      return(google.input.data)
    }
    
    
    
    # RESCALE GOOGLE DATA
    Rescale.gtrends = function(df.t1,df.t2){
      df.t1 = df.t1$interest_over_time
      df.t2 = df.t2$interest_over_time
      # In order to rescale, we look at the overlapping time span and try to find the best mutliplicative scaling factor, using a linear regression, without constant. Not sure if there are better ways to do this
      match1 = df.t1[match(df.t2$date,df.t1$date),]
      match1 = match1[!is.na(match1$date),]
      match2 = df.t2[match(df.t1$date,df.t2$date),]
      match2 = match2[!is.na(match2$date),]
      rescale.factor = lm(data=match1,match2$hits ~ hits+0) 
      df.t1$hits = round(predict(rescale.factor,newdata = df.t1),1) 
      df.t1=df.t1[df.t1$date<min(df.t2$date),]
      hits = as.numeric(c(df.t1$hits,df.t2$hits))
      return(hits)}
    
  
    ## eval functions
    fEvalModel = function(model){ 
      
      cv.plot = NULL
      pred.plot = NULL
      cor.train = NULL
      cor.test = NULL
      cv.Rsquared = NULL
      lowest.rmse = NULL
      
      
      
      tryCatch({ 
        if(!exists("parameter",where=model$bestTune)){
          
          cv.plot = plot(model)}
      },
      error=function(e) {})
      
      if(!is.null(model)){
        tryCatch({
          pred.train.temp = predict(model,newdata= df.train)
          rmse.train = round(sqrt(mean( (pred.train.temp - y.train)^2 )),6)
          pred.test.temp =  predict(model,newdata= df.test)

          train.temp.df  = data.frame(y=y.train,date=date.train,preds=pred.train.temp)
          test.temp.df = data.frame(date=date.test,preds=pred.test.temp)
          
          model.name.for.title = ifelse(exists("name.of.model"),name.of.model,model$method)
          
          pred.plot = ggplot(data=train.temp.df) +
            
            geom_line(data=train.temp.df,aes(x=date.train ,y=y.train),col="black") +
            geom_point(data=train.temp.df,aes(x=date.train,y=pred.train.temp),col="orange") +
            geom_line(data=train.temp.df,aes(x=date.train,y=pred.train.temp),col="orange") +
            geom_point(data=test.temp.df,aes(x=date.test,y=pred.test.temp),col="red")  +
            geom_line(data=test.temp.df,aes(x=date.test,y=pred.test.temp),col="red") +
            ggtitle(paste(model.name.for.title)) +
            ylab("y - actual vs. predicted") +
            xlab("Date - Training and Test period") +
            theme_light()
        },error = function(e){cat(": Error \n")})
        
      }
      
      tryCatch({
        cor.train =  cor(y.train,pred.train.temp)
        cv.Rsquared = model$results$Rsquared
        lowest.rmse =  min(model$results$RMSE,na.rm=T)
      },error = function(e){cat(": Error \n")})
      
      out = list(plots=list("cv.plot"=cv.plot,
                            "pred.plot" =pred.plot),
                 correlations = list("cor.train" = cor.train,
                                     "cor.test" = cor.test,
                                     "cv.Rsquared" = cv.Rsquared),
                 "lowest.rmse" = lowest.rmse)
      return(out)
    }
    
    
    
    
    