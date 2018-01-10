#
# # NIVEL FLU TREND 2018
# # by Paul Schneider
# # Version 10 JAN 2018

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
##
#
#
#
#
#
#
#difference
#
#
#
#
#
#
#

fGetWikipediaData = function(pages = wiki.pages[1:3],
                             language_of_interest =  "nl", 
                             from = as.Date("2010-08-01"),
                             to = as.Date("2016-07-31"),
                             status=1){
  
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
  
  aggregate.data = aggregate(wiki[,-1], list(ISOweek(wiki$date)),FUN=function(x){mean(x,na.rm=T)},simplify=T)[,2]
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
  
  
  time.span1 =  paste(as.Date(from), as.Date(to))
  time.span1.dates = ISOweek(seq(from=as.Date(from),to=as.Date(to), by=7))
  time.span = time.span1.dates
  
  time.span.5y = time.span1
  time.span.3m = "today 3-m"
  time.span.1m = "today 1-m"
  time.span.1w = "now 7-d"
  
  
  google.input.data = as.data.frame(
    matrix(nrow = length(time.span),
           ncol = 1+length(keyword)))
  google.input.data[,1] =  time.span
  
  
  for(p in 1:length(keyword)){
    if(status==1){cat("asking Google for statistics for",keyword[p]," - ",round(p/length(keyword),3)*100,"%" ,"\n")}
    tryCatch({ 
      google.temp.5y = gtrends(keyword = keyword[p],
                               geo = country_of_interest,
                               time= time.span.5y ,gprop = gprop) 
      google.temp.5y = google.temp.5y$interest_over_time[,c(1,2)]
      google.temp.5y$week = ISOweek(google.temp.5y$date)
      
      Sys.sleep(0.1) 
      google.temp.3m = gtrends(keyword = keyword[p],
                               geo = country_of_interest,
                               time= time.span.3m ,gprop = gprop) 
      google.temp.3m = google.temp.3m$interest_over_time[,c(1,2)]
      google.temp.3m$week = ISOweek(google.temp.3m$date)
      if(sum(as.Date(google.temp.3m$date)>to)>0){
      google.temp.3m = google.temp.3m[-which(as.Date(google.temp.3m$date)>to),]}
      google.temp.3m = aggregate(hits ~ week, google.temp.3m, mean)
      
      Sys.sleep(0.1) 
      google.temp.1m = gtrends(keyword = keyword[p],
                               geo = country_of_interest,
                               time= time.span.1m ,gprop = gprop)
      google.temp.1m = google.temp.1m$interest_over_time[,c(1,2)]
      google.temp.1m$week = ISOweek(google.temp.1m$date)
      if(sum(as.Date(google.temp.1m$date)>to)>0){
        google.temp.1m = google.temp.1m[-which(as.Date(google.temp.1m$date)>to),]}
      google.temp.1m = aggregate(hits ~ week, google.temp.1m, mean)
      
      Sys.sleep(0.1) 
      google.temp.1w = gtrends(keyword = keyword[p],
                               geo = country_of_interest,
                               time= time.span.1w ,gprop = gprop)
      google.temp.1w = google.temp.1w$interest_over_time[,c(1,2)]
      google.temp.1w$week = ISOweek(google.temp.1w$date)
      if(sum(as.Date(google.temp.1w$date)>to)>0){
        google.temp.1w = google.temp.1w[-which(as.Date(google.temp.1w$date)>to),]}
      google.temp.1w = aggregate(hits ~ week, google.temp.1w, mean)
      
      
      fRescaleGtrendsNivel = function(df.t1,df.t2){
        match1 = df.t1[match(df.t2$week,df.t1$week),]
        match1 = match1[!is.na(match1$week),]
        match2 = df.t2[match(df.t1$week,df.t2$week),]
        match2 = match2[!is.na(match2$week),]
        rescale.factor = lm(data=match1,match2$hits ~ hits+0) 
        df.t1$hits = round(predict(rescale.factor,newdata = df.t1),1) 
        df.t1=df.t1[df.t1$week<min(df.t2$week),]
        hits = as.numeric(c(df.t1$hits,df.t2$hits))
        weeks = c(df.t1$week, df.t2$week)
        scaled.df = data.frame(weeks,hits,stringsAsFactors = F)
        return(scaled.df)}
      
      scaled.df.5y.3m = fRescaleGtrendsNivel(google.temp.5y,google.temp.3m)
      scaled.df.5y.3m.1m = fRescaleGtrendsNivel(scaled.df.5y.3m,google.temp.1m)
      scaled.df.5y.3m.1m.1w = fRescaleGtrendsNivel(scaled.df.5y.3m.1m,google.temp.1w)
      
      google.input.data[,p+1] = as.numeric(scaled.df.5y.3m.1m.1w$hits)
    },  error=function(e) { 
        if(status==1){
          cat("\n Uups...Something went wrong with",keyword[p],"\n")
          }
      Sys.sleep(0.2)
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







NivelFluTrend = function(from = from,
                         incidence.data.path = incidence.data.path,
                         to = to,   
                         forecast.to = Sys.Date()-1 + 28)
{
  
  
  time1 = Sys.time()
  
  cat(" \n Loading ili data")
  influenza.nl = read.csv(incidence.data.path)
  names(influenza.nl) = c("date","y") 
  influenza.nl$date = as.Date(influenza.nl$date)
  influenza.nl = influenza.nl[influenza.nl$date>= from,]
  split.at = max(influenza.nl$date)+1
  influenza.nl = rbind(influenza.nl,
                       data.frame(date=seq(from=max(influenza.nl$date)+7,to=to,by=7),
                                  y=NA))
  
  # WIKIPEDIA DATA
  
  # Retrieve linked articles
  cat("\n Retrieving Wikipedia page info")
  wiki.pages = pft_wiki_lp(term = term,              # primary page of interest
                           language_of_interest = "nl",     # Wikipedia project language
                           backlinked = 0,                  # Also want to retrieve backlinks?
                           manual.pages=c(""))  # none specified
  
  
  # Download from Wikishark and Wikipedia API
  cat("\n Downloading Wikipedia data")
  wiki.df = fGetWikipediaData(pages = wiki.pages[1],            # Wikipedia article names
                              language_of_interest =  "nl",  # Project language
                              from = from,  # Download from
                              to = to,    # Download up to
                              status = 0)  
  wiki.df  = data.frame(date=wiki.df$date)
  
  for(i in 1:length(wiki.pages)){
    cat(i,"of",length(wiki.pages), " ",round(i/length(wiki.pages),4)*100,"% \n")
    tryCatch({
      nl.wikipedia.input.data = fGetWikipediaData(pages = wiki.pages[i],            # Wikipedia article names
                                                  language_of_interest =  "nl",  # Project language
                                                  from = from,  # Download from
                                                  to = to,    # Download up to
                                                  status = 0)                    # Print download status
      
      wiki.df = cbind(wiki.df,nl.wikipedia.input.data[,-1])
      names(wiki.df)[length(names(wiki.df))] = wiki.pages[i]
    }, error=function(e) cat("\n Something went wrong with ",wiki.pages[i],": page dropped, continue \n"))
  }
  
  names(wiki.df)[-1] = paste("wiki.",names(wiki.df)[-1],sep="")
  
  # GOOGLE DATA
  cat("\n Retrieving Google keyword info")
  google_primer = gtrends(keyword=term,              # term = "influenza"
                          geo=country_of_interest,  # "DE" = Germany in ISO_3166-2
                          time=paste(from,to),      # from= 2010-07-31 to=2017-07-31
                          gprop ="web")             # Search in webqueries
  tops = google_primer$related_queries$related_queries=="top" 
  google_related = google_primer$related_queries$value[tops]
  g.trends.keywords = c(term,google_related)
  
  for(i in 1:length(g.trends.keywords)){
    cat(round(i/length(g.trends.keywords),4)*100,"% \n")
    extended.related = gtrends(keyword=g.trends.keywords[i],              # term = "influenza"
                               geo=country_of_interest,  # "DE" = Germany in ISO_3166-2
                               time=paste(from,to),      # from= 2010-07-31 to=2017-07-31
                               gprop ="web")             # Search in webqueries
    tops = extended.related$related_queries$related_queries=="top" 
    google_related.extended = extended.related$related_queries$value[tops]
    google_related.extended = google_related.extended[1:5]
    g.trends.keywords = unique(g.trends.keywords,google_related.extended)
  }
  
  g.news.keyword = "griep"
  
  cat("\n Downloading Google data")
  g.trends.input =  fGetGoogleData(keyword = g.trends.keywords, # 25 ++ keywords from Google trends
                                   country_of_interest=country_of_interest,
                                   from=paste(as.Date(from)),
                                   to=paste(as.Date(to)),
                                   status= 1,    
                                   prefix="g.trends.")
  
  g.news.input =  fGetGoogleData(keyword = g.news.keyword,
                                 country_of_interest=country_of_interest,
                                 from=paste(as.Date(from)),
                                 to=paste(as.Date(to)),
                                 status= 1,    
                                 prefix="g.news.",
                                 gprop="news")    # Retrieving Google News search queries
  
  google.input.data = merge(g.trends.input,g.news.input,by="date",all=T)
  # IMMIDIATELY REMOVING 
  google.input.data = google.input.data[,-(nearZeroVar(google.input.data,uniqueCut = 25))]
  
  ###################  Data retrieving done ###################
  
  # merging and slicing data sets
  cat("\n Preprocessing data")
  influenza.nl$date = ISOweek(influenza.nl$date ) 
  
  df.full = merge(influenza.nl,google.input.data, by="date")
  df.full = merge(df.full,wiki.df, by="date")
  
  
  df.full$date = ISOweek2date(paste(df.full$date,"-1",sep="")) # 
  cat("\n Full data set:",dim(df.full)[1], "Weeks and",dim(df.full)[2]-2,"Predictors")
  
  split = which(df.full$date<split.at) 
  
  df.train = df.full[split,-c(1,2)] # Predictor training data set
  y.train = df.full[split,c(2)] # Outcome for training data set
  date.train = df.full[split,c(1)] # Date, not a predictor but useful for plotting
  
  df.test  = df.full[-split,-c(1,2)] # Predictors for testing/evaluation data set
  date.test = df.full[-split,c(1)] # date for test data set

  
  # PREPROCESSING
  # NA handling
  sum.NA.train = as.numeric(lapply(df.train,function(x){sum(is.na(x))})) 
  sum.NA.train = sum.NA.train > length(df.train[,1]) * 0.1 
  if(sum(sum.NA.train)>0){
    df.train = df.train[-which(sum.NA.train)]
    df.test = df.test[which(colnames(df.test) %in% colnames(df.train))]}
  # and test data separately
  sum.NA.test = as.numeric(lapply(df.test,function(x){sum(is.na(x))}))
  sum.NA.test = sum.NA.test > length(df.test[,1]) * 0.1 
  if(sum(sum.NA.test)>0){
    df.test = df.test[-which(sum.NA.test)]
    df.train = df.train[which(colnames(df.train) %in% colnames(df.test))]}
  
  # Imputing remaining NAs
  df.train = na.ma(df.train , k = 3, weighting = "exponential") 
  df.test = na.ma(df.test , k = 3, weighting = "exponential") 
  
  # Removing features with near zero variance
  nearZeroVar = nearZeroVar(df.train,freqCut = 95/5 , uniqueCut = 25) 
  if(sum(nearZeroVar)>0){
    df.train = df.train[,-nearZeroVar] 
    df.test = df.test[which(colnames(df.test) %in% colnames(df.train))]}
  
  ## ------------------------------------------------------------------------
  # Scaling, centering, transofrmation and imputation of remaining NAs by K-nearest neighbours
  preprocess.df.train = preProcess(df.train, method=c("scale","center"))
  df.train = predict(preprocess.df.train, newdata = df.train)
  df.test = predict(preprocess.df.train,newdata = df.test)
  
  
  # MODEL BUILDING
  controlObject <- trainControl(method = "timeslice",
                                initialWindow = 52,   # First model is trained on 2 years
                                horizon = 1, #4?!
                                fixedWindow = FALSE,  # Origin stays the same
                                allowParallel = TRUE) # Paralel computing can speed things up
  
  
  # paralel computing : PROBLEM FOR WINDOWS?
  no_cores <- detectCores() - 1  
  cl <- makeCluster(no_cores, type="FORK")
  registerDoParallel(cl)  
  
  cat("\n --- Building models ---")
  
  
  
  
  cat("\n --- Building PLS ---")
  
  # partial least square
  M.pls = train(y= y.train ,
                x = df.train,
                method = "pls",
                tuneLength = 20,
                trControl = controlObject)
  
  
  # lasso regression (glmnet)
  # lasso grid
  cat("\n --- Building Lasso ---")
  
  lassoGrid <- expand.grid(.alpha = c(.2, .4, .6, .8),.lambda = seq(.05, 1, length = 50)) # refined grid
  # Model
  M.lasso <- train(y= y.train ,
                   x = df.train,
                   method = "glmnet",
                   family = "gaussian", # tried poisson, worse!
                   tuneGrid = lassoGrid,
                   trControl = controlObject)
  
  # Cubist (cubist)
  # cubist grid
  
  cat("\n --- Building Cubist ---")
  
  cubistGrid <- expand.grid(.committees = seq(40,100,by=5),.neighbors=c(3,4,5,6,7,9))
  # Model
  M.cubist = train(y= y.train ,
                   x = df.train,
                   method = "cubist",
                   tuneGrid = cubistGrid,
                   trControl = controlObject)
  
  # Saving results
  models.nl = list(result.list = list(M.pls = M.pls,
                                      #M.ridge = M.ridge,
                                      M.lasso = M.lasso
                                      ,M.cubist = M.cubist
  )
  , eval.list = list())
  
  cat("\n --- Creating plots ---")
  
  
  for(i in 1:length(models.nl$result.list)){
    tryCatch({
      name.of.model = names(models.nl$result.list)[i]
      models.nl$eval.list[[i]] = fEvalModel(models.nl$result.list[[i]])
      names(models.nl$eval.list)[i] = names(models.nl$result.list)[i]},
      error = function(e){cat(names(models.nl$result.list)[i] ,": Error \n")})
  }
  
  # lowest CV RMSE per model
  means=NULL; sd = NULL; model.name = NULL
  for(m in 1:length(models.nl$result.list)){
    means[m] = mean(models.nl$result.list[[m]]$resample$RMSE,na.rm=T)
    sd[m] = sd(models.nl$result.list[[m]]$resample$RMSE,na.rm=T)
    model.name[m] = names(models.nl$result.list)[m]
  }
  sd = sd[order(means)]
  model.name = as.character(model.name)
  model.name = model.name[order(means)]
  means = means[order(means)]
  
  model.comparison = 
    ggplot() +
    geom_point(aes(x=means,y=model.name)) +
    geom_line(aes(x=c(means-sd,means+sd),y=rep(model.name,times=2))) +
    ggtitle("Model mean RMSE +/- 1 SD") +
    xlab("RMSE") +
    ylab("Model") 
  
  select.model = which(names(models.nl$result.list) == model.name[1])
  cat("\n --- Selected model:",model.name[1],"---")
  final.model  = models.nl$result.list[[select.model]]
  preds.train  = predict(final.model)
  nowcast      = predict(final.model,newdata=df.test)
  
  
  cat("\n --- Creating Forecast ---")
  
  forecast.date = seq(from=min(date.test), to = forecast.to,by=7)
  
  null.model = prophet(df=data.frame(ds = date.train,
                                     y=y.train),
                       growth = "linear",
                       yearly.seasonality = T,
                       weekly.seasonality = F)
  forecast = make_future_dataframe(null.model, periods = length(forecast.date),freq="week")
  null.model.forecast = predict(null.model, forecast)
  select.training.preds = ISOweek(null.model.forecast$ds) %in% ISOweek(date.train)
  preds.null.model = null.model.forecast$yhat[select.training.preds]
  forecast = null.model.forecast$yhat[-which(select.training.preds)]
  
  ## ----nowcast vs forecast plot, warning=FALSE,fig.height=3,fig.width=8----
  pnf.1 =
    ggplot() +
    
    geom_line(aes(x=date.train,y=y.train,col="black"),size=2) +
    
    geom_line(aes(x=forecast.date,y=forecast,col="cyan")) +
    geom_point(aes(x=forecast.date,y=forecast,col="cyan")) +
    geom_line(aes(x=date.train,y=preds.null.model,col="cyan")) +
    
    geom_line(aes(x=date.train,y=preds.train,col="orange")) +
    geom_line(aes(x=date.test,y=nowcast,col="red")) +
    geom_point(aes(x=date.test,y=nowcast,col="red")) +
    scale_color_manual(name ="", 
                       values = c("black" = "black",
                                  "cyan" = "cyan", 
                                  "orange" = "orange",
                                  "red" = "red"),
                       labels = c("Actual incidence",
                                  "Forecast", 
                                  "Nowcast (training)" ,
                                  "Nowcast")) +
    geom_vline(xintercept = as.numeric( as.Date(max(date.train)))) +
    geom_vline(xintercept = as.numeric( Sys.Date()),linetype=2,col="purple") +
    ylab("Influenza incidence") +
    xlab("2015/16") +
    ggtitle("Forecast vs Nowcast model: Influenza season 2015/16") +
    theme(plot.title = element_text(size = 10, face = "bold")) +
    #xlim(c(as.Date("2016-11-01"),as.Date("2017-05-01"))) +
    theme_minimal() 
  
  pnf.2 =
    pnf.1 + 
    xlim(min(date.test)-365,forecast.to)
  
  
  combined.plot = plot_grid(pnf.1,pnf.2,nrow=2)
  
  file.name = paste("nft_data_",Sys.time(),".rdata",sep="")
  save(list = ls(environment()), file = file.name)
  
  time2 = Sys.time()
  cat("\n --- Done! ---")
  
  cat("Time elapsed: ",time2-time1)
  
  cat(" \n All Data has been stored in", file.name)
  return(file.name)
}


