parse_glassdoor_dir <- function(directory) {
  
  if (!file_test("-d", directory)) {
    stop ("d is not a directory")
  }
  
  reviews_glassdoor <- data.frame()
  files <- list.files(directory, pattern = "*.htm", full.names = TRUE)
  
  for (file in files) {
    reviews_glassdoor <- rbind(reviews_glassdoor,parse_glassdoor_file(file))
  }
  
  return(reviews_glassdoor)
}

parse_glassdoor_file <- function (file) {
  library(rvest)
  
  page <- read_html(file, encoding = "UTF-8")
  reviews <- page %>% html_nodes("div.hreview")
  all_reviews <- data.frame()
  
  for (review in reviews) {
    all_reviews <- rbind(all_reviews,get_review_fields(review))
  }
  
  return(all_reviews)
}

get_review_fields <- function (review) {
  library(tidyverse)
  # library(lubridate)
  # library(stringr)
  # library(purrr)
  
  review.company <- review %>% 
    html_node("span.sqLogo.tighten.smSqLogo.logoOverlay") %>% 
    html_children() %>% html_attr("alt") %>% 
    str_remove("Logo") %>% str_trim()
  
  #
  # company without logo
  #
  if (is.na(review.company)) {
    review.company <- review %>% 
      html_node("span.sqLogo.tighten.smSqLogo.logoOverlay") %>% 
      html_children() %>% html_text() %>% str_remove("Logo") %>% 
      str_trim()
  }
  
  review.date <- review %>% 
    html_node("time.date.subtle.small") %>% 
    html_text() %>% mdy()
  
  review.helpful <- review %>% 
    html_node("span.helpfulCount.subtle") %>% 
    html_text() %>% str_extract("\\d+") %>% 
    as.numeric()
  
  review.title <- review %>% 
    html_node("span.summary") %>% 
    html_text() %>% str_remove_all("\"")
  if (is_empty(review.title)) { review.title <- NA }
  
  review.isEmployee <- review %>% 
    html_node("span.authorJobTitle.middle.reviewer") %>% 
    html_text() %>% str_detect("Current")
  
  review.position <- review %>% 
    html_node("span.authorJobTitle.middle.reviewer") %>% 
    html_text() %>% str_remove("\\w+ \\w+ - ")
  if (is_empty(review.position)) { review.position <- NA }
  
  review.location <- review %>% 
    html_node("span.authorLocation.middle") %>% html_text() %>% 
    str_remove(", \\w*$")
  if (is_empty(review.location)) { review.location <- NA }

  review.rating <- review %>% html_node ("span.value-title") %>% 
    html_attr("title") %>% as.numeric()

  ratings.name.list <- review %>% html_nodes ("ul.undecorated li") %>% 
    html_text()
  
  if (length(review %>% html_nodes ("ul.undecorated li")) == 0)  { 
    review.rating.WorkLifeBalance <- NA
    review.rating.CultureValues <- NA
    review.rating.CarrerOportunities <- NA
    review.rating.CompBenefits <- NA
    review.rating.SeniorManagement <- NA

  } else {
    ratings.value.list <- review %>% html_nodes ("ul.undecorated li") %>% 
      html_nodes("span") %>% html_attr("title")
    
    review.rating.WorkLifeBalance <- 
      ratings.value.list[str_detect(ratings.name.list,"Work/Life Balance")] %>% 
      as.numeric()
    if (is_empty(review.rating.WorkLifeBalance)) { 
      review.rating.WorkLifeBalance <- NA 
    }
    
    review.rating.CultureValues <- 
      ratings.value.list[str_detect(ratings.name.list,"Culture & Values")] %>% 
      as.numeric()
    if (is_empty(review.rating.CultureValues)) { 
      review.rating.CultureValues <- NA 
    }
    
    review.rating.CarrerOportunities <- 
      ratings.value.list[str_detect(ratings.name.list,"Career Opportunities")] %>% 
      as.numeric()
    if (is_empty(review.rating.CarrerOportunities)) { 
      review.rating.CarrerOportunities <- NA 
    }
    
    review.rating.CompBenefits <- 
      ratings.value.list[str_detect(ratings.name.list,"Comp & Benefits")] %>% 
      as.numeric()

    if (is_empty(review.rating.CompBenefits)) { 
      review.rating.CompBenefits <- NA 
    }    

    review.rating.SeniorManagement <- 
      ratings.value.list[str_detect(ratings.name.list,"Senior Management")] %>% 
      as.numeric()
    
    if (is_empty(review.rating.SeniorManagement)) { 
      review.rating.SeniorManagement <- NA 
    }
  }

  advices <- review %>% html_nodes ("div.flex-grid") %>% 
    html_nodes ("span.middle")
  
  advices_list <- sapply(advices, html_text)
  
  review.recommendation <- advices_list[str_detect(advices_list,"Recommend")]
  if (is_empty(review.recommendation)) { review.recommendation <- NA }
  
  review.outlook <- advices_list[str_detect(advices_list,"Outlook")]
  if (is_empty(review.outlook)) { review.outlook <- NA }
  
  review.adviceCEO <- advices_list[str_detect(advices_list,"CEO")]
  if (is_empty(review.adviceCEO)) { review.adviceCEO <- NA }  
  
  review.statement <- review %>% html_nodes ("p.tightBot.mainText") %>% 
    html_text()
  if (is_empty(review.statement)) { review.statement <- NA }  
  
  review.pros <- review %>% 
    html_nodes ("p.pros.mainText.truncateThis.wrapToggleStr") %>% 
    html_text()
  if (is_empty(review.pros)) { review.pros <- NA }
  
  review.cons <- review %>% 
    html_nodes ("p.cons.mainText.truncateThis.wrapToggleStr") %>% 
    html_text()
  if (is_empty(review.cons)) { review.cons <- NA }
  
  review.adviceMgmt <- review %>% 
    html_nodes ("p.adviceMgmt.mainText.truncateThis.wrapToggleStr") %>% 
    html_text()
  if (is_empty(review.adviceMgmt)) { review.adviceMgmt <- NA }
  
  review.as.list <- list(review.company,review.date, review.helpful, review.title, 
                         review.isEmployee, review.position, review.location, review.rating, 
                         review.rating.WorkLifeBalance, review.rating.CultureValues, 
                         review.rating.CarrerOportunities, review.rating.CompBenefits, 
                         review.rating.SeniorManagement, review.recommendation, 
                         review.outlook, review.adviceCEO, review.statement, review.pros, 
                         review.cons, review.adviceMgmt)
  
  review.as.df <- data.frame(review.as.list, stringsAsFactors = F)
  
  names(review.as.df) <- 
    c("Company", "Date","Helpful","Title","isEmployee","Position","Location",
      "Rating","WorkLifeBalance","CultureValues","CarrerOportunities",
      "CompBenefits","SeniorManagement","Recommendation","Outlook",
      "AdviceCEO","Statement","Pros","Cons","AdviceMgmt")
  
  return(review.as.df)
}