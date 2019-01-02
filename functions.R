#-----------------------------------------------------------------------#
# Install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, 
# then load them into the R session.
# @param pkg: require package name
#-----------------------------------------------------------------------#
check_packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, library, character.only = TRUE)
}

#-----------------------------------------------------------------------#
# Special Gift functions from Prof Roh
#-----------------------------------------------------------------------#
# for Statistical Inference using Regression
OLS_summary_function <- function(my_model_estimation, mydigit){
  mycoefs = tidy(my_model_estimation) %>% 
    mutate(
      est2=format(round(estimate, mydigit),mydigit),
      se2=format(round(std.error, mydigit),mydigit),
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      report=str_c(" ",est2,mystars,"\n(",se2,")",sep="")
    ) %>% select(term,report)
  myGOF=glance(my_model_estimation) %>% 
    mutate_all(
      funs(round(.,mydigit))
    ) %>% 
    mutate(
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      model_dfs = str_c("(",(df-1),", ",df.residual,")"),
      model_F = str_c(statistic,mystars),
      R2=format(round(r.squared,mydigit),mydigit),
      adjR2=format(round(adj.r.squared,mydigit),mydigit)
    ) %>% select(model_F, model_dfs, R2, adjR2) %>% 
    gather(key=term,value=report)
  mytable=bind_rows(mycoefs,myGOF) %>%
    mutate(sortid=row_number()) %>% 
    select(sortid,term,report) %>%
    mutate_at(vars(2:3),funs(as.character(.)))
  mytable
}

#-----------------------------------------------------------------------#
# Functions for Data collections
#-----------------------------------------------------------------------#
# Compute districts grouping between 3 main 
# regions -
# CCR: Core Central Region (D01,D02,D06,D09,D10,D11)
# RCR: Rest of Central Region (D03,D04,D05,D07,D08,D12,D13,D14,D15,D20)
# OCR: Outside Central Region (D16,D17,D18,D19,D21,D22...D28)

get_regiondistricts <- function(){
  CCR <- str_pad(c(1,2,6,9:11),2, "left", pad="0")
  RCR <- str_pad(c(3:5,7,8,12:15,20),2,"left", pad="0")
  OCR <- str_pad(c(16:19,21:28),2, "left", pad="0")
  
  region <- c("CCR", "RCR", "OCR")
  district <- list( CCR, RCR, OCR )
  
  scaffold <- tibble(region, district) %>% 
    unnest()
  
  return(scaffold)
}

#-----------------------------------------------------------------------#
# Parse an xmlnode set object to extract out the fields 
# - TOP and condo name
#-----------------------------------------------------------------------#
# @param nodes: xmlnode object
# @return: A tibble containing condo name and TOP
#-----------------------------------------------------------------------#
parse_nodecontent <- function(nodes) {
  condo_name <- html_nodes(nodes,".title_link") %>% html_attr("title")
  
  top_year <- html_nodes(nodes,".listcol2") %>% 
    html_node(xpath="div[4]") %>%
    html_text() %>%
    str_extract("\\d+$") 
  
  return(tibble(condo_name,top_year))
}

parse_nodecontent2 <- function(nodes) {
  condo_name <- html_nodes(nodes,".propertyname") %>%
    html_text()
  km_mrt <- html_nodes(nodes,".propertymrt") %>%
    html_text()
  
  return(tibble(condo_name,km_mrt))
  
}

#-----------------------------------------------------------------------#
# Perform webscraping for https://condo.singaporeexpats.com/%d/name/%s
# %d - page number
# %s - A-Z
#-----------------------------------------------------------------------#
# @param url: string object of the targeted url
# @return: A tibble containing condo name, district and age
#-----------------------------------------------------------------------#
scrape_singaporeexpats <- function(url) {
  records_per_page <- 50
  webpage <- read_html(url)
  total_page <- html_text(html_node(webpage,".pageno"))
  total_page <- ceiling(as.numeric(total_page)/records_per_page)
  
  new_url <- str_replace(url,"1","%d")
  dataset <- 1:total_page %>%
    sprintf(new_url,.) %>%
    map(~read_html(.x)) %>%
    map_df(~parse_nodecontent(.x)) 
  
  return (dataset)
}

scrape_singaporeexpats2 <- function(url) {
  base_list<-c("A-G", "H-S", "T-Z")
  
  dataset <- base_list %>%
    map(~sprintf(url,.x)) %>%
    map(~read_html(.x)) %>%
    map(~html_nodes(.x, "#contents")) %>%
    map_df(~parse_nodecontent2(.x))
  
  return(dataset)
}

collectdata_from_singaporeexpats <- function(){
  no_str <- "0-9"
  url1 <- "https://condo.singaporeexpats.com/1/name/%s"
  url2 <- "https://www.singaporeexpats.com/singapore-property-pictures/photos-%s.htm"
  
  singaporeexpats_dataset1 <- no_str %>%
    sprintf(url1,.) %>% {
      Sys.sleep(2)  
      map_df(.,~scrape_singaporeexpats(.x))
    }
  
  singaporeexpats_dataset2 <- LETTERS %>%
    sprintf(url1,.) %>% {
      Sys.sleep(2)  
      map_df(.,~scrape_singaporeexpats(.x))
    }
  
  # Merge the 2 datasets together
  singaporeexpats_dataset3 <- bind_rows(singaporeexpats_dataset1,
                                        singaporeexpats_dataset2)
  
  # Perform data cleaning
  singaporeexpats_dataset3 <- singaporeexpats_dataset3 %>%
    filter(complete.cases(.))
  
  # Dataset with km to mrt
  singaporeexpats_dataset4 <- scrape_singaporeexpats2(url2)
  
  singaporeexpats_dataset5 <- singaporeexpats_dataset3 %>%
    left_join(singaporeexpats_dataset4, by="condo_name")
  
  return(singaporeexpats_dataset5)
}

#-----------------------------------------------------------------------#
# Get an access token from URA data service api
# @param key: The access key included in the email upon 
#             successful activation of account.
# @return: the token generated from the request token service 
#          for daily data request.
#-----------------------------------------------------------------------#
get_onedaytoken <- function(access_key){
  url <- "https://www.ura.gov.sg/uraDataService/insertNewToken.action"
  response <- GET(url,
                  add_headers(
                    "Content-Type" = "application/json",
                    "AccessKey" = access_key
                  ))
  
  result <- fromJSON(content(response, as="text", encoding="utf-8"))
  return(result[[1]])
}

#-----------------------------------------------------------------------#
# This data service will return past 3 years of median rentals of private 
# non-landed residential properties with at least 10 rental contracts for 
# the reference period.
#
# Update Frequency: End of day of every 4th Friday of January, April, 
# July and October. If it is a public holiday, 
# the data will be updated on the following working day.
#-----------------------------------------------------------------------#
# @param key: The access key included in the email upon 
#             successful activation of account.
# @return a tibble data frame with the json results
#
# (Variable)      Description of variable
# (project)       Name of the property project
# (street)        Street name that the property is on.
# (x)             x coordinates of the address in SVY21 format
# (y)             y coordinates of the address in SVY21 format
# (rentalMedian)  Median rentals for the property
# (district)      Postal district of the property
# (refPeriod)     Reference period for the rental information, 
#                   in format of YYYYQQ 
# (psf25)         25th percentile rent rate per square feet per month  
#                   for the property for the reference period.
# (median)        Median rent rate per square feet per month
#                   for the property for the reference period.
# (psf75)         75th percentile rent rate per square feet per month
#                   for the property for the reference period.
#-----------------------------------------------------------------------#
collectdata_from_ura<- function(access_key){
  oneday_token <- get_onedaytoken(access_key)
  url <- "https://www.ura.gov.sg/uraDataService/invokeUraDS?service=PMI_Resi_Rental_Median"
  
  response <- GET(url,
                  add_headers(
                    "Content-Type" = "application/json",
                    "AccessKey" = access_key,
                    "Token" = oneday_token
                  ))
  
  result <- fromJSON(content(response, as="text",encoding="utf-8"))
  result <- as_tibble(result[[1]]) %>%
    unnest() %>%
    select(condo_name = project, 
           x_coord = x, 
           y_coord = y, 
           district, median, refPeriod) 
  
  return(result)
}

#-----------------------------------------------------------------------#
# Check if input path is empty 
# @param file: image path
# @return True if file path is specified else false
#-----------------------------------------------------------------------#
not_empty <- function(file) {
  return(file !="")  
}

#-----------------------------------------------------------------------#
# Generate 4x residual plots to test for homoscedasticity
# @param fit: lm model
# @param save_to: path of the image to save
#-----------------------------------------------------------------------#
plot_fit <- function(fit, save_to =""){
  if(not_empty(save_to)) png(save_to)
  par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
  par(mar=c(2.5,3,3,2.5))
  plot(fit)
  par(mfrow=c(1,1)) # Change back to 1 x 1
  if (not_empty(save_to)) dev.off()
}

#-----------------------------------------------------------------------#
# Generate a scatter plot with 3 regression lines using lm, 
# lm with poly(x,2) and loess method
#
# @param data: dataframe of the dataset
# @param var_x independent variable
# @param var_y: dependent variable
# @param var_color: 
# @labs: tibble object for labs
# @regression: T/F 
# @save_to: path of the image to save
# @return: the scatterplot
#-----------------------------------------------------------------------#
plot_scatterplot <- function(data,
                             var_x,
                             var_y,
                             var_color = "",
                             labs,
                             regression = FALSE,
                             save_to = "") {
  
  if (var_color != "") {
    plot <- ggplot(data, aes(x = var_x, y = var_y, color = var_color))
  } else {
    plot <- ggplot(data, aes_string(x = var_x, y = var_y))
  }
  
  plot <- plot + geom_point(alpha = 1/4,
                            position='jitter') 
  if (regression) {
    
    # use a locally weighted regression
    plot <- plot + geom_smooth(method = loess, 
                               formula = y ~ x, 
                               aes(colour = "darkgreen", 
                                   group = 1), 
                               size = 0.5, 
                               se = FALSE) +
      
      # use a linear fit,
      geom_smooth(method = lm, 
                  formula = y ~ x, 
                  aes(colour = "deepskyblue", group = 1), 
                  size = 0.5, 
                  se = FALSE) +
      
      geom_smooth(method = lm, 
                  formula = y ~ poly(x, 2),
                  aes(colour = "red", group = 1),
                  size = 0.5,
                  se = FALSE) +
      
      scale_colour_manual(name="legend",
                          labels = c("loess", "lm", "lm - poly(x, 2)"), 
                          values = c("darkgreen", "deepskyblue", "red")) 
  }
  
  plot <- plot + labs(title = labs$title,
                      subtitle = labs$subtitle,
                      x = labs$xlab,
                      y = labs$ylab) +
    
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) 
  
  if (not_empty(save_to)) png(save_to)
  print(plot)
  
  if (not_empty(save_to)) dev.off()
  
  return(plot)
}
