#' Region to districts mapping function
#'
#' Compute districts grouping between 3 main - regions
#' CCR: Core Central Region (D01,D02,D06,D09,D10,D11)
#' RCR: Rest of Central Region (D03,D04,D05,D07,D08,D12,D13,D14,D15,D20)
#' OCR: Outside Central Region (D16,D17,D18,D19,D21,D22...D28)
#'
#' @return a tibble object with region and districts mapping

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

#' Parse node content from read_html()
#'
#' Parse an xmlnode set object to extract out the fields
#' TOP and condo name
#' Only for content in - https://condo.singaporeexpats.com/%d/name/%s
#'
#' @param nodes xmlnode object
#' @return a tibble containing condo name and TOP

parse_nodecontent <- function(nodes) {
  condo_name <- html_nodes(nodes,".title_link") %>% html_attr("title")

  top_year <- html_nodes(nodes,".listcol2") %>%
    html_node(xpath="div[4]") %>%
    html_text() %>%
    str_extract("\\d+$")

  return(tibble(condo_name,top_year))
}

#' Parse node content from read_html()
#'
#' Parse an xmlnode set object to extract out the fields
#' TOP and condo name
#' Only for content in -
#' https://www.singaporeexpats.com/singapore-property-pictures/photos-%s.htm
#'
#' @param nodes xmlnode object
#' @return a tibble containing condo name and TOP

parse_nodecontent2 <- function(nodes) {
  condo_name <- html_nodes(nodes,".propertyname") %>%
    html_text()
  km_mrt <- html_nodes(nodes,".propertymrt") %>%
    html_text()

  return(tibble(condo_name,km_mrt))
}

#' Perform webscraping for https://condo.singaporeexpats.com/%d/name/%s
#' page number
#' A-Z
#'
#' @param url string object of the targeted url
#' @return a tibble containing condo name, district and age

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

#' Perform webscraping for
#' https://www.singaporeexpats.com/singapore-property-pictures/photos-%s.htm
#' A-G, H-S, T-Z
#'
#' @param url string object of the targeted url
#' @return a tibble containing condo name, district and age

scrape_singaporeexpats2 <- function(url) {
  base_list <- c("A-G", "H-S", "T-Z")

  dataset <- base_list %>%
    map(~sprintf(url,.x)) %>%
    map(~read_html(.x)) %>%
    map(~html_nodes(.x, "#contents")) %>%
    map_df(~parse_nodecontent2(.x))

  return(dataset)
}

#' Collect data from singaporeexpats website
#'
#' @return singaporeexpats dataset

data_from_singaporeexpats <- function(){
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

#' Get an access token from URA data service api
#'
#' @param access_key The access key included in the email upon
#'                   successful activation of account.
#' @return the token generated from the request token service
#'         for daily data request.

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

#' This data service will return past 3 years of median rentals of private
#' non-landed residential properties with at least 10 rental contracts for
#' the reference period.
#'
#' Update Frequency: End of day of every 4th Friday of January, April,
#' July and October. If it is a public holiday,
#' the data will be updated on the following working day.
#'
#' (Variable)      Description of variable
#' (project)       Name of the property project
#' (street)        Street name that the property is on.
#' (x)             x coordinates of the address in SVY21 format
#' (y)             y coordinates of the address in SVY21 format
#' (rentalMedian)  Median rentals for the property
#' (district)      Postal district of the property
#' (refPeriod)     Reference period for the rental information,
#'                   in format of YYYYQQ
#' (psf25)         25th percentile rent rate per square feet per month
#'                   for the property for the reference period.
#' (median)        Median rent rate per square feet per month
#'                   for the property for the reference period.
#' (psf75)         75th percentile rent rate per square feet per month
#'                   for the property for the reference period.
#'
#' @param access_key The access key included in the email upon
#                    successful activation of account.
#' @return a tibble data frame with the json results

data_from_ura <- function(access_key){
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

  return (result)
}

#' Function to check if input path is empty
#'
#' @param file image path
#' @return True if file path is specified else false

not_empty <- function(file) {
  return(file !="")
}

#' Data Collection from URA service API & singaporeexpats website
#'
#' @param ura_access_key access key for URA data servivce API
#' @param save_to path to save for the dataset
#' @param crawl default to F, otherwise get the dataset from actual website
#' @return tibble object of the dataset
#' @export

get_condo_dataset <- function(ura_access_key = "6117f3d4-81e2-4b3e-9ff9-2640045d2b5a",
                              save_to = "",
                              crawl = FALSE) {
  if(crawl == TRUE) {
    ura_rent_dataset <- data_from_ura(ura_access_key)
    #write_csv(ura_rent_dataset, paste0("ura_rent.csv"), na = "")
    #glimpse(ura_rent_dataset)

    # Group by condo_name and district in nested data frame
    ura_rent_dataset <- ura_rent_dataset %>%
     group_by(condo_name, district) %>%
      nest() %>%
      arrange(district, condo_name)

    #glimpse(ura_rent_dataset)
    singaporeexpats_dataset <- data_from_singaporeexpats()
    #write_csv(singaporeexpats_dataset, paste0("singaporeexpats.csv"), na = "")
    #glimpse(singaporeexpats_dataset)

    # Join ura_dataset with singaporeexpats_dataset
    condo_dataset <- ura_rent_dataset  %>%
      left_join(singaporeexpats_dataset, by="condo_name") %>%
      unnest() %>%
      filter(complete.cases(.))

    # Add in region into the merged dataset
    condo_dataset <- condo_dataset %>%
      left_join(get_regiondistricts(), by="district")

    # Derive new variables for "rentlease_year" etc..
    condo_dataset <- condo_dataset %>%
      mutate(ref_year = str_sub(refPeriod, 1, 4)) %>%
      mutate(condo_age = as.numeric(ref_year) - as.numeric(top_year)+1) %>%
      mutate(x_coord = as.numeric(as.character(x_coord))) %>%
      mutate(y_coord = as.numeric(as.character(y_coord))) %>%
      mutate(district = as.factor(district)) %>%
      mutate(region = as.factor(region)) %>%
      rename(median_rent = median) %>%
      filter(condo_age > 0)

  }else{

    condo_dataset <- read.csv("https://raw.githubusercontent.com/portergoh/capstone/master/data/condo_dataset.csv",
                              stringsAsFactors = F,
                              colClasses = c("character",
                                             "factor",
                                             "character",
                                             "character",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "character",
                                             "factor",
                                             "character",
                                             "numeric"))
  }

  #glimpse(condo_dataset)
  if (not_empty(save_to))
    write_csv(condo_dataset, paste0("condo_dataset.csv"), na = "")

  return(condo_dataset)
}

#' Load the require R packages and install them
#' if they are missing from the system
#'
#' @param requirePackages a vector containing the require packages
#' @export

check_packages <- function (requiredPackages) {
  for (p in requiredPackages){
    if(!require(p,character.only = TRUE))
      install.packages(p)

    library(p,character.only = TRUE)
  }
}

#' Wrapper for ggplot to include the basic attributes of a plot
#'
#' @param df a data.frame (or list) from which the variables
#'           in formula should be taken.
#' @param var_x aes mapping for x axis
#' @param var_y aes mapping for y axis
#' @param var_fill aes mapping for fill attribute
#' @param labs tibble object that contains the ggplot attributes
#' @return ggplot object
#' @export

ggplot_basic <- function(data,
                         var_x,
                         var_y,
                         var_fill=NULL,
                         labs){
  g1 <- ggplot(data,
               aes_string(x = var_x,
                          y = var_y,
                          fill= var_fill)) +

    labs(title = labs$title,
         subtitle = labs$subtitle,
         x = labs$xlab,
         y = labs$ylab) +

    theme_bw() +

    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  return (g1)
}

#' Residual plot
#'
#' @param fit lm object
#' @export

plot_fit <- function(fit){

  par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
  par(mar=c(2.5,3,3,2.5))
  plot(fit)
  par(mfrow=c(1,1)) # Change back to 1 x 1
}

#' Scatter plot with regression lines using ggplot
#'
#' @param df a data.frame (or list) from which the variables
#'           in formula should be taken.
#' @param var_x aes mapping for x axis
#' @param var_y aes mapping for y axis
#' @param labs tibble object that contains the ggplot attributes
#' @export

ggplot_scatter <- function(data,
                           var_x,
                           var_y,
                           labs){

  s1 <- ggplot_basic(data, var_x, var_y, labs=labs) +

          geom_point(alpha = 1/4,
                     position='jitter') +

          scale_colour_manual(name="lines",
                              breaks = c("lm",
                                         "lm - poly(x,2)",
                                         "loess"),

                              values = c("deepskyblue",
                                         "red",
                                         "darkgreen"))

  s2 <- s1 +  geom_smooth(method = lm,
                          formula = y ~ x,
                          aes(colour = "lm", group = 1), size = 1, se = FALSE)

  s3 <- s2 + geom_smooth(method = lm,
                         formula = y ~ poly(x, 2),
                         aes(colour = "lm - poly(x,2)", group = 1), size = 1, se = FALSE)

  s4 <- s3 + geom_smooth(method = loess,
                         formula = y ~ x,
                         aes(colour = "loess", group = 1), size = 1, se = FALSE)

  s4
}

#' Basic wrapper function for boxplot
#'
#' @param df a data.frame (or list) from which the variables
#'           in formula should be taken.
#' @param formula a formula, such as y ~ grp, where y is a numeric vector of data
#'                values to be split into groups according to the
#'                grouping variable grp (usually a factor)
#' @param labs tibble object that contains the boxplot attributes
#' @export

boxplot_basic <- function(df,
                          formula,
                          labs){

    boxplot(formula, data = df,
            notch=FALSE, # Show confidence interval
            varwidth=TRUE, # Show sample size
            col  = "deepskyblue",
            main = labs$title,
            xlab = labs$xlab,
            ylab = labs$ylab)

}
