initialize_s3 <- function(){
  require("paws")
  s3 <- paws::s3(config = list(
    credentials = list(
      creds = list(
        access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
        secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
        session_token = Sys.getenv("AWS_SESSION_TOKEN")
      )),
    endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
    region = Sys.getenv("AWS_DEFAULT_REGION")))
  
  return(s3)
}

s3_read <- function(.file){
  require(paws)
  require(tidyverse)
  if(str_detect(.file, "\\.csv$")){
    s3 <- initialize_s3()
    f <- s3$get_object(Bucket = "scoavoux", Key = .file)
    d <- f$Body %>% rawToChar() %>% read_csv()
  }
  return(d)
}

read_mama_data <- function(){
  require(tidyverse)
  m <- s3_read("datasets/mama_records_NUM.csv")
  m <- select(m, -`...1`)
  # compute nb of contexts
  # for now we keep them all.
  m <- select(m, uid_mama, starts_with("mama_q98")) %>% 
    pivot_longer(-uid_mama) %>% 
    count(uid_mama, value) %>% 
    filter(value == 1) %>% 
    select(uid_mama, nb_contexts = "n") %>% 
    right_join(m)
  # compute nb of genres
  # for now we keep them all.
  m <- select(m, uid_mama, starts_with("mama_q99")) %>% 
    pivot_longer(-uid_mama) %>% 
    count(uid_mama, value) %>% 
    filter(value == 1) %>% 
    select(uid_mama, nb_genres = "n") %>% 
    right_join(m)
  
  # Code factors
  m <- m %>% 
    mutate(sexe = factor(eayy_a1, levels = 1:2, labels = c("Man", "Woman")),
           age_cat = factor(eayy_a2a_rec, levels = 4:14, labels = c("24yo and less", "25-29yo", "30-34yo", "35-39yo", "40-44yo", "45-49yo", "50-54yo", "55-59yo", "60-64yo", "65-69yo", "70+yo")),
           education = factor(eayy_b18c, levels = 1:5, labels = c("Middle school and less", "Vocational training", "High school degree", "College", "Graduate")),
           hh_income = factor(eayy_e2a_rec, levels = 3:13, labels = c("Less than 800€", "800-999€", "1000-1199€", "1200-1499€", "1500-1799€", "1800-1999€", "2000-2499€", "2500-2999€", "3000-3999€", "4000-5999€", "6000€ and more")))
  
  return(m)
}
