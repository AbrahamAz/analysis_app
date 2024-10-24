
od = get_business_onedrive()
file_list <- od$list_items("Documents/Shapefiles")%>% 
  mutate(country = str_extract(name, '(.*?)(?=\\_)'),
         adminBnd = str_extract(name, '(?<=\\_)(.*?)(?=\\.)'))

# sp_con <-
#   sp_connection(
#     Address  = 'https://acted-my.sharepoint.com/personal/nestor_cheryba@reach-initiative.org',
#     Username  = 'nestor.cheryba@reach-initiative.org',
#     Password  = "Par1s1sBurning"
#   )
# 
# file_list <- sp_list_contents(sp_con, 'Documents/Analysis tool') %>% 
#   as.data.frame() %>% 
#   filter(Type == 'File') %>% 
#   mutate(country = str_extract(Name, '(.*?)(?=\\_)'),
#          adminBnd = str_extract(Name, '(?<=\\_)(.*?)(?=\\.)'))

countries <-  file_list %>%
  arrange(country) %>%
  pull(country) %>% unique()


# file_list <- file_list %>%
#   # filter(str_detect(adminBnd, "0")) %>%
#   mutate(rurl = str_extract(ServerRelativeUrl, '(?<=_org\\/)(.*?)$')) %>%
#   mutate(country = str_extract(rurl, "(?<=Analysis tool\\/)(.*?)(?=\\_)")) %>%
#   mutate(admin = as.numeric(str_extract(rurl,"(?<=Admin)(.*?)."))) %>%
#   filter(admin == 0)
# 
# 
# rurl_list <- file_list %>%
#   pull(rurl)
# 
# country_list <- file_list %>%
#   pull(country) %>% unique
# 
# # shape_file <- data.frame()
# country_list <- c("Afghanistan","Haiti","Myanmar")
# for (country in country_list){
#
#   newrurl_list <- rurl_list[grepl(country,rurl_list)]
#
#
#   # admin_level <- file_list %>%
#   #   filter(country == i)%>%
#   #   pull(admin) %>% max
#
#   # for(n in 0:admin_level){
#     # newrurl_list_admin <- newrurl_list[grepl(n,newrurl_list)]
#     for(i in newrurl_list){
#       rurl <- i
#       extension <- str_extract(i, "(?=\\.)(.*?)$")
#       if (grepl(sp_con$site$site, rurl)) {
#         rurl <- sub(paste0("[\\/]?", sp_con$site$site, "[\\/]?"),
#                     "", i)
#       }
#       rurl = sub("^\\/?", "", rurl)
#       url <- file.path(do.call(file.path, sp_con$site), URLencode(rurl))
#       destfile  <-  basename(url)
#
#       temp_file <- tempfile(fileext = extension, pattern = paste0(country,"_","Admin0"))
#       temp_file <- str_remove(temp_file,paste0('(?<=Admin0)(.*?)(?=\\.)'))
#       if(extension == ".shp"){
#         save_name <- temp_file
#       }
#       response <-  httr::GET(
#         url,
#         httr::set_cookies(
#           rtFa = sp_con$cookies$rtFa,
#           FedAuth = sp_con$cookies$FedAuth
#         ),
#         config = sp_con$config,
#         httr::write_disk(temp_file, overwrite = T)
#       )
#     }
#     if (country %in% c("Lebanon","Tchad","Central African Republic")){
#       admin0_shp <- st_read(save_name)%>%
#         select(admin4Name,admin4Pcod) %>%
#         rename(ADM_NAME = "admin4Name",
#                ADM_PCODE = admin4Pcod) %>%
#         mutate(REGEX = paste0("^[A-Z]{",nchar(str_extract(ADM_PCODE,"^\\d?[a-zA-Z\\s]+")[1]),"}[0-9]{",nchar(str_extract(ADM_PCODE,"\\d+")[1]),"}"))
#     } else if (country == "Syria"){
#       admin0_shp <- st_read(save_name)%>%
#         select(NAME_EN,PCODE) %>%
#         rename(ADM_NAME = "NAME_EN",
#                ADM_PCODE = "PCODE")%>%
#         mutate(REGEX = paste0("^[A-Z]{",nchar(str_extract(ADM_PCODE,"^\\d?[a-zA-Z\\s]+")[1]),"}[0-9]{",nchar(str_extract(ADM_PCODE,"\\d+")[1]),"}"))
#       }else if (country == "Benin"){
#         admin0_shp <- st_read(save_name) %>%
#           select(adm4_name,admin4Pcod) %>%
#           rename(ADM_NAME = "adm4_name",
#                  ADM_PCODE = "admin4Pcod")%>%
#           mutate(REGEX = paste0("^[A-Z]{",nchar(str_extract(ADM_PCODE,"^\\d?[a-zA-Z\\s]+")[1]),"}[0-9]{",nchar(str_extract(ADM_PCODE,"\\d+")[1]),"}"))
#       }else if (country %in% c("Senegal","Mali","Ivory Coast","Democratic Republic of the Congo","Burkina Faso")){
#       admin0_shp <- st_read(save_name)%>%
#         select(ADM4_FR,ADM4_PCODE) %>%
#         rename(ADM_NAME = "ADM4_FR",
#                ADM_PCODE = "ADM4_PCODE")%>%
#         mutate(REGEX = paste0("^[A-Z]{",nchar(str_extract(ADM_PCODE,"^\\d?[a-zA-Z\\s]+")[1]),"}[0-9]{",nchar(str_extract(ADM_PCODE,"\\d+")[1]),"}"))
#     }else if (country %in% c("Colombia")){
#       admin0_shp <- st_read(save_name)%>%
#         select(ADM2_ES,ADM2_PCODE) %>%
#         rename(ADM_NAME = "ADM2_ES",
#                ADM_PCODE = "ADM2_PCODE")%>%
#         mutate(REGEX = paste0("^[A-Z]{",nchar(str_extract(ADM_PCODE,"^\\d?[a-zA-Z\\s]+")[1]),"}[0-9]{",nchar(str_extract(ADM_PCODE,"\\d+")[1]),"}")) %>%
#         st_simplify(preserveTopology = T,dTolerance = 2000)
#     }else {
#
#       admin0_shp <- st_read("www/shapefile/Ukraine_Admin4.shp") %>%
#         select(ADM4_EN, ADM4_PCODE) %>%
#         rename(ADM_NAME = "ADM4_EN",
#                ADM_PCODE = "ADM4_PCODE")%>%
#         mutate(REGEX = paste0("^[A-Z]{",nchar(str_extract(ADM_PCODE,"^\\d?[a-zA-Z\\s]+")[1]),"}[0-9]{",nchar(str_extract(ADM_PCODE,"\\d+")[1]),"}"))
#       %>%
#         st_simplify(preserveTopology = T,dTolerance = 1000)
#
#     }
#
#     st_write(admin0_shp, paste0("www/shapefile/Ukraine_Admin4.shp"))
#     rm(admin0_shp)
#     temp_file <- str_extract(temp_file,paste0('(.*?)(?=',country,')'))
#     for (i in list.files(temp_file)){
#       if(str_detect(i,"Admin")){
#         file <- paste0(temp_file,i)
#         file.remove(file)
#       }
#     }
#   # }
# }


### FIX SUDAN
## continue from admin 2

# shape_file <- shape_file %>%
#   mutate(ADM0_EN = case_when(ADM0_EN == "Republic of Moldova (the)" ~ "Moldova",
#                              ADM0_EN == "Republic of Armenia" ~ "Armenia",
#                              ADM0_EN == "Mali (le)" ~ "Mali",
#                              ADM0_EN == "Congo (le)" ~ "Democratic Republic of the Congo",
#                              ADM0_EN == "Côte d'Ivoire" ~ "Ivory Coast",
#                              TRUE ~ ADM0_EN))
#
# shape_file <- shape_file %>%
#   st_simplify(preserveTopology = T, dTolerance = 1000)

#
# st_write(shape_file, "www/country_shapefile.shp")
#
#
# shape_file <- st_read("www/country_shapefile.shp") %>%
#   mutate(ADM0_EN = ifelse(ADM0_EN == "Syrian Arab Republic","Syria",ADM0_EN))

# all_countries <- st_read("www/shapefile/All_Countries.shp")
# countries <- c("Colombia", "Haiti", "Senegal", "Mali", "Ivory Coast", "Burkina Faso",
#                "Niger", "Nigeria", "Libya", "Chad", "Central African Republic", "Democratic Republic of the Congo",
#                "Sudan", "South Sudan", "Uganda", "Ethiopia", "Kenya", "Somalia", "Yemen", "Moldova", "Ukraine", "Lebanon", "Syria",
#                "Afghanistan", "Bangladesh", "Myanmar", "Sri Lanka", "Kyrgyzstan")
# past <- all_countries %>% 
#   filter(ADMIN %in% c("Republic of Serbia","Bosnia and Herzegovina", "Montenegro",
# "Kosovo","Albania","North Macedonia", "Vanuatu", "Cameroon", 
# "Mozambique", "Republic of the Congo", "Indonesia", "Philippines", "Nepal", "Jordan", "Azerbaijan",
# "Armenia", "Spain", "Italy", "Greece",
# "Romania", "Poland", "Slovakia", "Tunisia", "Venezuela", "Brazil", "Peru")) %>% 
#   select(ADMIN)
# 
# ongoing <- all_countries %>% 
#   filter(ADMIN %in% countries) %>% 
#   select(ADMIN)
# st_write(past, "www/shapefile/past.shp")
# st_write(ongoing, "www/shapefile/ongoing.shp")
