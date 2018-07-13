list.of.packages <- c("scrapeR","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/p20_spatial_2018")

available.datasets = "https://dhsprogram.com/data/available-datasets.cfm"
baselink = "https://dhsprogram.com"

page_source = scrape(available.datasets,headers=T,follow=T,parse=T)[[1]]
link_elems = getNodeSet(page_source,"//td[@id='survey']/a")
links = sapply(link_elems,xmlGetAttr,"href")
links = unique(links[order(links)])
links = subset(links,links!="#footNotesLink")


svy_df_list = list()
svy_df_index = 1

for(link in links){
  SVYID = regmatches(link,regexpr("[[:digit:]]+", link))
  svy_source = scrape(paste0(baselink,link),headers=T,follow=T,parse=T)[[1]]
  datset = getNodeSet(svy_source,"//div[@id='datset_div']/a")
  if(length(datset)>0){
    datset_link = xmlGetAttr(datset[[1]],"href")
    datset_source = scrape(paste0(baselink,datset_link),headers=T,follow=T,parse=T)[[1]]
    tds = getNodeSet(datset_source,"//table[@title='Survey Data Files']/tr/td")
    td_text = paste(sapply(tds,xmlValue),collapse=" ")
    filename = regmatches(td_text,regexpr("[[:alpha:]]{2}HR..", td_text))
    if(length(filename)>0){
      filename = paste0(filename[1],"DT")
      message(filename)
      tmp_svy_df = data.frame(SVYID,filename)
      svy_df_list[[svy_df_index]] = tmp_svy_df
      svy_df_index = svy_df_index + 1
    }  
  }
  
}

svy_ids = rbindlist(svy_df_list)
save(svy_ids,file="project_data/svy_ids.RData")
