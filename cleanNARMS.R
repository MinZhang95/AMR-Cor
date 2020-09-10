library("readxl")
library("openxlsx")
library("dplyr")
library("ggplot2")
library("stringr")
library("Hmisc")
library("gridExtra")
library("openxlsx")

#----- Read and clean human CDC data -----
humanDat <- read.csv("data/humanDatWithMonth.csv", stringsAsFactors = FALSE)
humanDat <- humanDat %>% 
  rename(Year = Data.Year, Month = Month.of.collection) %>% 
  rename_at(vars(ends_with(".Equiv")), funs(str_replace(., ".Equiv", ".Sign"))) %>% 
  rename_at(vars(ends_with(".Rslt")), funs(str_replace(., ".Rslt", ""))) %>% 
  arrange(Year, Month)
## To match the drug abbr in animal and retail datasets
humanDat <- humanDat %>% 
  rename_at(vars(starts_with("AUG")), funs(str_replace(., "AUG", "AMC"))) %>% 
  rename_at(vars(starts_with("AZM")), funs(str_replace(., "AZM", "AZI")))

#----- Read and clean food animal USDA data -----
animalDat1 <- readxl::read_xlsx("data/NARMS-Bacterial-Isolate-Level-Data-Food-Producing-Animals-USDA-ARS-1997-2005.xlsx")
animalDat2 <- readxl::read_xlsx("data/NARMS-Bacterial-Isolate-Level-Data-Food-Producing-Animals-USDA-ARS-2006-2013.xlsx")
animalDat <- rbind(animalDat1, animalDat2)
animalDat <- animalDat %>% 
  rename(Serotype = SEROTYPE) %>% 
  rename_at(vars(ends_with(" Sign")), funs(str_replace(., " Sign", ".Sign"))) %>% 
  arrange(Year, Month)
## To match the drug abbr in human dataset
animalDat <- animalDat %>% 
  rename_at(vars(starts_with("Str")), funs(str_replace(., "Str", "STR")))

#----- Read and clean retail FDA data -----
retailDat <- readxl::read_xlsx("data/NARMS-Bacterial-Isolate-Level-Data-Retail-Meats.xlsx")
retailDat <- retailDat %>% 
  rename(Serotype = SEROTYPE) %>% 
  rename_at(vars(ends_with(" Sign")), funs(str_replace(., " Sign", ".Sign"))) %>% 
  arrange(Year, Month)
## To match the drug abbr in human dataset
retailDat <- retailDat %>% 
  rename_at(vars(starts_with("Str")), funs(str_replace(., "Str", "STR")))

#----- Read and clean ISU VDL Dat -----
# vdlDat <- read.csv("data/VDL_alldat.csv") 
# vdlDat <- vdlDat %>% 
#   filter(speciesName == "Porcine") %>% 
#   dplyr::select(Year, Month, NVSL_Result, ends_with("MIC")) 
# vdlDat <- vdlDat %>% 
#   mutate(AMP.Sign = gsub("[0-9.]", "",  Ampicillin_MIC), AMP = gsub("[^0-9.]", "",  Ampicillin_MIC) %>% as.numeric(), AMP.Sign = replace(AMP.Sign, AMP.Sign=="" & is.numeric(AMP), "="), 
#          TIO.Sign = gsub("[0-9.]", "",  Ceftiofur_MIC), TIO = gsub("[^0-9.]", "",  Ceftiofur_MIC) %>% as.numeric(), TIO.Sign = replace(TIO.Sign, TIO.Sign=="" & is.numeric(TIO), "="), 
#          TET1.Sign = gsub("[0-9.]", "",  Chlortetracyline_MIC), TET1 = gsub("[^0-9.]", "",  Chlortetracyline_MIC) %>% as.numeric(), TET1.Sign = replace(TET1.Sign, TET1.Sign=="" & is.numeric(TET1), "="), 
#          CIP1.Sign = gsub("[0-9.]", "",  Danofloxacin_MIC), CIP1 = gsub("[^0-9.]", "",  Danofloxacin_MIC) %>% as.numeric(), CIP1.Sign = replace(CIP1.Sign, CIP1.Sign=="" & is.numeric(CIP1), "="), 
#          CIP2.Sign = gsub("[0-9.]", "",  Enrofloxacin_MIC), CIP2 = gsub("[^0-9.]", "",  Enrofloxacin_MIC) %>% as.numeric(), CIP2.Sign = replace(CIP2.Sign, CIP2.Sign=="" & is.numeric(CIP2), "="), 
#          GEN1.Sign = gsub("[0-9.]", "",  Gentamicin_MIC), GEN1 = gsub("[^0-9.]", "",  Gentamicin_MIC) %>% as.numeric(), GEN1.Sign = replace(GEN1.Sign, GEN1.Sign=="" & is.numeric(GEN1), "="), 
#          GEN2.Sign = gsub("[0-9.]", "",  Neomycin_MIC), GEN2 = gsub("[^0-9.]", "",  Neomycin_MIC) %>% as.numeric(), GEN2.Sign = replace(GEN2.Sign, GEN2.Sign=="" & is.numeric(GEN2), "="), 
#          TET2.Sign = gsub("[0-9.]", "",  Oxytetracyline_MIC), TET2 = gsub("[^0-9.]", "",  Oxytetracyline_MIC) %>% as.numeric(), TET2.Sign = replace(TET2.Sign, TET2.Sign=="" & is.numeric(TET2), "="), 
#          SMX.Sign = gsub("[0-9.]", "",  Sulfadimethoxine_MIC), SMX = gsub("[^0-9.]", "",  Sulfadimethoxine_MIC) %>% as.numeric(), SMX.Sign = replace(SMX.Sign, SMX.Sign=="" & is.numeric(SMX), "="), 
#          STR.Sign = gsub("[0-9.]", "",  Spectinomycin_MIC), STR = gsub("[^0-9.]", "",  Spectinomycin_MIC) %>% as.numeric(), STR.Sign = replace(STR.Sign, STR.Sign=="" & is.numeric(STR), "="), 
#          COT.Sign = gsub("[0-9.]", "",  Trimethoprim_Sulphamethozazole_MIC), COT = gsub("[^0-9.]", "",  Trimethoprim_Sulphamethozazole_MIC) %>% as.numeric(), COT.Sign = replace(COT.Sign, COT.Sign=="" & is.numeric(COT), "="),  
#          AZI.Sign = gsub("[0-9.]", "",  Tulathromycin_MIC), AZI = gsub("[^0-9.]", "",  Tulathromycin_MIC) %>% as.numeric(), AZI.Sign = replace(AZI.Sign, AZI.Sign=="" & is.numeric(AZI), "=")) %>% 
#   mutate(NVSL_Result = R.utils::capitalize(str_replace(NVSL_Result, "Salmonella ", ""))) %>% 
#   rename(Serotype = NVSL_Result) %>% dplyr::select(-ends_with("_MIC"))

#----- 
humanAnti <- humanDat %>% dplyr::select(ends_with("Sign")) %>% colnames() %>% gsub(".Sign", "", .)
retailAnti <- retailDat %>% dplyr::select(ends_with("Sign")) %>% colnames() %>% gsub(".Sign", "", .)
animalAnti <- animalDat %>% dplyr::select(ends_with("Sign")) %>% colnames() %>% gsub(".Sign", "", .)
# vdlAnti <- vdlDat %>% dplyr::select(ends_with("Sign")) %>% colnames() %>% gsub(".Sign", "", .)

commonAnti <- intersect(intersect(humanAnti, retailAnti), animalAnti)

#----- define Concl group for all datasets -----
cutoffDf <- readxl::read_xlsx("data/cutoff3.xlsx") %>%
  filter(!is.na(`Cutoff Sign`)) %>% 
  dplyr::select(`CDC NARMS Ab`, starts_with("Cutoff")) %>% 
  rename(anti = `CDC NARMS Ab`, Sign = `Cutoff Sign`, Rslt = `Cutoff Rslt`) %>% 
  mutate(RsltX = ifelse(Sign == ">", Rslt + 0.0001, Rslt)) %>% 
  tibble::column_to_rownames(var = "anti")

source("formatDat.R")
source("dat2Stats.R")
source("scatterPlot.R")

naiveRes <- function(sero, anti) {
  
  humanSub <- formatDat(humanDat, sero, anti) %>% filter(Time >= 2011, Time < 2018)
    
  retailSub <- formatDat(retailDat, sero, anti) %>% filter(Time >= 2011, Time < 2018)
  
  animalSub <- formatDat(animalDat, sero, anti) %>% filter(Time >= 2011, Time < 2018)
  
  # use when vdl's two drugs correspond to the same antibiotic
  # vdlSub1 <- formatDat(vdlDat, sero, paste0(anti,1))
  # vdlSub2 <- formatDat(vdlDat, sero, paste0(anti,2))
  # vdlSub <- rbind(vdlSub1, vdlSub2) %>% arrange(Time)
  
  # vdlSub <- formatDat(vdlDat, sero, anti) # comment out without vdl data
  
  all <- dat2Stats(humanSub) %>% 
    {if(nrow(dat2Stats(retailSub))>0) left_join(., dat2Stats(retailSub), c("Time"="Time", "Concl"="Concl")) else .} %>%
    {if(nrow(dat2Stats(animalSub))>0) left_join(., dat2Stats(animalSub), c("Time"="Time", "Concl"="Concl")) else .} %>% 
    {if(F) left_join(., dat2Stats(vdlSub), c("Time"="Time", "Concl"="Concl")) else .} %>% 
    filter(Time >= 2011)
    # F <--> nrow(dat2Stats(vdlSub))>0, use F as condition when without vdl data
  colnames(all) <- paste("Time", "Concl", 
        ifelse(nrow(dat2Stats(humanSub))>0, "humanProp humanMean", ""),
        ifelse(nrow(dat2Stats(retailSub))>0, "retailProp retailMean", ""),
        ifelse(nrow(dat2Stats(animalSub))>0, "animalProp animalMean", ""),
        ifelse(F, "vdlProp vdlMean", "")) %>% strsplit(., "\\s+") %>% .[[1]]
  
  corOfMean <- all %>% filter(Concl=="S") %>%  dplyr::select(ends_with("Mean")) %>% 
    rename_at(vars(ends_with("Mean")), funs(str_replace(., "Mean", ""))) %>% 
    as.matrix() %>% rcorr(., type = "pearson")
  # write.csv(corOfMean$r,
  #           file = paste0("./naiveRes/", gsub(",|:", "", sero), "_", anti, "_",
  #                         corOfMean$r %>% as.vector() %>% unique() %>% sort(decreasing = T) %>% .[2] %>% round(.,2),
  #                         ".csv"))

  # corOfProp <- all %>% filter(Concl=="R") %>%  dplyr::select(ends_with("Prop")) %>% as.matrix() %>%
  #   rcorr(., type = "pearson")
  # write.csv(corOfProp$r, # NARMS folder when without VDL
  #           file = paste0("./naiveRes/prop/", gsub(",|:", "", sero), "_", anti, "_",
  #                         corOfProp$r %>% as.vector() %>% unique() %>% sort(decreasing = T) %>% .[2] %>% round(.,2),
  #                         ".csv"))
  
  # reslist <- list()
  # # scatter matrix
  op <- options(warn = (-1)) # suppress warnings
  all %>% ggplot(., aes(x=humanMean, y=animalMean)) +
    geom_point() + geom_jitter(width = 0.02, height = 0.02) +
    xlab( expression(paste(bold("Human"), ": monthly mean of ", log[2],"MIC"))) +
    ylab( expression(paste(bold("Food animal"), ": monthly mean of ", log[2],"MIC"))) +
    scale_x_continuous(breaks = seq(1.25, 2.25, 0.25), limits = c(1.2,2.25)) +
    scale_y_continuous(breaks = seq(1, 2.5, 0.25)) +
    theme_bw() +
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=15), 
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) 
  
  reslist[[1]] <- pairs(all %>% ungroup() %>% filter(Concl == "S") %>% dplyr::select(contains("Mean")) %>% 
                          rename_at(vars(ends_with("Mean")), funs(str_replace(., "Mean", ""))),
                        lower.panel = panel.cor, upper.panel = upper.panel)

  # reslist[[2]] <- pairs(all %>% ungroup() %>% filter(Concl == "R") %>% dplyr::select(ends_with("Prop")),
  #                       lower.panel = panel.cor, upper.panel = upper.panel)
  # 
  # # line plot along time
  # reslist[[3]] <- all %>% filter(Concl == "R") %>%
  #   dplyr::select(Time, ends_with("Prop")) %>%
  #   tidyr::gather(population, proportion, contains("Prop")) %>%
  #   mutate(population = str_replace(population, "Prop", "")) %>%
  #   filter(!is.na(proportion)) %>%
  #   ggplot(., aes(x = Time, y = proportion, color = population)) +
  #   geom_point() + geom_line() +
  #   labs(title = paste0(sero, "/", anti, ", resistance proportion"))
  # 
  reslist[[4]] <- all %>% filter(Concl == "S") %>%
    dplyr::select(Time, ends_with("Mean")) %>%
    tidyr::gather(population, naive_mean, contains("Mean")) %>%
    mutate(population = str_replace(population, "Mean", "")) %>%
    filter(!is.na(naive_mean)) %>% filter(population %in% c("animal", "human")) %>% 
    ggplot(., aes(x = Time, y = naive_mean, color = population)) +
    geom_point() + geom_line() +
    labs(title = paste0(sero, "/", anti, ", naive mean in non-resistant component")) +
    xlim(c(NA,2016))
# 
#   for (i in 3:4) {
#     pdf(paste0("naiveRes/", gsub(",|:", "", sero), "/", anti, "_p", i, ".pdf"))
# 
#     print(reslist[[i]])
# 
#     dev.off()
#   }

}
for (i in seroLarge) {
  for (j in c("TET", "CIP", "GEN")) {
    naiveRes(i,j)
  }
}

# realdata description
animalDat %>% filter(GENUS=="S", Serotype=="Typhimurium") %>% 
  .[, grep("^[A-Z]{3}$", colnames(animalDat))] %>%
  tidyr::gather(., "anti", "value") %>% 
  mutate(notNA=ifelse(is.na(value), 0, 1)) %>% 
  group_by(anti) %>% summarise(n=sum(notNA)) %>% 
  arrange(-n) %>% View()

formatDat(humanDat, sero, anti) %>% filter(Time >= 2011, Time < 2016) %>% nrow()
952/60

formatDat(animalDat, sero, anti) %>% filter(Time >= 2011, Time < 2016) %>% nrow()
572/60

formatDat(humanDat, sero, anti) %>% filter(Time >= 2011, Time < 2016) %>% count(Sign, Rslt, Concl) %>% arrange(Rslt)
formatDat(animalDat, sero, anti) %>% filter(Time >= 2011, Time < 2016) %>% count(Sign, Rslt, Concl) %>% arrange(Rslt)

formatDat(humanDat, sero, anti) %>% filter(Time >= 2011, Time < 2016, Sign==">") 
formatDat(animalDat, sero, anti) %>% filter(Time >= 2011, Time < 2016, Sign==">") 
