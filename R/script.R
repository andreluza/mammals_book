# load packages
source("R/packages.R")

# load coord data
data_coords <- read.xlsx(here ("data","data_collection.xlsx"),sheet=1)

# unique papers
# all papers
unique(data_coords$`UT.(Unique.WOS.ID)`)

# aggregated papers
length(unique(data_coords$`UT.(Unique.WOS.ID)`)) - length(grep("WOS", unique(data_coords$`UT.(Unique.WOS.ID)`)))

# eligible papers
data_coords <- (data_coords [is.na(data_coords$Locality_name)!= T,])
unique(data_coords$`UT.(Unique.WOS.ID)`)

# N coords
dim(data_coords)

# transform coords
#data_coords$LatDecDeg <- round(as.numeric(data_coords$LatDecDeg),3)
#data_coords$LongDecDeg <- round(as.numeric(data_coords$LongDecDeg),3)

# convert the data frame into a spatial coordinates object
complet_set_of_coordinates_sp <- (data_coords[,c("LatDecDeg", "LongDecDeg")])
coordinates(complet_set_of_coordinates_sp) <- ~LongDecDeg + LatDecDeg # as coord
crs (complet_set_of_coordinates_sp)<- "+proj=longlat +datum=WGS84 +no_defs" # set crs

# create a grid for mapping
# based on the extent of extracted data
grd_df <- expand.grid(x = seq(from = extent (complet_set_of_coordinates_sp)[1]-8,
                              to = extent (complet_set_of_coordinates_sp)[2]+8, 
                              by = 0.5),
                      y = seq(from = extent (complet_set_of_coordinates_sp)[3]-8,                                           
                              to = extent (complet_set_of_coordinates_sp)[4]+8, 
                              by = 0.5))  # expand points to grid

# Convert grd object to a matrix and then turn into a spatial
# points object
coordinates(grd_df) <- ~x + y

# Sp points into raster
grd_raster <- (raster(grd_df,resolution = 0.5))
crs(grd_raster) <-crs(complet_set_of_coordinates_sp)

# the campos sulinos
# for mapping, cropping, etc
campos<- readOGR(dsn= here("data","Re_ Shapefile Campos Sulinos base IBGE 1_250mil",
                           "campos sulinos_base ibge 250mil"),encoding="latin1", 
                 layer="campos sulinos_base ibge 250mil")
# mask campos
grd_raster <- (crop (grd_raster,extent(campos)))
grd_raster <- (mask (grd_raster,campos))

# table(matched_data_coordinates_mpas_institutions$YearCat)
data_to_map <- unique(data_coords$`UT.(Unique.WOS.ID)`)
complet_set_of_coordinates <- lapply (data_to_map, function (i) {
  # subsetting based on DOI (also rm this column )
  dat<-data_coords [which(data_coords$`UT.(Unique.WOS.ID)` %in% i),
                                                        c("LongDecDeg","LatDecDeg")]
  # aggregate
  #data_DOI<-aggregate(data_DOI,by=list(rep(1,nrow(data_DOI))), FUN=mean)
  # convert  data into spatial coordinates
  coordinates(dat) <- ~LongDecDeg + LatDecDeg
  
  # rasterize to count the number of papers per cell
  overlap_grid_dois <- rasterize(dat,
                                 grd_raster,
                                 fun="count")
  #clamp
  overlap_grid_dois<-clamp(overlap_grid_dois, 
                           lower=0,
                           useValues=T)
  
  
})

# stack rasters from the several papers
stack_DOIS <- (stack(complet_set_of_coordinates))
# sum the number of papers per cell
rs1 <- calc(stack_DOIS, sum,na.rm=T)
rs1_total<-clamp(rs1, lower=0, useValues=T)

# number of cells covered by research
(table(values(rs1_total)>0))[2]/sum(table(values(rs1_total)>0))

# plot - number of sites per cell

plot2 <- gplot(rs1_total) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -60, -45), 
               ylim = c(-40, -20), ratio = 1) +
  scale_fill_viridis(option="magma",direction=-1,begin=0,
                     breaks = seq (range(values(rs1_total),na.rm=T)[1],
                                   range(values(rs1_total),na.rm=T)[2],
                                   1),
                     limits=c(range(values(rs1_total),na.rm=T)[1]+1,
                              range(values(rs1_total),na.rm=T)[2]),
                     na.value=NA,
                     name="# coordinates") +
  ggtitle ("C) Mapping of the scientific knowledge") + 
  theme_classic() +
  theme (legend.position = "top",
         legend.direction = "horizontal") + 
  xlab("Longitude")+
  ylab("Latitude")

# south america
# for mapping, cropping, etc
southAme<- readOGR(dsn= here("data","South_America"),encoding="latin1", 
                   layer="South_America")

# plot
plot2_total <- plot2 + geom_polygon(data=southAme, 
                                    aes(x=long, y=lat, group=group),
                                    size = 0.1, 
                                    fill="white", 
                                    colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")

# plot campos
plot2_total <- plot2_total + geom_polygon(data=campos, 
                              aes(x=long, y=lat, group=group),
                              size = 0.1, 
                              fill="gray95", 
                              colour="gray95",
                              alpha=1)

plot2_total

# papers per year

data_bar<-data.frame(table(aggregate (data_coords, by=list(data_coords$`UT.(Unique.WOS.ID)`), 
                                      FUN="mean")[,"Publication.Year"]))
to_paste <- data.frame ("Var1"=as.factor(seq(2002,2021) [which(seq(2002,2021) %in% data_bar$Var1 == F)]),
                   "Freq"=0)
# paste
data_bar<- rbind (data_bar,
                  to_paste)
# order
data_bar<- data_bar[order(as.numeric(paste(data_bar$Var1))),]
data_bar$Var1 <- as.numeric(paste(data_bar$Var1))

barplot_n <- ggplot (data_bar, aes(x= (Var1),y=Freq,fill=Freq)) + 
  geom_bar(stat="identity")+
  theme_classic() + 
  xlab("Year") +
  ylab("Number of articles") +
  ggtitle ("A) Number of articles over time")+
  theme(axis.text.x = element_text(angle=0),
        legend.position = "none") + 
  scale_fill_viridis_c(option="magma", direction=-1,begin=0.2)

# --------------------------------------------
# topic modeling
# nice help here:
# https://cran.r-project.org/web/packages/corpus/vignettes/corpus.html
# https://cran.r-project.org/web/packages/corpus/vignettes/textdata.html
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html
# https://www.tidytextmining.com/topicmodeling.html

# aggregate data
papers <- unique(data_coords$`UT.(Unique.WOS.ID)`)

subset_data <- lapply (papers, function (i) {
    # subset
    subset_data <- data_coords[which(data_coords$`UT.(Unique.WOS.ID)` %in% i),]
    # only the first line of each paper
    subset_data <- subset_data[1,]

})
subset_data<- do.call (rbind.data.frame, subset_data)# melt

# text into corpus 
text <- as_corpus_text(subset_data$Abstract)
# edits
clean_text <- tolower(text)
clean_text <- gsub("[']", "", clean_text) 
clean_text <- gsub("[[:punct:]]", " ", clean_text)
clean_text <- gsub('[[:digit:]]+', " ", clean_text)
clean_text <- gsub("[[:space:]]+", " ", clean_text)
clean_text <- gsub("brazilian", " ", clean_text)
clean_text <- gsub("brazil", " ", clean_text)
clean_text <- gsub("southern", " ", clean_text)
clean_text <- gsub("south", " ", clean_text)
clean_text <- gsub("species", " ", clean_text)
clean_text <- gsub("study", " ", clean_text)
clean_text <- gsub("area*", " ", clean_text)
clean_text <- gsub("mammal*", " ", clean_text)
clean_text <- gsub("forests", "forest", clean_text)
clean_text <- gsub("grasslands", "grassland", clean_text)
clean_text <- gsub("occupation", "occupancy", clean_text)
clean_text <- gsub("[[:space:]]+", " ", clean_text)
clean_text <- trimws(clean_text) # remove white space

# remove stopwords
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
clean_text <- stringr::str_replace_all(clean_text, stopwords_regex, '')

# tokenize to find small words
#small_words<-unlist(text_tokens(clean_text))[nchar (unlist(text_tokens(clean_text))) <=2]
#clean_text <- stringr::str_replace_all(clean_text, small_words, '')

# matrix of terms for LDA
text_terms <- term_matrix(clean_text)# ,ngrams =1

# choose the number of topics
result <- FindTopicsNumber(
  text_terms,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
(FindTopicsNumber_plot(result))

# also save the plot
pdf (file=here ("output", "Ntopics.pdf"))

(FindTopicsNumber_plot(result))

dev.off()

# topics to choose (maximum grifitths)
n_topics<- result[which(result$CaoJuan2009 == min(result$CaoJuan2009)),"topics"]

# topic modeling
require(topicmodels)
chapters_lda <- LDA(text_terms, k = n_topics, 
                    control = list(seed = 1234))
# tidy text
library(tidytext)
ap_topics <- tidy(chapters_lda, matrix = "beta")
ap_topics <- ap_topics[which(nchar (ap_topics$term)>=3),]

# represent

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# labels
labels_facet <- c(
  `1` = "Distribution & activity",
  `2` = "Small mammals' ecology",
  `3` = "NA"
)

plot3 <- ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free",
             labeller = as_labeller(labels_facet))+
             theme_classic() + 
  theme (axis.text.y = element_text(size=10),
         axis.text.x = element_text(size=7))+
  scale_y_reordered() + 
  scale_fill_viridis_d(option="magma",begin=0.1,end = 0.8) + 
  xlab("Probability (Latent Dirichlet allocation, LDA)") + 
  ylab("Term") + 
  ggtitle("B) Topic modeling")

plot3

## classification per paper
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

# arrange all the plots
pdf(here ("output","fig1.pdf"),
	height=7,width=12)
grid.arrange(barplot_n,
             plot3,
             plot2_total,
             layout_matrix = rbind (c(1,1,3,3),
                                    c(1,1,3,3),
                                    c(2,2,3,3),
                                    c(2,2,3,3),
                                    c(2,2,3,3)))
dev.off()

