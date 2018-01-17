library(xml2)

convert_one_file <- function(url) {
    x <- read_xml(url)

    Sted <- xml_find_first(x, ".//Sted")
    StedType <- xml_attr(Sted, "Type")
    StedTekst <- xml_text(Sted)

    Parti <- xml_find_all(x, ".//Parti")
    PartiId <- xml_attr(Parti, "Id")
    PartiBogstav <- xml_attr(Parti, "Bogstav")
    PartiNavn <- xml_attr(Parti, "Navn")


    StemmerAntal <- xml_attr(Parti, "StemmerAntal")
    Stemmeberettigede <- xml_integer(xml_find_first(x, ".//Stemmeberettigede"))
    DeltagelsePct <- xml_double(xml_find_first(x, ".//DeltagelsePct"))
    IAltGyldigeStemmer <- xml_integer(xml_find_first(x, ".//IAltGyldigeStemmer"))
    BlankeStemmer <- xml_integer(xml_find_first(x, ".//BlankeStemmer"))
    AndreUgyldigeStemmer <- xml_integer(xml_find_first(x, ".//AndreUgyldigeStemmer"))

    data.frame(StedType, StedTekst, PartiId, PartiBogstav, PartiNavn,
    	             StemmerAntal, Stemmeberettigede, DeltagelsePct, IAltGyldigeStemmer,
 		     BlankeStemmer, AndreUgyldigeStemmer, stringsAsFactors = FALSE)
}

raw_path <- "../raw"
filenames <- dir(path = raw_path, pattern = "fintal_.*", full.names = T)

result <- data.frame()

for (i in 1:length(filenames)) {
    #cat(paste0(filenames[i],"\n"))
    #if (filenames[i] != "../raw/fintal_1194919.xml") {
    returnCode <-  tryCatch({
	result <- rbind(result, convert_one_file(filenames[i]))
    }, error = function(e) {
	cat(paste0(filenames[i]," failed:\n",e,"\n"))
    })
}

result$StedType <- as.factor(result$StedType)
result$PartiId <- as.factor(result$PartiId)
result$PartiBogstav <- as.factor(result$PartiBogstav)
result$PartiNavn <- as.factor(result$PartiNavn)
result$StemmerAntal <- as.integer(result$StemmerAntal)
result$Stemmeberettigede <- as.integer(result$Stemmeberettigede)
result$DeltagelsePct <- as.numeric(result$DeltagelsePct)
result$IAltGyldigeStemmer <- as.integer(result$IAltGyldigeStemmer)
result$BlankeStemmer <- as.integer(result$BlankeStemmer)
result$AndreUgyldigeStemmer <- as.integer(result$AndreUgyldigeStemmer)
head(result)
str(result)
#save(result, file = "folkeafstemning2009.Rdata")




