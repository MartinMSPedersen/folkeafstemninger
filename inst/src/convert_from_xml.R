library(xml2)

convert_one_file <- function(uri) {
    x <- read_xml(uri)

    Sted <- xml_find_first(x, ".//Sted")
    StedType <- xml_attr(Sted, "Type")
    StedTekst <- xml_text(Sted)

    Parti <- xml_find_all(x, ".//Parti")
    Stemme <- xml_attr(Parti, "Bogstav")


    StemmerAntal <- xml_attr(Parti, "StemmerAntal")
    Stemmeberettigede <- xml_integer(xml_find_first(x, ".//Stemmeberettigede"))
    DeltagelsePct <- xml_double(xml_find_first(x, ".//DeltagelsePct"))
    BlankeStemmer <- xml_integer(xml_find_first(x, ".//BlankeStemmer"))
    AndreUgyldigeStemmer <- xml_integer(xml_find_first(x, ".//AndreUgyldigeStemmer"))

    nyeNavne <- c("Dato", "Område", "Sted", "Stemme", "Antal", "Stemmeberettigede", "DeltagelsePct")
    df_JaNej <- data.frame(Dato, StedType, StedTekst, Stemme, 
    	             StemmerAntal, Stemmeberettigede, DeltagelsePct, 
 		     stringsAsFactors = FALSE)
    colnames(df_JaNej) <- nyeNavne
    df_Blanke <- data.frame(Dato, StedType, StedTekst, "BLANK", 
    	             Stemme = BlankeStemmer, Stemmeberettigede, DeltagelsePct,
    		     stringsAsFactors = FALSE)
    df_Ugyldige <- data.frame(Dato, StedType, StedTekst, "UGYLDIG", 
     	             AndreUgyldigeStemmer, Stemmeberettigede, DeltagelsePct,
    		     stringsAsFactors = FALSE)
    colnames(df_JaNej) <- nyeNavne
    colnames(df_Blanke) <- nyeNavne
    colnames(df_Ugyldige) <- nyeNavne
    rbind(df_JaNej, df_Blanke, df_Ugyldige)
}

# main

folkeafstemninger <- data.frame()

for (aar in c(2009, 2014, 2015)) {
    raw_path <- paste0("raw/", aar)
    if (aar == 2009) { Dato <- "2009-06-07" }
    if (aar == 2014) { Dato <- "2014-05-25" }
    if (aar == 2015) { Dato <- "2015-12-03" }
    message(raw_path)

    filenames <- dir(path = raw_path, pattern = "fintal_.*", full.names = T)


    for (i in seq(filenames)) {
	returnCode <-  tryCatch({
	    folkeafstemninger <- rbind(folkeafstemninger, convert_one_file(filenames[i]))
	}, error = function(e) {
	    cat(paste0(filenames[i]," failed:\n",e,"\n"))
	})
    }
}

folkeafstemninger$Dato <- as.factor(folkeafstemninger$Dato)
folkeafstemninger$Område <- as.factor(folkeafstemninger$Område)
#folkeafstemninger$Stemme <- as.integer(folkeafstemninger$Stemme)

save(folkeafstemninger, file = "folkeafstemninger.RData")

