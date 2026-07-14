library(RSelenium)
library(fs)

dir_create("dados")

url <- "https://datasus.saude.gov.br/transferencia-de-arquivos/"



# Iniciando navegador
rD <- rsDriver(
  browser = "chrome",
  chromever = NULL,
  port = 4567L,
  verbose = FALSE
)

remDr <- rD$client

remDr$navigate(url)

Sys.sleep(5)




# Função auxiliar
seleciona <- function(xpath, texto){
  select <- remDr$findElement("xpath", xpath)
  
  opts <- select$findChildElements("tag name","option")

  nomes <- sapply(opts, function(x)x$getElementText()[[1]])

  pos <- which(trimws(nomes)==texto)

  if(length(pos)==0)
    stop(paste("Não encontrou:", texto))

  opts[[pos]]$clickElement()
}



# Fonte
seleciona(
 "(//select)[1]",
 "SINAN - Sistema de Informação de Agravos de Notificação"
)

Sys.sleep(2)

# Modalidade: "Dados"
seleciona(
 "(//select)[2]",
 "Dados"
)

Sys.sleep(2)


                  
# Tipo: "SIFC" 
seleciona(
 "(//select)[3]",
 "SIFC - Sífilis Congênita"
)

Sys.sleep(2)


                  
# Arquivo mais recente
ano <- remDr$findElement("xpath","(//select)[4]")

opts <- ano$findChildElements("tag name","option")

anos <- sapply(opts,function(x)x$getElementText()[[1]])

anos <- anos[grepl("^[0-9]{4}$",anos)]

ultimo <- max(as.numeric(anos))

seleciona("(//select)[4]", as.character(ultimo))

Sys.sleep(2)


               
# UF: "BR"
seleciona(
 "(//select)[5]",
 "BR"
)

Sys.sleep(2)


               
# Enviar
remDr$
  findElement(
    "xpath",
    "//button[contains(.,'Enviar')]"
  )$
  clickElement()

Sys.sleep(8)


               
# Download
remDr$
  findElement(
    "link text",
    "Download"
  )$
  clickElement()

Sys.sleep(5)

zip <- remDr$
  findElement(
    "partial link text",
    "arquivo.zip"
  )

href <- zip$getElementAttribute("href")[[1]]

destino <- file.path(
  "dados",
  paste0("SIFC_",ultimo,".zip")
)

download.file(
  href,
  destino,
  mode="wb"
)

unzip(
  destino,
  exdir=file.path(
    "dados",
    paste0("SIFC_",ultimo)
  ),
  overwrite=TRUE
)

unlink(destino)

cat("Download finalizado.\n")
