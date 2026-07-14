# =============================================================================
# Extração automática dos dados mais atualizados de SÍFILIS CONGÊNITA (SIFC)
# do SINAN, publicados no FTP público do DATASUS (mesma fonte que alimenta
# a página https://datasus.saude.gov.br/transferencia-de-arquivos/).
#
# Este script foi escrito para rodar DENTRO de um job do GitHub Actions
# (ver .github/workflows/atualizar_sifc.yml), já dentro do repositório
# clonado automaticamente pelo `actions/checkout`.
#
# Fluxo:
#   1) Lista os arquivos disponíveis nas pastas FINAIS e PRELIM do SINAN/SIFC
#   2) Identifica o arquivo mais recente (maior ano)
#   3) Baixa o .dbc e salva (sem conversão) na pasta "Bases", renomeado com
#      a data de extração (ex.: sifc_14072026.dbc)
#   4) Grava um log de extrações
#   5) git add / commit / push (autenticado pelo próprio Actions)
# =============================================================================

## ---- 0. Pacotes necessários ------------------------------------------------
## OBS: o pacote "read.dbc" foi removido do CRAN em 14/12/2025 (arquivado),
## então ele precisa ser instalado a partir do código-fonte no GitHub
## (o pacote em si continua funcional, só não está mais nos repositórios
## padrão). Usamos o "remotes" para isso.

pacotes_cran <- c("remotes", "RCurl", "dplyr", "stringr")
novos <- pacotes_cran[!(pacotes_cran %in% installed.packages()[, "Package"])]
if (length(novos) > 0) install.packages(novos, repos = "https://cloud.r-project.org")

if (!("read.dbc" %in% installed.packages()[, "Package"])) {
  remotes::install_github("danicat/read.dbc", upgrade = "never")
}

library(RCurl)
library(read.dbc)
library(dplyr)
library(stringr)

## ---- 1. Caminhos (relativos à raiz do repositório) -------------------------
## Quando rodado via GitHub Actions, o working directory já é a raiz do repo.

repo_root      <- normalizePath(".")
pasta_relativa <- "Bases"                                # pasta de dados dentro do repo
pasta_destino  <- file.path(repo_root, pasta_relativa)

dir.create(pasta_destino, recursive = TRUE, showWarnings = FALSE)

## ---- 2. Localizar os arquivos SIFC mais recentes no FTP do DATASUS --------
## Nomenclatura dos arquivos: SIFCBR<AA>.dbc  (ex.: SIFCBR24.dbc = ano 2024)

ftp_bases <- c(
  "finais_v1" = "ftp://ftp.dados.saude.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/",
  "prelim_v1" = "ftp://ftp.dados.saude.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/",
  "finais_v2" = "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/",
  "prelim_v2" = "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
)

listar_arquivos_sifc <- function(url_pasta) {
  tryCatch({
    conteudo <- getURL(url_pasta, ftp.use.epsv = FALSE, dirlistonly = TRUE,
                        connecttimeout = 30)
    arquivos <- strsplit(conteudo, "\r?\n")[[1]]
    arquivos <- arquivos[str_detect(arquivos, regex("^SIFCBR.*\\.dbc$", ignore_case = TRUE))]
    if (length(arquivos) == 0) return(NULL)
    data.frame(
      arquivo = arquivos,
      url     = paste0(url_pasta, arquivos),
      pasta   = url_pasta,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Não foi possível acessar: ", url_pasta, " -> ", conditionMessage(e))
    NULL
  })
}

lista_arquivos <- bind_rows(lapply(ftp_bases, listar_arquivos_sifc))

if (nrow(lista_arquivos) == 0) {
  stop("Nenhum arquivo SIFC encontrado nas pastas do FTP do DATASUS.\n",
       "Possíveis causas: o runner do GitHub Actions bloqueou a porta FTP (21), ",
       "ou a estrutura de pastas do site mudou (confira manualmente em ",
       "https://datasus.saude.gov.br/transferencia-de-arquivos/).")
}

lista_arquivos <- lista_arquivos %>%
  mutate(
    ano2    = as.integer(str_extract(arquivo, "(?<=SIFCBR)\\d{2}(?=\\.dbc)")),
    ano     = ifelse(ano2 < 50, 2000 + ano2, 1900 + ano2),
    e_final = str_detect(pasta, "FINAIS")
  ) %>%
  distinct(arquivo, .keep_all = TRUE) %>%
  arrange(desc(ano), desc(e_final))

print(lista_arquivos[, c("arquivo", "ano", "e_final", "url")])

arquivo_mais_recente <- lista_arquivos[1, ]

message(sprintf(
  "Arquivo mais recente identificado: %s (ano %d, %s)",
  arquivo_mais_recente$arquivo,
  arquivo_mais_recente$ano,
  ifelse(arquivo_mais_recente$e_final, "dados finais", "dados preliminares")
))

## ---- 3. Download do arquivo .dbc (renomeado com a data de extração) --------
## Nome final: sifc_DDMMAAAA.dbc (ex.: baixado hoje -> sifc_14072026.dbc)

nome_dbc    <- sprintf("sifc_%s.dbc", format(Sys.Date(), "%d%m%Y"))
destino_dbc <- file.path(pasta_destino, nome_dbc)

download.file(
  url      = arquivo_mais_recente$url,
  destfile = destino_dbc,
  mode     = "wb",
  quiet    = FALSE
)

message("Arquivo .dbc salvo em: ", destino_dbc)

## ---- 4. Log de extrações ----------------------------------------------------
## Lê o .dbc apenas para contabilizar o número de registros no log
## (o arquivo em si permanece salvo em formato .dbc, sem conversão).

dados_sifc <- read.dbc::read.dbc(destino_dbc)

message(sprintf("Total de registros: %s | Total de colunas: %s",
                 format(nrow(dados_sifc), big.mark = "."), ncol(dados_sifc)))

log_path <- file.path(pasta_destino, "log_extracoes.csv")
novo_log <- data.frame(
  data_extracao  = as.character(Sys.time()),
  arquivo_origem = arquivo_mais_recente$arquivo,
  ano_referencia = arquivo_mais_recente$ano,
  tipo           = ifelse(arquivo_mais_recente$e_final, "final", "preliminar"),
  dbc_gerado     = nome_dbc,
  n_registros    = nrow(dados_sifc)
)

if (file.exists(log_path)) {
  log_existente <- read.csv(log_path, stringsAsFactors = FALSE)
  log_final <- bind_rows(log_existente, novo_log)
} else {
  log_final <- novo_log
}
write.csv(log_final, log_path, row.names = FALSE)

## ---- 5. Commit e push (autenticação herdada do GitHub Actions) -------------
## O actions/checkout já deixa o git configurado para autenticar o push
## usando o GITHUB_TOKEN do workflow (requer `permissions: contents: write`
## no .yml). Aqui só precisamos definir um autor para o commit.

git_run <- function(args) {
  res <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    warning("Comando git falhou: git ", paste(args, collapse = " "), "\n",
            paste(res, collapse = "\n"))
  }
  res
}

git_run(c("config", "user.name",  shQuote("github-actions[bot]")))
git_run(c("config", "user.email", shQuote("41898282+github-actions[bot]@users.noreply.github.com")))

git_run(c("add", shQuote(pasta_relativa)))

status_saida <- git_run(c("status", "--porcelain"))

if (length(status_saida) > 0 && any(nzchar(status_saida))) {
  commit_msg <- sprintf(
    "Atualização automática SIFC %d (%s) - %s",
    arquivo_mais_recente$ano,
    ifelse(arquivo_mais_recente$e_final, "dados finais", "dados preliminares"),
    format(Sys.time(), "%Y-%m-%d %H:%M")
  )
  git_run(c("commit", "-m", shQuote(commit_msg)))
  git_run(c("push"))
  message("Push realizado com sucesso para o GitHub.")
} else {
  message("Nenhuma mudança detectada em relação ao último commit — nada para enviar ao GitHub.")
}
