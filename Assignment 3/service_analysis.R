# =============================================================================
# service_analysis.R  -  Module 3: service summaries, exposure and risk
# =============================================================================

# Reference table of commonly risky exposed services (port + protocol).
# Educational reference list - adjust to the policy of the organisation.
risky_service_table <- function() {
  txt <- "port,protocol,risk_level,reason
21,TCP,High,FTP sends credentials in clear text
23,TCP,High,Telnet sends credentials in clear text
69,UDP,High,TFTP has no authentication
135,TCP,High,MS-RPC endpoint mapper exposed
139,TCP,High,NetBIOS session service exposed
445,TCP,High,SMB file sharing exposed
512,TCP,High,Legacy r-service (rexec)
513,TCP,High,Legacy r-service (rlogin)
514,TCP,High,Legacy r-service (rsh)
1433,TCP,High,Database port exposed (MS SQL)
3306,TCP,High,Database port exposed (MySQL)
3389,TCP,High,Remote Desktop exposed
5900,TCP,High,VNC remote control exposed
25,TCP,Medium,SMTP open - check relay configuration
111,TCP,Medium,rpcbind exposes service information
161,UDP,Medium,SNMP exposure - check community strings
500,UDP,Medium,IKE/VPN endpoint exposed
1900,UDP,Medium,UPnP should not be exposed
8080,TCP,Medium,Alternative HTTP/proxy port exposed
22,TCP,Low,Remote administration (SSH) reachable
53,UDP,Low,DNS reachable - check recursion
80,TCP,Low,Unencrypted HTTP
123,UDP,Low,NTP reachable - check amplification"
  utils::read.csv(text = txt, stringsAsFactors = FALSE)
}

# List open (and possibly open) services with their risk level.
#   services : cleaned services or merged data (needs ip_address, port, protocol, state)
# Returns one row per exposed service, highest risk first.
classify_exposure <- function(services) {
  .assert_df(services, c("ip_address", "port", "protocol", "state"), "services")
  ref <- risky_service_table()
  d <- services[services$state %in% c("open", "open|filtered"), , drop = FALSE]
  d$port <- suppressWarnings(as.integer(d$port))
  d$exposure_status <- ifelse(d$state == "open", "Exposed", "Possibly exposed")
  d <- merge(d, ref, by = c("port", "protocol"), all.x = TRUE, sort = FALSE)
  d$risk_level[is.na(d$risk_level)] <- "Informational"
  d$reason[is.na(d$reason)] <- "No known risk pattern in reference table"
  rank <- c(High = 1, Medium = 2, Low = 3, Informational = 4)
  d <- d[order(rank[d$risk_level], d$ip_address, d$port), , drop = FALSE]
  cols <- intersect(c("ip_address", "hostname", "os", "scan_session", "port", "protocol",
                      "service_name", "version", "state", "exposure_status",
                      "risk_level", "reason"), names(d))
  d <- d[, cols, drop = FALSE]
  rownames(d) <- NULL
  d
}

# Per-service summary: how many hosts/records/sessions expose each port+protocol.
summarize_services <- function(merged) {
  .assert_df(merged, c("ip_address", "port", "protocol", "service_name", "state", "scan_session"), "merged")
  if (nrow(merged) == 0) stop("No records to summarise.", call. = FALSE)
  d <- merged
  key <- paste(d$port, d$protocol, d$service_name, sep = "|")
  idx <- split(seq_len(nrow(d)), key)
  rows <- lapply(idx, function(i) {
    data.frame(port = d$port[i[1]], protocol = d$protocol[i[1]],
               service_name = d$service_name[i[1]], records = length(i),
               hosts_seen = length(unique(d$ip_address[i])),
               open_records = sum(d$state[i] == "open"),
               sessions = length(unique(d$scan_session[i])),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$pct_open <- round(100 * out$open_records / out$records, 1)
  out <- out[order(-out$open_records, out$port), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Host-level configuration findings that a port table alone does not show.
#   * large_attack_surface : host has more than 'open_port_threshold' open ports
#   * legacy_version_banner: open service banner matches a known-legacy pattern
flag_configuration_issues <- function(merged, open_port_threshold = 8) {
  .assert_df(merged, c("ip_address", "port", "protocol", "state", "version"), "merged")
  if (!is.numeric(open_port_threshold) || open_port_threshold < 1) {
    stop("'open_port_threshold' must be a positive number.", call. = FALSE)
  }
  out <- list()
  op <- merged[merged$state == "open", , drop = FALSE]
  if (nrow(op) > 0) {
    cnt <- tapply(paste(op$port, op$protocol), op$ip_address, function(x) length(unique(x)))
    big <- cnt[cnt > open_port_threshold]
    if (length(big) > 0) {
      out[[1]] <- data.frame(ip_address = names(big), finding = "large_attack_surface",
                             detail = paste(big, "open ports"), stringsAsFactors = FALSE)
    }
    pat <- "vsftpd 2\\.3|OpenSSH[ _][1-6]\\.|Apache 2\\.2|MySQL 5\\.[0-6]|SMBv1|Telnetd"
    leg <- op[!is.na(op$version) & grepl(pat, op$version, ignore.case = TRUE), , drop = FALSE]
    leg <- leg[!duplicated(paste(leg$ip_address, leg$port, leg$protocol)), , drop = FALSE]
    if (nrow(leg) > 0) {
      out[[length(out) + 1]] <- data.frame(ip_address = leg$ip_address,
        finding = "legacy_version_banner",
        detail = paste0(leg$port, "/", leg$protocol, " ", leg$version), stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, out)
  if (is.null(res)) {
    res <- data.frame(ip_address = character(0), finding = character(0),
                      detail = character(0), stringsAsFactors = FALSE)
  }
  rownames(res) <- NULL
  res
}
