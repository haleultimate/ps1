com.env <- new.env(parent=globalenv())

symbols <- c(
  "XLF", # Financial sector ETF
  #"BRK-B",    not valid name
  "JPM",
  "WFC",
  "BAC",
  "C",
  "USB",
  "GS",
  "AIG",
  "CB",
  "AXP",
  "MET",
  "MS",
  "BLK",
  "PNC",
  "BK",
  "SCHW",
  "CME",
  "COF",
  "MMC",
  "PRU",
  "TRV",
  "SPGI",
  "ICE",
  "BBT",
  "AON",
  "AFL",
  "STT",
  "ALL",
  "DFS",
  "STI",
  "PGR",
  "MTB",
  "HIG",
  "TROW",
  "AMP",
  "FITB",
  "NTRS",
  "PFG",
  "KEY",
  "IVZ",
  "BEN",
  "RF",
  "CINF",
  "L",
  "HBAN",
  "LNC",
  "XL",
  "AJG",
  "UNM",
  "CMA",
  "NDAQ",
  "AMG",
  "ETFC",
  "TMK",
  "ZION",
  "LUK",
  "AIZ",
  "LM",
  "GDX", #gold miners ETF
  "ABX",
  "NEM",
  "GG",
  "FNV",
  "AEM",
  "SLW",
  "GOLD",
  "AU",
  "RGLD",
  "KGC",
  "BVN",
  "TAHO",
  "AUY",
  "GFI",
  "EGO",
  "PAAS",
  "BTG",
  "HL",
  "AGI",
  "NGD",
  "IAG",
  "CDE",
  #"SBGL",   #too recent
  "AG",
  "SSRI",
  #"OR",    #problem loading data from YHOO
  "HMY",
  "MUX",
  "KLDX",
  "AKG",
  "SAND"
)

#create cmn lookup (stock contains etf used as cmn, cmn contains the word 'cmn')
com.env$cmn.symbols <- c('XLF',
              'GDX'
)
num_symbols <- length(symbols)
com.env$cmns <- length(com.env$cmn.symbols)
com.env$stx <- num_symbols - com.env$cmns
cmn_num <- which(symbols %in% com.env$cmn.symbols)
if (length(cmn_num) != com.env$cmns) stop()        #bug in ticker or cmn list

com.env$cmn_lookup <- rep('cmn',num_symbols)
for (i in 1:com.env$cmns) {
  start_idx <- cmn_num[i] + 1
  end_idx <- ifelse(i < com.env$cmns,cmn_num[i+1]-1,num_symbols)
  com.env$cmn_lookup[start_idx:end_idx] <- com.env$cmn.symbols[i]
}
names(com.env$cmn_lookup) <- symbols

com.env$stx.symbols <- symbols[!(symbols %in% com.env$cmn.symbols)]
com.env$stx_list <- symbols

#stx_n <- c(2:length(symbols)) #c(2:length(symbols))  c(2:length(symbols))  #c(5:10) #
#stx.symbols <- symbols[stx_n]      #list of stx to trade
#load mktdata
#stx_list <- append(1,stx_n)
#stx_list <- symbols[stx_list]      #cmn index + stx to trade