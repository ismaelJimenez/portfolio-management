library(quantmod)

sp500 <- c("MMM", "ABT", "ABBV", "ACN", "ATVI", "AYI", "ADBE", "AAP", "AES", "AET", "AFL", "AMG", 
           "A", "APD", "AKAM", "ALK", "ALB", "AA", "AGN", "LNT", "ALXN", "ALLE", "ADS", "ALL", "GOOGL",
           "GOOG", "MO", "AMZN", "AEE", "AAL", "AEP", "AXP", "AIG", "AMT", "AWK", "AMP", "ABC", "AME", 
           "AMGN", "APH", "APC", "ADI", "ANTM", "AON", "APA", "AIV", "AAPL", "AMAT", "ADM", "AJG", "AIZ",
           "T", "ADSK", "ADP", "AN", "AZO", "AVGO", "AVB", "AVY", "BHI", "BLL", "BAC", "BK", "BCR", "BAX",
           "BBT", "BDX", "BBBY", "BRK-B", "BBY", "BIIB", "BLK", "HRB", "BA", "BWA", "BXP", "BSX", "BMY", 
           "BF-B", "CHRW", "CA", "COG", "CPB", "COF", "CAH", "HSIC", "KMX", "CCL", "CAT", "CBG", "CBS", 
           "CELG", "CNC", "CNP", "CTL", "CERN", "CF", "SCHW", "CHTR", "CHK", "CVX", "CMG", "CB", "CHD", "CI",
           "XEC", "CINF", "CTAS", "CSCO", "C", "CTXS", "CLX", "CME", "CMS", "COH", "KO", "CTSH", "CL", 
           "CMCSA", "CMA", "CAG", "CXO", "COP", "ED", "STZ", "GLW", "COST", "COTY", "CCI", "CSRA", "CSX", 
           "CMI", "CVS", "DHI", "DHR", "DRI", "DVA", "DE", "DLPH", "DAL", "XRAY", "DVN", "DLR", "DFS", 
           "DISCA", "DISCK", "DG", "DLTR", "D", "DOV", "DOW", "DPS", "DTE", "DD", "DUK", "DNB", "ETFC", "EMN",
           "ETN", "EBAY", "ECL", "EIX", "EW", "EA", "EMR", "ENDP", "ETR", "EOG", "EQT", "EFX", "EQIX", "EQR",
           "ESS", "EL", "ES", "EXC", "EXPE", "EXPD", "ESRX", "EXR", "XOM", "FFIV", "FB", "FAST", "FRT", "FDX",
           "FIS", "FITB", "FSLR", "FE", "FISV", "FLIR", "FLS", "FLR", "FMC", "FTI", "FL", "F", "FTV", "FBHS", 
           "BEN", "FCX", "FTR", "GPS", "GRMN", "GD", "GE", "GGP", "GIS", "GM", "GPC", "GILD", "GPN", "GS", 
           "GT", "GWW", "HAL", "HBI", "HOG", "HAR", "HRS", "HIG", "HAS", "HCA", "HCP", "HP", "HES", "HPE", 
           "HOLX", "HD", "HON", "HRL", "HST", "HPQ", "HUM", "HBAN", "ITW", "ILMN", "IR", "INTC", "ICE", "IBM",
           "IP", "IPG", "IFF", "INTU", "ISRG", "IVZ", "IRM", "JEC", "JBHT", "JNJ", "JCI", "JPM", "JNPR", 
           "KSU", "K", "KEY", "KMB", "KIM", "KMI", "KLAC", "KSS", "KHC", "KR", "LB", "LLL", "LH", "LRCX", 
           "LM", "LEG", "LEN", "LVLT", "LUK", "LLY", "LNC", "LLTC", "LKQ", "LMT", "L", "LOW", "LYB", "MTB", 
           "MAC", "M", "MNK", "MRO", "MPC", "MAR", "MMC", "MLM", "MAS", "MA", "MAT", "MKC", "MCD", "MCK", 
           "MJN", "MDT", "MRK", "MET", "MTD", "KORS", "MCHP", "MU", "MSFT", "MHK", "TAP", "MDLZ", "MON", 
           "MNST", "MCO", "MS", "MOS", "MSI", "MUR", "MYL", "NDAQ", "NOV", "NAVI", "NTAP", "NFLX", "NWL", 
           "NFX", "NEM", "NWSA", "NWS", "NEE", "NLSN", "NKE", "NI", "NBL", "JWN", "NSC", "NTRS", "NOC", "NRG",
           "NUE", "NVDA", "ORLY", "OXY", "OMC", "OKE", "ORCL", "OI", "PCAR", "PH", "PDCO", "PAYX", "PYPL", 
           "PNR", "PBCT", "PEP", "PKI", "PRGO", "PFE", "PCG", "PM", "PSX", "PNW", "PXD", "PBI", "PNC", "RL", 
           "PPG", "PPL", "PX", "CFG", "PCLN", "PFG", "PG", "PGR", "PLD", "PRU", "PEG", "PSA", "PHM", "PVH", 
           "QRVO", "PWR", "QCOM", "DGX", "RRC", "RTN", "O", "RHT", "REGN", "RF", "RSG", "RAI", "RHI", "ROK",
           "COL", "ROP", "ROST", "RCL", "R", "CRM", "SCG", "SLB", "SNI", "STX", "SEE", "SRE", "SHW", "SIG", 
           "SPG", "SWKS", "SLG", "SJM", "SNA", "SO", "LUV", "SWN", "SE", "SPGI", "STJ", "SWK", "SPLS", "SBUX",
           "STT", "SRCL", "SYK", "STI", "SYMC", "SYF", "SYY", "TROW", "TGT", "TEL", "TGNA", "TDC", "TSO", 
           "TXN", "TXT", "COO", "HSY", "TRV", "TMO", "TIF", "TWX", "TJX", "TMK", "TSS", "TSCO", "TDG", "RIG", 
           "TRIP", "FOXA", "FOX", "TSN", "UDR", "ULTA", "USB", "UA", "UA.C", "UNP", "UAL", "UNH", "UPS", 
           "URI", "UTX", "UHS", "UNM", "URBN", "VFC", "VLO", "VAR", "VTR", "VRSN", "VRSK", "VZ", "VRTX", 
           "VIAB", "V", "VNO", "VMC", "WMT", "WBA", "DIS", "WM", "WAT", "WFC", "HCN", "WDC", "WU", "WRK", "WY",
           "WHR","WFM", "WMB", "WLTW", "WEC", "WYN", "WYNN", "XEL", "XRX", "XLNX", "XL", "XYL", "YHOO", "YUM",
           "ZBH", "ZION", "ZTS")

sp500_div <- NULL
index <- 0
for (ticker in rev(sp500)) {
  index <- index + 1
  print(paste(index, "of", length(sp500), "items processed :",ticker))
  divs <- getDividends(ticker, auto.assign=FALSE)
  colnames(divs) <- c(ticker)
  sp500_div <- merge(divs, sp500_div)
}
head(sp500_div)

write.csv(t(sp500_div), file = "/home/ismael/Documents/portfolio-management/sp500_divs_aux.csv")
