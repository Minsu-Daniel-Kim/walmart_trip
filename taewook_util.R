

## category 

get.dep.category <- function(data) {
  # define variables
  wear <- c("SHOES", "BOYS WEAR", "MENS WEAR" ,"LADIESWEAR","SHEER HOSIERY","LADIES SOCKS","GIRLS WEAR, 4-6X  AND 7-14","BRAS & SHAPEWEAR","SLEEPWEAR/FOUNDATIONS","MENSWEAR","SWIMWEAR/OUTERWEAR","ACCESSORIES")
  food <- c("DSD GROCERY", "MEAT - FRESH & FROZEN", "DAIRY", "PRODUCE","GROCERY DRY GOODS","FROZEN FOODS","SERVICE DELI","PRE PACKED DELI","COMM BREAD","BAKERY","SEAFOOD")
  medicine <- c("PHARMACY OTC","PHARMACY RX")
  beauty <- c("PERSONAL CARE","BEAUTY","HEALTH AND BEAUTY AIDS")
  luxury <- c("JEWELRY AND SUNGLASSES","OPTICAL - FRAMES","OPTICAL - LENSES")
  electronics <- c("WIRELESS","ELECTRONICS","CAMERAS AND SUPPLIES","PLAYERS AND ELECTRONICS","MEDIA AND GAMING")
  services <- c("FINANCIAL SERVICES","1-HR PHOTO")
  household <- c("PETS AND SUPPLIES", "HOUSEHOLD CHEMICALS/SUPP", "HOME MANAGEMENT","COOK AND DINE","HOUSEHOLD PAPER GOODS","BEDDING","BATH AND SHOWER","HOME DECOR","FURNITURE","LARGE HOUSEHOLD GOODS")
  alcohol <- c("LIQUOR,WINE,BEER")
  tool <- c("PAINT AND ACCESSORIES","AUTOMOTIVE","LAWN AND GARDEN")
  other <- c("NULL",  "IMPULSE MERCHANDISE" ,"CANDY, TOBACCO, COOKIES","FABRICS AND CRAFTS","CELEBRATION","HARDWARE","BOOKS AND MAGAZINES","OFFICE SUPPLIES","HORTICULTURE AND ACCESS","SPORTING GOODS","OTHER DEPARTMENTS","SEASONAL","CONCEPT STORES")
  baby <- c("INFANT CONSUMABLE HARDLINES","TOYS","INFANT APPAREL", "PLUS AND MATERNITY")

  data$DepartmentDescription <- as.character(data$DepartmentDescription)
  data$DepartmentDescription[data$DepartmentDescription %in% c("MENS WEAR", "MENSWEAR")] <- "MENS WEAR" # (MENSWEAR / MENS WEAR) same thing
  data$DepartmentCategory <- ifelse(data$DepartmentDescription %in% wear, "WEAR",
                                    ifelse(data$DepartmentDescription %in% food, "FOOD",
                                           ifelse(data$DepartmentDescription %in% medicine, "MEDICINE",
                                                  ifelse(data$DepartmentDescription %in% beauty, "BEAUTY",
                                                         ifelse(data$DepartmentDescription %in% luxury, "LUXURY",
                                                                ifelse(data$DepartmentDescription %in% electronics, "ELECTRONICS",
                                                                       ifelse(data$DepartmentDescription %in% services, "SERVICES",
                                                                              ifelse(data$DepartmentDescription %in% household, "HOUSEHOLD",
                                                                                     ifelse(data$DepartmentDescription %in% alcohol, "ALCOHOL",
                                                                                            ifelse(data$DepartmentDescription %in% tool, "TOOL",
                                                                                                   ifelse(data$DepartmentDescription %in% other, "OTHER",
                                                                                                          ifelse(data$DepartmentDescription %in% baby, "BABY", "NONE"))))))))))))
  
  data$DepartmentDescription <- factor(data$DepartmentDescription)
  data$DepartmentCategory <- factor(data$DepartmentCategory)
  return(data)
}



