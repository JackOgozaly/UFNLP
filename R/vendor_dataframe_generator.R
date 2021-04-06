#' vendor_dataframe_generator
#' @export
vendor_dataframe_generator <- function(labels, #vector of labels
                                       vendor) #vector of vendor names
{
  vendor_df <- as.data.frame(vendor) #convert inputs to dataframe
  colnames(vendor_df) <- c("Vendor_Name")
  vendor_df$Label <- labels
  vendor_df$Label <- as.factor(vendor_df$Label)
  #count how common each label class is, order them in descending
  level_order <- vendor_df %>%
    dplyr::group_by(Label) %>%
    dplyr::summarise(no_rows = length(Label))
  label_levels <- level_order$Label
  #Create a dataframe object data can be added too
  dataframe <- as.data.frame(c('dummy'))
  colnames(dataframe) <- c("location") #change name so we can remove this later
  i <- 1 #establish count
  while (i <= length(label_levels)) #loop will run for every label class
  {
    #Subset for label class, count vendors, attach vendor names in descending order
    label_1 <- subset(vendor_df, vendor_df$Label == label_levels[i])
    vendor_count <- plyr::count(label_1$Vendor_Name)
    vendor_count <- plyr::arrange(vendor_count, -vendor_count$freq)
    rownames(vendor_count) <- NULL
    colnames(vendor_count)[1] <- paste0("new", i)
    dataframe2 <- gdata::cbindX(dataframe, vendor_count[1])
    dataframe <- dataframe2
    i = i+1
  }
  n <- match("location",names(dataframe)) #find location of dummy column
  dataframe <- dataframe[,-n] #remove dummy column
  colnames(dataframe) <- label_levels #change column names
  assign("vendor_count_df", dataframe, envir = globalenv()) #export dataframe
}
