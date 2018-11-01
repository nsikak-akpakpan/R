check_pckg = function(x){
  
  pckgs = row.names(installed.packages())
  if(x %in% pckgs){
    
    library(x, character.only = TRUE)
    
  } else {
    
    message(paste(x, "not installed, installing now"))
    install.packages(x)
    library(x, character.only = TRUE)
    
  }
  
}

## Common SAS proc's
proc_commands = c("sql",
                  "sort",
                  "export",
                  "import",
                  "print",
                  "datasets",
                  "freq",
                  "surveyselect",
                  "contents")
proc_commands = paste("proc", proc_commands)
## ACTION ITEM: put this into a dynamic data frame
## ACTION ITEM: Add more common proc's

## Main function that iterates through each command and records the incidence of that command
get_proc_summary = function(code, key_cmds){
  
  code = tolower(code)
  key_cmds = tolower(key_cmds)
  
  if(length(key_cmds) == 1){
    
    occurrence = lapply(X = key_cmds, FUN = grep, x = code, fixed = TRUE)
    
  } else {
    
    occurrence = sapply(X = key_cmds, FUN = grep, x = code, fixed = TRUE)
    
  }
  
  occurrence_df = as.data.frame(sapply(X = occurrence, FUN = length)) 
  names(occurrence_df) = "Occurrence"
  # occurrence_df$Proportion = signif(occurrence_df$Occurrence/sum(occurrence_df$Occurrence)*100, 4)
  occurrence_df = occurrence_df[order(occurrence_df$Occurrence, decreasing = TRUE),,drop = FALSE]
  occurrence_df$Command = row.names(occurrence_df)
  occurrence_df = occurrence_df[,c("Command", "Occurrence")]
  return(occurrence_df)
  
}

get_extensions = function(raw_code, pattern){
  
  raw_code = tolower(raw_code)
  pattern = tolower(pattern)
  outfile = raw_code[grep(x = raw_code, 
                          pattern = pattern, 
                          fixed = TRUE)]
  if(length(outfile) == 0){
    ext_df = data.frame("File_Extensions" = "None", Occurrence = "N/A")
    names(ext_df)[1] = "File Extensions"
    return(ext_df)
  }
  outfile_split = strsplit(outfile, 
                           split = ".", 
                           fixed = TRUE)
  outfile_ext = unlist(lapply(outfile_split, tail, n = 1))
  outfile_ext = trimws(gsub("[^[:alnum:][:space:]]","",outfile_ext))
  outfile_ext = outfile_ext[which(nchar(outfile_ext) < 5)]
  outfile_ext = outfile_ext[which(nchar(outfile_ext) > 0)]
  if(length(outfile_ext) == 0){
    ext_df = data.frame("File_Extensions" = "None", Occurrence = "N/A")
    names(ext_df)[1] = "File Extensions"
    return(ext_df)
  }
  outfile_ext_df = as.data.frame(table(outfile_ext))
  names(outfile_ext_df) = c("File Extensions", "Occurrence")
  outfile_ext_df = outfile_ext_df[order(outfile_ext_df$Occurrence, 
                                        decreasing = TRUE),]
  return(outfile_ext_df)
  
}


get_libnames = function(raw_code){
  
  raw_code = tolower(raw_code)
  libnames = raw_code[grep(x = raw_code, pattern = "libname", fixed = TRUE)]
  libnames_df = data.frame(table(libnames))
  libnames_df = libnames_df[order(libnames_df$Freq, decreasing = TRUE),]
  names(libnames_df)[2] = "Occurrence"
  libnames_df$libnames = substring(as.character(libnames_df$libnames), 8)
  return(libnames_df)
  
}
