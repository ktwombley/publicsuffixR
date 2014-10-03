

psl_URL <- "https://publicsuffix.org/list/effective_tld_names.dat"

psl_raw_list <- function() {
 readLines(file(psl_URL, "r"))
}


raw_list <- psl_raw_list

if ('memoise' %in% installed.packages()) {
  require(memoise)
  
  raw_list <- memoise(psl_raw_list)

}


create_rules <- function(intext) {
  rules <- vector()
  # The Public Suffix List consists of a series of lines, separated by \n.
  for (aline in intext){
    print(aline)
    # Each line is only read up to the first whitespace; entire lines can also be commented using //.
    if(substr(aline,1,2)=="//") {
      print("skipping comment!")
      next
    }

    # Each line which is not entirely whitespace or begins with a comment contains a rule.
  
    aline <- strsplit(aline, split="(\\s+)", perl=TRUE)[1]
    print(sprintf("chop whitespace: %s", aline))
    
    print(sprintf("length=%d", nchar(aline)))
    
    #wtf is this a bug?
    if(nchar(aline)==0 || aline=="character(0)") {
      print("skipping blank!")
      next
    }
    
    # A rule may begin with a "!" (exclamation mark). If it does, it is labelled as a "exception rule" 
    
    if(substr(aline,1,1)=="!") {
      print("an exception!")
      #  and then treated as if the exclamation mark is not present.
      aline <- substring(aline, 2)
      attr(aline, "is.except") <- TRUE
      print(aline)      
    } else {
      attr(aline, "is.except") <- FALSE
    }
    # A domain or rule can be split into a list of labels using the separator "." (dot). The separator 
    # is not part of any of the labels.

    #Note: these are stored reversed
    aline <- splitDomain(aline)
    print("split up:")
    print(aline)
    
    rules <- c(rules, aline)
    # A domain is said to match a rule if, when the domain and rule are both split, and one compares
    # the labels from the rule to the labels from the domain, beginning at the right hand end, one finds
    # that for every pair either they are identical, or that the label from the rule is "*" (star). The
    # domain may legitimately have labels remaining at the end of this matching process.
  }
  return(rules)
  
}

matchDomain <- function(domainname, rule) {
  
  
}

splitDomain <- function(domainname) lapply(strsplit(as.character(domainname), ".", fixed=TRUE), FUN=rev)

publicSuffix <- function(domainname) {
  #Pseudocode from https://publicsuffix.org/list/
  # Match domain against all rules and take note of the matching ones.
  allrules <- create_rules(raw_list())
  
  
  
  # If no rules match, the prevailing rule is "*".
  # If more than one rule matches, the prevailing rule is the one which is an exception rule.
  # If there is no matching exception rule, the prevailing rule is the one with the most labels.
  # If the prevailing rule is a exception rule, modify it by removing the leftmost label.
  # The public suffix is the set of labels from the domain which directly match the labels of the prevailing rule (joined by dots).
  # The registered or registrable domain is the public suffix plus one additional label.
  
  return(FALSE)
}


checkPublicSuffix <- function(domainname, suffix) {
  my_suffix <- publicSuffix(domainname)
  if(my_suffix!=suffix) {
    stop(sprintf("Mismatch! publicSuffix(\"%s\") -> \"%s\" does not match \"%s\"", domainname,my_suffix,suffix))
  }
  
}