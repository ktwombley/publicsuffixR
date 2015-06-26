

psl_URL <- "https://publicsuffix.org/list/effective_tld_names.dat"

psl_raw_list <- function() {
  return(tryCatch({
    readLines(file(psl_URL, "r"))
  },
  error = function(e) {
    readLines(pipe(paste("wget -q -O -", psl_URL)))
  }))  
}


raw_list <- psl_raw_list

if ('memoise' %in% installed.packages()) {
  require(memoise)
  
  raw_list <- memoise(psl_raw_list)

}


create_rules <- function(intext) {
  
  rules <- lapply(intext, FUN=create_a_rule)
  return(rules[!is.na(rules)])
  
}

create_a_rule <- function(aline) {
  # Each line is only read up to the first whitespace; entire lines can also be commented using //.
  if(substr(aline,1,2)=="//") {
    return(NA)
  }
  
  # Each line which is not entirely whitespace or begins with a comment contains a rule.
  
  aline <- strsplit(aline, split="(\\s+)", perl=TRUE)[1]
  
  #wtf is this a bug?
  if(nchar(aline)==0 || aline=="character(0)") {
    return(NA)
  }
  
  # A rule may begin with a "!" (exclamation mark). If it does, it is labelled as a "exception rule" 
  
  if(substr(aline,1,1)=="!") {
    #  and then treated as if the exclamation mark is not present.
    aline <- substring(aline, 2)
    is.except <- TRUE
  } else {
    is.except <- FALSE
  }
  # A domain or rule can be split into a list of labels using the separator "." (dot). The separator 
  # is not part of any of the labels.
  
  #Note: these are stored reversed
  aline <- splitDomain(aline)
  attr(aline, 'is.except')<-is.except
  return(aline)
}

#The domain and all rules must be canonicalized in the normal way for hostnames - lower-case, Punycode (RFC 3492).
#TODO: Punycode
splitDomain <- function(domainname) {
  if(is.na(domainname)) return(NA)
  if(substr(domainname,1,1)==".") {
    domainname <- substring(domainname,2)
  }
  return(as.vector(lapply(strsplit(tolower(as.character(domainname)), ".", fixed=TRUE), FUN=rev))[[1]])
}

joinDomain <- function(splitdomain) {
  if(is.na(splitdomain)){
    return(NA)
  } else {
    return(paste(rev(splitdomain), sep="", collapse="."))
  }
}

                                                                                                                  
matchDomain <- function(splitdomainname, rules) {
  # A domain is said to match a rule if, when the domain and rule are both split, and one compares
  # the labels from the rule to the labels from the domain, beginning at the right hand end, one finds
  # that for every pair either they are identical, or that the label from the rule is "*" (star). The
  # domain may legitimately have labels remaining at the end of this matching process.

  if(!is.list(rules)) {
    stop("rules doesn't seem to contain any rules.")
  }
  if(all(is.na(splitdomainname))) return(list())
  
  
  eachfunc <- function(rule) {
    if(length(splitdomainname)<length(rule)) {
      return(FALSE)
    }        
    for(pos in 1:length(rule)) {
      if(rule[pos]!="*" && rule[pos]!=splitdomainname[pos]) {
        return(FALSE)
      } 
    }
    return(TRUE)
  }
  
  ans <- rules[sapply(rules, FUN=eachfunc)]
  return(ans)  
}


publicSuffix <- function(domainname) {
  #Pseudocode from https://publicsuffix.org/list/
  # Match domain against all rules and take note of the matching ones.
  allrules <- create_rules(raw_list())
  
  sdn <- splitDomain(domainname)
  
  matches <- matchDomain(sdn, allrules)
  
  prevailing_rule <- NA
  
  # If no rules match, the prevailing rule is "*".
  if(length(matches)==0) {

    prevailing_rule <- "*"
  } else {
  
    # If more than one rule matches, the prevailing rule is the one which is an exception rule.
    matching_exceptions <- matches[sapply(matches, function(ar) {attr(ar, 'is.except')})]
    if(length(matching_exceptions)==1) {
      prevailing_rule <- matching_exceptions[[1]]
  
      # If the prevailing rule is a exception rule, modify it by removing the leftmost label.
      length(prevailing_rule) <- length(prevailing_rule)-1
  
    } else if(length(matching_exceptions>1)) {
      stop(paste("multiple matching exceptions", matching_exceptions, collapse=","))
    } else {
    
      # If there is no matching exception rule, the prevailing rule is the one with the most labels.
      max_len <- max(sapply(matches, length))
      
      longest_match <- matches[sapply(matches, function(m) {length(m)==max_len})]
      if(length(longest_match)==1) {
        prevailing_rule <- longest_match[[1]]    
      } else if (length(longest_match>1)) {
        stop(paste("multiple longest matches", longest_match, collapse=","))
      }
    }
  }
  pubsuf <- vector()
  regable <- NA
  
  # The public suffix is the set of labels from the domain which directly match the labels of the prevailing rule (joined by dots).
  for(pos in 1:length(prevailing_rule)) {
    if(prevailing_rule[pos]=="*" || (prevailing_rule[pos]==sdn[pos])) {
      pubsuf[pos] <- sdn[pos]
      regable[pos] <- sdn[pos]
    } else {
      break
    }
  }
  
  # The registered or registrable domain is the public suffix plus one additional label.
  if(length(sdn)>length(pubsuf)){
    regable <- pubsuf
    regable[length(regable)+1] <- sdn[length(regable)+1]
  } else {
    regable <- NA
  }
  
  return(list('public_suffix'=joinDomain(pubsuf), 'registerable'=joinDomain(regable)))
  
}


checkPublicSuffix <- function(domainname, suffix) {
  my_suffix <- publicSuffix(domainname)
  regable <- my_suffix$registerable
  if(any(is.na(c(suffix, regable)))) {
    return(all(is.na(c(suffix, regable))))
  } else {
    return(regable==tolower(suffix))
  }
}

