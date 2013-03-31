Workhorse function is `selfAssign`. 
There are a few wrappers to it as follows: 

    NAtoNULL(obj)      # Replaces NA values in obj with NULL.
    NAtoVal(obj, val)  # Replaces NA values in obj with val.
    selfReplace(obj, toReplace, val)  # Replaces toReplace values in obj with val

    # and selfAssign can be called directly, but I'm not sure there is a good reason to
    selfAssign(obj, ind, val)  # equivalent to obj[ind] <- val

Example: 

    # sample df
    df <- structure(list(subj=c("A",NA,"C","D","E",NA,"G"),temp=c(111L,112L,NA,114L,115L,116L,NA),size=c(0.7133,NA,0.7457,NA,0.0487,NA,0.8481)),.Names=c("subj","temp","size"),row.names=c(NA,-7L),class="data.frame")
  
    df
      subj temp   size
    1    A  111 0.7133
    2 <NA>  112     NA
    3    C   NA 0.7457
    4    D  114     NA
    5    E  115 0.0487
    6 <NA>  116     NA
    7    G   NA 0.8481

    # Make some replacements
    NAtoNULL(df$size)    # Replace all NA's in df$size wtih NULL's
    NAtoVal(df$temp, 0)  # Replace all NA's in df$tmp wtih 0's
    NAtoVal(df$subj, c("B", "E"))   # Replace all NA's in df$subj with alternating "B" and "E" 

    # the modified df is now:  
    df

      subj temp   size
    1    A  111 0.7133
    2    B  112   NULL
    3    C    0 0.7457
    4    D  114   NULL
    5    E  115 0.0487
    6    E  116   NULL
    7    G    0 0.8481


    # replace the 0's in temp for NA
    selfReplace(df$temp, 0, NA)

    # replace NULL's in size for 1's
    selfReplace(df$size, NULL, 1)

    # replace all "E"'s in subj with alternate c("E", "F")
    selfReplace(df$subj, c("E"), c("E", "F"))

    df
    
      subj temp   size
    1    A  111 0.7133
    2    B  112      1
    3    C   NA 0.7457
    4    D  114      1
    5    E  115 0.0487
    6    F  116      1
    7    G   NA 0.8481


Right now this works for vectors, but will fail with *apply. 
