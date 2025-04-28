###
## function to take a dataframe with a column of repeated information sep by semicolons
## and expand the rows


##col is the name of the column with repeats
##df is the dataframe
expandDF = function(df, cols, sep=";")
{
    if (length(cols)==0)
        stop("need to specify names of columns")
    else if (length(cols)==1)
    {
        odf=df
        df$rn=1:nrow(df)
        ndf=df[!grepl(sep,df[,cols]),]
        tosplit=df[grepl(sep,df[,cols]),]
        accum = NULL
        for (i in 1:nrow(tosplit))
        {
            vals = strsplit(tosplit[i,cols],sep)[[1]]
            for (j in 1:length(vals))
            {
                slice=tosplit[i,which(names(tosplit)!=cols)]
                slice$newname=vals[j]
                names(slice)[which(names(slice)=="newname")]=cols
                accum=rbind(accum,slice)
            }
        }
        ret=rbind(ndf,accum)
        ret=ret[order(ret$rn),]
       # ret[,cols[1]]=as.numeric(ret[,cols[1]])
        ret
    } else if (length(cols)==2)
    {
        odf=df
        df$rn=1:nrow(df)
        tosplit=df[grepl(sep,df[,cols[1]])|grepl(sep,df[,cols[2]]),]
        ndf=df[!df$rn %in% tosplit$rn,]

        accum = NULL
        for (i in 1:nrow(tosplit))
        {
            vals1 = strsplit(as.character(tosplit[i,cols[1]]),sep)[[1]]
            vals2 = strsplit(as.character(tosplit[i,cols[2]]),sep)[[1]]
            if (length(vals1)==length(vals2))
            {
                for (j in 1:length(vals1))
                {
                    slice=tosplit[i,which(!names(tosplit)%in%cols)]
                    slice$newname1=vals1[j]
                    slice$newname2=vals2[j]
                    names(slice)[which(names(slice)=="newname1")]=cols[1]
                    names(slice)[which(names(slice)=="newname2")]=cols[2]
                    accum=rbind(accum,slice)
                }
            } else if (length(vals1)>length(vals2))
            {
                for (j in 1:length(vals1))
                {
                    slice=tosplit[i,which(!names(tosplit)%in%cols)]
                    slice$newname1=vals1[j]
                    slice$newname2=vals2
                    names(slice)[which(names(slice)=="newname1")]=cols[1]
                    names(slice)[which(names(slice)=="newname2")]=cols[2]
                    accum=rbind(accum,slice)
                }
            } else  if (length(vals1)<length(vals2))
            {
                for (j in 1:length(vals2))
                {
                    slice=tosplit[i,which(!names(tosplit)%in%cols)]
                    slice$newname2=vals2[j]
                    slice$newname1=vals1
                    names(slice)[which(names(slice)=="newname1")]=cols[1]
                    names(slice)[which(names(slice)=="newname2")]=cols[2]
                    accum=rbind(accum,slice)
                }
            }
        }
        
        ret=rbind(ndf[,names(accum)],accum)
        ret=ret[order(ret$rn),]
       # ret[,cols[1]]=as.numeric(ret[,cols[1]])
        #ret[,cols[2]]=as.numeric(ret[,cols[2]])
        ret

    }
            
}

