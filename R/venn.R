# This code plots Venn Diagrams for up to 5 sets. The
# function getVennCounts is passed a list of vectors.
# This is transformed into a table indicating the
# number of members for each intersection. This table
# is generated for any number of sets.

# The function drawVennDiagram plots circles (up to three
# sets) or ellipses (4 and 5 sets) to depict the sets.
# The sum of values placed is the number of entries of
# each set.

# Function to determine values of a venn diagram
# It works for an arbitrary large set of input sets.
#

getVennCounts <- function(l, universe, ...)
  UseMethod("getVennCounts")

getVennCounts.data.frame <- function(l, universe=NA, ...)
  {
    if( !all(unique(unlist(l)) %in% c(0,1))  )
      stop("Only indicator columns permitted")

    l <- sapply( l, function(x) which(as.logical(x)))
    getVennCounts.list(l)
  }

getVennCounts.list<-function(l,universe=NA) {
	numSets<-length(l)
	result.table<-NULL
	result.table.names<-NULL
	for (i in 0:(-1 + 2^numSets)) {
		# i2 is a binary representation of that number
		i2<-baseOf(i,2,numSets)

		# some debug output
		#print(paste(i,":",paste(i2,collapse="",sep="")))

		# p.pos determines the position in number
		#       which is also the set that is inspected

		sel<-universe

		# positive selection first
		for (p.pos in which(1 == i2) ) {
			current.set<-l[[p.pos]]
			#print(paste("set ",p.pos,", val=1: ",paste(current.set,collapse=",")))
			if (is.null(sel)) {
				#print("Sel is null")
			} else if (1 == length(sel) && is.na(sel)) {
				sel<-current.set
			}
			else {
				w<-which(sel %in% current.set)
				if (length(w)>0) {
					sel<-sel[w]
				}
				else {
					sel<-NULL
				}
                        }
		}

		# something should be in sel now, otherwise 
		# the number will be 0

		# negative selection
		for (p.pos in which(0 == i2) ) {
			if (is.null(sel) || ( 1 == length(sel) && is.na(sel))) {
				# The complement is not known, hence no checks done
			}
			else {
				current.set<-l[[p.pos]]
				w<-which( ! sel %in% current.set)
				#print(paste("set ",p.pos,", val=1: ",paste(current.set,collapse=",")))
				if (length(w)>0) {
					sel<-sel[w]
				}
				else {
					sel<-NULL
				}
			}
		}
		#print(paste("sel:",paste(sel,collapse=",")))

		if(is.null(sel) || (1 == length(sel) && is.na(sel))) {
			sel<-NULL
		}

		r<-length(sel)
		r.name<-paste(i2,collapse="")
		result.row<-c(r,i2)
		dim(result.row)<-c(1,length(result.row))
		rownames(result.row)<-c(r.name)
		#print(paste("Adding ",r.name))
		if (is.null(result.table)) {
			result.table<-result.row
		}
		else {
			result.table<-rbind(result.table,result.row)
		}
		#if (is.null(result.table)) {
		#	result.table<-r
		#	result.table.names<-r.name
		#}
		#else {
		#	result.table<-c(result.table,r)
		#	result.table.names<-c(result.table.names,r.name)
		#}
	}
	#names(result.table)<-result.table.names
	if (is.null(names(l))) {
		colnames(result.table)<-c("num",LETTERS[1:numSets])
	}
	else{
		colnames(result.table)<-c("num",names(l))
	}
        class(result.table) <- "venn"
	return(result.table)
}

#print(getVennCounts(list(A,B,C,D)))
#print(getVennCounts(list(a=A,b=B,c=C,d=D)))

venn <- function(data,
                 universe=NA,
                 small=0.7,
                 showSetLogicLabel=FALSE,
                 simplify=FALSE,
                 show.plot=TRUE)
{
  counts <- getVennCounts(data, universe=universe)
  
  if(show.plot)
    drawVennDiagram(data=counts,
                    small=small,
                    showSetLogicLabel=showSetLogicLabel,
                    simplify=simplify
                    )


  invisible(counts)
}

