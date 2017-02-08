# pull out actor names from list
getActor = function(list){ sort(unique(unlist(lapply(list, rownames))))  }

# get cleaned mat
getNameKey = function(yList){
	vNameKey = data.frame(dirty=getActor(yList), clean=getActor(yList), stringsAsFactors = FALSE)
	vNameKey$clean = gsub(' (Nigeria)', '',vNameKey$clean, fixed=TRUE)
	vNameKey$clean = gsub(' Ethnic', '',vNameKey$clean)
	vNameKey$clean = gsub(' Militia', '\nMilitia',vNameKey$clean)
	vNameKey$clean = trim(vNameKey$clean)
	vNameKey$clean[vNameKey$clean=='MASSOB: Movement for the Actualization of a Sovereign State of Biafra'] = 'MASSOB'
	vNameKey$clean[vNameKey$clean=='MEND: Movement for the Emancipation of the Niger Delta'] = 'MEND'
	vNameKey$clean[vNameKey$clean=='OPC: Oodua Peoples Congress'] = 'OPC'
	vNameKey$clean[vNameKey$clean=='Military Forces of Nigeria'] = 'Military\n(Nigeria)'
	vNameKey$clean[vNameKey$clean=='Police Forces of Nigeria'] = 'Police\n(Nigeria)'
	vNameKey$clean[vNameKey$clean=='Kutep Communal\nMilitia'] = 'Kutep\nMilitia'
	vNameKey$clean[vNameKey$clean=='Shiite Muslim\nMilitia'] = 'Shiite\nMilitia'
	vNameKey$clean[vNameKey$clean=='Sunni Muslim\nMilitia'] = 'Sunni\nMilitia'
	vNameKey$clean[vNameKey$clean=='Bakassi Boys\nMilitia'] = 'Bakassi\nMilitia'
	vNameKey$clean[vNameKey$clean=='Area Boys\nMilitia'] = 'Area\nBoys\nMilitia'
	vNameKey$clean[vNameKey$clean=='Kalo-Kato\nMilitia'] = 'Kalo\nKato\nMilitia'	
	vNameKey$clean[vNameKey$clean=='Boko Haram'] = 'Boko\nHaram'
	return(vNameKey)
}