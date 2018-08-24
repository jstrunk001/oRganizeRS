oRganizeRS=function(
										dir_projects
										, dir_out
										, rescan = F
										, check = T  #ask before copy?
										, copy = T
										, cut = F
										, templateRDS=c("c:/temp/template1.rds")
										, return_templates = T
										){

	options(stringsAsFactors = F)

	if(!rescan & file.exists(templateRDS)){

		proj_templates = readRDS(templateRDS)

	}else{

		if(!dir.exists(dirname(templateRDS))) dir.create(dirname(templateRDS))

		#left to right
		dir_projects = gsub("\\\\","/",dir_projects)
		dir_out = gsub("\\\\","/",dir_out)

		#list projects
		dir_projects_in = list.dirs(dir_projects, full.names = TRUE, recursive = FALSE)

		print(c("all projects:",dir_projects_in))

		#summarize projects iteratively
		#proj_contents = lapply(dir_projects_in,.scan_1project)
		proj_contents = lapply(dir_projects_in[1:5],.scan_1project)

		#create templates for generic reorganization
		proj_templates = lapply(proj_contents,.template_1project, dir_out)

		saveRDS(proj_templates,templateRDS)

	}

		#browser()

	intro_folders=c(
		",This is a template for moving lidar projects into a consistent format."
		,",Column 'dir_old' is the current structure of the project."
		,",Column 'dir_new1' is the default target folder."
		,",Column 'dir_new2' is an alternative location inside of a 'misc' folder."
		,",If you wish to use dir_new2 place an X (or any text) in 'use_dirnew2'."
		,",You can manually updated dir_new1 if you don't like 'dir_new1' or 'dir_new2'."
		)
	intro_folders=c(intro_folders, rep("",10-length(intro_folders)))

	intro_files=c("Please see the folder template! Do not change anything in this folder","","","","")

	#write csv files and batch files to projects
	for(i in 1:length(proj_templates)){

		dfi = proj_templates[[i]][["folders"]]

		if(nrow(dfi) > 0){

			dfi_files = proj_templates[[i]][["files"]]
			dir_i = proj_templates[[i]][["in_dir"]]
			outi_folders = paste(dir_i,"\\","XXMOVE_FOLDER_TEMPLATEXX.CSV",sep="")
			outi_files = paste(dir_i,"\\","XXMOVE_FILE_TEMPLATEXX.CSV",sep="")

			dfi_folders=data.frame(dfi[,c(1:3)],use_dirnew2="",dfi[,c(4),drop=F])

			#write introductions and templates
			writeLines(intro_folders,con = outi_folders)
			writeLines(intro_files,con = outi_files)
			suppressWarnings(write.table(dfi_folders,outi_folders, append = T,row.names = F,col.names=T,sep=","))
			suppressWarnings(write.table(dfi_files,outi_files, append = T,row.names = F,col.names=T,sep=","))

		}

	}



	if(return_templates) return(proj_templates)

}

moveRS=function(dir=NA,copy=F,cut=F){

	if(is.na(dir)) dir_in = choose.dir()
	else dir_in = dir

	if(copy){
		if(cut){
			cut=F
			print("Cannot set 'cut = T' if 'copy = T', cut will revert to 'F'")
			warning("Cannot set 'cut = T' if 'copy = T', cut will revert to 'F'")
		}
	}
	if(cut){
		do_cut = readline(prompt="Are you sure you want to 'cut' this project? File movement is permanent (y/n)")
		if(tolower(do_cut) != "y"){
			cut = F
			copy = T
		}
	}

	this_template = paste(dir_in,"\\","XXMOVETEMPLATEXX.csv")

	if(!file.exists(this_template)){

			dir_out = choose.dir(dir_in, caption = "Select folder")
			oRganizeRS(dir_in,dir_out, rescan = T)
			warning("Please review the template:",this_template,"then re-run moveRS")

	}else{

		template_in = read.csv(this_template,skip=5,header=T)

		#test / fix primary target directory
		swap = sapply(template_in[,3],function(x)nchar(x)>0)
		template_in[,"temporary"] = NA
		template_in[,"temporary"] = template_in[,1]
		template_in[swap,1] = template_in[swap,3]

		for(i in 1:nrow(template_in)){


			if(copy) try(file.copy(template_in[i,1],template_in[i,2],recursive=T))
			if(cut) try(file.move(template_in[i,1],template_in[i,2],recursive=T))

		}

	}

}


.scan_1project = function(dir_project){

	print(paste("Scan project:",dir_project))

	l_proj = list(
		dir_project = dir_project
		,subdirs_project = list.dirs(dir_project, full.names = TRUE, recursive = T)
		,files_all = list.files(dir_project, full.names = TRUE, recursive = T)
		)

	return(l_proj)

}

.template_1project = function(l_proj, dir_out ){

	require(plyr)

	from_project_dir  = l_proj[["dir_project"]]
	to_project_dir = file.path(dir_out, basename(l_proj[["dir_project"]]))
	to_original_dir = file.path(dir_out, basename(l_proj[["dir_project"]]),"original")
	subdirs = c("las","laz","dtm_original","dtm_fusion","tile_vectors","original","projection")
	subdirs_project = paste(to_project_dir,subdirs,sep="/")



	df_template=data.frame(
		type = ""
		,dir_old = tolower(l_proj$subdirs_project)
		,dirs_new1 = tolower(gsub(from_project_dir,to_original_dir,l_proj$subdirs_project))
		,dirs_new2 = ""
	)

	#files_check =  c("[.]las$","[.]laz$","[.]dtm$","[.]tif$","[.]img$","[.]adf$","[.]shp$","[.]doc","[.]pdf$","[.]html$")

	# las = grep("[.]las$",l_proj$files_all,value=T,ignore.case=T)
	# laz = grep("[.]laz$",l_proj$files_all,value=T,ignore.case=T)
	# dtm = grep("[.]dtm$",l_proj$files_all,value=T,ignore.case=T)
	# tif = grep("[.]tif$",l_proj$files_all,value=T,ignore.case=T)
	# img = grep("[.]img$",l_proj$files_all,value=T,ignore.case=T)
	# adf = grep("[.]adf$",l_proj$files_all,value=T,ignore.case=T)
	# shp = grep("[.]shp$",l_proj$files_all,value=T,ignore.case=T)
	# doc = grep("[.]doc",l_proj$files_all,value=T,ignore.case=T)
	# pdf = grep("[.]pdf$",l_proj$files_all,value=T,ignore.case=T)
	# html = grep("[.]html$",l_proj$files_all,value=T,ignore.case=T)

	l_types = list(
		las = grep("[.]las.{0,1}$",l_proj$files_all,value=T,ignore.case=T)
		,laz = grep("[.]laz$",l_proj$files_all,value=T,ignore.case=T)
		,dtm = grep("[.]dtm$",l_proj$files_all,value=T,ignore.case=T)
		,tif = grep("[.]tif$",l_proj$files_all,value=T,ignore.case=T)
		,img = grep("[.]img$",l_proj$files_all,value=T,ignore.case=T)
		,asc = grep("[.]asc$",l_proj$files_all,value=T,ignore.case=T)
		,adf = grep("[.]adf$",l_proj$files_all,value=T,ignore.case=T)
		#,shp = grep("[.]shp$",l_proj$files_all,value=T,ignore.case=T)
		,doc = grep("[.]doc.{0,1}$",l_proj$files_all,value=T,ignore.case=T)
		,pdf = grep("[.]pdf$",l_proj$files_all,value=T,ignore.case=T)
		,html = grep("[.]html$",l_proj$files_all,value=T,ignore.case=T)
	)

	l_types[["others"]] = l_proj$files_all[!l_proj$files_all %in% unique(unlist(l_types))]

	l_folders = lapply(l_types, function(x)unique(dirname(x)))

	df_template = read.csv(text="type,dir_old,dir_new1,dir_new2",stringsAsFactors=F,colClasses="character")

#first cut - look at individual files
	if(length(l_folders$las) > 0){
		df_template = rbind.fill(df_template
					,data.frame(type=".las",dir_old=l_types$las
											,dir_new1=gsub(from_project_dir,paste(to_project_dir,"las",sep="/"),l_types$las)
											,dir_new2=gsub(from_project_dir,to_original_dir,l_types$las)
											)		)
	}
	if(length(l_folders$laz) > 0){
		df_template = rbind.fill(df_template
														 ,data.frame(type=".laz",dir_old=l_types$laz
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"laz",sep="/"),l_types$laz)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$laz)
														 )		)
	}
	if(length(l_folders$dtm) > 0){
		df_template = rbind.fill(df_template
														 ,data.frame(type=".dtm",dir_old=l_types$dtm
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_fusion",sep="/"),l_types$dtm)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$dtm)
														 )		)
	}
	if(length(l_folders$tif) > 0){
		df_template = rbind.fill(df_template
														 ,data.frame(type=".tif",dir_old=l_types$tif
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),l_types$tif)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$tif)
														 )		)
	}
	if(length(l_folders$img) > 0){
		df_template = rbind.fill(df_template
														 ,data.frame(type=".img",dir_old=l_types$img
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),l_types$img)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$img)
														 )		)
	}
	if(length(l_folders$asc) > 0){
		df_template = rbind.fill(df_template
														 ,data.frame(type=".asc",dir_old=l_types$asc
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),l_types$asc)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$asc)
														 )		)
	}
	if(length(l_folders$adf) > 0){
		esri_files = unique(dirname(l_types$adf))
		df_template = rbind.fill(df_template
														 ,data.frame(type=".adf",dir_old=l_types$adf
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),esri_files)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,esri_files)
														 )		)
	}
	if(length(l_folders$doc) > 0){

		doc_dirs = unique(dirname(l_types$doc))
		df_template = rbind.fill(df_template
														 ,data.frame(type=".doc",dir_old=l_types$doc
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"original_metafiles",sep="/"),l_types$doc)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$doc)
														 )		)
	}
	if(length(l_folders$pdf) > 0){

		pdf_dirs = unique(dirname(l_types$pdf))
		df_template = rbind.fill(df_template
														 ,data.frame(type=".pdf",dir_old=l_types$pdf
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"original_metafiles",sep="/"),l_types$pdf)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$pdf)
														 )		)
	}
	if(length(l_folders$html) > 0){

		html_files = unique(dirname(l_types$html))
		df_template = rbind.fill(df_template
														 ,data.frame(type=".html",dir_old=l_types$html
														 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"original_metafiles",sep="/"),l_types$html)
														 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$html)
														 )		)
	}
	if(length(l_folders$other) > 0){

		other_dirs = unique(dirname(l_types$others))
		df_template = rbind.fill(df_template
														 ,data.frame(type="other",dir_old=l_types$others
														 						,dir_new1=gsub(from_project_dir,to_original_dir,l_types$others)
														 						,dir_new2=""#gsub(from_project_dir,to_original_dir,l_types$others)
														 )		)
	}

	#second cut - organize by folders, las and laz may be in same folder ...
	spl_template = split(df_template, df_template$type)
	df_dirs_temp = rbind.fill(lapply(spl_template,.fn_temp_dirs))

	if(nrow(df_template)==0) df_dirs_temp = df_template

	return(list(files = df_template,folders = df_dirs_temp, in_dir = from_project_dir, out_dir = to_project_dir ))

}

.fn_temp_dirs=function(df_tempi){

	#get unique set of directories
	dir_old_in = dirname(df_tempi$dir_old)
	dup_dir = duplicated(dir_old_in)
	df_tempi_unq =  df_tempi[!dup_dir,]

	#select first instance into template, set 2nd,3rd... instances into alternate "dir_new2" folder"
	if(length(df_tempi_unq) > 0){
		#if(sum(grepl(c(".las",".laz",".dtm",".tif",".adf",".img",".asc") , df_tempi_unq)) > 0){ #if grepl not really necessary
			df_tempi_unq_a = df_tempi_unq
			df_tempi_unq[-1,"dir_new1"] = df_tempi_unq_a[-1,"dir_new2"]
			df_tempi_unq[-1,"dir_new1"] = df_tempi_unq_a[-1,"dir_new1"]
		#}
	}

	#return directories only
	df_tempi_unq[,-1] = apply(df_tempi_unq[,-1],2,dirname)
	df_tempi_unq

}

.template_1project_old = function(l_proj, dir_out ){


	if(length(l_proj$dirs_types$dirs_laz) >=1  | length(l_proj$dirs_types$dirs_laz) >=1){

		from_project_dir  = l_proj[["dir_project"]]
		to_project_dir = file.path(dir_out, basename(l_proj[["dir_project"]]))
		to_original_dir = file.path(dir_out, basename(l_proj[["dir_project"]]),"original")
		subdirs = c("las","laz","dtm_original","dtm_fusion","tile_vectors","original","projection")
		subdirs_project = paste(to_project_dir,subdirs,sep="/")

		l_template = list(

			dir_project = to_project_dir
			,subdirs_project = subdirs_project

		)

		l_template[["dirs_types"]][["dirs_las"]] = rep(paste(to_project_dir,"las",sep="/"),length(l_proj[["dirs_types"]][["dirs_las"]]))
		l_template[["dirs_types"]][["dirs_laz"]] = rep(paste(to_project_dir,"laz",sep="/"),length(l_proj[["dirs_types"]][["dirs_laz"]]))
		l_template[["dirs_types"]][["dirs_dtm_fsn"]] = rep(paste(to_project_dir,"dtm_fusion",sep="/"),length(l_proj[["dirs_types"]][["dirs_dtm"]]))
		l_template[["dirs_types"]][["dirs_dtm_org"]] = paste(to_project_dir,"dtm_original",sep="/")
		l_template[["dirs_types"]][["dirs_tif"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_tif"]])
		l_template[["dirs_types"]][["dirs_img"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_img"]])
		l_template[["dirs_types"]][["dirs_adf"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_adf"]])
		l_template[["dirs_types"]][["dirs_shp"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_shp"]])
		l_template[["dirs_types"]][["dirs_doc"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_doc"]])
		l_template[["dirs_types"]][["dirs_pdf"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_pdf"]])


	}else{

		warning(paste("Skipped",l_proj$dir_project,"no las or laz files found"))

		l_template = list(dir_project = l_proj$dir_project, note = "no las or laz files found" )

	}

	if(return_templates) return( list(original=l_proj,template=l_template ))


}

.seekFiles = function(pattern, dir){

	list.files(dir, pattern=pattern, full.names=T,recursive = T,ignore.case = T)

}


#external hdd
if(F){
	templates = oRganizeRS("d:\\temp\\test_dir\\","d:\\temp\\test_dir1\\", rescan = T)
}
