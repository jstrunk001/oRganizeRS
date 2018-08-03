oRganizeRS=function(dir_projects, dir_out){


	#list projects
	dir_projects_in = list.dirs(dir, full.names = TRUE, recursive = FALSE)

	#organize projects iteratively
	lapply(dir_projects_in,.scan_1project)



}


.scan_1project = function(dir_project){

	l_in = list(
		dir_project = dir_project

		,dirs_proj_in = list.dirs(dir_project, full.names = TRUE, recursive = T)

		,project_las = .seekFiles("[.]las",dir_project)
		,project_laz = .seekFiles("[.]laz",dir_project)
		,project_dtm = .seekFiles("[.]dtm",dir_project)
		,project_tif = .seekFiles("[.]tif",dir_project)
		,project_img = .seekFiles("[.]img",dir_project)
	)

	l_in[["dirs_las"]] = unique(dirname(l_in[["project_las"]]))
	l_in[["dirs_laz"]] = unique(dirname(l_in[["project_laz"]]))
	l_in[["dirs_dtm"]] = unique(dirname(l_in[["project_dtm"]]))
	l_in[["dirs_tif"]] = unique(dirname(l_in[["project_tif"]]))
	l_in[["dirs_img"]] = unique(dirname(l_in[["project_img"]]))

	return(l_in)

}

.seekFiles = function(pattern, dir){

	list.files(dir, pattern=pattern, full.names=T,recursive = T,ignore.case = T)

}
