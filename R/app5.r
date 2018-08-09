library(shiny)
library(DT)
shinyApp(

	ui <- fluidPage(

		sidebarLayout(
			sidebarPanel(
				actionButton("get_folder", "Select Project")
			)
			,mainPanel(
				verbatimTextOutput("one_proj")
			)

			,DT::dataTableOutput('template')

		)




	)


	,server = function(input, output) {

		proj_dir = observeEvent(input$get_folder, {

			one_proj_in = choose.dir(caption = "Select RS Project")
			output$one_proj = renderText(one_proj_in)
			output$subdirs = DT::renderDataTable(data.frame(dirs = list.dirs(one_proj_in,recursive = F)))

		})


		# create a character vector of shiny inputs
		shinyInput = function(FUN, len, id, ...) {
			inputs = character(len)
			for (i in seq_len(len)) {
				inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
			}
			inputs
		}

		# obtain the values of inputs
		shinyValue = function(id, len) {
			unlist(lapply(seq_len(len), function(i) {
				value = input[[paste0(id, i)]]
				if (is.null(value)) NA else value
			}))
		}

		# a sample data frame
		res = data.frame(
			v1 = shinyInput(numericInput, 100, 'v1_', value = 0),
			v2 = shinyInput(checkboxInput, 100, 'v2_', value = TRUE),
			v3 = rnorm(100),
			v4 = sample(LETTERS, 100, TRUE),
			stringsAsFactors = FALSE
		)

		one_scan = .scan_1project(proj_dir)
		one_template = .template_1project(one_scan, "c:\\temp\\")

		res = data.frame(
			original = one_template[,1]
			target = one_template[,2]
			use_alt = shinyInput(checkboxInput, 100, 'v2_', value = F),
			alternate = one_template[,3]
			stringsAsFactors = FALSE
		)

		# render the table containing shiny inputs
		output$template = DT::renderDataTable(
			res, server = FALSE, escape = FALSE, selection = 'none', options = list(
				preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
				drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
			)
		)

	}
)
