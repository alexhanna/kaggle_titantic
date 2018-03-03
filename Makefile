
output/script.html:
	R -e "rmarkdown::render(input = 'src/script.rmd', output_file = '../$@')"