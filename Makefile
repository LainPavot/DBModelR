
R_BINARY=/home/lainou/R/4.1.1//bin/R
R_SCRIPT=/home/lainou/R/4.1.1//bin/Rscript
EXEC_R=/home/lainou/R/4.1.1//bin/R -q -e
R_CMD=/home/lainou/R/4.1.1//bin/R CMD

all		: check

install_deps:
	@$(R_SCRIPT) ./install_deps.R

clean	:
	@rm DBModelR_*.tar.gz || true
	@rm ./NAMESPACE 2> /dev/null || true
	@rm ./README.html 2> /dev/null || true
	@rm -rf ./man/ 2> /dev/null || true
	@rm -rf ./vignettes/ 2> /dev/null || true

test	:
	@echo "Running tests..."
	@$(EXEC_R) "devtools::test('.');warnings()"
	@$(EXEC_R) "covr::report(covr::package_coverage(path='/home/lainou/DBModelR', clean=FALSE), file='/home/lainou/DBModelR/coverage.html')"
	@echo "Finished."

doc		: clean
	@echo "Generating doc..."
	$(EXEC_R) "devtools::document('.')"
	$(EXEC_R) "roxygen2::roxygenize('.');warnings()"
	$(EXEC_R) "rmarkdown::render('README.Rmd')"
	@echo "Generated."

build	: doc test
	@echo "Building package..."
	$(R_CMD) build .
	@echo "Built."

install	: build
	@$(EXEC_R) 'install.packages(".", repos=NULL, type="source")'

check	: build
	@echo "Checking package..."
	cd /tmp/ && $(R_CMD) check --as-cran /home/lainou/DBModelR/*.tar.gz
	@echo "Checked."
# 		$(R_CMD) BiocCheck "$${dot}"/*.tar.gz	\

submit	: check install
	$(EXEC_R) 'devtools::submit_cran(pkg=".", args=NULL)'

.PHONY	: all test build clean doc

