VERSION?=0.1.0

PROGRAM_NAME=layer

SRC_DIR=src
BUILD_DIR=build

development:
	csc $(SRC_DIR)/*.scm -o $(PROGRAM_NAME)

deployment:
	mkdir -p $(BUILD_DIR)
	csc -deploy $(SRC_DIR)/*.scm -o $(BUILD_DIR)/$@
	chicken-install -deploy -p $(BUILD_DIR)/$@ blas getopt-long input-parse
	mv $(BUILD_DIR)/$@/$@ $(BUILD_DIR)/$@/$(PROGRAM_NAME)
	mv $(BUILD_DIR)/$@ $(BUILD_DIR)/$(PROGRAM_NAME)-$(VERSION)
	tar czf $(BUILD_DIR)/$(PROGRAM_NAME)-$(VERSION).tar.gz -C $(BUILD_DIR) $(PROGRAM_NAME)-$(VERSION)

clean:
	rm -rf $(BUILD_DIR) $(PROGRAM_NAME) *.o
