VERSION?=0.1.0
PLATFORM?=macosx
ARCHITECTURE?=x86_64

PROGRAM_NAME=layer
ARTIFACT=$(PROGRAM_NAME)-$(VERSION)-$(PLATFORM)-$(ARCHITECTURE)

SRC_DIR=src
TEST_DIR=test
DIST_DIR=dist
BUILD_DIR=build

EGG_DEPS=blas getopt-long input-parse

main:
	csc $(SRC_DIR)/*.scm -o $(PROGRAM_NAME)

deploy:
	mkdir -p $(BUILD_DIR)
	csc -deploy $(SRC_DIR)/*.scm -o $(BUILD_DIR)/$@
	chicken-install -deploy -p $(BUILD_DIR)/$@ $(EGG_DEPS)
	rm -f $(BUILD_DIR)/$@/*.setup-info
	cp $(DIST_DIR)/install.sh $(BUILD_DIR)/$@
	mv $(BUILD_DIR)/$@/$@ $(BUILD_DIR)/$@/$(PROGRAM_NAME)
	mv $(BUILD_DIR)/$@ $(BUILD_DIR)/$(ARTIFACT)
	tar czf $(BUILD_DIR)/$(ARTIFACT).tar.gz -C $(BUILD_DIR) $(ARTIFACT)

tests:
	csi -script $(TEST_DIR)/*.scm

clean:
	rm -rf $(BUILD_DIR) $(PROGRAM_NAME) *.o
