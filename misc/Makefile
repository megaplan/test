INCLUDE_DIR = include
INCLUDES = $(INCLUDE_DIR:%=-I%)
SRC_DIR = src
TEST_DIR = test
EBIN_DIR := ebin
ERLC_OPTS = +debug_info
ERLC := erlc $(ERLC_OPTS)

all: $(EBIN_DIR)
	$(ERLC) -W $(INCLUDES) -o $(EBIN_DIR) $(SRC_DIR)/*.erl
	@#cp $(SRC_DIR)/misc.app $(EBIN_DIR)
	
tests: $(EBIN_DIR)
	@$(ERLC) -W $(INCLUDES) -o $(EBIN_DIR) $(TEST_DIR)/*.erl
	
clean:
	@rm -rvf $(EBIN_DIR)/*

ctags:
	cd $(SRC_DIR) ; ctags -R . ../include 

$(EBIN_DIR) :
	@( test -d $(EBIN_DIR) || mkdir -p $(EBIN_DIR) )

dia:
	PATH=$(HOME)/util/erlang/dist/r14b3/bin:$(PATH) \
	dialyzer -I include --src -r $(SRC_DIR)

