.PHONY: rust

define test_rust
	@cd $* &&     \
	cargo test && \
	cd ..;
endef

DIRS := $(shell find . -maxdepth 1 -type d ! -path ".")
RUST_JOBS := $(addprefix rjob,${DIRS})

rust: ${RUST_JOBS} ; @echo "[$@] finished!"

${RUST_JOBS}: rjob%:
	$(test_rust)
