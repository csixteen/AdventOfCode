.PHONY: all 2020

all: Rust

Rust:
	@cd aoc-rs && cargo test
