# Don't run all the targets at once because the variable values gets all messed
# up Run everything one by one

RUNS?=10

define normal_defaults
	$(eval STEP_FN?="i*10")
	$(eval CHANGES?=1)
endef

define bench_command
	RUNS=$(RUNS) bash bench_runner.sh $@.exe $(LO) $(HI) $(STEP_FN) $(CHANGES)
endef

filter:
	$(eval LO?=1000)
	$(eval HI?=100000)
	$(normal_defaults)
	$(bench_command)

merge_sort:
	$(eval LO?=1000)
	$(eval HI?=100000)
	$(normal_defaults)
	$(bench_command)

rabin_karp:
	$(eval LO?=10000)
	$(eval HI?=100000)
	$(normal_defaults)
	$(bench_command)

sum_array:
	$(eval LO?=1000)
	$(eval HI?=100000)
	$(normal_defaults)
	$(bench_command)

spellcheck:
	$(eval LO?=100)
	$(eval HI?=1000)
	$(eval STEP_FN?="i+200")
	$(eval CHANGES?=10)
	$(bench_command)

clean:
	dune clean
