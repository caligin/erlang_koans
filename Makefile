PROJECT=erlang_koans

include erlang.mk

run: app
	$(gen_verbose) erl $(SHELL_PATH) $(SHELL_OPTS) -noshell -eval 'application:ensure_all_started(erlang_koans).'
