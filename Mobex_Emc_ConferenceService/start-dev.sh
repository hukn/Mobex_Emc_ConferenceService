#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
# exec erl -pa ebin edit deps/*/ebin -boot start_sasl -sname emcs_dev -s emcs   -s reloader
exec erl -pa $PWD/ebin edit $PWD/deps/*/ebin -boot start_sasl  -s emcs   -noinput -hidden
