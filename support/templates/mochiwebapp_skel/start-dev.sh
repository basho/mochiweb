#!/bin/sh
exec erl \
    -pa _build/default/lib/*/ebin \
    -boot start_sasl \
    -sname {{appid}}_dev \
    -s {{appid}} \
    -s reloader
