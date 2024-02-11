#!/usr/bin/env sh

until systemctl --user --is-active --quiet emacs.service; do
    sleep 1
done

emacsclient -c -a='' &
