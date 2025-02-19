#!/usr/bin/env sh

while ! systemctl --user is-active --quiet emacs.service; do
    sleep 1
done

emacsclient -c -a='' &
