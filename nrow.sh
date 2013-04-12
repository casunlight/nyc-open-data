#!/bin/sh

wc -l rows/*|sort -n|sed -e '$ d' -e 's/^ *//'|cut -d. -f1
