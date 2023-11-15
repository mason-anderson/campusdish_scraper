#!/bin/sh

campusdish-scraper | pandoc -t pdf -f markdown -o - | zathura -
