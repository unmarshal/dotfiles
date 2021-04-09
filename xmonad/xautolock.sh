#!/bin/sh

xautolock -time 1 -locker "slock /usr/sbin/s2ram" -notify 10 \
  -notifier "notify-send -t 5000 -i gtk-dialog-info 'Locking in 10 seconds'"
