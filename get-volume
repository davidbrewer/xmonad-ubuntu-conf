#!/bin/bash

# get-volume script returns the current volume status of your system in a
# format suitable for use in xmobar. I found this script on this page:
#   http://karuppuswamy.com/wordpress/2011/09/03/how-to-get-a-productive-desktop-based-on-xmonad-and-xmobar/
# 
# Original author unknown -- credited to "a nice guy online".  :-)
#
# Note that if this is not working for your hardware, it may be because 
# you have more than one sound card. In that case you may be able to make
# it work by adding arguments to the amixer command in the first line.
# For example, the "-c N" argument makes it choose the card identified by
# N, where N is an integer. If you have two cards, try using "-c 1" to make
# it use the second card.

str=`amixer sget Master,0`
str1=${str#Simple*[}
v1=${str1%%]*]}
il=`expr index "$str1" [`
o="off"
mutel=''
if [ ${str1:$il:3} == $o ]; then mutel='M'; fi
s=${str1:0:1}
str2=${str1#${str1:0:1}*[}
str1=$str2
str2=${str1#${str1:0:1}*[}
ir=`expr index "$str2" [`
muter=''
if [ ${str2:0:3} = $o ]; then muter='[M]'; fi
v2=${str2%\]}
v=${v1}\ $muter
echo $v
