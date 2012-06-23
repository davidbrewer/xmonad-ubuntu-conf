#!/bin/bash
# 
# Installation script which setup up an Ubuntu Precise machine to use this
# xmonad configuration. 
#
# WARNING!!! 
# * This has only been tested on a limited number of machines running 
#   Ubuntu 12.04 64-bit.
# * This is not a sophisticated installation script by any stretch 
#   of the imagination. 
# * I take no responsibility if this overwrites any configuration settings 
#   or otherwise messes up your system. 
#
# Please review the readme file to find out exactly what it does and does not 
# do. Or, visit the repository for more information: 
# https://github.com/davidbrewer/xmonad-ubuntu-conf
# 
# Author: David Brewer

echo "Installing required packages..."
sudo apt-get install xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar xcompmgr nitrogen stalonetray moreutils synapse ssh-askpass-gnome thunar terminator remmina

echo "Creating xmonad xsession configuration..."
sudo mv /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.original
sudo cp ~/.xmonad/xmonad.desktop /usr/share/xsessions
sudo cp ~/.xmonad/images/custom_xmonad_badge.png /usr/share/unity-greeter

echo "Linking to customized gnome 2 configuration..."
mv ~/.gtkrc-2.0 ~/gtkrc-2.0.original
ln -s .xmonad/.gtkrc-2.0 ~/.gtkrc-2.0

