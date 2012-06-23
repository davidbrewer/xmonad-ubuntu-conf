xmonad-ubuntu-conf
==================

My xmonad config for Ubuntu 12.04, including package list, config files, and instructions.

Overview
--------

Installation
------------

### Caveats ###

This process is based on the assumption that you have not yet installed xmonad. If you HAVE already installed it, whether this will work is dependent on how you installed it:
* If you installed xmonad using the standard Ubuntu repositories, you should back up any existing configuration before proceeding, but otherwise this should still work.
* If you installed xmonad from source using Haskell tools such as Cabal, my installation process will most likely NOT work for you. The Ubuntu packages may conflict with your from-source installation. You should proceed with extreme caution in this case.  

Finally, this whole process is intended for someone who likes to mess with their system configuration and is comfortable at the command line. I'm guessing you probably wouldn't have found this if that wasn't the case.

### Checkout repository ###

As your first step, you should check out this github repository or download an archive of the files. The contents of the repository should be placed in your home directory in a folder called ".xmonad". Note that if you have already installed xmonad, this directory will already exist! If you want to be able to revert to your existing configuration, you should rename this directory to something like ".xmonad-original".

### Installation: the short version ###

I've provided a script which performs all the remaining operations lined out the installation instructions. PLEASE skim the rest of the installation section to see what the script does before running it. If you are happy with all the steps described, you can run the script as a shortcut. If there's anything you want to tweak during installation, you may be better off running the steps manually.
   
     ~/.xmonad/install-xmonad

If you prefer to perform these steps manually to understand what they are doing, read on.

### Install packages ###

This xmonad configuration uses a variety of different packages. Some of them are required for xmonad, others are not specific to xmonad but are core parts of the overall desktop configuration, and others are simply tools which I use frequently enough that my default configuration runs them on startup.

If you want to install the entire list of packages, you can run the following command:

    sudo apt-get install xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar xcompmgr nitrogen stalonetray moreutils synapse ssh-askpass-gnome thunar terminator remmina

If you prefer to pick and choose, the following packages can be omitted while still maintaining the overall functionality:
 * remmina
 * thunar
 * ssh-askpass-gnome

### Install customized xmonad session ###

To launch our xmonad session, we want to be able to pick it from the normal list of sessions available in Ubuntu's login screen, which is called "Unity Greeter". To make our customized version of Xmonad show up in the list, you will need to copy the file xmonad.desktop to the location where the greeter expects to find definitions of all the available sessions. You might also want to back up the default xmonad desktop session in case you want to revert later on.

    sudo mv /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.original
    sudo cp ~/.xmonad/xmonad.desktop /usr/share/xsessions

Ubuntu's packages do not include any icon for xmonad when showing it in the login screen. This means its icon defaults to a plain white circle. But, no worries... I've got you covered. Just copy the custom xmonadbadge into the appropriate location for a nice consistent login experience. 

    sudo cp ~/.xmonad/images/custom_xmonad_badge.png /usr/share/unity-greeter

### Make Gnome 2-based components less ugly ###

If you don't give Gnome some hints about how it should look, anything still based on Gnome 2 is going to be styled using Gnome 2 defaults -- that is to say, ugly. I've provided a configuration file which makes some configuration changes that make things look better, at least if you like dark styling. If you have already tweaked your .gtkrc-2.0 file or otherwise prefer not to replace this file, you can skip these steps. Everything should still be functional.

    mv ~/.gtkrc-2.0 ~/gtkrc-2.0.original
    ln -s .xmonad/.gtkrc-2.0 ~/.gtkrc-2.0

Configuration
-------------

### start-xmonad ###

### xmobarrc ###

### xmonad.hs ###

Other Notes
-----------

### GIMP 2.8 ###

If you are a user of GIMP, you may find the GIMP experience in xmonad somewhat lacking while using version 2.6 (which is what Ubuntu 12.04 comes with by default). This is because xmonad tries to manage all your palettes as tiles which can lead to a somewhat confusing interface. However, with GIMP 2.8, single-window mode has been introduced. This is ideal for using GIMP under xmonad, so upgrading is highly recommended.

### Video Drivers ###
