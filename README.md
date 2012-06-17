xmonad-ubuntu-conf
==================

My xmonad config for Ubuntu 12.04, including package list, config files, and instructions.

Overview
--------

Installation
------------

### Install packages ###

### Install customized xmonad session ###

TODO: figure out how to make desktop file refer to our xmonad start script no matter what the user's home directory is called.

To launch our xmonad session, we want to be able to pick it from the normal list of sessions available in Ubuntu's login screen, which is called "Unity Greeter". To make our customized version of Xmonad show up in the list, you will need to copy the file xmonad.desktop to the location where the greeter expects to find definitions of all the available sessions. You might also want to back up the default xmonad desktop session in case you want to revert later on.

    sudo mv /usr/share/xessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.original
    sudo cp ~/.xmonad/xmonad.desktop /usr/share/xsessions

Ubuntu's packages do not include any icon for xmonad when showing it in the login screen. This means its icon defaults to a plain white circle. But, no worries... I've got you covered. Just copy the custom xmonadbadge into the appropriate location for a nice consistent login experience. 

    sudo cp ~/.xmonad/custom_xmonad_badge.png /usr/share/unity-greeter

Configuration
-------------

### start-xmonad ###

### xmobarrc ###

### xmonad.hs ###

Other Notes
-----------

If you are a user of GIMP, you may find the GIMP experience in xmonad somewhat lacking while using version 2.6 (which is what Ubuntu 12.04 comes with by default). This is because xmonad tries to manage all your palettes as tiles which can lead to a somewhat confusing interface. However, with GIMP 2.8, single-window mode has been introduced. This is ideal for using GIMP under xmonad, so upgrading is highly recommended.


