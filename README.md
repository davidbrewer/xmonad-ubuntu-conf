xmonad-ubuntu-conf
==================

My xmonad config for Ubuntu 12.04, including package list, config files, and instructions.

Overview
--------

What you're looking at is my personal xmonad configuration setup, heavily commented and organized as clearly as I could manage.

I have been using some form of this setup on a daily basis for over a year now in my work as a web developer. An xmonad configuration usually ends up being a very individualized thing, but I thought it would be valuable to share this as a starting point for people new to xmonad. I know that when I was starting with xmonad I found reading other people's configuration files the best way to learn. I hope you will find this equally helpful.

This configuration has the following features and properties:
* Lightweight standalone configuration, not intended to be run inside Gnome or XFCE.
* Workspace layout concept based on a grid corresponding to the number pad. Workspaces can be selected with numpad keys, number keys, or via arrows in a consistent and intuitive fashion.
* A restrained but useful set of layout options. I have tried to stick with simple, flexible layouts which are useful on a daily basis.
* Basic status bar and task tray configuration using xmobar and stalonetray
* Relies on synapse for launching applications
* Includes tray icons for network management, chat, and remote desktop connections.
* Adds xmonad as an option to your Unity login greeter, including a proper icon.
* Wallpaper handling and support for basic transparency.


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


Launching xmonad
----------------

Once installed, xmonad will show up in the Unity greeter, where you normally choose which desktop you want to run. If you've just finished installing it:
1. Log out.
2. Click on the round badge next to your user name.
3. Choose "Xmonad" from the list.
4. Log in again.


Using xmonad
------------

In this section I provide a quick start guide to using xmonad, with an emphasis on the specific features of this configuration. Keybindings are provided in a format like `mod-keyname`, or `mod-shift-keyname`. In this configuration we are using the "super" key as the mod key, which on many keyboards is the windows logo key.

### Launching software

When you start xmonad for the first time, you're not looking at much. You will see a status bar near the top of your screen, and that's about it.

There are no menus for selecting programs to run. Everything is launched in one of two ways:
* `mod-shift-enter`: launches a terminal window (Terminator). You can run other programs from the terminal.
* `mod-alt-space`: launches a Synapse prompt. You can run any program by starting to type its name, and then hitting enter once Synapse has found the program you want. 

### The status bar

Let's take a look at the status bar. Some of what's up there may not make much sense yet, but it will be useful to know about it as we go through the remaining sections of this guide.

The status bar can be divided into 6 major zones.  Running from left to right we have:
1. The **workspace list**, which contains a list of all the workspaces which currently contain any windows, plus the workspace we are currently viewing. The currently viewed workspace will be bracketed in square brackets and will be a different color from any inactive workspaces. For more on the concept of workspaces, see the Workspaces section below.
2. The **current layout**, which displays the name of the layout which is applied to the currently viewed workspace.
3. The **title area**, which shows the title of the currently focused window. It's up there to save the vertical space that would normally be consumed by title bars on windows.
4. The **system information area**, which displays a few useful pieces of system information, including: battery charge status, CPU utilization, memory usage, and current volume.
5. The **date**.
6. The **icon tray**, which is home to any status icons for programs you are running. By defaults you should see a network manager icon, a chat icon, and an icon for launching remote desktop connections.

If at any point you would like to reclaim some extra vertical space on your screen, you can toggle the visiblity of the status bar by hitting `mod-b`. 

### Changing layouts

As you launch software, it will automatically be positioned on the screen following rules defined by your current "layout", which is like a strategy that xmonad uses to determine how windows should be positioned. At any time you can cycle through various layouts:
* `mod-space`: cycle through the available layouts`
* `mod-shift-space`: jump to the default layout

Note that as you change your layout, the name of the current layout is displayed in the status bar, immediately after the list of workspaces.

There are six main layouts I have provided in my configuration:

1. **ResizableTall** has a large master pane on the left, and remaining windows tile on the right. By default each area takes up half the screen, but you can resize using keys defined in "Manipulating windows", below.
2. **Mirror ResizableTall** is similar to the first layout, but the larger master pane is at the top, and the remaining windows tile at the bottom of the screen. By default each area takes up half the screen, but this layout can also be resized.
3. **Full** layout makes every window full screen with no borders. When you cycle through the windows, as each window becomes active it will be brought to the front. 
4. **Grid** layout tries to equally distribute windows in the available space, increasing the number of columns and rows as necessary. The master pane is at top left, but does not get priority over other windows in any other way. Not a resizeable layout.
5. **ThreeCol** layout puts the large master pane in the center of the screen taking up most of the available screen space. Remaining windows tile to both the left and right of the master pane. This layout is resizeable.
6. **Circle** layout places the master pane in the center of the screen, with space on all sides. Remaining windows appear positioned in a circle around it, partially overlapping it. The focused window is brought to the front so you can see all of its contents. Not a resizable layout. 

In addition to the six main layouts, there is also a special layout called **IM Grid**, which is only activated on the Chat workspace. See the Workspaces section for more information.

Now would be a good time to try out the layouts to get a sense of what they offer. Hit `mod-shift-enter` several times to launch some empty terminals, and then experiment with hitting `mod-space` to cycle through the different layouts to see what they are like.

### Manipulating windows

Once you've got multiple windows on the page and you actually start trying to use them, you'll probably find yourself wanting to manipulate the windows in various ways. 

#### Focusing windows

The currently focused window is outlined with a 1-pixel red border. The other, unfocused windows have a grey border. 

You can change your focus as follows:
* `mod-j` or `mod-shift-tab`  moves your focus to the previous window
* `mod-k` or `mod-tab` moves your focus to the next window
* `mod-m` moves your focus to the master pane (see next section)
* or, you can focus a window by moving your mouse cursor over it 

#### Master pane

Many xmonad layouts (but not all) make a distinction between a "master pane" and "everything else". By default, the first program launched on a workspace goes into the master pane. In most layouts, this means it is given priority over the other windows in some way; it may be larger, centrally located, or otherwise made more prominent. 

Although by default there is just one window in the master pane, in some layouts it is possible to move more than one window into the master pane. They will evenly share the available space.

You can manipulate the contents of the master pane as follows:
* `mod-enter` will swap the currently focused window with the contents of the master pane, making it the new master.
* `mod-comma` will make the master pane contain more windows, if the current layout supports this.
* `mod-period` will make the master pane contain fewer contains, if the current layout supports this.

#### Resizing windows

There are a couple of different ways you can resize windows in xmonad. These options are only available in the layouts that support them; in some layouts, you do not have control over the size of the windows.
* `mod-l` and `mod-h` will grow or shrink the size of the master pane. 
* `mod-a` and `mod-z` will grow or shrink the size of the currently focused window.

The wonderful thing about resizing windows in xmonad is that all the other windows are moved to make room, so that the contents of all windows are always visible.

#### Closing windows

You can use `mod-shift-c` to close the focused window if you are done with it.

### Workspace

Now that you're creating, focusing, and resizing windows like crazy, you may find yourself running out of space on your screen. No worries, xmonad has got you covered with the concept of **workspaces**. Workspaces are similar to the idea of virtual desktops. However, unlike virtual desktops in most other window managers, workspaces are assigned names. 

There are many ways to organize workspaces in xmonad, but I have chosen to associate them with keys on the number pad. Here's how you can think of the layout of workspaces which are provided by default in this configuration:

    7:Chat  8:Dbg  9:Pix
    4:Docs  5:Dev  6:Web
    1:Term  2:Hub  3:Mail
    0:VM    Extr1  Extr2

Associating workspaces with number pad keys has a number of advantages:
* If you are using a full-sized desktop keyboard, it is very intuitive to select workspaces using the numpad.
* If you are using a laptop, you can still select workspaces numerically using the regular number keys.
* Because the number pad provides a grid layout, it's also intuitive and efficient to navigate between workspaces using the arrow keys.

The twelve workspaces I use are named for the kinds of work I regularly do. I have tried to train myself to always keep certain types of work on specific workspaces, because it makes it much easier to keep track of what I'm doing and find it again if I get interrupted.

You are likely to want to rename these to suit your own needs. Nevertheless, here's an explanation of what I use each workspace for in case it gives you some ideas:
* Chat: all instant messenger sessions are launched here.
* Dbg: debugging. Depending on what I'm doing this may contain a terminal or a browser.
* Pix: image manipulation; specifically, I run GIMP on this workspace.
* Docs: documentation. I try to keep whatever docs I am referring to on this workspace.
* Dev: development work. I spend the bulk of my time on this workspace. I often have an IDE and a terminal on this workspace. Note that it is centrally located to all the other workspaces; I treat it like a "home row", and I can jump to most other workspaces with one or two arrow keystrokes.
* Web: general web surfing.
* Term: I keep a terminal open here for tasks which are not directly development related.
* Hub: ticket tracking (my workplace calls our ticket tracking and project planning server "hub").
* Mail: email (and also calendaring).
* VM: I launch virtual machines here.
* Extr1: general usage
* Extr2: general usage

#### Selecting a workspace

You can use the following commands to change which workspace you are looking at:
* `mod-NUMBER`: move to a workspace using its number 
* `mod-KEYPADNUMBER`: move to a workspace using its number (on the keypad)
* `mod-ARROW`: move to a workspace directionally, using the arrow keys. The workspaces are organized in a grid which matches the layout of the number pad. 

#### Moving a window to a workspace

You can move the currently focused window to any workspace by simply adding "shift" to the commands listed above. So:
* `mod-shift-NUMBER`: move a window to a workspace using its number 
* `mod-shift-KEYPADNUMBER`: move a window to a workspace using its number (on the keypad)
* `mod-shift-ARROW`: move to a window to workspace directionally, using the arrow keys. The workspaces are organized in a grid which matches the layout of the number pad. 

Note that when you move a window to a workspace using numbers or the keypad, the window is sent to that workspace but your focus stays on the same workspace you were on. However, when you move a window to a workspace using the arrows, your focus goes along with the window.

#### Special workspaces

There are two workspaces which have a special configuration. They are locked to a special layout, and certain programs will always spawn on those workspaces.
* The **Chat** workspace (7) uses a special chat-focused layout called **IM Grid**. In this layout, one window is identified as the "roster" and it is displayed tall and skinny on the left side of the screen. The remaining windows, which are individual chat sessions, are displayed in a grid to the right of the roster area. This default configuration assumes you are using Empathy for your chat software, and any Empathy windows that are launched will automatically get sent to the chat workspace.
* The **Pix** workspace (9) is locked on the **ThreeCol** layout, which is especially suitable for working with GIMP because you can put the image you are working on in the master pane and let all the panels tile to the left and right of the image. Any GIMP windows that are launched will automatically be sent to this workspace.

### Multiple monitors

Multiple monitor support in xmonad can take some doing to set up and get comfortable with, but once you are used to it, you will find it extraordinarily powerful.

What makes xmonad's monitor support different from traditional window managers is that your virtual desktops are not linked together in pairs, but are instead completely independent. In other words, you can instantly place any workspace on any screen, and then place any other workspace on another screen. You have complete control over what you want to see on each of your screens.

#### Setup

Getting multiple monitors working in xmonad is tricky, and more than I can thoroughly document here. I have had the best luck using the command line tool xrandr. In order to get your configuration working to your liking, you will probably have to tweak the Screen Configuration section of the `start-xmonad` file. The example xrandr commands I have provided should be a helpful starting point.

Depending on the relative positions of your screens you may also need to tweak `xmonad.hs` to modify which screen each of the screen seletion key refers to. Otherwise, they may be in the wrong order, which can be confusing. I prefer that the screen selection keys are in the same order on my keyboard as the monitors they refer to are physically located. 

#### Using multiple screens

In my default configuration, I have two screens and I use "w" to refer to the left and "e" to refer to the right screen. 

* `mod-w` moves the focus to the left screen
* `mod-e` moves the focus to the right screen

Once you have a screen focused, you can use the regular workspace selection keys to put whatever you want on the focused screen. 

### Quitting or restarting xmonad

The following commands involve quitting or restarting xmonad:
* `mod-shift-q`: quits xmonad, returning to the login screen. 
* `mod-q`: restarts and recompiles xmonad. Does not require restarting any other programs you may be running, and happens very quickly. Very useful while experimenting with changes to your `xmonad.hs` file!

Configuration
-------------

This section discusses making change to this xmonad configuration. There are no GUI tools for accomplishing this; everything is handled through configuration files. I have extensively commented all my configuration files and tried to make things as clear as possible; for the most part you should refer to the configuration files themselves for documentation. What I provide here is more like a map of where to start looking for specific pieces of functionality.

### Changing your wallpaper ###

To change your wallpaper, locate a director which contains some images you would like to use for wallpaper.  Then run the following at the command line:

    nitrogen /path/to/your/images

You will be presented with a GUI interface for selecting your wallpaper.

### Editing start-xmonad ###

The `start-xmonad` file runs during the login process, BEFORE xmonad itself has run. It runs a number of configuration tools, and starts some components used by the xmonad configuration.

You should take a look at the `start-xmonad` file if you want to make any of the following kinds of changes:
* you want to get multi-monitor support working properly and the default setup isn't working for you
* you want to tweak the settings on the icon tray part of the status bar, perhaps to change its height or width, or to make room for more icons
* you want to disable or change any of the following:
  * compositing (support for transparency)
  * background images 

If you make changes to `start-xmonad`, the only way to see the changes is to log out of xmonad and log back in using `mod-shift-q`. This can make testing changes to this file a little tedious.

### Editing startup-hook ###

The `startup-hook` script runs immediately after xmonad is initialized, via the startupHook mechanism of xmonad itself. 

You should take a look at editing the `startup-hook` script if you want to modify any of the software that is started by default, such as:
* applicaiton launcher (synapse)
* network management software
* chat software
* remote desktop software
* ssh keychain unlocking prompt

Note that by default I have commented out the ssh keychain unlocking prompt, assuming that this would be annoying when getting started with xmonad. If you are a heavy ssh user you might consider uncommenting that feature.

### Editing xmobarrc ###

The `xmobarrc` file is used to configure the ovreall appearance of the status bar, as well as provide part of its content. You should consider editing it if you want to make any of the following kinds of changes:
* you want to change the font or default colors used for the bar
* you want to change the width or position of the bar (expect to also make changes to stalonetray in the `start-xmonad` file as well in this case
* you want to change the contents or formatting of the system information and/or date which is displayed near the right side of the status bar

You can see whether the changes you have made to `xmobarrc` have been effective by recompiling xmonad using `mod-q`. This typically happens very quickly. If you try this and your status bar disappears, it means you made a syntax error in your configuration file. Undo the change and hit `mod-q` again to confirm things are working again.

### Editing xmonad.hs ###

The `xmonad.hs` file is the main xmonad configuration file. Actually, it's not really a configuration file, but a Haskell program. This can make its syntax a little daunting to grasp. 

I have done my best to format the file in a comprehensible manner and provide extensive comments. Still, editing this file can be a little risky. It's best to make small changes and compile frequently (using `mod-q`) to test if your changes work. If you get an error message, simply undo your change and compile again.

You should take a look at `xmonad.hs` if you want to:
* change key bindings for xmonad commands
* change the color or width of the border around windows
* change the default terminal
* configure the appearance of the workspace list, layout name, or window title parts of the status bar
* rename the workspaces
* modify the layout defaults, add new layouts, or remove layouts
* set up "management hooks" which allow you to assign special treatment to certain windows (such as telling certain types of dialogs not to be tiled by xmonad, or sending certain programs to a specific workspace)

### Editing get-volume ###

The `get-volume` script is a small utility used by xmobar to display volume information in the status bar. I borrowed it completely unchanged from a blog post. In some cases, it may not work for your hardware and need to be modified.

In particular I have noticed problems with machines that have more than one sound card. On my machine I was able to fix this by modifying one line of the script:

    # I changed this first line of the script:
    str=`amixer sget Master,0`
    
    # so that instead it read like this:
    str=`amixer -c 1 sget Master,0`


Other Notes
-----------

### GIMP 2.8 ###

If you are a user of GIMP, you may find the GIMP experience in xmonad somewhat lacking while using version 2.6 (which is what Ubuntu 12.04 comes with by default). This is because xmonad tries to manage all your palettes as tiles which can lead to a somewhat confusing interface. However, with GIMP 2.8, single-window mode has been introduced. This is ideal for using GIMP under xmonad, so upgrading is highly recommended.

### Video Drivers ###

I have an nVidia graphics card in my laptop, and I had some problems when I was using the proprietary nVidia drivers. In particular I had a very difficult time getting fonts to look the way I expected. I have had more luck with the Nouveau driver in my particular scenario. On other machines I have tried (also with nVidia cards), the proprietary drivers worked fine. It might be worth experimentation.
