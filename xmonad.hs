{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.

  Author:     David Brewer
  Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf
-}

import XMonad
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))


{-
  Xmonad configuration variables. These settings control some of the 
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}
xmFocusedBorderColor = "Red" -- color of focused border
xmBorderWidth = 1
xmTerminal = "terminator" -- which terminal software to use
myModMask = mod4Mask -- changes the mod key to "super"

{-
  Xmobar configuration variables. These settings control the appearance 
  of text which xmonad is sending to xmobar via the DynamicLog hook. 
-}
xmbTitleColor = "#eeeeee"  -- color of window title
xmbTitleLength = 80 -- truncate window title to this length
xmbCurrentWorkspaceColor = "#e6744c" -- color of active workspace
xmbVisibleWorkspaceColor = "#c185a7" -- color of inactive workspace
xmbCurrentWorkspaceLeft = "["
xmbCurrentWorkspaceRight = "]"
xmbVisibleWorkspaceLeft = "("
xmbVisibleWorkspaceRight = ")"

{-
  Layout configuration.
-}

defaultLayouts = avoidStruts (ResizableTall 1 (3/100) (1/2) [] ||| Mirror (ResizableTall 1 (3/100) (1/2) []) ||| Grid ||| ThreeColMid 1 (3/100) (3/4))

chatLayout = avoidStruts(withIM (1%7) (Title "Contact List") Grid)
gimpLayout = avoidStruts(ThreeColMid 1 (3/100) (3/4))
fullLayout = avoidStruts(noBorders Full)
circleLayout = avoidStruts(Circle)

myLayouts = onWorkspace "7:Chat" chatLayout $ onWorkspace "9:Pix" gimpLayout $ defaultLayouts ||| fullLayout ||| circleLayout

{-
  Workspace configuration.
-}

myWorkspaces =
  [
    "7:Chat",  "8:Dbg", "9:Pix",
    "4:Docs",  "5:Dev", "6:Web",
    "1:Term",  "2:Hub", "3:Mail",
    "0:VM",    "Extr1", "Extr2"
  ]

{-
  Management hooks.
-}

managementHooks :: [ManageHook]
managementHooks = [
  resource  =? "synapse"   --> doIgnore
  , resource =? "stalonetray" --> doIgnore
  , className =? "rdesktop"  --> doFloat
  , (className=? "Komodo IDE") --> doF (W.shift "5:Dev")
  , (className=? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
  , (className=? "Komodo IDE" <&&> resource =? "Komodo_gotofile") --> doFloat
  , (className=? "Komodo IDE" <&&> resource =? "Toplevel") --> doFloat
  , (className=? "Empathy") --> doF (W.shift "7:Chat")
  , (className=? "Pidgin") --> doF (W.shift "7:Chat")
  , (className=? "Gimp-2.8") --> doF (W.shift "9:Pix")
  ]

{-
  Keyboard configuration.
-}

-- Non-numeric num pad keys, top to bottom
numPadKeys =
  [
    xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
    , xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  ]

-- Number keys in same order as numpad keys
numKeys =
  [
    xK_7, xK_8, xK_9
    , xK_4, xK_5, xK_6
    , xK_1, xK_2, xK_3
    , xK_0, xK_minus, xK_equal
  ]

myKeys =
  [
    ((myModMask,   xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_a), sendMessage MirrorShrink)
    , ((myModMask, xK_z), sendMessage MirrorExpand)
    , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 10%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 10%+")
  ] ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  M.toList (planeKeys myModMask (Lines 4) Finite)
  ++
  [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2] 
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

{-
  Here we actually stitch together all the configuration settings
  and run xmonad..
-}

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ defaultConfig {
    focusedBorderColor = xmFocusedBorderColor
  , terminal = xmTerminal
  , borderWidth = xmBorderWidth
  , manageHook = manageHook defaultConfig <+> composeAll managementHooks <+> manageDocks
  , logHook = dynamicLogWithPP $ xmobarPP { 
        ppOutput = hPutStrLn xmproc
       ,ppTitle = xmobarColor xmbTitleColor "" . shorten xmbTitleLength
       ,ppCurrent = xmobarColor xmbCurrentWorkspaceColor "" . wrap xmbCurrentWorkspaceLeft xmbCurrentWorkspaceRight
       ,ppVisible = xmobarColor xmbVisibleWorkspaceColor "" . wrap xmbVisibleWorkspaceLeft xmbVisibleWorkspaceRight
    }
  , layoutHook = myLayouts
  , workspaces = myWorkspaces
  , modMask = myModMask
  }
    `additionalKeys` myKeys
