import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Running
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- Layout
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import XMonad.Hooks.EwmhDesktops -- _NET_ACTIVE_WINDOW support

-- Bar
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- Keys
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86


myTerminal              = "st"
myBorderWidth           = 4
myNormalBorderColor     = "#3b4252"
myFocusedBorderColor    = "#81a1c1"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = True


myLayout = avoidStruts $ smartBorders $ spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


-- xmobar

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
    where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    where clickable l = [ "<action=xdotool key alt+" ++ show n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


-- Manage Hook
myManageHook :: XMonad.Query(Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "st-floating"    --> doFloat
    , className =? "Dragon-drag-and-drop" --> doFloat
    , title     =? "Steam - News"   --> doFloat
    , title     =? "Friends List"   --> doFloat
    , title     =? "Clipboard Editor" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]


-- Startup hook

myStartupHook = do
    spawnOnce "~/.xmonad/autostart.sh &"


-- Main

main = do
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobar/xmobar.hs"
    xmproc1 <- spawnPipe "xmobar -x 1 ~/.xmonad/xmobar/xmobar.hs"

    xmonad $ ewmh $ docks $ defaultConfig
        { manageHook            = manageDocks <+> manageHook defaultConfig <+> myManageHook
        , handleEventHook       = handleEventHook def <+> fullscreenEventHook
        , workspaces            = myWorkspaces
        , terminal              = myTerminal
        , focusFollowsMouse     = myFocusFollowsMouse
        , clickJustFocuses      = myClickJustFocuses
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        , layoutHook            = myLayout
        , logHook               = dynamicLogWithPP xmobarPP
                                    { ppOutput  = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                                    , ppCurrent = xmobarColor "#eceff4" "#81a1c1:0" . wrap " " " "  -- Current workspace
                                    , ppVisible = xmobarColor "#88c0d0" ""                          -- Visible but not current workspace (other monitor)
                                    , ppHidden  = xmobarColor "#d8dee9" "" . wrap "*" ""            -- Hidden workspaces, contain windows
                                    , ppHiddenNoWindows = xmobarColor "#d8dee9" ""                  -- Hidden workspaces, no windows
                                    , ppTitle   = xmobarColor "#eceff4" "" . shorten 100            -- Title of active window
                                    , ppSep     = "<fc=#434c5e> | </fc>"                            -- Separator
                                    , ppUrgent  = xmobarColor "#ebcb8b" "" . wrap "!" "!"           -- Urgent workspaces
                                    , ppExtras  = [windowCount]                                     -- Number of windows in current workspace
                                    , ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                    }
        , modMask               = mod1Mask
        , startupHook           = myStartupHook
        } `additionalKeys`
          [ ((mod1Mask .|. shiftMask        , xK_p              ), spawn "powermenu")
          , ((mod1Mask .|. shiftMask        , xK_l              ), spawn "light-locker-command -l")

          -- Program keys
          , ((mod1Mask .|. shiftMask        , xK_e              ), spawn "clipedit")
          , ((mod4Mask                      , xK_f              ), spawn "firefox-nightly")
          , ((mod4Mask                      , xK_d              ), spawn "discord-canary")
          , ((mod4Mask                      , xK_m              ), spawn "st -e neomutt")
          , ((mod4Mask                      , xK_r              ), spawn "st -e ranger")
          , ((mod4Mask                      , xK_p              ), spawn "passmenu")

          -- Dunst keys
          , ((mod4Mask                      , xK_Escape         ), spawn "dunstctl close")
          , ((mod4Mask .|. shiftMask        , xK_Escape         ), spawn "dunstctl close-all")
          , ((mod4Mask                      , xK_grave          ), spawn "dunstctl history-pop")
          , ((mod4Mask .|. shiftMask        , xK_period         ), spawn "dunstctl context")

          -- Upload keys
          , ((mod4Mask                      , xK_u              ), spawn "imupdrag")
          , ((mod4Mask .|. shiftMask        , xK_u              ), spawn "imupclip")

          -- Keyboard layout keys
          , ((mod4Mask .|. mod1Mask         , xK_z              ), spawn "setxkbmap -layout us")
          , ((mod4Mask .|. mod1Mask         , xK_x              ), spawn "setxkbmap -layout us -variant altgr-intl")

          -- Screenshot keys
          , ((0                             , xK_Print          ), spawn "ssclip select")
          , ((controlMask                   , xK_Print          ), spawn "ssclip window")
          , ((controlMask .|. shiftMask     , xK_Print          ), spawn "ssclip full")

          -- Volume keys
          , ((0                             , xF86XK_AudioMute          ), spawn "audioctl toggle")
          , ((0                             , xF86XK_AudioRaiseVolume   ), spawn "audioctl up")
          , ((0                             , xF86XK_AudioLowerVolume   ), spawn "audioctl down")
          ]
