import System.IO
import System.Exit
import qualified Data.List as L
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Gaps
import XMonad.Layout.Fullscreen
import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ZoomRow
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Cursor
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "st -e /bin/zsh"
myXmobarrc = "~/.xmonad/xmobar.hs"
startapps = "~/.xmonad/autostart.sh"
myLauncher = "dmenu_run"
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0"]

myManageHook = composeAll
    [
      className =? "Firefox"                      --> doShift "2"
    , resource  =? "desktop_window"               --> doIgnore
    , className =? "Thunderbird"                  --> doShift "3"
    , className =? "Todoist"                      --> doShift "1"
    , className =? "Steam"                        --> doShift "4"
    , className =? "Lutris"                       --> doShift "4"
    , className =? "League"                       --> doShift "4"
    , className =? "TeamSpeak 3"                  --> doShift "4"
    , className =? "keepassxc"                    --> doShift "7"
    , className =? "Kodi"                         --> doShift "8"
    , className =? "TeamViewer"                   --> doShift "9"
    ]


outerGaps    = 10
myGaps       = gaps [(U, outerGaps), (R, outerGaps), (L, outerGaps), (D, outerGaps)]
addSpace     = renamed [CutWordsLeft 2] . spacing gap
tab          =  avoidStruts
               $ renamed [Replace "Tabbed"]
               $ addTopBar
               $ myGaps
               $ tabbed shrinkText myTabTheme

layouts      = avoidStruts (
                (
                    renamed [CutWordsLeft 1]
                  $ addTopBar
                  $ windowNavigation
                  $ renamed [Replace "BSP"]
                  $ addTabs shrinkText myTabTheme
                  $ subLayout [] Simplest
                  $ myGaps
                  $ addSpace (BSP.emptyBSP)
                )
                ||| tab
               )

myLayout    = smartBorders
              $ mkToggle (NOBORDERS ?? FULL ?? EOT)
              $ layouts

myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full",          centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
                                  ]
    }


xmobarTitleColor = "#C678DD"
xmobarCurrentWorkspaceColor = "#51AFEF"
myBorderWidth = 0

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

myFont      = "xft:monospace:size=15:bold:antialias=true"
myBigFont   = "xft:monospace:size=15:bold:antialias=true"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"


topBarTheme = def
    {
      fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

addTopBar =  noFrillsDeco shrinkText topBarTheme

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }


myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --
  [ ((modMask .|. shiftMask,               xK_Return), spawn $ XMonad.terminal conf)
  , ((modMask,                                  xK_d), spawn myLauncher)
  , ((modMask .|. shiftMask,                xK_Right), sendMessage $ ExpandTowards R)
  , ((modMask .|. shiftMask,                 xK_Left), sendMessage $ ExpandTowards L)
  , ((modMask .|. shiftMask,                 xK_Down), sendMessage $ ExpandTowards D)
  , ((modMask .|. shiftMask,                   xK_Up), sendMessage $ ExpandTowards U)
  , ((0,                                  0x1008ff02), spawn "light -A 10")
  , ((0,                                  0x1008ff03), spawn "light -U 10")
  , ((modMask,                                xK_F10), spawn "screen-switcher")
  , ((modMask,                                  xK_l), spawn "cmdlock")
  , ((modMask .|. shiftMask,                   xK_F1), spawn "shut-sup-rest")
  , ((modMask,                                  xK_o), spawn "pavucontrol")
  , ((modMask,                                  xK_k), spawn "qalculate-gtk")
  , ((modMask,                                  xK_r), spawn "st -e ranger")
  , ((modMask,                                 xK_F4), spawn "dmenumount")
  , ((modMask,                                 xK_F5), spawn "dmenuumount")
  , ((0,                                  0x1008ff12), spawn "ponymix toggle")
  , ((0,                                  0x1008ff11), spawn "ponymix decrease 5")
  , ((0,                                  0x1008ff13), spawn "ponymix increase 5")
  , ((modMask,                                 xK_F1), spawn "ponymix toggle")
  , ((modMask,                                 xK_F2), spawn "ponymix decrease 5")
  , ((modMask,                                 xK_F3), spawn "ponymix increase 5")
  , ((0,                                    xK_Print), spawn "scrot ~/Screenshots/%Y-%m-%d-%H%M%S.png")
  , ((modMask,                                  xK_f), sendMessage $ Toggle FULL)
  , ((modMask .|. shiftMask,                    xK_q), kill)
  , ((modMask,                              xK_space), sendMessage NextLayout)
  , ((modMask .|. shiftMask,                xK_space), setLayout $ XMonad.layoutHook conf)
  , ((modMask,                                  xK_n), refresh)
  , ((modMask,                                xK_Tab), windows W.focusDown)
  , ((modMask,                               xK_Left), windows W.focusDown)
  , ((modMask,                               xK_Right), windows W.focusUp  )
  , ((modMask,                                   xK_m), windows W.focusMaster  )
  , ((modMask,                              xK_Return), windows W.swapMaster)
  , ((modMask .|. shiftMask,                     xK_j), windows W.swapDown  )
  , ((modMask .|. shiftMask,                     xK_k), windows W.swapUp    )
  , ((modMask,                                   xK_t), withFocused $ windows . W.sink)
  , ((modMask,                               xK_comma), sendMessage (IncMasterN 1))
  , ((modMask,                              xK_period), sendMessage (IncMasterN (-1)))
  , ((0,                                   0x1008ff14), spawn "cmus-remote -u")
  , ((0,                                   0x1008ff17), spawn "cmus-remote -n")  
  , ((0,                                   0x1008ff16), spawn "cmus-remote -r")        
--, ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
--, ((modMask, xK_q), restart "xmonad" True) 
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))
  ]

myStartupHook :: X ()
myStartupHook = do
   setWMName "LG3D"
   spawn     "bash ~/.xmonad/startup.sh"

main = do
  spawn "xrandr --output HDMI-1-1 --left-of eDP-1-1"
  xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
  xmonad $ defaults {
z
      , manageHook = manageDocks <+> myManageHook
      , handleEventHook = docksEventHook
  }


defaults = defaultConfig {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}
