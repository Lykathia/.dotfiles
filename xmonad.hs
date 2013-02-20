import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO
import System.IO.Unsafe
import System.Exit

import XMonad

import Data.Char (chr)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (noBorders, smartBorders, withBorder)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.Run (hPutStrLn, spawnPipe)

---------------------------------------------------------------------
--  Bar Setup
---------------------------------------------------------------------

-- Workspace Names
allWorkspaces :: [WorkspaceId]
allWorkspaces =
    [   workspace "" "TERM"
    ,   workspace "arch.xbm" "WEB"
    ,   workspace "" "CHAT"
    ,   workspace "" "CODE"
    ,   workspace "" "CODE"
    ,   workspace "" "TEMP"
    ,   workspace "" "TEMP"
    ,   workspace "" "STEAM"
    ,   workspace "" "VM"
    ]
    where
        workspace image text = unsafePerformIO $ do
            homeDir <- getHomeDirectory
            return ("^i(" ++ homeDir </> "configs/icons/sm4tik" </> image ++ ") " ++ text)

-- Dzen
workspaceDzenCmd :: String
workspaceDzenCmd = "dzen2 -dock -y -1 -ta l -p -e ''"
tempDzen1 = "dzen2 -xs 0 -dock -ta l -p -e ''"
tempDzen2 = "while true; do date; sleep 1; done | dzen2 -xs 1 -dock -ta r -p -e ''"

-- Formatting for dzen
myDzenPP :: Handle -> PP
myDzenPP h = dzenPP
    {   ppCurrent           = dzenColor colourRed       colourBlack . pad
    ,   ppVisible           = dzenColor colourBlue      colourBlack . pad
    ,   ppHidden            = dzenColor colourWhiteAlt  colourBlack . pad
    ,   ppHiddenNoWindows   = dzenColor colourGray      colourBlack . pad
    ,   ppUrgent            = dzenColor colourGreen     colourBlack . pad
    ,   ppTitle             = dzenColor colourWhiteAlt  colourBlack . pad . titleText . dzenEscape
    ,   ppLayout            = dzenColor colourBlue      colourBlack . pad . clickableLayout
    ,   ppOrder             = \(ws:l:t:_) -> [l,ws,t]
    ,   ppSep               = "^fg(" ++ colourGray ++ ")|"
    ,   ppWsSep             = ""
    ,   ppOutput            = hPutStrLn h
    }
    where
        titleText [] = "Desktop"
        titleText x = (shorten 82 x)
        clickableLayout x = "^ca(1,xdotool key alt+space)" ++ x ++ "^ca()"

---------------------------------------------------------------------
--  Colours / Fonts / Appearance
---------------------------------------------------------------------

-- Fonts
barFont :: String
barFont = "inconsolata"

delimChar :: Char
delimChar = chr 127

-- Color Definitions
colourBlack     = "#020202"
colourBlackAlt  = "#1c1c1c"
colourGray      = "#444444"
colourGrayAlt   = "#161616"
colourWhite     = "#a9a6af"
colourWhiteAlt  = "#9d9d9d"
colourMagenta   = "#8e82a2"
colourBlue      = "#3475aa"
colourRed       = "#d74b73"
colourGreen     = "#99cc66"

myFocusedBorder     = colourBlackAlt
myUnfocusedBorder   = colourGray

myTabTheme :: Theme
myTabTheme = defaultTheme
    {   fontName            = barFont
    ,   activeColor         = colourBlackAlt
    ,   activeTextColor     = colourWhiteAlt
    ,   activeBorderColor   = colourGray
    ,   inactiveColor       = colourBlack
    ,   inactiveTextColor   = colourGray
    ,   inactiveBorderColor = colourBlackAlt
    ,   urgentBorderColor   = colourGray
    ,   urgentTextColor     = colourGreen
    }

---------------------------------------------------------------------
--  Hooks
---------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? "Xfce4-notifyd" --> doIgnore ]
    , [ isFullscreen                  --> doFullFloat ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doShift "1:main"    | x <- my1Shifts ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doShift "8:steam"   | x <- my1Shifts ]
    ]
    where
        my1Shifts = []
        my8Shifts = ["Steam", "Friends"]

---------------------------------------------------------------------
--  Layouts
---------------------------------------------------------------------

-- Layouts
tiledLayout     = renamed [Replace "A"] $ smartBorders $ ResizableTall 1 0.03 0.5 []
gridLayout      = renamed [Replace "G"] $ smartBorders $ spacing 5 $ Grid
fullLayout      = renamed [Replace "F"] $ smartBorders $ tabbedAlways shrinkText myTabTheme
chatLayout      = renamed [Replace "C"] $ withIM (0.15) (Title "Buddy List") $ Mirror $ ResizableTall 1 0.03 0.5 []
steamLayout     = renamed [Replace "S"] $ withIM (0.15) (Title "Friends") $ Mirror $ ResizableTall 1 0.03 0.5 []

-- Hook
myLayout = avoidStruts 
    -- $ onWorkspace (allWorkspaces !! 1) webLayout
    $ onWorkspace (allWorkspaces !! 2) chatLayout
    $ onWorkspace (allWorkspaces !! 7) steamLayout
    $ allLayouts
    where
        allLayouts = tiledLayout ||| gridLayout ||| fullLayout

--------------------------------------------------------------------
--  Keybindings
---------------------------------------------------------------------

--  Keyboard Bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -----------------------------------------------------------------
    --  Programs
    -----------------------------------------------------------------
    -- terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -----------------------------------------------------------------
    --  Global Controls 
    -----------------------------------------------------------------
    {- 
    -- ncmpcpp
    -}

    -----------------------------------------------------------------
    --  Window Control
    -----------------------------------------------------------------
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -----------------------------------------------------------------
    --  Misc
    -----------------------------------------------------------------
    -- Colemak
    , ((modm,               xK_a     ), spawn "setxkbmap us -variant colemak")

    -- Qwerty
    , ((modm,               xK_z     ), spawn "setxkbmap us")

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall dzen2 trayer nm-applet; xmonad --recompile; xmonad --restart")
    ]
    ++
 
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

---------------------------------------------------------------------
--  Startup Commands
---------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawn "trayer --edge bottom --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --transparent true --alpha 0 --tint 0x0c0c0b --height 16 --monitor 1"
    spawn "nm-applet"
    setWMName "LG3D"
    setDefaultCursor xC_left_ptr 

---------------------------------------------------------------------
--  XMonad Main
---------------------------------------------------------------------

main :: IO ()
main = do
    -- Bars
    workspaceBar    <- spawnPipe workspaceDzenCmd
    topLeftDzenBar  <- spawnPipe tempDzen1 
    topRightDzenBar <- spawnPipe tempDzen2

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        {   terminal                = "urxvtc"
        ,   workspaces              = allWorkspaces

        -- Hooks / Layouts
        ,   layoutHook              = myLayout
        ,   manageHook              = myManageHook <+> manageDocks <+> (doF W.swapDown)
        ,   handleEventHook         = fullscreenEventHook
        ,   startupHook             = myStartupHook
        ,   logHook                 = do
                dynamicLogWithPP $ myDzenPP workspaceBar
                fadeInactiveLogHook 1.0

        -- Graphical stuff
        ,   focusFollowsMouse       = True
        ,   borderWidth             = 1
        ,   normalBorderColor       = myUnfocusedBorder
        ,   focusedBorderColor      = myFocusedBorder

        -- Bindings
        ,   keys                    = myKeys
        ,   modMask                 = mod1Mask
        }
