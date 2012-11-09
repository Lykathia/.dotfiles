import System.IO
import System.Exit

import XMonad

import Data.Char (chr)
import Data.List (isInfixOf)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.Named
import XMonad.Layout.NoBorders (noBorders, smartBorders, withBorder)
import XMonad.Layout.Tabbed

import XMonad.Util.Run (hPutStrLn, spawnPipe)

---------------------------------------------------------------------
--  Dzen Bars / Trayer
---------------------------------------------------------------------

-- Dzen
dzenCmd :: String
dzenCmd = "dzen2 -ta 'l'"

delim :: Char
delim = chr 127

-- Formatting for dzen
myDzenPP :: Handle -> PP
myDzenPP h = dzenPP
    {   ppCurrent           = dzenColor "#333333" "#339933" . pad
    ,   ppVisible           = dzenColor "#333333" "#3399FF" . pad
    ,   ppHidden            = dzenColor "#EEEEEE" "#444444" . pad
    ,   ppHiddenNoWindows   = dzenColor "#999999" "#333333" . pad
    ,   ppUrgent            = dzenColor "#333333" "#CCCC33" . pad
    ,   ppSep               = "|"
    ,   ppWsSep             = "|"
    ,   ppTitle             = dzenColor "#66CC66" "#333333" . pad
    ,   ppOrder             = \(ws:l:t:_) -> [l,ws,t]
    ,   ppLayout            = dzenColor "#EEEEEE" "#3939EE" . pad
    ,   ppOutput            = (\x -> hPutStrLn h $ formatStatus x)
    }
    where
        formatStatus s = concat $ map filterDividers $ strSplit delim s

mkSolidDivider :: String -> String -> Int -> String
mkSolidDivider _ _ 0 = ""
mkSolidDivider fg bg n = colorLead $ concat $ map mkRect $ map scaleSizes sizes
  where
    sizes = [0..(n-1)]
    scaleSizes = (\x -> n*3 - x*3)
    mkRect = (\x -> "^r(1x" ++ show (x) ++ ")")
    colorLead rects = "^pa(;0)^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")" ++ rects ++ "^pa(;2)"

mkLineDivider :: String -> Int -> String
mkLineDivider _ 0 = ""
mkLineDivider color n = colorLine $ concat $ map mkRect $ map scaleSizes sizes
  where
    sizes = [0..(n-1)]
    scaleSizes = (\x -> (n-1)*3 - x*3)
    mkRect = (\x -> "^pa(;" ++ show x ++ ")^r(1x3)")
    colorLine rects = "^fg(" ++ color ++ ")" ++ rects ++ "^pa(;2)"

strSplit :: Char -> String -> [String]
strSplit _ "" = []
strSplit c str = case dropWhile (==c) str of
    "" -> []
    str' -> w : strSplit c str''
        where (w, str'') = break (== c) str'

filterDividers :: String -> String
filterDividers str
    | length str == 15 && ['|'] `isInfixOf` str = makeDivider $ strSplit '|' str
    | otherwise = str
    where
        makeDivider (fg:bg:_)
            | fg /= bg = mkSolidDivider fg bg 5
            | otherwise = mkLineDivider "#666666" 5

-- Workspace Names
myWorkspaces = ["1:main", "2:web", "3:chat", "4:code", "5:code"] ++ map show [6..8] ++ ["9:misc"]

---------------------------------------------------------------------
--  Colours / Fonts / Appearance
---------------------------------------------------------------------

barFont             = "inconsolata"
barXFont            = "inconsolata:size=12"
xftFont             = "xft: inconsolata-14"
myFocusedBorder     = "#33cc33"
myUnfocusedBorder   = "#333333"

myTabTheme = defaultTheme
    {   fontName            = barXFont
        --activeColor         = ,
        --inactiveColor       = ,
        --activeBorderColor   = ,
        --inactiveBorderColor = ,
        --activeTextColor     = ,
        --inactiveTextColor   = 
    }

---------------------------------------------------------------------
--  Hooks
---------------------------------------------------------------------

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "steam"          --> doFloat <+> doShift "1:main"
    , className =? "Xfce4-notifyd"  --> doF W.focusDown ]

---------------------------------------------------------------------
--  Layouts
---------------------------------------------------------------------
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Nnoell%27s_xmonad.hs

-- Hook
{-myLayout = avoidStruts 
    $ gaps [(U,16), (D,16), (L,0), (R,0)]
    $ layoutHints 
    $ smartBorders
    $ allLayouts
    where
        allLayouts  = myTabs
        --webLayouts  =
        --codeLayouts = 
        --chatLayouts = 
-}

-- Layouts
{-
myTabs  = named "TS"    $ smartBorders $ tabbed shrinkText myTabTheme
-}
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

--------------------------------------------------------------------
--  Keybindings
---------------------------------------------------------------------

--  Toggle Dzen
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = mMask} = (mMask, xK_b)

--  Keyboard Bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
 
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
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
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
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall trayer nm-applet; xmonad --recompile; xmonad --restart")
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

---------------------------------------------------------------------
--  Startup Commands
---------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --transparent true --alpha 0 --tint 0x0c0c0b --height 18 --monitor 1"
    spawn "nm-applet"

---------------------------------------------------------------------
--  XMonad Main
---------------------------------------------------------------------

main :: IO ()
main = do
    h <- spawnPipe dzenCmd
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        {   terminal                = "urxvt"
        ,   workspaces              = myWorkspaces

        -- Hooks / Layouts
        ,   layoutHook              = myLayout
        ,   manageHook              = (doF W.swapDown) <+> myManageHook
        --,   handleEventHook         = myEventHook
        ,   startupHook             = myStartupHook
        ,   logHook                 = do
                dynamicLogWithPP $ myDzenPP h
                fadeInactiveLogHook 1.0

        -- Graphical stuff
        ,   focusFollowsMouse       = True
        ,   borderWidth             = 0
        ,   normalBorderColor       = myUnfocusedBorder
        ,   focusedBorderColor      = myFocusedBorder

        -- Bindings
        ,   keys                    = myKeys
        ,   modMask                 = mod1Mask
        }
