import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
import XMonad.Util.EZConfig
import System.IO

baseConfig = desktopConfig

main = xmonad baseConfig
	{ terminal        = "urxvt"
	, modMask         = mod4Mask
	, handleEventHook = fullscreenEventHook
	, borderWidth     = 1
	}
