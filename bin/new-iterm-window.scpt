#!/usr/bin/env osascript

set appName to "iTerm"

if application appName is running then
	tell application "iTerm"
		create window with default profile
	end tell
else
	tell application appName to activate
end if
