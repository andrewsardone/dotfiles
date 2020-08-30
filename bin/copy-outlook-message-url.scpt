#!/usr/bin/env osascript
# Usage: ./copy-outlook-message-url.scpt
#
# Grabs the currently selected message ID in Outlook and
# places an outlook:// URI onto your clipboard for later
# reopening. Pairs with `Pass to OutlookUriHandler.app` to
# register the outlook:// URI and forward opening of the
# message within Outlook.app.
#
# Via http://blog.hakanserce.com/post/outlook_automation_mac/

tell application "Microsoft Outlook"
	set selectedMessages to selected objects
	if selectedMessages is {} then
		display notification "Please select a message in Outlook before running the script."
	else
		set messageId to id of item 1 of selectedMessages
		set uri to "outlook://" & messageId
		set the clipboard to uri
		display notification "URI " & uri & " copied to clipboard"
	end if
end tell
