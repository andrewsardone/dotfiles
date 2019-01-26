#!/usr/bin/env osascript

-- https://thesweetsetup.com/working-email-urls-macos/
tell application "Mail"
    set selectedMessages to selection
    set theMessage to item 1 of selectedMessages
    set messageid to message id of theMessage
    -- Make URL (must use URL-encoded values for "<" and ">")
    set messageidEncoded to do shell script "php -r 'echo urlencode(\"" & messageid & "\");'"
    set urlText to "message://" & "%3C" & messageidEncoded & "%3E"
    set the clipboard to urlText
end tell
