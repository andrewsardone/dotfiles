A collection of [`launchd` property list files][1] for custom daemons that I
run on macOS.

## Install via `launchd`

Example:

```bash
ln -s ~/.dotfiles/osx/LaunchAgents/com.andrewsardone.kitty-instance.plist ~/Library/LaunchAgents/com.andrewsardone.kitty-instance.plist
launchctl load ~/Library/LaunchAgents/com.andrewsardone.kitty-instance.plist
```

[1]: https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html
