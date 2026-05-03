# LaunchBar configuration and Spotlight shortcut remap.
#
# Remaps Spotlight's Cmd+Space to Option+Space so LaunchBar can use
# Cmd+Space. LaunchBar's main activation shortcut (Cmd+Space) must be
# set manually in the app since it's not stored in standard defaults.
{ ... }:
{
  system.defaults.CustomUserPreferences = {
    "com.apple.symbolichotkeys" = {
      AppleSymbolicHotKeys = {
        "64" = {
          enabled = true;
          value = {
            parameters = [ 32 49 524288 ];
            type = "standard";
          };
        };
      };
    };
    "at.obdev.LaunchBar" = {
      ShowDockIcon = 0;
    };
  };
}
